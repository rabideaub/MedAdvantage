libname dat "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data";
libname med "/schaeffer-b/sch-protected/VERTICAL-INTEGRATION/Data/2009-13_PAC/Raw/MedPAR";
libname hosp "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data/POS";
libname hc "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data/HospitalCompare";

/*******************************************************************************
DATA PREP
*******************************************************************************/
proc sort data=dat.nber_ime_dsh_2011 out=nber_ime_dsh_2011 nodupkey; by prvdr_num; run;
proc sort data=dat.nber_ime_dsh_2012 out=nber_ime_dsh_2012 nodupkey; by prvdr_num; run;
proc sort data=dat.nber_ime_dsh_2013 out=nber_ime_dsh_2013 nodupkey; by prvdr_num; run;

data nber_ime_dsh (keep=ime1 dsh1 prvdr_num year rename=(prvdr_num=provid));
	length prvdr_num $6 year $4;
	set nber_ime_dsh_2011 (in=a)
		nber_ime_dsh_2012 (in=b)
		nber_ime_dsh_2013 (in=c);
	if a then year='2011';
	if b then year='2012';
	if c then year='2013';
run;


/*Calculate total yearly hospital discharges using MedPAR*/
	data medpar (rename=(prvdrnum=provid));
		length prvdrnum $6 year $4;
		set med.medpar2011 (in=a keep=prvdrnum DRG_CD)
			med.medpar2012 (in=b keep=prvdrnum DRG_CD)
			med.medpar2013 (in=c keep=prvdrnum DRG_CD);
		if a then year='2011';
		if b then year='2012';
		if c then year='2013';
		counter=1;
	 index_condition = "other";
	 if DRG_CD in("291","292","293") then index_condition = "hf" ;  
	 if DRG_CD in("469","470") then index_condition = "jr" ;  
	 if DRG_CD in("177","178","179","193","194","195") then index_condition ="pn" ;  
	 if DRG_CD in("190","191","192","202","203") then index_condition = "cd"  ; 
	 if DRG_CD in("533","534","535","536") then index_condition = "ff" ;  
	 if DRG_CD in("480","481","482") then index_condition = "hp" ; 
	 if DRG_CD in("061","062","063","064","065","066") then index_condition = "sk" ; 
	 if 1<=(input(substr(prvdrnum,length(prvdrnum)-3,4),?? 4.))<=899;
	run;

	proc sort data=nber_ime_dsh nodupkey; by provid year; run;
	proc sort data=medpar; by provid year; run;
			
	data hosp_comp;
		merge nber_ime_dsh (in=a)
			  medpar (in=b);
		by provid year;
		if a & b;
	run;

	proc sort data=hosp_comp; by provid year; run;

	data hosp_comp;
		set hosp_comp;
		report_ma=(ime1~=. | dsh1~=.);
		retain tot_disch tot_hf tot_jr tot_pn tot_cd tot_ff tot_hp tot_sk tot_pn tot_other tot_years;
		by provid year;
		if first.provid then do;
			tot_years=0;
		end;
		if first.year then do;
			tot_disch=0;
			tot_hf=0;
			tot_jr=0;
			tot_pn=0;
			tot_cd=0;
			tot_ff=0;
			tot_sk=0;
			tot_pn=0;
			tot_hp=0;
			tot_other=0;
			tot_years+1; /*Total number of years a hospital reports in*/
		end;
		tot_disch=sum(tot_disch,counter);
		%macro loop(cond);
			if index_condition="&cond." then tot_&cond.=sum(tot_&cond.,counter);
		%mend;
		%loop(hf); %loop(jr); %loop(pn); %loop(cd); %loop(ff); %loop(hp); %loop(sk); %loop(pn); %loop(other);
		all3=(tot_years=3);
		if last.year then output;
	run;

	proc freq data=hosp_comp;
		tables report_ma*year report_ma;
	run;

	proc freq data=hosp_comp;
		tables year tot_years all3 all3*report_ma;
		where year='2013';
	run;

	/*Merge on hospital characteristics from the impact files and POS files*/
	proc sort data=hosp.pos_other_dec2011_2013 out=pos; by prvdr_num pos_other_dec_year; run;
	proc sort data=hosp.cms_impact2011_2013 out=impact; by provider_number cms_impact_year; run;

	data pos;
		set pos;
		year=put(pos_other_dec_year,4.);
		provid=put(input(prvdr_num,?? 8.),z6.);
	run;

	proc sort data=pos; by provid year; run;

	data impact;
		set impact;
		year=put(cms_impact_year,4.);
		provid=put(provider_number*1,z6.);
	run;

	proc print data=impact (obs=10); run;

	proc sort data=impact; by provid year; run;

	data hc (drop=provider_id);
		set hc.hosp_compare_general (keep=provider_id Hospital_Ownership);
		provid=put(provider_id*1,?? z6.);
		for_profit=index(upcase(Hospital_Ownership),"PROPRIETARY")>0;
		non_profit=index(upcase(Hospital_Ownership),"NON-PROFIT")>0;
		gov_owned=index(upcase(Hospital_Ownership),"GOVERNMENT")>0;
		physician_owned=index(upcase(Hospital_Ownership),"PHYSICIAN")>0;
	run;

	proc sort data=hc nodupkey; by provid; run;

	data hosp_comp;
		merge hosp_comp (in=a)
			  pos (in=b);
		by provid year;
		if a;
	run;

	data hosp_comp;
		merge hosp_comp (in=a)
			  impact (in=b);
		by provid year;
		if a;
		match=(a & b);
	run;

	proc contents data=hosp_comp; run;
	proc print data=hosp_comp (obs=10); run;

	proc freq data=hosp_comp;
		tables match;
	run;
	proc contents data=hosp_comp; run;
	proc print data=hosp_comp (obs=10); run;

	data hosp_comp;
		set hosp_comp;
	run;

	data hosp_comp;
		merge hosp_comp (in=a)
			  hc (in=b);
		by provid;
		if a;
	run;

	proc freq data=hosp_comp;
		tables report_ma*(hospital_ownership for_profit non_profit gov_owned);
	run;

	/*Read in the data and quantify categorical variables*/
	data hosp_comp;
		set hosp_comp;
		if cmiv30~=. then cmiv=cmiv30;
		else if cmiv29~=. then cmiv=cmiv29;
		else cmiv=cmiv28;
		urban=(index(upcase(urspa),"RURAL")=0);
		teaching=(resident_to_bed_ratio~=. & resident_to_bed_ratio>0);
		adc=input(average_daily_census,?? 8.);
		medicare_pct=input(mcr_pct,?? 8.);
		/*Create ownership indicator variables*/
		NONPROFIT_IND = 0;
		PROFIT_IND = 0;
		GOVT_IND = 0;
		if GNRL_CNTL_TYPE_CD in("04","05","06") then NONPROFIT_IND = 1 ;
		if GNRL_CNTL_TYPE_CD in("01","02","03","13") then PROFIT_IND = 1 ;
		if GNRL_CNTL_TYPE_CD in("07","08","09","10","11","12") then GOVT_IND = 1 ;
		  /* 01=FOR PROFIT - INDIVIDUAL
             02=FOR PROFIT - PARTNERSHIP
             03=FOR PROFIT - CORPORATION
             04=NONPROFIT - CHURCH RELATED
             05=NONPROFIT - CORPORATION
             06=NONPROFIT - OTHER
             07=GOVERNMENT - STATE
             08=GOVERNMENT - COUNTY
             09=GOVERNMENT - CITY
             10=GOVERNMENT - CITY/COUNTY
             11=GOVERNMENT - HOSPITAL DISTRICT
             12=GOVERNMENT - FEDERAL
             13=FOR PROFIT - LIMITED LIABILITY CORPORATION*/
	run;

	proc freq data=hosp_comp;
		tables Hospital_Ownership report_ma*(for_profit non_profit gov_owned);
	run;

	/*Keep only the most recent year for each hospital*/
	proc sort data=hosp_comp; by provid descending year; run;

	data hosp_comp;
		set hosp_comp;
		by provid;
		counter=1;
		if first.provid;
	run;

	proc freq data=hosp_comp;
		tables report_ma;
	run;

/*******************************************************************************
ANALYSIS
*******************************************************************************/
/*This macro makes the ttest output a single line dataset with means and ttest*/
%macro ttest2(var);
	ods output  "Statistics" = stats
	 			"T-Tests" = ttests;
	proc ttest data=hosp_comp;
		class report_ma;
		var &var.;
	run;
	ods output close;

	data stats (keep = variable class MA_Reporting Non_Reporting);
		set stats(rename=(mean=avg));
		if trim(left(class)) = '0' then Non_Reporting = avg;
		if trim(left(class)) = '1' then MA_Reporting = avg; 
	run;

	 data Non_Reporting(drop = class MA_Reporting)
	 	  MA_Reporting (drop = class Non_Reporting);
	 	set stats;
	 	if trim(left(class)) = '0' then output Non_Reporting;
	 	if trim(left(class)) = '1' then output MA_Reporting;
	 run;

	 proc sort data = Non_Reporting; by variable; run;
	 proc sort data = MA_Reporting; by variable; run;

	 data stats_final;
	 	merge Non_Reporting
		   	  MA_Reporting;
	 	by variable;
	 run;

	 data ttest (keep=variable probt rename=(probt=Significance));
	 	set ttests;
		if trim(left(Method))="Pooled";
	run;

	proc print data=ttest; run;
	proc sort data=ttest; by variable; run;

	data mean_&var.;
		length variable $ 50;
		merge stats_final
			  ttest;
		by variable;
		variable="&var.";
	run;

	proc print data=mean_&var.; run;
%mend;
%ttest2(tot_disch);
%ttest2(tot_hf);
%ttest2(tot_jr);
%ttest2(tot_hp);
%ttest2(tot_sk);
%ttest2(tot_hf);
%ttest2(tot_ff);
%ttest2(tot_cd);
%ttest2(tot_pn);
%ttest2(tot_other);
%ttest2(bed_cnt);
%ttest2(adc);
%ttest2(urban);
%ttest2(teaching);
%ttest2(cmiv);
%ttest2(NONPROFIT_IND);
%ttest2(PROFIT_IND);
%ttest2(GOVT_IND);
%ttest2(non_profit);
%ttest2(for_profit);
%ttest2(gov_owned);
%ttest2(physician_owned);
%ttest2(medicare_pct);

data mean_all;
	set mean_:;
run;

data table_all;
	length Variable $50;
	set mean_all;
	if trim(left(Variable))="tot_disch" then Variable="Average Discharges";
	if trim(left(Variable))="tot_hf" then Variable="Average Heart Failure Discharges";
	if trim(left(Variable))="tot_jr" then Variable="Average LEJR Discharges";
	if trim(left(Variable))="tot_hp" then Variable="Average Hip and Femur Procedure Discharges";
	if trim(left(Variable))="tot_cd" then Variable="Average COPD Discharges";
	if trim(left(Variable))="tot_ff" then Variable="Average Femur Fracture Discharges";
	if trim(left(Variable))="tot_sk" then Variable="Average Stroke Discharges";
	if trim(left(Variable))="tot_pn" then Variable="Average Pneumonia Discharges";
	if trim(left(Variable))="tot_other" then Variable="Average 'Other' Discharges";
	if trim(left(Variable))="bed_cnt" then Variable= "Average Hospital Beds";
	if trim(left(Variable))="adc" then Variable= "Average Daily Census";
	if trim(left(Variable))="cmiv" then Variable= "Average Hospital Case Mix Index";
	if trim(left(Variable))="urban" then Variable= "% Urban Hospitals";
	if trim(left(Variable))="teaching" then Variable= "% Teaching Hospitals";
	if trim(left(Variable))="NONPROFIT_IND" then Variable= "% Non Profit Hospitals";
	if trim(left(Variable))="PROFIT_IND" then Variable= "% For Profit Hospitals";
	if trim(left(Variable))="GOVT_IND" then Variable= "% Gov't Owned Hospitals";
	if trim(left(Variable))="non_profit" then Variable= "% Non Profit Hospitals (HC)";
	if trim(left(Variable))="for_profit" then Variable= "% For Profit Hospitals (HC)";
	if trim(left(Variable))="gov_owned" then Variable= "% Gov't Owned Hospitals (HC)";
	if trim(left(Variable))="physician_owned" then Variable= "% Physician Owned Hospitals (HC)";
	if trim(left(Variable))="medicare_pct" then Variable= "% Medicare Days";
run;

proc sort data=table_all; by variable; run;
/*proc print data=freq_all; run;*/
proc print data=table_all; run;

proc sort data=hosp_comp; by year; run;

data mdcr_disch;
	set hosp_comp end=eof;
	retain ma_mdcr_disch non_mdcr_disch;
	if report_ma=1 then ma_mdcr_disch=sum(ma_mdcr_disch,bills);
	if report_ma=0 then non_mdcr_disch=sum(non_mdcr_disch,bills);
	if eof then do;
		MA_Reporting=ma_mdcr_disch/(ma_mdcr_disch+non_mdcr_disch);
		Non_Reporting=non_mdcr_disch/(ma_mdcr_disch+non_mdcr_disch);
		Variable="% Total Medicare Discharges";
		output;
	end;
run;

proc print data=mdcr_disch;
	var Variable MA_Reporting Non_Reporting ma_mdcr_disch non_mdcr_disch;
run;

data table_all;
	set table_all
		mdcr_disch (keep=Variable MA_Reporting Non_Reporting);
run;

proc print data=table_all; run;
























