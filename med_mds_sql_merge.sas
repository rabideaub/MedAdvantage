libname proc "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Processed";
libname pos "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/POS";
libname raw "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/PAC/Data/Raw/MedPAR";
libname temp "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/Temp";
%let out=/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Output;
libname out "&out.";

/******************************************
 DATA PREP
******************************************/

/*Keep MedPAR hospitalizations from STACHs that report Medicare Advantage claims*/
data med ;
	set proc.med2009_2013 /*(keep=bene_id PRVDR_NUM admsn_dt dschrg_dt report_ma year ma_claim index_condition)*/;
	if 1<=(input(substr(PRVDR_NUM,length(PRVDR_NUM)-3,4),?? 4.))<=899 & report_ma=1 & year<2011;
run;

proc freq data=med;
	tables year;
run;

/*Drop benes if their index hospitalization was preceded by a nursing home stay within 1 day*/
data mds_drop;
	set proc.mds2_2006_2010 (where=(R4_DISCHARGE_DT~=.) keep=R4_DISCHARGE_DT bene_id entry_dt disch_dt);
	drop_obs=1;
run;

proc sort data=med; by bene_id admsn_dt; run;
proc sort data=mds_drop; by bene_id disch_dt; run;

proc sql;
	create table med as
	select * from med as med left join mds_drop as mds
	on med.bene_id = mds.bene_id  and (-1<=(med.admsn_dt-mds.disch_dt)<=1); /*Identifies if a bene was discharged in the MDS +-1 day from index hospitalization*/
quit;

proc freq data=med;
	tables drop_obs*year;
run;

data med (drop=drop_obs R4_DISCHARGE_DT entry_dt disch_dt);
	set med;
	if drop_obs~=1;
run;

/*If a readmission occurs, get the date of the readmission to use as an endpoint for the episode of care of the original admisssion*/
proc sort data=med; by bene_id descending admsn_dt; run;

data med; 
	set med;
	retain readm_dt;
	by bene_id;
	if first.bene_id then do; /*If most recent bene admission, make the end date 30 days after discharge*/
		readm_dt=admsn_dt;
		end_dt=dschrg_dt+30;
	end;
	/*If not the most recent bene admission, make the end date either the subsequent admission date or 30 days after discharge */
	if ~first.bene_id then do; 
		end_dt=min(readm_dt,(dschrg_dt+30));
		readm_dt=admsn_dt;
		if end_dt-dschrg_dt<30 then use_readm=1;
	end;

	format readm_dt end_dt MMDDYY8.;
run;

proc print data=med (obs=100);
	var bene_id admsn_dt dschrg_dt end_dt readm_dt;
run;

data mds (keep=bene_id sort_date mcr_gp a3a_asmt_ref_dt a4a_reentry_dt ab1_entry_dt AA6B_FAC_MCARE_NBR 
			   r2b_complete_dt r4_discharge_dt aa8a_pri_rfa aa8b_spc_rfa entry_dt disch_dt);
	set proc.mds2_2006_2010;
run;

/*Keep all MDS observations that happen within 30days of a STACH discharge, +/- 3 days*/
proc sql;
	create table med_mds as
	select * from med as med left join mds as mds
	on med.bene_id = mds.bene_id  and
	   ((med.dschrg_dt~=. & mds.sort_date between med.dschrg_dt and med.end_dt) or 
		(0<=(mds.sort_date-med.dschrg_dt)<=3) or (0<=(med.dschrg_dt-mds.sort_date)<=3));
quit;

/*If the MDS entry date is before the hospitalization discharge, make entry=discharge */
data med_mds;
	set med_mds;
	if entry_dt~=. & entry_dt<dschrg_dt then entry_dt=dschrg_dt;
	if disch_dt~=. & disch_dt>end_dt & use_readm=1 then disch_dt=end_dt;
run;

proc sort data=med_mds; by bene_id admsn_dt sort_date; run;

title "Check Dates";
proc print data=med_mds (obs=200);
	var bene_id ma_claim admsn_dt dschrg_dt end_dt sort_date mcr_gp a3a_asmt_ref_dt a4a_reentry_dt ab1_entry_dt 
		r2b_complete_dt r4_discharge_dt aa8a_pri_rfa aa8b_spc_rfa entry_dt disch_dt;
run;

proc freq data=med_mds;
	tables aa8b_spc_rfa;
run;

/*Flatten out the Med-MDS file and refine the MDS Entry and Discharge dates using supplemental MedPAR data*/
data med_mds;
	set med_mds;
	retain temp_entry temp_disch disch_flag five_day_assess fourteen_day thirty_day sixty_day mds RUG;
	by bene_id admsn_dt;
	if first.admsn_dt then do;
		disch_flag=0;
		five_day_assess=0;
		fourteen_day=0;
		thirty_day=0;
		sixty_day=0;
		mds=0;
		RUG=0;
		temp_entry=entry_dt;
	end;

	if temp_entry<entry_dt & temp_entry~=. then entry_dt=temp_entry; /*Keep the earliest entry associated with a stay*/
	if ab1_entry_dt>temp_entry then temp_entry=ab1_entry_dt; /*If an entry date is found that comes after STACH disch, set that as entry*/
	if a4a_reentry_dt>temp_entry then temp_entry=a4a_reentry_dt; /*Reentry dates take precedence over entry dates*/
	if r4_discharge_dt~=. then temp_disch=r4_discharge_dt;

	if trim(left(aa8b_spc_rfa))="1" | trim(left(aa8b_spc_rfa))="01" then five_day_assess=1; /*Retain certain medicare assessments*/
	if trim(left(aa8b_spc_rfa))="7" | trim(left(aa8b_spc_rfa))="07" then fourteen_day=1;
	if trim(left(aa8b_spc_rfa))="2" | trim(left(aa8b_spc_rfa))="02" then thirty_day=1;
	if trim(left(aa8b_spc_rfa))="3" | trim(left(aa8b_spc_rfa))="03" then sixty_day=1;
	if sort_date~=. then mds=1;
	if index(mcr_gp,"*")=0 & mcr_gp~='' then RUG=1;
	if last.admsn_dt then output;
	format temp_entry temp_disch MMDDYY8.;
run;

proc freq data=med_mds;
	tables year mds RUG five_day_assess fourteen_day thirty_day;
run;

/*Merge on the POS file*/
proc sort data=med_mds; by AA6B_FAC_MCARE_NBR; run;

data med_mds;
	merge med_mds (in=a)
		  pos.pos_snf_dec08 (in=b);
	by AA6B_FAC_MCARE_NBR;
	if a;
run;

data med_mds;
	set med_mds;
	if disch_dt~=. & entry_dt~=. & disch_dt>=entry_dt then mds_los=disch_dt-entry_dt;
run;

proc sort data=med_mds out=proc.med_mds; by bene_id admsn_dt sort_date; run;

title "Check Collapsed";
proc print data=proc.med_mds (obs=100);
	var bene_id ma_claim admsn_dt dschrg_dt end_dt sort_date entry_dt disch_dt temp_entry temp_disch five_day_assess;
run;
title;




%macro out;

/******************************************
 SUMMARY STATS
******************************************/
%macro by_cond(cond);
	/*Frequencies and Chi-Squared Tests*/
	%macro freqs(var);
		proc freq data=proc.med_mds;
			tables &var.*ma_claim/chisq out=has_&var. outpct;
			%if "&var."~="mds" & "&var."~="RUG" %then %do;
				where mds=1 & index(index_condition,"&cond.")>0;
			%end;
			%if "&var."="mds" | "&var."="RUG" %then %do;
				where index(index_condition,"&cond.")>0;
			%end;
			output out=chisqdat pchi;
		run;

		proc print data=has_&var.; run;

		data freq_ma (keep=&var. PCT_COL rename=(PCT_COL=MA_Claims))
			 freq_ffs(keep=&var. PCT_COL rename=(PCT_COL=FFS_Claims));
			set has_&var.;
			if &var.=1 & ma_claim=1 then output freq_ma;
			if &var.=1 & ma_claim=0 then output freq_ffs;
		run;

		data freqs_&var. (drop= &var.);
			merge freq_ma
				  freq_ffs;
			by &var.;
			Variable="&var.";
		run;

		data frequencies_&var.(drop=_PCHI_ DF_PCHI rename=(p_pchi=Significance));
			merge freqs_&var.
				  chisqdat;
		run;

		proc print data=freqs_&var.; run;
	%mend;
	%freqs(mds);
	%freqs(RUG);
	%freqs(five_day_assess);
	%freqs(fourteen_day);
	%freqs(thirty_day);
	%freqs(sixty_day);

	data all_freqs_&cond.;
		set frequencies_:;
	run;

	title "Frequencies for &cond.";
	proc print data=all_freqs_&cond.; run;
	title;






/*Average Length of Stay and T-Tests*/
	title "All LOS";
	ods output  "Statistics" = stats
	 			"T-Tests" = ttests;
	proc ttest data=proc.med_mds;
		class ma_claim;
		var mds_los;
		where index(index_condition,"&cond.")>0;
	run;
	ods output close;

	/*This macro makes the ttest output a single line dataset with means and ttest*/
	%macro ttest(name,text,varname);
		data stats (keep = variable class MA_Claims FFS_Claims);
			set stats(rename=(mean=avg));
			if trim(left(class)) = '0' then FFS_Claims = avg;
			if trim(left(class)) = '1' then MA_Claims = avg; 
		run;

		 data FFS(drop = class MA_Claims)
		 	  MA (drop = class FFS_Claims);
		 	set stats;
		 	if trim(left(class)) = '0' then output FFS;
		 	if trim(left(class)) = '1' then output MA;
		 run;

		 proc sort data = FFS; by variable; run;
		 proc sort data = MA; by variable; run;

		 data stats_final;
		 	merge FFS
			   	  MA;
		 	by variable;
		 run;

		 data ttest (keep=variable probt);
		 	set ttests;
			if trim(left(Method))="Pooled";
		run;

		proc print data=ttest; run;

		proc sort data=ttest; by variable; run;

		data los_&name.;
			length Description $300 variable $ 50;
			merge stats_final
				  ttest;
			by variable;
			variable=&varname.;
			Description=&text.;
		run;

		proc print data=los_&name.; run;
	%mend;
	%ttest(name=all_mds,text="Mean LoS for all patients in the MDS. This stat captures both SNF and NH", varname="MDS Length of Stay");
	 

	title "If RUG LOS";
	ods output  "Statistics" = stats
	 			"T-Tests" = ttests;
	proc ttest data=proc.med_mds;
		class ma_claim;
		var mds_los;
		where RUG=1 & index(index_condition,"&cond.")>0;
	run;
	ods output close;
	%ttest(name=RUG,text="Mean LoS for patients in the MDS that have a RUG. Reasoning is that if you have a RUG, you are in a SNF",
		   varname="SNF Length of Stay (RUG)");


	title "All LOS, 100 day max";
	ods output  "Statistics" = stats
	 			"T-Tests" = ttests;
	proc ttest data=proc.med_mds;
		class ma_claim;
		var mds_los;
		where mds_los<100 & index(index_condition,"&cond.")>0;
	run;
	ods output close;
	%ttest(name=day_100,text="Mean LoS for patients in the MDS that have a total stay of less than 100 days. 
							  Reasoning is that stays greater than 100 days indicate presence in a NH",
		   varname="SNF Length of Stay (100 day max)");

	title "Provider Type=4 LOS";
	ods output  "Statistics" = stats
	 			"T-Tests" = ttests;
	proc ttest data=proc.med_mds;
		class ma_claim;
		var mds_los;
		where PROVIDER_TYPE=4 & index(index_condition,"&cond.")>0;
	run;
	ods output close;
	%ttest(name=provtype, text="Mean LoS for patients in the MDS who are in a facility labelled '04' in the 
								Provider of Service file. Value 04 indicates a SNF that is not a dual SNF/NH. 
								Only 10% of providers are value 04",
		   varname="SNF Length of Stay (POS file)");
	title;


	/*Calculate the LOS for SNFs in MedPAR - only FFS claims will be consistently visible*/
	data medpar_snf;
		set raw.medpar2009 (keep=PRVDRNUM admsndt dschrgdt bene_id 
							rename=(admsndt=snf_admsndt dschrgdt=snf_dschrgdt))
			raw.medpar2010 (keep=PRVDRNUM admsndt dschrgdt bene_id 
							rename=(admsndt=snf_admsndt dschrgdt=snf_dschrgdt));

		if 5000<=(input(substr(PRVDRNUM,length(PRVDRNUM)-3,4),?? 4.))<=6499 & snf_admsndt~=. & snf_dschrgdt~=.;
		snf_los=snf_dschrgdt-snf_admsndt;
	run;

	data medpar2009; /*Keep only non-ma claims from ma_reporting hospitals*/
		set proc.med2009_2013 (keep=PRVDR_NUM admsn_dt dschrg_dt bene_id index_condition ma_claim report_ma);
		if 1<=(input(substr(PRVDR_NUM,length(PRVDR_NUM)-3,4),?? 4.))<=899 & report_ma=1 & ma_claim=0 & year<2011;
		enddt=dschrg_dt+3;
	run;

	proc sql;
	create table med_snf as
	select * from medpar2009 as med left join medpar_snf as snf
	on med.bene_id = snf.bene_id  and
	   ((med.dschrg_dt~=. & snf.snf_admsndt between med.dschrg_dt and med.enddt)); /*SNF admission within 3 days of index discharge*/
	quit;

	title "MedPAR SNF LOS";
	proc means data=med_snf;
		var snf_los;
		where index(index_condition,"&cond.")>0;
		output out=los_medpar;
	run;
	title;

	proc print data=los_medpar; run;

	data los_medpar (keep=variable description snf_los rename=(snf_los=FFS_Claims));
		set los_medpar;
		if trim(left(_STAT_))="MEAN";
		variable="SNF Length of Stay (MedPAR)";
		description="Mean LoS for FFS patients only as seen in the MedPAR file.";
	run;

	/*Append all of the ttest datasets together to form a table*/
	data ma_ffs_comp_&cond. (rename=(probt=Significance));
		set los_:;
	run;

	data ma_ffs_comp_&cond.;
		set ma_ffs_comp_&cond.
			all_freqs_&cond.;
	run;

	proc print data=ma_ffs_comp_&cond.; run;
%mend;
%by_cond(CHF);
%by_cond(Other);
%by_cond(LEJR);
%by_cond(Pneumonia);
%by_cond(COPD);
%by_cond(Fracture);
%by_cond(Hip);
%by_cond(Stroke);

/************************************************************************************
PRINT CHECK
************************************************************************************/
/*Output a sample of each of the datasets to an excel workbook*/
ods tagsets.excelxp file="&out./med_adv_comp.xml" style=sansPrinter;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="CHF" frozen_headers='yes');
proc print data=ma_ffs_comp_chf (obs=1000);
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Other" frozen_headers='yes');
proc print data=ma_ffs_comp_other (obs=1000);
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="LEJR" frozen_headers='yes');
proc print data=ma_ffs_comp_LEJR (obs=1000);
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Pneumonia" frozen_headers='yes');
proc print data=ma_ffs_comp_Pneumonia (obs=1000);
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="COPD" frozen_headers='yes');
proc print data=ma_ffs_comp_COPD (obs=1000);
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Femur Fracture" frozen_headers='yes');
proc print data=ma_ffs_comp_Fracture (obs=1000);
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Hip/Femur" frozen_headers='yes');
proc print data=ma_ffs_comp_Hip (obs=1000);
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Stroke" frozen_headers='yes');
proc print data=ma_ffs_comp_Stroke (obs=1000);
run;
ods tagsets.excelxp close;
/*********************
CHECK END
*********************/
%mend;


