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
data med;
	set proc.med2009_2013 /*(keep=bene_id PRVDR_NUM admsn_dt dschrg_dt report_ma year ma_claim index_condition)*/;
	if 1<=(input(substr(PRVDR_NUM,length(PRVDR_NUM)-3,4),?? 4.))<=899 & report_ma=1 & year>=2011;
run;

/*Drop benes if their index hospitalization was preceded by a nursing home stay within 1 day*/
data mds_drop;
	set proc.mds3_2011_2013 (where=(first_case=1) keep=first_case final_dt1 bene_id entry_dt disch_dt);
	drop_obs=1;
run;

proc sort data=med; by bene_id admsn_dt; run;
proc sort data=mds_drop; by bene_id final_dt1; run;

proc sql;
	create table med as
	select * from med as med left join mds_drop as mds
	on med.bene_id = mds.bene_id  and ((0<=(med.admsn_dt-mds.final_dt1)<=3) /*Identifies if a bene was discharged in the MDS within 3days from index hospitalization*/
	or (med.admsn_dt between mds.entry_dt and mds.final_dt1)); /*Identifies is a hospitalization happened in the middle of an MDS stay and the MDS disch wasn't recorded*/
quit;

data med (drop=first_case A2000_DSCHRG_DT /*entry_dt disch_dt final_dt1*/);
	set med;
	if drop_obs=1 then drop_obs=admsn_dt-final_dt1;
	if entry_dt<=admsn_dt<=final_dt1 then drop_obs=0; /*If a hospitalization happened in the middle of a nursing home stay, 
														we assume the bene went straight from the nursing home to the hospital with 0 days in between*/
run;

proc print data=med (obs=50);
	var bene_id entry_dt final_dt1 admsn_dt dschrg_dt drop_obs;
	where drop_obs>3; 
run;

proc freq data=med;
	tables drop_obs;
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

data mds (keep=bene_id trgt_dt entry_dt disch_dt snf_entry_dt snf_disch_dt c_mdcr_rug3_idx_max_grp_txt a0310b_pps_cd
			   first_case final_dt1 first_rug first_cog last_cog disch_cd rug five_day_assess a0310a_fed_obra_cd 
			   C_MDCR_RUG4_HIRCHCL_GRP_TXT);
	set proc.mds3_2011_2013;
run;

/*Keep all MDS observations that happen within 30days of a STACH discharge, +/- 3 days*/
proc sql;
	create table med_mds as
	select * from med as med left join mds as mds
	on med.bene_id = mds.bene_id  and
	   ((med.dschrg_dt~=. & mds.trgt_dt between med.dschrg_dt and med.end_dt) or 
		(0<=(mds.trgt_dt-med.dschrg_dt)<=3) or (0<=(med.dschrg_dt-mds.trgt_dt)<=3));
quit;

/*If the MDS entry date is before the hospitalization discharge, make entry=discharge */
data med_mds;
	set med_mds;
	if entry_dt~=. & entry_dt<dschrg_dt then entry_dt=dschrg_dt;
	if snf_entry_dt~=. & snf_entry_dt<dschrg_dt then snf_entry_dt=dschrg_dt;
	if disch_dt~=. & disch_dt>end_dt & use_readm=1 then disch_dt=end_dt;
	if snf_disch_dt~=. & snf_disch_dt>end_dt & use_readm=1 then snf_disch_dt=end_dt;
run;

proc sort data=med_mds; by bene_id admsn_dt trgt_dt; run;

title "Check Dates";
proc print data=med_mds (obs=200);
	var bene_id ma_claim admsn_dt dschrg_dt end_dt trgt_dt c_mdcr_rug3_idx_max_grp_txt a0310b_pps_cd entry_dt disch_dt snf_entry_dt snf_disch_dt;
run;

proc freq data=med_mds;
	tables a0310b_pps_cd;
run;

/*Flatten out the Med-MDS file and refine the MDS Entry and Discharge dates using supplemental MedPAR data*/
data med_mds (drop=temp_entry temp_snf_entry);
	set med_mds;
	retain five_day_assess fourteen_day thirty_day sixty_day mds RUG RUG4 snf temp_entry temp_snf_entry;
	by bene_id admsn_dt;
	if first.admsn_dt then do;
		five_day_assess=0;
		fourteen_day=0;
		thirty_day=0;
		sixty_day=0;
		qtr_assess=0;
		mds=0;
		RUG=0;
		snf=0;
		temp_entry=entry_dt;
		temp_snf_entry=snf_entry_dt;
		RUG4='';
	end;

	if temp_entry<entry_dt & temp_entry~=. then entry_dt=temp_entry; /*Keep the earliest entry associated with a stay*/
	if temp_snf_entry<snf_entry_dt & temp_snf_entry~=. then snf_entry_dt=temp_snf_entry; /*Keep the earliest entry associated with a stay*/
	if trim(left(a0310b_pps_cd))="1" | trim(left(a0310b_pps_cd))="01" then five_day_assess=1; /*Retain certain medicare assessments*/
	if trim(left(a0310b_pps_cd))="2" | trim(left(a0310b_pps_cd))="02" then fourteen_day=1;
	if trim(left(a0310b_pps_cd))="3" | trim(left(a0310b_pps_cd))="03" then thirty_day=1;
	if trim(left(a0310b_pps_cd))="4" | trim(left(a0310b_pps_cd))="04" then sixty_day=1;
	if trim(left(a0310a_fed_obra_cd))="02" | trim(left(a0310a_fed_obra_cd))="2" then qtr_assess=1;
	if trgt_dt~=. then mds=1;
	if index(c_mdcr_rug3_idx_max_grp_txt,"*")=0 & c_mdcr_rug3_idx_max_grp_txt~='' then RUG=1;
	if RUG4='' then RUG4=C_MDCR_RUG4_HIRCHCL_GRP_TXT; /*Keep the first occurrence of a RUG4 code*/
	if snf_entry_dt~=. then snf=1;
	if last.admsn_dt then output;
run;

proc freq data=med_mds;
	tables mds RUG five_day_assess fourteen_day thirty_day;
run;

					/*Merge on the POS file*/
					%macro out;
					proc sort data=med_mds; by AA6B_FAC_MCARE_NBR; run;

					data med_mds;
						merge med_mds (in=a)
							  pos.pos_snf_dec08 (in=b);
						by AA6B_FAC_MCARE_NBR;
						if a;
					run;
					%mend;

data med_mds;
	set med_mds;
	if disch_dt~=. & entry_dt~=. & disch_dt>=entry_dt then mds_los=disch_dt-entry_dt;
	if snf_disch_dt~=. & snf_entry_dt~=. & snf_disch_dt>=snf_entry_dt then snf_los=snf_disch_dt-snf_entry_dt;
run;

proc sort data=med_mds out=proc.med_mds3_temp; by bene_id admsn_dt trgt_dt; run;

title "Check Collapsed";
proc print data=proc.med_mds3_temp (obs=100);
	var bene_id ma_claim admsn_dt dschrg_dt end_dt trgt_dt entry_dt disch_dt snf_entry_dt snf_disch_dt five_day_assess;
run;
title;

%macro out;
/******************************************
 SUMMARY STATS
******************************************/
%macro by_cond(cond);


	/**********************************
	 Frequencies and Chi-Squared Tests
    ***********************************/
	%macro freqs(var);
		proc freq data=proc.med_mds3_temp;
			tables &var.*ma_claim/chisq out=has_&var. outpct;
			%if "&var."~="mds" & "&var."~="RUG" & "&var."~="snf" %then %do; /*The denom for these measures are anyone with an index discharge and in the MDS*/
				where mds=1 & index(index_condition,"&cond.")>0 & drop_obs~=1;
			%end;
			%if "&var."="mds" | "&var."="RUG" | "&var."="snf" %then %do; /*The denom for these measures are anyone with an index discharge*/
				where index(index_condition,"&cond.")>0 & drop_obs~=1;
			%end;
			output out=chisqdat pchi;
		run;

		proc print data=has_&var.; run;

		/*Rearrange the output so that it's 1 line per measure with means for MA, FFS, and the chisq value*/
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
	%freqs(snf);

	data all_freqs_&cond.;
		set frequencies_:;
	run;

	title "Frequencies for &cond.";
	proc print data=all_freqs_&cond.; run;
	title;



	/**********************************
	 Means and T-Tests
    ***********************************/

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

	/*Take LOS means for all entries in the MDS*/
	title "All LOS";
	ods output  "Statistics" = stats
	 			"T-Tests" = ttests;
	proc ttest data=proc.med_mds3_temp;
		class ma_claim;
		var mds_los;
		where index(index_condition,"&cond.")>0 & drop_obs~=1;
	run;
	ods output close;
	%ttest(name=all_mds,text="Mean LoS for all patients in the MDS. This stat captures both SNF and NH", varname="MDS Length of Stay");
	 

	/*Take LOS means only if at least 1 MDS claim in the stay has a RUG assigned*/
	title "If RUG LOS";
	ods output  "Statistics" = stats
	 			"T-Tests" = ttests;
	proc ttest data=proc.med_mds3_temp;
		class ma_claim;
		var mds_los;
		where RUG=1 & index(index_condition,"&cond.")>0 & drop_obs~=1;
	run;
	ods output close;
	%ttest(name=RUG,text="Mean LoS for patients in the MDS that have a RUG. Reasoning is that if you have a RUG, you are in a SNF",
		   varname="SNF Length of Stay (RUG)");


	/*Take LOS means only if the total LOS is <100 days*/
	title "All LOS, 100 day max";
	ods output  "Statistics" = stats
	 			"T-Tests" = ttests;
	proc ttest data=proc.med_mds3_temp;
		class ma_claim;
		var mds_los;
		where mds_los<100 & index(index_condition,"&cond.")>0 & drop_obs~=1;
	run;
	ods output close;
	%ttest(name=day_100,text="Mean LoS for patients in the MDS that have a total stay of less than 100 days. 
							  Reasoning is that stays greater than 100 days indicate presence in a NH",
		   varname="SNF Length of Stay (100 day max)");

	%macro out2;
					/*Take LOS means only if the provider type is 4 in the POS, indicating strictly SNF, not SNF/NH dual*/
					title "Provider Type=4 LOS";
					ods output  "Statistics" = stats
					 			"T-Tests" = ttests;
					proc ttest data=proc.med_mds3_temp;
						class ma_claim;
						var snf_los;
						where PROVIDER_TYPE=4 & index(index_condition,"&cond.")>0 & drop_obs~=1;
					run;
					ods output close;
					%ttest(name=provtype, text="Mean LoS for patients in the MDS who are in a facility labelled '04' in the 
												Provider of Service file. Value 04 indicates a SNF that is not a dual SNF/NH. 
												Only 10% of providers are value 04",
						   varname="SNF Length of Stay (POS file)");
					title;
	%mend;

	/*Take LOS means for stays labelled with Medicare Entry and Medicare Disch*/
	title "MEDICARE ENTRY AND DISCH DATES";
	ods output  "Statistics" = stats
	 			"T-Tests" = ttests;
	proc ttest data=proc.med_mds3_temp;
		class ma_claim;
		var snf_los;
		where snf=1 & index(index_condition,"&cond.")>0 & drop_obs~=1;
	run;
	ods output close;
	%ttest(name=provtype, text="Mean LoS for patients in the MDS who have a Medicare Entry and Disch Date",
		   varname="SNF Length of Stay (Medicare Entry and Disch file)");
	title;


	/*Calculate the LOS for SNFs in MedPAR - only FFS claims will be consistently visible*/
	data medpar_snf;
		set raw.medpar2013 (keep=PRVDRNUM admsndt dschrgdt bene_id 
							rename=(admsndt=snf_admsndt dschrgdt=snf_dschrgdt))
			raw.medpar2012 (keep=PRVDRNUM admsndt dschrgdt bene_id 
							rename=(admsndt=snf_admsndt dschrgdt=snf_dschrgdt))
			raw.medpar2011 (keep=PRVDRNUM admsndt dschrgdt bene_id 
							rename=(admsndt=snf_admsndt dschrgdt=snf_dschrgdt));

		if 5000<=(input(substr(PRVDRNUM,length(PRVDRNUM)-3,4),?? 4.))<=6499 & snf_admsndt~=. & snf_dschrgdt~=.;
		snf_los=snf_dschrgdt-snf_admsndt;
	run;

	data medpar2013; /*Keep only non-ma claims from ma_reporting hospitals*/
		set proc.med2009_2013 (keep=PRVDR_NUM admsn_dt dschrg_dt bene_id index_condition ma_claim report_ma year);
		if 1<=(input(substr(PRVDR_NUM,length(PRVDR_NUM)-3,4),?? 4.))<=899 & report_ma=1 & ma_claim=0 & year>=2011;
		enddt=dschrg_dt+3;
	run;

	proc sql;
	create table med_snf as
	select * from medpar2013 as med left join medpar_snf as snf
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
ods tagsets.excelxp file="&out./med_adv_comp2013.xml" style=sansPrinter;
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



