libname proc "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Processed";
libname pos "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/POS";
libname raw "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/PAC/Data/Raw/MedPAR";
libname temp "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/Temp";
libname irf "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/PAC/Data/Raw/IRF";
%let out=/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Output;
libname out "&out.";

/******************************************
 DATA PREP
******************************************/

/*Keep MedPAR hospitalizations from STACHs that report Medicare Advantage claims*/
data med;
	set proc.med2009_2013 (keep=PRVDR_NUM admsn_dt dschrg_dt report_ma ma_claim bene_id index_condition elix_cnt);
	if 1<=(input(substr(PRVDR_NUM,length(PRVDR_NUM)-3,4),?? 4.))<=899 & report_ma=1;
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
		if end_dt-dschrg_dt<30 then use_readm=1; /*If the end date is a readmission date within 30 days, then indicate it*/
	end;

	format readm_dt end_dt MMDDYY8.;
run;

proc print data=med (obs=100);
	var bene_id admsn_dt dschrg_dt end_dt readm_dt;
run;

data irf;
	set irf.irf_pai2013 (keep=bene_id _12 _40 rename=(_12=ENTRY_DT _40=DISCH_DT))
		irf.irf_pai2012 (keep=bene_id _12 _40 rename=(_12=ENTRY_DT _40=DISCH_DT))
		irf.irf_pai2011 (keep=bene_id _12 _40 rename=(_12=ENTRY_DT _40=DISCH_DT))
		irf.irf_pai2010 (keep=bene_id _12 _40 rename=(_12=ENTRY_DT _40=DISCH_DT))
		irf.irf_pai2009 (keep=bene_id _12 _40 rename=(_12=ENTRY_DT _40=DISCH_DT));
run;

/*Keep all IRF observations that happen within 30days of a STACH discharge*/
proc sql;
	create table med_irf as
	select * from med as med left join irf as irf
	on med.bene_id = irf.bene_id  and
	   ((med.dschrg_dt~=. & irf.ENTRY_DT between med.dschrg_dt and med.end_dt));
quit;

/*If the IRF entry date is before the hospitalization discharge, make entry=discharge */
data med_irf;
	set med_irf;
	if disch_dt~=. & disch_dt>end_dt & use_readm=1 then disch_dt=end_dt;
	irf=(entry_dt~=.);
	if disch_dt~=. & entry_dt~=. then irf_los=disch_dt-entry_dt;
run;

proc sort data=med_irf out=proc.med_irf; by bene_id admsn_dt entry_dt; run;

title "Check Dates";
proc print data=proc.med_irf (obs=200);
	var bene_id ma_claim admsn_dt dschrg_dt entry_dt disch_dt;
run;

%macro out;
/******************************************
 SUMMARY STATS
******************************************/
%macro by_cond(cond);

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

		 data stats_final (drop=MA_Claims FFS_Claims rename=(MA_Claims2=MA_Claims FFS_Claims2=FFS_Claims));
		 	length MA_Claims2 FFS_Claims2 $13;
		 	merge FFS
			   	  MA;
		 	by variable;
			MA_Claims2=put(trim(left(MA_Claims)),13.);
			FFS_Claims2=put(trim(left(FFS_Claims)),13.);
		 run;

		 proc print data=stats_final; run;

		 data ttest (keep=variable probt);
		 	set ttests;
			if trim(left(Method))="Pooled";
		run;

		proc print data=ttest; run;

		proc sort data=ttest; by variable; run;

		data los_&cond._&name. (rename=(Probt=Significance));
			length Description $300 variable $ 50;
			merge stats_final
				  ttest;
			by variable;
			variable=&varname.;
			Description=&text.;
		run;

		proc print data=los_&cond._&name.; run;
	%mend;

/*Macro to format the IRF prevalence and calculate ChiSq*/
	%macro freqs(var);
		proc freq data=proc.med_irf;
			tables &var.*ma_claim/chisq out=has_&var. outpct;
			where index(index_condition,"&cond.")>0;
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

		data freqs_&var. (drop= &var. MA_Claims FFS_Claims rename=(MA_Claims2=MA_Claims FFS_Claims2=FFS_Claims));
			length MA_Claims2 FFS_Claims2 $13;
			merge freq_ma
				  freq_ffs;
			by &var.;
			Variable="&var.";
			MA_Claims=trim(left(MA_Claims));
			FFS_Claims=trim(left(FFS_Claims));
			MA_Claims2=cats(MA_Claims,'%');
			FFS_Claims2=cats(FFS_Claims,'%');
		run;

		proc print data=freqs_&var.; run;

		data frequencies_&cond._&var.(drop=_PCHI_ DF_PCHI rename=(p_pchi=Significance));
			length MA_Claims2 FFS_Claims2 $6;
			merge freqs_&var.
				  chisqdat;
		run;
	%mend;




	/*Run the freq with the select measures*/
	%freqs(irf);

	/*Run the ttest with the select measures*/
	title "All LOS";
	ods output  "Statistics" = stats
	 			"T-Tests" = ttests;
	proc ttest data=proc.med_irf;
		class ma_claim;
		var irf_los;
		where index(index_condition,"&cond.")>0;
	run;
	ods output close;

	%ttest(name=all_irf,text="Mean LoS for all patients with an IRF record",
	   	   varname="IRF Length of Stay");

	/*Append the freqs and means*/
	data ma_ffs_comp_&cond.;
		set los_&cond.: /*All length of stay outputs for this condition*/
			frequencies_&cond.:; /*All frequency outputs for this condition*/
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
ods tagsets.excelxp file="&out./med_adv_irf_comp2013.xml" style=sansPrinter;
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
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Hip and Femur Procedure" frozen_headers='yes');
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
