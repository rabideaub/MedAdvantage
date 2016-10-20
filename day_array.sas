/*This program makes a dataset with variables of a bene's wherabouts for each day of an episode of care.
  It requires R.collapsed_&condition, which comes from the &condition_readmission_v2014_short.sas program*/


%let include = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/MedAdvantage;
%include "&include./00_Assign_Macro_Variables_and_Libraries.sas";
%include "&include./comoanaly2012_2015.sas";

%let year=&YY.&YYE.; /* e.g., 0910, or 11  */;
%LET CONDITION=&DX.; 
%LET PATH1= /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Final; /*RAW DATA FILES PATH, MUST BE CHANGED */

%LET PATH4= /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Final; /* for derived data sets, MUST BE CHANGED */
%LET PATH5= /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/MedAdvantage ;
			/* for SAS macros and HCC FORMAT CATALOG RESIDES, MUST BE CHANGED  */

LIBNAME RAW "&PATH1"; 
LIBNAME R "&PATH4"; 

%let out=/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Output;

%let model_vars = CHF VALVE PULMCIRC PERIVASC HTN_C PARA NEURO CHRNLUNG DM
         		  DMCX HYPOTHY RENLFAIL LIVER ULCER AIDS LYMPH METS TUMOR ARTH COAG OBESE WGHTLOSS 
				  LYTES BLDLOSS ANEMDEF ALCOHOL DRUG PSYCH DEPRESS;

data day_array;
	set R.collapsed_&CONDITION._elix;
run;

proc contents data=day_array; run;
proc contents data=R.sample_&CONDITION.; run;

%macro day_array(var);
	/*Determine the maximum number of stays in the episode of care for each facility type*/
	data sample_&condition. (keep=hicno case hicno_case case_count _factype ma_claim entry_dt disch_dt cstshr:
								  ADMIT DISCH PROVID DIAG1 pmt_amt tot_chg DRGCD DDEST sslssnf
								  _ADMIT _DISCH _PROVID _DIAG1 _pmt_amt _tot_chg _DRGCD _DDEST _sslssnf
								  drop_obs MALE RACE ZIP RTI_RACE_CD AGE BUYIN_MO first_rug disch_cd
								  prior_month_cov1 dis_month_cov1 dis_month_cov2 dis_month_cov3
								  _21a _21d _22 ghopdcd qtr_assess RUG4 premo spclunit
								  stroke_TCI stroke_occ_no_infarc stroke_occ_pre_infarc stroke_occ_intra_infarc 
								  stroke_occ_unspec_infarc stroke_hemorrhage stroke_other morbid_obesity bilateral_hip 
								  hf_acute_diastolic hf_chronic_diastolic hf_acute_systolic hf_chronic_systolic hf_acute_combined 
								  hf_chronic_combined);
		set R.sample_&CONDITION.;
		retain case_count;
		by hicno_case;
		if first.hicno_case then case_count=0;
		if upcase(_factype)="&var." then case_count+1;
	run;

	proc means data=sample_&condition. noprint missing;
	 var case_count;
	 output out=max_obs (drop=_FREQ_ _TYPE_)
	 max=max_case_count;
	run;

	data _null_;
	 set max_obs;
	 call symput ('N',Trim(Left(max_case_count)));
	run; 

	/*If any given day is between the admission and disch of a given factype, make the day variable = that factype*/
	data day_array (keep=hicno hicno_case DIAG: PROVID ADMIT DISCH DEATH ma_claim entry_dt disch_dt drop_obs MALE RACE ZIP RTI_RACE_CD AGE BUYIN_MO
						 drop_obs cstshr: DRGCD first_rug disch_cd pmt_amt snf_los: mds_los: irf_los: day:
						 prior_month_cov: dis_month_cov1 dis_month_cov2 dis_month_cov3 
						 STA_admit: SNF_admit: IRF_admit: MDS_admit: STA_disch: SNF_disch: IRF_disch: MDS_disch:
						 SNF_pmt_amt1-SNF_pmt_amt3 SNF_provid1-SNF_provid3 IRF_pmt_amt1-IRF_pmt_amt3 IRF_provid1-IRF_provid3
						 MDS_pmt_amt1-MDS_pmt_amt3 MDS_provid1-MDS_provid3 STA_pmt_amt1-STA_pmt_amt3 STA_provid1-STA_provid3
						 radm30 radm45 dd30 year FIM_39: irf_20a: irf_20b: irf_21a: irf_21d: irf_22: 
						 CHF VALVE PULMCIRC PERIVASC HTN_C PARA NEURO CHRNLUNG DM
         		 		 DMCX HYPOTHY RENLFAIL LIVER ULCER AIDS LYMPH METS TUMOR ARTH COAG OBESE WGHTLOSS 
				  		 LYTES BLDLOSS ANEMDEF ALCOHOL DRUG PSYCH DEPRESS elix_cnt  _21a _21d _22 ghopdcd qtr_assess RUG4 spclunit
						 stroke_TCI stroke_occ_no_infarc stroke_occ_pre_infarc stroke_occ_intra_infarc 
						 stroke_occ_unspec_infarc stroke_hemorrhage stroke_other morbid_obesity bilateral_hip 
						 hf_acute_diastolic hf_chronic_diastolic hf_acute_systolic hf_chronic_systolic hf_acute_combined 
						 hf_chronic_combined);
		length day1-day90 $4;
		set day_array;
			array day{90} $ day1-day90;
			do i=1 to dim(day);
			date=intnx('day',disch,i);
				%do j=1 %to &N.;
					if &var._admit&j.~=. & ((&var._admit&j.<=date<=&var._disch&j.) | (&var._admit&j.<=date & &var._disch&j.=.)) 
					then day[i]="&var.";
				%end;
				if DEATH~=. & DEATH<=date then day[i]="DEAD";
			end;

		format sta_admit1-sta_admit3 sta_disch1-sta_disch3 snf_admit1-snf_admit3 snf_disch1-snf_disch3
		   	   irf_admit1-irf_admit3 irf_disch1-irf_disch3 mds_admit1-mds_admit3 mds_disch1-mds_disch3
		   	   DATE9.;
	run;
%mend;
%day_array(MDS); /*Order matters here because SNF will override MDS entries.*/
*%day_array(HHA);
*%day_array(LTC);
%day_array(IRF);
%day_array(SNF);
%day_array(STA);

proc sort data=day_array; by hicno_case; run;

proc print data=day_array (obs=50); run;

/*Make some flags*/
data freq_array;
	set day_array;
	array day {90} day1-day90;

	consec_com=0;
	com30=0;
	MDS=0;
	STA=0;
	SNF=0;
	LTC=0;
	IRF=0;
	HHA=0;
	COM=0;
	DEAD=0;

	tot_MDS=0;
	tot_STA=0;
	tot_SNF=0;
	tot_LTC=0;
	tot_IRF=0;
	tot_HHA=0;
	tot_COM=0;

	do i=1 to 90;
		if day[i]="MDS" then MDS=1;
		if day[i]="STA" then STA=1;
		if day[i]="SNF" then SNF=1;
		if day[i]="LTC" then LTC=1;
		if day[i]="IRF" then IRF=1;
		if day[i]="HHA" then HHA=1;
		if day[i]="DEAD" then DEAD=1;
		if day[i]="" then COM=1;

		if day[i]="MDS" then tot_MDS+1;
		if day[i]="STA" then tot_STA+1;
		if day[i]="SNF" then tot_SNF+1;
		if day[i]="LTC" then tot_LTC+1;
		if day[i]="IRF" then tot_IRF+1;
		if day[i]="HHA" then tot_HHA+1;
		if day[i]="" then tot_COM+1;

		/*See if a bene was ever in the community for 30consecutive days*/
		if day[i]="" then consec_com=consec_com+1;
		if day[i]~="" then consec_com=0;
		if consec_com>=30 then com30=1;
	end;
	if day30="" then rel_com30=1; else rel_com30=0;
	if day90="" then rel_com90=1; else rel_com90=0;
	*if day180="" then rel_com180=1; *else rel_com180=0;
	first_pac_disch=min(SNF_disch1,IRF_disch1,MDS_disch1);
	if first_pac_disch~=. & tot_COM+(first_pac_disch-DISCH)=90 then com_after_pac=1;
	if tot_COM=90 then com90=1;
run;

proc sort data=freq_array out=R.freq_array_&CONDITION.; by PROVID year; run;

proc freq data=R.freq_array_&CONDITION.; 
	tables com90 com_after_pac;
run;

proc print data=R.freq_array_&CONDITION. (obs=10);
	where com_after_pac=1;
run;

title "check irf funtional measures";
proc freq data=R.freq_array_&CONDITION.;
	tables FIM_39: irf_20a1 irf_20b1 irf_21a1 irf_21d1 irf_22_1 / missing;
	where irf=1;
run;
title;


%macro out;
/*Additional Analysis*/
proc freq data=freq_array;
	tables drop_obs MDS STA SNF LTC IRF HHA DEAD COM com30 rel_com30 rel_com90 radm30 radm45 dd30;
run;

proc freq data=freq_array;
	tables year*(MDS STA SNF LTC IRF HHA);
run;

%macro ttest(fac);
proc ttest data=freq_array;
	class ma_claim;
	var tot_&fac.;
	where &fac.=1;
run;
%mend;
%ttest(MDS);
%ttest(IRF);
%ttest(STA);
%ttest(COM);
proc ttest data=freq_array;
	class ma_claim;
	var elix_cnt;
run;


/*This macro makes the ttest output a single line dataset with means and ttest*/
%macro ttest2(fac);

	ods output  "Statistics" = stats
	 			"T-Tests" = ttests;
	proc ttest data=freq_array;
		class ma_claim;
		var tot_&fac.;
		where &fac.=1;
	run;
	ods output close;

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

	 data ttest (keep=variable probt rename=(probt=Significance));
	 	set ttests;
		if trim(left(Method))="Pooled";
	run;

	proc print data=ttest; run;
	proc sort data=ttest; by variable; run;

	data mean_&fac.;
		length variable $ 50;
		merge stats_final
			  ttest;
		by variable;
		variable="&fac.";
	run;

	proc print data=mean_&fac.; run;
%mend;
%ttest2(MDS);
%ttest2(IRF);
%ttest2(STA);
%ttest2(COM);

data mean_all;
	set mean_:;
run;


%macro freqs(var);
	proc freq data=freq_array;
		tables &var.*ma_claim/chisq out=has_&var. outpct;
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
		length Variable $50;
		merge freq_ma
			  freq_ffs;
		by &var.;
		Variable="freq_&var.";
	run;

	data frequencies_&var.(drop=_PCHI_ DF_PCHI rename=(p_pchi=Significance));
		merge freqs_&var.
			  chisqdat;
	run;

	proc print data=frequencies_&var.; run;
%mend;
%freqs(MDS);
%freqs(IRF);
%freqs(STA);
%freqs(COM);
%freqs(radm30);
%freqs(dd30);
%freqs(rel_com30);
%freqs(rel_com90);

data freq_all;
	set frequencies_:;
run;

data table_all;
	length Variable $50;
	set mean_all
		freq_all;
	if Variable="COM" then Variable="Days in Community";
	if Variable="STA" then Variable="Days in Acute Care";
	if Variable="MDS" then Variable="Days in Nursing Home";
	if Variable="IRF" then Variable="Days in Rehab";
	if Variable="freq_COM" then Variable="Percent in Community";
	if Variable="freq_STA" then Variable="Percent in Acute Care";
	if Variable="freq_MDS" then Variable="Percent in Nursing Home";
	if Variable="freq_IRF" then Variable="Percent in Rehab";
	if Variable="freq_radm30" then Variable="Percent 30 Day Readmission";
	if Variable="freq_dd30" then Variable="Percent 30 Day Mortality";
	if Variable="freq_rel_com30" then Variable="Percent in Community at 30 Days";
	if Variable="freq_rel_com90" then Variable="Percent in Community at 90 Days";
run;

proc print data=freq_all; run;
proc print data=mean_all; run;
	
proc freq data=freq_array;
	tables ma_claim*(MDS IRF STA COM radm30 dd30) / chisq;
run;

proc sort data=freq_array out=R.freq_array_&CONDITION.; by PROVID year; run;

/*********************
PRINT CHECK
*********************/
/*Output a sample of each of the datasets to an excel workbook*/
ods tagsets.excelxp file="&out./med_adv_table_&CONDITION..xml" style=sansPrinter;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="&CONDITION" frozen_headers='yes');
proc print data=table_all (obs=1000);
run; 

ods tagsets.excelxp close;
/*********************
CHECK END
*********************/
%mend;




