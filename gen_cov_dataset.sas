/************************************************************************************************************
************************************************************************************************************
Program: gen_cov_dataset
Created By: Brendan Rabideau
Created Date: 5/15/15
Updated Date: 11/24/15
Purpose: The purpose of this program is to to generate the raw datasets used in Yale's Analytic Files, 
		 which create datasets to be used in Yale's 30day Unplanned Readmission Algorithim. The program creates
		 the coverage dataset.

Notes: This program actually skips a step. We did not have exact matches for Yale's Analytic Files, but we 
	   do have data that looks like the output of one of their Analytic File programs (0B_format_coverage_and_hospice.sas).
	   This program attempts to create the output of that program. Note that since we do not have the raw data, 
	   more assumptions go into the creation of this dataset than into others.

	   We have no access to hospice info, unlike Yale's coverage dataset.

Updates:
	6/12/15 - Added years going back to 2002
	7/06/15 - Updated to drop dup bene_id's
	9/17/15 - Updated to include 2012
	10/30/15 - Updated to include 2013 - the directory structure is non-parallel, so this is hardcoded. May need updating later
	11/24/15- Converting for the HRRP project
	11/24/14- Making the process of reading in raw data more automated - macro-ing libnames, making start and end year set statements self-generating
************************************************************************************************************
************************************************************************************************************/

%let out = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Processed;
libname out "&out.";

%let include = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/MedAdvantage;
%include "&include./00_Assign_Macro_Variables_and_Libraries.sas";
%let syr=20&YY.;
%let eyr=20&YYE.;

%macro hmo_buy(styear,endyear);
	%do i=&styear. %to &endyear.;
	%let j=%substr(&i,3,2);

		/*Different years require different renaming. 2002-2005*/
		%if 2002<=&i. & &i.<=2005 %then %do;
			libname den "&xw./BeneStatus";

			data cov_&i. (keep=HICNO Y&j.BUY1-Y&j.BUY12 Y&j.HMO1-Y&j.HMO12);
				set den.den&i._xw;
				if bene_id ~="";
				rename ENTITL1-ENTITL12   = Y&j.BUY1-Y&j.BUY12
					   HMO1-HMO12 = Y&j.HMO1-Y&j.HMO12
					   bene_id = HICNO;
			run;

			proc sort data=cov_&i nodupkey; by HICNO; run;
		%end;

		/*2006-2008*/
		%if 2006<=&i. & &i.<=2008 %then %do;
			libname den&i. "&den.";

			data cov_&i. (keep=HICNO Y&j.BUY1-Y&j.BUY12 Y&j.HMO1-Y&j.HMO12);
				set den&i..den&i.;
				if bene_id ~="";
				rename BUYIN01-BUYIN12   = Y&j.BUY1-Y&j.BUY12
					   HMOIND01-HMOIND12 = Y&j.HMO1-Y&j.HMO12
					   bene_id = HICNO;
			run;

			proc sort data=cov_&i nodupkey; by HICNO; run;
		%end;

		/*2009-2011*/
		%if &i. >=2009 & &i.<=2011 %then %do;
			libname bsf&i. "&bsf.";

			data cov_&i. (keep=HICNO Y&j.BUY1-Y&j.BUY12 Y&j.HMO1-Y&j.HMO12);
				set bsf&i..mbsf_ab_summary&i.;
				if bene_id ~="";
				rename BUYIN01-BUYIN12   = Y&j.BUY1-Y&j.BUY12
					   HMOIND01-HMOIND12 = Y&j.HMO1-Y&j.HMO12
					   bene_id = HICNO;
			run;

			proc sort data=cov_&i nodupkey; by HICNO; run;
		%end;

		/*2012*/
		%if &i.>=2012 %then %do;
			libname bsf&i. "&bsf.";

			data cov_&i. (keep=HICNO Y&j.BUY1-Y&j.BUY12 Y&j.HMO1-Y&j.HMO12);
				set bsf&i..mbsf_ab_summary&i.;
				if bene_id ~="";
				rename BUYIN01-BUYIN12   = Y&j.BUY1-Y&j.BUY12
					   HMOIND01-HMOIND12 = Y&j.HMO1-Y&j.HMO12
					   bene_id = HICNO;
			run;

			proc sort data=cov_&i nodupkey; by HICNO; run;
		%end;

		/*2013*/
		/*%if &i.=2013 %then %do;
			libname bsf&i. "&bsf./&i./1";

			data cov_&i. (keep=HICNO Y&j.BUY1-Y&j.BUY12 Y&j.HMO1-Y&j.HMO12);
				set bsf&i..mbsf_ab_summary&i.;
				if bene_id ~="";
				rename BUYIN01-BUYIN12   = Y&j.BUY1-Y&j.BUY12
					   HMOIND01-HMOIND12 = Y&j.HMO1-Y&j.HMO12
					   bene_id = HICNO;
			run;

			proc sort data=cov_&i nodupkey; by HICNO; run;
		%end;*/
	%end;
%mend;
%hmo_buy(&syr.,&eyr.);

data out.coverage&YYE._&MM.; /*When we run the whole thing call it coverage&YYE._&MM.*/
	merge cov_:;
	by HICNO;
run;

proc contents data=out.coverage&YYE._&MM. out=cov_dataset_cont (keep=NAME TYPE LENGTH VARNUM LABEL FORMAT);
run;

/*********************
PRINT CHECK
*********************/
/*Output a sample of each of the datasets to an excel workbook*/
ods tagsets.excelxp file="&out./look_coverage.xml" style=sansPrinter;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Coverage" frozen_headers='yes');
proc print data=out.coverage&YYE._&MM. (obs=1000);
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Coverage Contents" frozen_headers='yes');
proc print data=cov_dataset_cont (obs=1000);
run;
ods tagsets.excelxp close;
/*********************
CHECK END
*********************/

