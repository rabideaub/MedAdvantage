/************************************************************************************************************
************************************************************************************************************
Program: gen_bene_dataset
Created By: Brendan Rabideau
Created Date: 5/15/15
Updated Date: 11/24/15
Purpose: The purpose of this program is to to generate the raw datasets used in Yale's Analytic Files, 
  		 which create datasets to be used in Yale's 30day Unplanned Readmission Algorithim. This program uses 
  		 beneficiary datasets for select years to create the bene_dataset which is used to create 
  		 the initial index dataset (index01) along with the coverage_dataset and stay_dataset.

Notes: Beneficiary Summary Files have taken 3 different forms from 2002-2011 - denominator with EHIC ID,
       denominator with Bene_ID, and BSF with Bene_ID. and EHIC-Bene xwalk is used to create the data 
       from 2002-2005. The xwalking is done in a different program.
Updates:
	6/12/15 - Added years going back to 2002
	9/17/15 - Updated to include 2012
	11/24/15- Converting for the HRRP project
	11/24/14- Making the process of reading in raw data more automated - macro-ing libnames, making start and end year set statements self-generating
************************************************************************************************************
************************************************************************************************************/


/*The purpose of this program is to to generate the raw datasets used in Yale's Analytic Files, 
  which create datasets to be used in Yale's Readmission Algorithim.  */

options compress=yes;

%let out = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Raw;
libname out "&out.";

%let include = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/MedAdvantage;
%include "&include./00_Assign_Macro_Variables_and_Libraries.sas";
%let syr=20&YY.;
%let eyr=20&YYE.;

/*We have denominator files from 2002-2008, then BSF files from 2009 onwards. Den2002-2005 has a different ID (EHIC)
  so we have special files that are x-walked to bene_id for those years. */

%macro denom(styear,endyear);
%put &styear. &endyear.;
	%do i=&styear. %to &endyear.;
	

		/*2002-2005*/
		%if 2002<=&i. & &i.<=2005 %then %do;
			libname den&i. "&xw./BeneStatus";
			data den&i. (drop=A_MO_CNT B_MO_CNT HMO_MO /*BUYIN_MO*/); /*These vars are unnecessary and cause type mismatches*/
				set den&i..den&i._xw
				rename
					sdob 	 = BIRTH
					STATE    = BSTATE
					COUNTY   = COUNTY
					sdod 	 = DEATH
					DEATH_SW = EDBVDETH
					BENE_ID  = HICNO
					RACE     = RACE
					SEX      = SEX
					zipcode  = ZIP;

				year="&i.";
			run;
		%end;

		/*2006-2008*/
		%if 2006<=&i. & &i.<=2008 %then %do;
			libname den&i. "&den.";
			data den&i. (drop=A_MO_CNT B_MO_CNT HMO_MO /*BUYIN_MO*/);
				set den&i..den&i.;
				rename
					BENE_DOB = BIRTH
					STATE_CD = BSTATE
					CNTY_CD  = COUNTY
					DEATH_DT = DEATH
					V_DOD_SW = EDBVDETH
					BENE_ID  = HICNO
					RACE     = RACE
					SEX      = SEX
					BENE_ZIP = ZIP;

				year="&i.";
			run;
		%end;

		/*2009+*/
		%if 2009<=&i. %then %do;
			libname den&i. "&bsf.";
			data den&i. (drop=A_MO_CNT B_MO_CNT HMO_MO /*BUYIN_MO*/);
				set den&i..mbsf_ab_summary&i.;
				rename
					BENE_DOB = BIRTH
					STATE_CD = BSTATE
					CNTY_CD  = COUNTY
					DEATH_DT = DEATH
					V_DOD_SW = EDBVDETH
					BENE_ID  = HICNO
					RACE     = RACE
					SEX      = SEX
					BENE_ZIP = ZIP;

				year="&i.";
			run;

			/*For 2009+ add in the cost share info from the MBSF Part D files*/
			data den_d&i. (keep=hicno year cstshr:);
				set den&i..mbsf_d_cmpnts&i.;
				rename BENE_ID=HICNO;
				year=put(RFRNC_YR,4.);
			run;
		%end;
	%end;
%mend;
%denom(&syr.,&eyr.);

/*Only have 1 record per bene_id per year - make this the most recent record, particularly the most updated death_dt*/
data bene;
	length ZIP $9;
	set den&syr.-den&eyr.;
run;

data bene_d;
	set den_d:;
run;

proc sort data=bene; by hicno year death edbvdeth ; run;
proc sort data=bene_d nodupkey; by hicno year; run;

/*Merge on the cost share information from part D*/
data bene;
	merge bene (in=a)
		  bene_d (in=b);
	by hicno year;
	if a;
run;

data out.bene_dataset (keep=BIRTH BSTATE COUNTY DEATH EDBVDETH HICNO RACE SEX ZIP AGE BUYIN_MO RTI_RACE_CD year cstshr:);
	set bene;
	by hicno year;
	if last.year then output;
run;

proc contents data=out.bene_dataset out=bene_dataset_cont (keep=NAME TYPE LENGTH VARNUM LABEL FORMAT);
run;

/*********************
PRINT CHECK
*********************/
/*Output a sample of each of the datasets to an excel workbook*/
ods tagsets.excelxp file="&out./look_bene.xml" style=sansPrinter;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Bene" frozen_headers='yes');
proc print data=out.bene_dataset (obs=1000);
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Bene Contents" frozen_headers='yes');
proc print data=bene_dataset_cont (obs=1000);
run;
ods tagsets.excelxp close;
/*********************
CHECK END
*********************/

