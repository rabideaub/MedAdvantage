*****************************************************************;
* For CD 30-day readmission measure using Medicare claim data.  *;
* Input data must conform to the specifications of R&A.         *;
* SAS 9.2 or greater WINDOWS                                    *;
* ZQ LIN, YALE/YNHH CORE                                        *;
* Created on 2/15/2009                                          *;
* Modified on 5/4/2009                                          *;
* Modified on 2/18/2010 FOR 2010 PUBLIC REPORTING.              *;
* Modified on 1/31/2011 for 2011 public reporting.              *;
* modified on 3/30/2011                                         *;

/* Modified on 01/31/2013   CW
   -Add new planned readmission algorithm                                                
   -Correct Maryland fix sort issue      
   -Change the # of Diagnosis Codes to 35
   -Change the # of Procedure Codes to 25
   -Maryland rehab/psych change
   - correct maryland fix sort issue      
*Modified on 10/22/2013 by VZ to replace the codes for creating Post Index File
 by a macro 'POST_OneYear' in SAS pack 'Readmission Macros' 
*Modified on 4/2014 by JNG to: 
   -update planned readmission to 3.0
   -remove Random _residual_ from GLIMMIX
   -add HSE to provider level file
***************************************************************/
OPTIONS SYMBOLGEN MPRINT; /* leave this on for program checking purpose */
*****************************************************************************;
* SPECIFY VARIOUS FILE PATHES, DISEASE CONDITION, AND YEAR                  *;
*****************************************************************************;
libname mds "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/Temp";
%let include = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/MedAdvantage;
%include "&include./00_Assign_Macro_Variables_and_Libraries.sas";

%let year=&YY.&YYE.; /* e.g., 0910, or 11  */;
%LET CONDITION=&DX.; 
%let PRE_COV=N; /*Y or N, determines if inclusion criteria demands 12 months of consecutive coverage before admission. BR 7/8/15*/
%LET PATH1= /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Final  ; /*RAW DATA FILES PATH, MUST BE CHANGED */

%LET PATH4= /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Final ; /* for derived data sets, MUST BE CHANGED */
%LET PATH5= /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/MedAdvantage;
			/* for SAS macros and HCC FORMAT CATALOG RESIDES, MUST BE CHANGED  */

/*Import the transfer file formats for CCMap and CCS*/
%let path6 = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/HRRP/CCMap2008_2012; *CC Map directory;
%let path7 =/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/HRRP/CCS_2013; *CCS Format DIrectory;
%let fmt = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/HRRP/CC_and_CCS_FORMATS_TRN;
libname fmt "&fmt";
libname F "&path6";
libname C "&path7";
filename datfile "&fmt./CC2008_2012.TRN";
filename datfile2 "&fmt./CCS2013.TRN";
proc cimport lib=F infile=datfile;
run;
proc cimport lib=C infile=datfile2;
run;

LIBNAME RAW "&PATH1"; 
LIBNAME R "&PATH4"; 

/*Formats taken care of above - code changed to deal with transport file format catalog BR*/
*LIBNAME F "&PATH7"; 
*LIBNAME C "&PATH6"; 
OPTIONS FMTSEARCH=(F C) nofmterr;

%INCLUDE "&PATH5./Readmission_Macros_2014.sas";

%LET ADMISSION=RAW.READMISSIONS_INDEX_&CONDITION._&YEAR; 
%LET POST=RAW.POSTINDEX_&CONDITION._&YEAR; 
%LET HXDIAG=RAW.DIAGHISTORY_&CONDITION._&YEAR;
%LET HXPROC=RAW.PROCHISTMM_&CONDITION._&YEAR;

%LET ALL=R.&CONDITION._READM_ALL;
%LET ANALYSIS=R.&CONDITION._READM_ANALYSIS;
%LET RSRR=R.&CONDITION._READM_RSRR;
%LET RESULTS=R.&CONDITION._READM_RSRR_BS;
%LET EST=R.&CONDITION._READM_EST;

	/* CD READMISSION MODEL VARIABLES */

%LET MODEL_VAR=	AGE_65 MALE HXCABG DIABETES DIS_FLUID IRON_DEFICIENCY
			CARDIO_RESPIRATORY CHF VASDIS_WCOMP COPD PNEUMONIA
			RENAL_FAILURE OTHER_UTD DECUBITUS_ULCER OTHER_GI ACS
			VAL_RHE_HEART ARRHYTHMIAS ASTHMA PEPTIC_ULCER CANCER		
			DRUG_ALCOHOL MAJOR_PSYCH ESRD_DIALYSIS HEMATOLOGICAL
			NEPHRITIS ESLD MCANCER STROKE DEMENTIA CAD_ANGINA
			OTHER_HEART OTHER_PSYCH PARALYSIS_FUNCTDIS
			LUNG_FIBROSIS MALNUTRITION DEPRESSION;

%LET CDIAG=&&DIAGSELECT_&DX.;

%LET TRANS_DIAG=R.DIAGHISTORY_&CONDITION._&YEAR._TRANS;
%LET TRANS_PROC=R.PROCHISTORY_&CONDITION._&YEAR._TRANS;

%RETRIEVE_HX(&ADMISSION, &CDIAG, &TRANS_DIAG, &TRANS_PROC);

proc freq data=&ADMISSION;
	table DIAG1;
run;
data INDEX;
	set &ADMISSION (IN=A WHERE=(PARA=1  and VA_OBS_STAY = 0)); *take out va_obs_stays jng-2012;

	IF &&DIAGSELECT_&DX. THEN CD_CASE=1;
	ELSE CD_CASE=0;


RUN;
			
/* ELIMINATE ADMISSIONS THAT APPEAR TWICE (ACROSS YEARS) */

PROC SORT DATA=INDEX NODUPKEY DUPOUT=QA_DupOut EQUALS;
	BY HICNO ADMIT DISCH PROVID;
RUN;

/* IDENTIFY AND COMBINE TWO ADJACENT CD ADMISSIONS (disch1=admit2), USE DISCHARGE DATE
	OF 2ND ADMISSION TO REPLACE DISCHARGE DATE OF 1ST ADMISSION (disch1=disch2), 
	SAME FOR DISCHARGE STATUS, TRANS_FIRST, TRANS_MID, POSTMOD. 
	ALSO, CREATE CASE_P TO BE USED FOR FINDING READMISSION.  
	THIS WORKS WHEN THERE ARE MORE THAN TWO ADJACENT CD ADMISSIONS. */

DATA TEMP; 
	SET INDEX;
	BY HICNO;
if (admit <= lag(disch) <= disch) and lag(provid)=provid
	and lag(hicno)=hicno
	and lag(CD_case)=CD_case=1 then combine0=1;
else combine0=0;
RUN;

proc sort data=TEMP;
	by hicno descending admit descending disch;
run;

data TEMP2 QA_CombOut_mid;
set TEMP;
by hicno;

if (admit <= lag(admit) <= disch) and 
	lag(provid)=provid
	and lag(hicno)=hicno
	and lag(CD_case)=CD_case=1 then combine=1;
else combine=0;
if combine0 and combine then output QA_CombOut_mid;
else output TEMP2;

run;

data TEMP3 QA_CombOut_last;
set TEMP2;

disch_2=lag(disch);
case_2=lag(case);
ddest_2=lag(ddest);
trans_first_2=lag(trans_first);
trans_mid_2=lag(trans_mid);
postmod_2=lag(postmod);
if lag(provid)=provid and lag(hicno)=hicno and lag(combine0)=1 then do;
	disch=disch_2;
	case_p=case_2;
	ddest=ddest_2;
	trans_first=trans_first_2;
	trans_mid=trans_mid_2;
	postmod=postmod_2;
	end;
else case_p=case;

drop disch_2 case_2 ddest_2 trans_first_2 trans_mid_2 postmod_2;

if combine0 ^=1 then output TEMP3;
else output QA_CombOut_last;

run;

PROC SORT DATA=TEMP3;
	BY HICNO DESCENDING ADMIT  DESCENDING DISCH PROVID;
RUN;

/* APPLY THE FOLLOWING INCLUSION AND EXCLUSION CRITERIA:
	CD_CASE=1, AGE >=65, DEAD=0, PREMO=12, POSTMOD=1, 2, 3,
	TRANS_COMBINE=0, AMA=0 */

DATA ALL; 
	SET TEMP3 (DROP=COMBINE0);
	BY HICNO;

ATTRIB TRANSFER_OUT LABEL='TRANSFER OUT' LENGTH=3.;
ATTRIB TRANS_COMBINE LABEL='TRANSFER OUT' LENGTH=3.;
ATTRIB DD30 LABEL='30-DAY MORTALITY FROM DISCHARGE' LENGTH=3.;
ATTRIB AGE_65 LABEL='YEARS OVER 65' LENGTH=3.;
ATTRIB AGE65 LABEL='AGE GE 65' LENGTH=3.;
ATTRIB DEAD LABEL='IN HOSPITAL DEATH' LENGTH=3.;
ATTRIB CRITERIA LABEL='MEET INC & EXL CRITERIA' LENGTH=3.;

/* TRANSFER OUT INDICATOR, SAME PTS, DIFFERENT HOSP, 0 OR 1 DAY IN BETWEEN, NOT V CODE VISIT */
TRANSFER_OUT=(ADMIT <= LAG(ADMIT) <= DISCH +1) AND (HICNO=LAG(HICNO)) AND (PROVID ^=LAG(PROVID)) AND 
			 factype ~in("IRF","SNF","LTC","HHA","OUT","MDS");
*IF SUBSTR(DIAG1, 1, 1)='V' OR SUBSTR(LAG(DIAG1), 1, 1)='V' THEN TRANSFER_OUT=0;

/* add post_flag to account for possible transfer that is outside of study period. 
	1/11/2010. ZQ */

TRANS_COMBINE=(TRANSFER_OUT OR TRANS_FIRST OR TRANS_MID or post_flag);

MALE=(SEX=1);
AGE=INT((ADMIT - BIRTH)/365.25);
AGE65=(AGE >=65);
AGE_65=AGE - 65;

DEAD=(DDEST=20);
AMA=(DDEST=7);

DD180=0;
IF DEATH ^=. THEN DO;
	IF 0 < (DEATH - DISCH) <=30 THEN DD30=1;
	ELSE IF (DEATH - DISCH) > 30 THEN DD30=0;
	ELSE IF (DEATH - DISCH) <= 0 THEN DD30=0;
	IF 0 < (DEATH - DISCH) <=180 THEN DD180=1;
	END;
ELSE DD30=0;
IF DD180=1 THEN POSTMOD=6;

/*Toggle the 12 months of prior coverage inclusion criteria on and off. Brendan Rabideau 7/8/15*/
%macro prior_cov;
	%IF "&PRE_COV." ~="Y" & "&PRE_COV."~="N" %THEN 
		PUTLOG 'ERROR: PLEASE INDICATE WHETHER 12 MONTHS PRIOR COVERAGE IS REQUIRED, &PRE_COV = Y/N';
	%ELSE %IF "&PRE_COV." = "Y" %THEN PRIOR12=(PREMO=12);
	%ELSE %IF "&PRE_COV." = "N" %THEN PRIOR12=1;
%mend;
%prior_cov;

POST1=(POSTMOD >=3);  /* take out dead 3/30/11 */

/* INCLUSIOIN CRITERIA: FFS, FF CASE, AGE GE 65, WITH 12-MONTH HISTORY,
   EXCLUSION  CRITERIA: TRANSFER OUT, IN HOSPITAL DEATH, WITHOUT >= 1 MOTNH POST,
						AMA */

IF (CD_CASE=1) AND (TRANS_COMBINE=0) AND (AGE65=1) AND (DEAD=0)
		AND (PRIOR12=1) AND (POST1=1) AND AMA=0 THEN CRITERIA=1;
ELSE CRITERIA=0;

RUN;

proc freq data=ALL;
	table CD_CASE TRANS_COMBINE AGE65 DEAD PRIOR12 POST1 AMA;
run;

/* IDENTIFY # OF ADMISSIONS PER PATIENT */
PROC SQL;
	CREATE TABLE STEP1WCOUNT AS
	SELECT DISTINCT HICNO, COUNT(HICNO) AS ADM_COUNT
	FROM ALL (WHERE=(CRITERIA=1))
	GROUP BY HICNO;
QUIT;


/* CREATING 2 FILES, ONE FOR PTS WITH 1 ADM, & ANOTHER FOR PTS WITH 1+ ADMS */
DATA ONE (DROP=ADM_COUNT) ONEPLUS (DROP=ADM_COUNT);
	MERGE ALL (WHERE=(CRITERIA=1)) STEP1WCOUNT;
	BY HICNO;

KEEP HICNO CASE CASE_P ADMIT DISCH ADM_COUNT YEAR;

IF ADM_COUNT=1 THEN OUTPUT ONE;
ELSE IF ADM_COUNT > 1 THEN OUTPUT ONEPLUS;

RUN;

PROC SORT DATA=ONEPLUS;
	BY HICNO ADMIT DISCH;
RUN;

/* KEEP ADMS THAT ARE AT LEAST 30 DAYS APART */
DATA ONEPLUS30DAY;
	SET ONEPLUS;
	BY HICNO;
RETAIN BASEDATE 0;
IF FIRST.HICNO THEN DO;
	BASEDATE=DISCH;
	KEEP=1;
	DIFF=0;
	END;
ELSE IF (ADMIT - BASEDATE) <=30 THEN DO;
	KEEP=0;
	DIFF=ADMIT - BASEDATE;
	END;
ELSE IF (ADMIT - BASEDATE) > 30 THEN DO;
	KEEP=1;
	DIFF=ADMIT - BASEDATE;
	BASEDATE=DISCH;
	END;

IF KEEP=1;

DROP BASEDATE;

RUN;

DATA INDEXADMISSION;
	SET ONE ONEPLUS30DAY;
RUN;

PROC SORT DATA=INDEXADMISSION;
	BY HICNO CASE;
RUN;

PROC SORT DATA=ALL;
	BY HICNO CASE;
RUN;

DATA _ALL;
MERGE ALL (IN=A WHERE=(CD_CASE=1)) INDEXADMISSION (IN=B KEEP=HICNO CASE);
BY HICNO CASE;

IF A;

ATTRIB _30DAYS LABEL='30 DAYS WINDOW EXCLUSION' LENGTH=3.;
ATTRIB SAMPLE LABEL='ADMISSION IN FINAL SAMPLE' LENGTH=3.;

if criteria=1 and not b then _30Days=1;
else _30Days=0;

IF B THEN SAMPLE=1;
ELSE SAMPLE=0;

RUN;

PROC SORT DATA=_ALL;
BY HICNO CASE_P;
RUN;


******The original codes for creating post index file has been put 
		as a macro 'POST_OneYear' and included in SAS pack 'Readmission Macros'******
*******Oct.22, 2013**************;
%POST_OneYear;

 
PROC SORT DATA=POST out=post;
	BY HICNO CASE ADMIT;
RUN;



data readm1 QA_DupIndex; **remove va_obs_stays from postindex - jng 1/9/12;
MERGE _ALL (IN=A)
	 post (IN=B /*where=(va_obs_stay=0)*/ KEEP=HICNO ADMIT DISCH PROVID /*va_obs_stay*/ DIAG1 CASE planned 
			factype sslssnf pmt_amt tot_chg DRGCD DDEST index_condition _39: _20a _20b
		RENAME=(DIAG1=_DIAG1 ADMIT=_ADMIT DISCH=_DISCH PROVID=_PROVID CASE=CASE_P factype=_factype
			sslssnf=_sslssnf pmt_amt=_pmt_amt tot_chg=_tot_chg DRGCD=_DRGCD DDEST=_DDEST
			index_condition=_index_condition));
BY HICNO CASE_P;

IF A;

/* RADM30ALL: ANY READMISSION WITHIN 30 DAYS */

IF NOT B THEN RADM30=0;
ELSE IF 0 <= _ADMIT - DISCH <=30 & _factype ~in("IRF","SNF","LTC","HHA","OUT","MDS") then RADM30=1;
ELSE IF _ADMIT - DISCH > 30 then RADM30=0;

IF NOT B THEN RADM45=0;
ELSE IF 0 <= _ADMIT - DISCH <=45 & _factype ~in("IRF","SNF","LTC","HHA","OUT","MDS") then RADM45=1;
ELSE IF _ADMIT - DISCH > 45 then RADM45=0;

INTERVAL=_ADMIT - DISCH;
SAME=(PROVID=_PROVID); /* SAME HOSPITAL READMISSION */


************************************************************************************; 
radm30p=0;
radm45p=0;
if planned =1 and Radm30 =1 then do;
	Radm30  = 0;
	Radm45  = 0;
	radm30p = 1;
	radm45p = 1;
end;
******************************************************************************************;

   
IF _index_condition ="COPD" THEN RADM_CD=1;
ELSE RADM_CD=0;

/* any readmission with principal diagnosis eq V57  is not counted as readmission,
	added 1/11/2010. ZQ */

if upcase(_diag1)=:'V57' then Radm_rehab=1;
else Radm_rehab=0;

/* any readmission with psych principal diagnosis eq in range of 290-319 that was 
	within 1 day of the discharge date of index admission with discharge dispostion
	eq 65 is not counted as readmission, added 1/11/2010. ZQ */ 

if (_diag1=:'29' or _diag1=:'30' or _diag1=:'31') and (interval in (0,1)) and
	ddest=65 & _factype ~in("IRF","SNF","LTC","HHA","OUT","MDS") then radm_psy=1;
else Radm_psy=0;

****** These psych and rehab stays will not be counted as planned or unplanned readmissions 12/19/12***;

if radm_rehab=1 and (radm30=1 or radm30p = 1) then do;  radm30=0; radm30p = 0; interval = 999; end;
if radm_psy=1 and (radm30=1 or radm30p = 1) then do;  radm30=0; radm30p = 0; interval = 999; end;
 

hicno_case=strip(hicno)||strip(case_p);

/* PART OF TRANS BUNDLE, SHOULD HAVE BEEN EXCLUDED */
IF RADM30=1 AND RADM_CD=1 AND INTERVAL=0 AND SAME=1 THEN TRANS_BUNDLE=1;
ELSE TRANS_BUNDLE=0; 

IF TRANS_BUNDLE=1 THEN SAMPLE=0;

IF ADMIT=_ADMIT AND DISCH=_DISCH AND PROVID=_PROVID AND DIAG1=_DIAG1 THEN OUTPUT QA_DupIndex;
ELSE OUTPUT readm1;

run;
 

*****************************************************************************************************************; 
***** Sort Order has changed so that the first readmission is the only one counted either as planned or unplanned**;
***** November 2012   ***************;
*****************************************************************************************************************; 

proc sort data=readm1;
by hicno_case interval; 
*by hicno_case  descending radm30 descending radm30p _admit;
run;

/*This step will save all post-index care received as its own observation if the care started within X-days of discharge.
  This creates a long-file with duplicates on the values hicno and case*/
data readm1data;
set readm1;
if interval <=180;
/*by hicno_case;
if first.hicno_case;
DROP HICNO_CASE;*/
run;
 
 proc freq data=readm1;
 where sample =1;
 tables radm30 radm30p interval;
 run;

proc sort data=readm1data;
by hicno case;
run;

DATA sample;
	set readm1data (where=(sample=1));
	csex=put(sex, $1.);
	drop sex;
	if _factype="SNF" then snf_los=_DISCH-_ADMIT;
	if _factype="MDS" then mds_los=_DISCH-_ADMIT;
	if _factype="IRF" then irf_los=_DISCH-_ADMIT;

	/*Add in clarifying flags for stroke, joint replacement, heart failure, and obesity*/
	stroke_TCI=diag1 in('4351','4350','4353','4358','4359','4377') | diag1='32634';
	stroke_occ_no_infarc=diag1 in('43300','43320','43310','43330','43390',
								  '43301','43321','43311','43331','43391');
	stroke_occ_pre_infarc=diag1 in('43301','43321','43311','43331','43391');
	stroke_occ_intra_infarc=diag1 in('43401','43411','43491');
	stroke_occ_unspec_infarc=diag1 in('43491');
	stroke_hemorrhage=diag1 in('430','431','4321','4320','4329');
	stroke_other=diag1 in('4373','4370','4371','4372','4374','4375','4376','325','29012','44321','44324','44329','4378');

	%macro loop_obesity(n);
		morbid_obesity=0;
		%do i=1 %to &n.;
			if diag&i. in('27801') then morbid_obesity=1;
		%end;
	%mend;
	%loop_obesity(n=25);

	bilateral_hip=(proc1 in('8151','8152') & proc2 in('8151','8152')) | (proc1='8154' & proc2='8154');

	hf_acute_diastolic=diag1 in('42831','42833');
	hf_chronic_diastolic=diag1 in('42832');
	hf_acute_systolic=diag1 in('42821','42823');
	hf_chronic_systolic=diag1 in('42822');
	hf_acute_combined=diag1 in('42841','42843');
	hf_chronic_combined=diag1 in('42842');
RUN;

proc sort data=sample out=R.sample_&CONDITION.;
by hicno_case;
run;

proc freq data=R.sample_&CONDITION.;
	tables radm30 year _factype _factype*year;
run;

proc print data=R.sample_&CONDITION. (obs=10); run;

title "Print SNF Provider IDs";
proc print data=R.sample_&CONDITION. (obs=10); 
	var _PROVID;
	where _factype="SNF";
run;
title;


/***************************************************************************************************************** 
FLATTEN THE FILE OUT SO EACH BENE/CASE IS ITS OWN OBSERVATION, AND ALL SUBSEQUENT CARE DATES ARE STORED AS VARS
*****************************************************************************************************************/

/*Transpose to make a single observation for each index admission that has all utilizations within 30 days of discharge*/
%macro factype(var);

	data sample_&condition. (keep=hicno case hicno_case case_count _factype radm30 radm45 dd30 cstshr:
				      ADMIT DISCH PROVID PROC1 pmt_amt tot_chg DRGCD DDEST sslssnf
				      _ADMIT _DISCH _PROVID _PROC1 _pmt_amt _tot_chg _DRGCD _DDEST  cstshr:
				      _sslssnf DEATH year DIAG1-DIAG25 ma_claim entry_dt disch_dt drop_obs _20a _20b _39:
				      RACE MALE ZIP RTI_RACE_CD AGE BUYIN_MO first_rug disch_cd mds_los snf_los irf_los RUG4
				      prior_month_hmo1 dis_month_hmo1 dis_month_hmo2 dis_month_hmo3 _21a _21d _22 ghopdcd qtr_assess
					  stroke_TCI stroke_occ_no_infarc stroke_occ_pre_infarc stroke_occ_intra_infarc stroke_occ_unspec_infarc
					  stroke_hemorrhage stroke_other morbid_obesity bilateral_hip hf_acute_diastolic hf_chronic_diastolic
					  hf_acute_systolic hf_chronic_systolic hf_acute_combined hf_chronic_combined
					  );
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

	data sample_&condition._&var. (drop=radm30 radm45 dd30 
								   %if "&var."="STA" %then %do;
								   	rename=(rradm30=radm30 rradm45=radm45 rdd30=dd30) /*Readmissions only count from STA facilities*/
								   %end;
								   %else %do;
								   	rename=(rdd30=dd30) /*Deaths count from all facilities*/
								   %end;
								   );
		set sample_&condition. (where=(_factype="&var."));
		by hicno_case;


			array &var.adm{&N.}   &var._admit1-&var._admit&N.;
			array &var.dis{&N.}   &var._disch1-&var._disch&N.;
			array &var.pmt{&N.}   &var._pmt_amt1-&var._pmt_amt&N.;
			array &var.tot{&N.}   &var._tot_chg1-&var._tot_chg&N.;
			array &var.drg{&N.} $ &var._drg1-&var._drg&N.;
			array &var.dst{&N.} $ &var._dstntncd1-&var._dstntncd&N.;
			array &var.dgn{&N.} $ &var._diag1-&var._diag&N.;
			array &var.prv{&N.} $ &var._provid1-&var._provid&N.;
			array &var.ssl{&N.} $ &var._sslssnf1-&var._sslssnf&N.;
			array &var.z20a{&N.} $ &var._20a1-&var._20a&N.;
			array &var.z20b{&N.} $ &var._20b1-&var._20b&N.;
			array &var.z21a{&N.} $ &var._21a1-&var._21a&N.;
			array &var.z21d{&N.} $ &var._21d1-&var._21d&N.;
			array &var.z22{&N.}  $ &var._221-&var._22&N.;
			array &var.slos{&N.}  &var.snf_los1-&var.snf_los&N.;
			array &var.mlos{&N.}  &var.mds_los1-&var.mds_los&N.;
			array &var.ilos{&N.}  &var.irf_los1-&var.irf_los&N.;


		if first.hicno_case then do;
			i=1;
			/*Only the first hospitalization counts towards a readmission*/
			%if "&var."="STA" %then %do;
				rradm30=radm30;
				rradm45=radm45;
			%end;
				rdd30=0;

				FIM_39aa=_39aa; /*Retain the first of each of the FIM variables from the IRF-PAI. Only counts for the 1st IRF stay*/
				FIM_39ad=_39ad;
				FIM_39ag=_39ag;
				FIM_39ba=_39ba;
				FIM_39bd=_39bd;
				FIM_39bg=_39bg;
				FIM_39ca=_39ca;
				FIM_39cd=_39cd;
				FIM_39cg=_39cg;
				FIM_39da=_39da;
				FIM_39dd=_39dd;
				FIM_39dg=_39dg;
				FIM_39ea=_39ea;
				FIM_39ed=_39ed;
				FIM_39eg=_39eg;
				FIM_39fa=_39fa;
				FIM_39fd=_39fd;
				FIM_39fg=_39fg;
				FIM_39ga=_39ga;
				FIM_39gd=_39gd;
				FIM_39gg=_39gg;
				FIM_39ha=_39ha;
				FIM_39hd=_39hd;
				FIM_39hg=_39hg;
				FIM_39ia=_39ia;
				FIM_39id=_39id;
				FIM_39ig=_39ig;
				FIM_39ja=_39ja;
				FIM_39jd=_39jd;
				FIM_39jg=_39jg;
				FIM_39ka=_39ka;
				FIM_39kd=_39kd;
				FIM_39kg=_39kg;
				FIM_39la=_39la;
				FIM_39laa=_39laa;
				FIM_39ld=_39ld;
				FIM_39ldd=_39ldd;
				FIM_39lg=_39lg;
				FIM_39ma=_39ma;
				FIM_39md=_39md;
				FIM_39mg=_39mg;
				FIM_39na=_39na;
				FIM_39naa=_39naa;
				FIM_39nd=_39nd;
				FIM_39ndd=_39ndd;
				FIM_39ng=_39ng;
				FIM_39oa=_39oa;
				FIM_39oaa=_39oaa;
				FIM_39od=_39od;
				FIM_39odd=_39odd;
				FIM_39og=_39og;
				FIM_39pa=_39pa;
				FIM_39pd=_39pd;
				FIM_39pg=_39pg;
				FIM_39qa=_39qa;
				FIM_39qd=_39qd;
				FIM_39qg=_39qg;
				FIM_39ra=_39ra;
				FIM_39rd=_39rd;
				FIM_39rg=_39rg;
		end; 
		else i+1;


			if upcase(_factype)="&var." then do;
				&var.adm{i}=_ADMIT;
				&var.dis{i}=_DISCH;
				&var.pmt{i}=_pmt_amt;
				&var.tot{i}=_tot_chg;
				&var.drg{i}=_DRGCD;
				&var.dst{i}=_DDEST;
				&var.dgn{i}=_DIAG1;
				&var.prv{i}=_PROVID;
				&var.z20a{i}=_20a;
				&var.z20b{i}=_20b;
				&var.z21a{i}=_21a;
				&var.z21d{i}=_21d;
				&var.z22{i}=_22;
				&var.ssl{i}=_sslssnf;
				&var.slos{i}=snf_los;
				&var.mlos{i}=mds_los;
				&var.ilos{i}=irf_los;
			end;

		*if radm30=1 then rradm30=1;
		*if radm45=1 then rradm45=1;
		if dd30=1 then rdd30=1;

		if last.hicno_case;
		if i lt &N then do i=i+1 to &N;
				&var.adm{i}=.;
				&var.dis{i}=.;
				&var.pmt{i}=.;
				&var.tot{i}=.;
				&var.slos{i}=.;
				&var.mlos{i}=.;
				&var.ilos{i}=.;
				&var.drg{i}='';
				&var.dst{i}='';
				&var.dgn{i}='';
				&var.prv{i}='';
				&var.ssl{i}='';
				&var.z20a{i}='';
				&var.z20b{i}='';
				&var.z21a{i}='';
				&var.z21d{i}='';
				&var.z22{i}='';
		end;

		retain  &var._admit1-&var._admit&N.
				&var._disch1-&var._disch&N.
				&var._pmt_amt1-&var._pmt_amt&N.
				&var._tot_chg1-&var._tot_chg&N.
				&var._drg1-&var._drg&N.
				&var._dstntncd1-&var._dstntncd&N.
				&var._proc1-&var._proc&N.
				&var._provid1-&var._provid&N.
				&var._sslssnf1-&var._sslssnf&N.
				&var.snf_los1-&var.snf_los&N.
				&var.mds_los1-&var.mds_los&N.
				&var.irf_los1-&var.irf_los&N.
				&var._20a1-&var._20a&N.
				&var._20b1-&var._20b&N.
				&var._21a1-&var._21a&N.
				&var._21d1-&var._21d&N.
				&var._221-&var._22&N.
				
				rradm30 rradm45 rdd30 FIM_39:;
	run;

	title "Collapsed &var.";
	proc print data=sample_&condition._&var. (obs=10); run;
	title;

	proc sort data=sample_&condition._&var.; by hicno_case; run;
%mend;
%factype(STA);

proc freq data=sample_&condition._STA;
	tables radm30 radm45;
run;

%factype(SNF);
%factype(IRF);
*%factype(LTC);
*%factype(HHA);
%factype(MDS);
%factype(); /*Blank macro parameter to account for beneficiaries with no PAC or readmission*/
*%factype(OUT);

data R.collapsed_&CONDITION.;
	merge sample_&condition._:;
	by hicno_case;
	/*Set missing values for these variables to 0*/
	radm30=(radm30=1);
	radm45=(radm45=1);
	dd30=(dd30=1);
run;

proc print data=R.sample_&CONDITION. (obs=100);
	var hicno case hicno_case ADMIT DISCH PROVID pmt_amt tot_chg DRGCD DDEST sslssnf
		_ADMIT _DISCH _PROVID _pmt_amt _tot_chg _DRGCD _DDEST _sslssnf factype _factype;
run;
proc print data=R.collapsed_&CONDITION. (obs=100);
	var hicno_case admit disch provid pmt_amt tot_chg drgcd ddest sslssnf
		sta_admit1-sta_admit3 sta_disch1-sta_disch3 sta_pmt_amt1-sta_pmt_amt3
		snf_admit1-snf_admit3 snf_disch1-snf_disch3 snf_pmt_amt1-snf_pmt_amt3
		irf_admit1-irf_admit3 irf_disch1-irf_disch3 irf_pmt_amt1-irf_pmt_amt3
		mds_admit1-mds_admit3 mds_disch1-mds_disch3 mds_pmt_amt1-mds_pmt_amt3
		/*out_admit1-out_admit3 out_disch1-out_disch3 out_pmt_amt1-out_pmt_amt3*/;
	format sta_admit1-sta_admit3 sta_disch1-sta_disch3 snf_admit1-snf_admit3 snf_disch1-snf_disch3
		   irf_admit1-irf_admit3 irf_disch1-irf_disch3 mds_admit1-mds_admit3 mds_disch1-mds_disch3
		   DATE9.;
run;

proc print data=R.collapsed_&CONDITION. (obs=100);
	var hicno_case admit disch provid pmt_amt tot_chg drgcd ddest sslssnf
		sta_admit1-sta_admit3 sta_disch1-sta_disch3 sta_pmt_amt1-sta_pmt_amt3
		snf_admit1-snf_admit3 snf_disch1-snf_disch3 snf_pmt_amt1-snf_pmt_amt3
		irf_admit1-irf_admit3 irf_disch1-irf_disch3 irf_pmt_amt1-irf_pmt_amt3
		mds_admit1-mds_admit3 mds_disch1-mds_disch3 mds_pmt_amt1-mds_pmt_amt3;

	where mds_admit1~=.;

	format sta_admit1-sta_admit3 sta_disch1-sta_disch3 snf_admit1-snf_admit3 snf_disch1-snf_disch3
		   irf_admit1-irf_admit3 irf_disch1-irf_disch3 mds_admit1-mds_admit3 mds_disch1-mds_disch3
		   DATE9.;
run;

proc freq data=R.collapsed_&CONDITION.;
	tables radm30 radm45 dd30 radm30*_factype;
run;

proc contents data=R.collapsed_&CONDITION.; run;
