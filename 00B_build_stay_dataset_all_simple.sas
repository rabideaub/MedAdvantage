/************************************************************************************************************
                               SAS Program Description

PROGRAM NAME: 00B_build_stay_dataset2

FOR USE WITH: ALL MEASURES

PURPOSE:      Build Stay level dataset 

OVERVIEW:     

INPUT DATA: 

OUTPUT FILES: 
	data_sty.&stay_dataset_all.

************************************************************************************************************/

OPTIONS COMPRESS=YES REUSE=YES MLOGIC MPRINT NOMACROGEN NOSYMBOLGEN STIMER FULLSTIMER OBS=MAX ;

%let include = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/MedAdvantage;
%include "&include./00_Assign_Macro_Variables_and_Libraries.sas";

DATA _NULL_;
StartDate = "%SYSFUNC(DATE(),WORDDATE.)";
StartTime = "%SYSFUNC(TIME(),TIME.)";
PUT "===================================================================================";
PUT "  STARTING JOB INFORMATION:" /;
put "  Job Name: 00_build_stay_dataset" /;
PUT "  Start Date: " StartDate ;
PUT "  Start Time: " StartTime ;
PUT "===================================================================================";
RUN;

%let out = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Raw;
libname out "&out.";
libname mds "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Processed";
libname irf "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/PAC/Data/Raw/IRF";
libname med "/schaeffer-b/sch-protected/VERTICAL-INTEGRATION/Data/2009-13_PAC/Raw/MedPAR";

%let include = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/MedAdvantage;
%include "&include./00_Assign_Macro_Variables_and_Libraries.sas";
%let syr=20&YY.;
%let eyr=20&YYE.;

/************************************************************************************
PART A INPATIENT BASE DATASET (IP Claims)
************************************************************************************/
proc contents data=mds.mds3_2011_2013; run;

data mds (keep=hicno admit disch factype provid);
	set /*mds.mds2_2006_2010 (in=a where=(first_case=1) rename=(bene_id=hicno))*/
		mds.mds3_2011_2013 (in=b where=(first_case=1) rename=(bene_id=hicno));
	ADMIT=ENTRY_DT;
	DISCH=FINAL_DT;
	provid=A0100B_CMS_CRTFCTN_NUM;
	FACTYPE="MDS";
run;

data irf;
	set irf.irf_pai2013 (keep=bene_id _12 _40 _1B _20a _20b _21a _21d _22 _39: rename=(_12=ADMIT _40=DISCH bene_id=hicno _1B=provid))
		irf.irf_pai2012 (keep=bene_id _12 _40 _1B _20a _20b _21a _21d _22 _39: rename=(_12=ADMIT _40=DISCH bene_id=hicno _1B=provid))
		irf.irf_pai2011 (keep=bene_id _12 _40 _1B _20a _20b _21a _21d _22 _39: rename=(_12=ADMIT _40=DISCH bene_id=hicno _1B=provid))
		irf.irf_pai2010 (keep=bene_id _12 _40 _1B _20a _20b _21a _21d _22 _39: rename=(_12=ADMIT _40=DISCH bene_id=hicno _1B=provid))
		irf.irf_pai2009 (keep=bene_id _12 _40 _1B _20a _20b _21a _21d _22 _39: rename=(_12=ADMIT _40=DISCH bene_id=hicno _1B=provid));
	factype="IRF";
	provid=tranwrd(provid,"T","0");
	provid=tranwrd(provid,"R","1");
run;

proc freq data=irf;
	tables _20a _20b _21a _21d _22 _39: / missing;
run;

/*Add in pmt_amt for IRFs from MedPAR*/
/*test MedPAR provider ID for alpha characters in 3rd digit*/
data test_provid;
	set med.medpar2011 (keep=prvdrnum);
	alpha=substr(prvdrnum,3,1);
run;
proc freq data=test_provid;
	tables alpha;
run;

data irf_pmt (rename=(bene_id=hicno admsndt=ADMIT prvdrnum=provid));
	set med.medpar2011 (keep=prvdrnum pmt_amt bene_id admsndt spclunit)
		med.medpar2012 (keep=prvdrnum pmt_amt bene_id admsndt spclunit)
		med.medpar2013 (keep=prvdrnum pmt_amt bene_id admsndt spclunit);
	if 3025<=(input(substr(prvdrnum,length(prvdrnum)-3,4),?? 4.))<=3099 | spclunit in("R","T");
run;

proc sort data=irf; by hicno ADMIT provid; run;
proc sort data=irf_pmt nodupkey; by hicno ADMIT provid; run;

data irf;
	merge irf (in=a)
		  irf_pmt (in=b);
	by hicno admit provid;
	if a;
	if a & b then match=1;
	if a & ~b then match=0;
run;

proc freq data=irf;
	tables match spclunit;
	where year(admit)>=2011;
run;

data test_irf;
	merge irf (in=a)
		  irf_pmt (in=b);
	by hicno admit provid;
	if a & b then match=1;
	if a & ~b then match=2;
	if ~a & b then match=3;
run;

proc freq data=test_irf;
	tables match;
	where year(admit)>=2011; 
run;

/*Test out a fuzzy merge as well*/
proc sql;
	create table fuzzy_irf as
	select * from irf as irf full join irf_pmt as irf_pmt
	on irf.hicno = irf_pmt.hicno  and (-1<=(irf.admit-irf_pmt.admit)<=1) and irf.provid=irf_pmt.provid;
quit;

data fuzzy_irf;
	set fuzzy_irf;
	if pmt_amt~=. & factype~='' then match=1;
	if pmt_amt=. & factype~='' then match=2;
	if pmt_amt~=. & factype='' then match=3;
run;

proc sort data=fuzzy_irf nodupkey; by hicno admit provid; run;

proc freq data=fuzzy_irf;
	tables _20a*match;
	where year(admit)>=2011; 
run;
	
proc contents data=med.medpar2011; run;

data snf;
	length provid $6;
	set med.medpar2011 (keep=bene_id PRVDRNUM admsndt dschrgdt pmt_amt 
					    rename=(bene_id=hicno admsndt=ADMIT dschrgdt=DISCH))
		med.medpar2012 (keep=bene_id PRVDRNUM admsndt dschrgdt pmt_amt
					    rename=(bene_id=hicno admsndt=ADMIT dschrgdt=DISCH))
		med.medpar2013 (keep=bene_id PRVDRNUM admsndt dschrgdt pmt_amt
					    rename=(bene_id=hicno admsndt=ADMIT dschrgdt=DISCH));
	if 5000<=(input(substr(PRVDRNUM,length(PRVDRNUM)-3,4),?? 4.))<=6499;
	PROVID=PRVDRNUM;
	FACTYPE="SNF";
run;

proc print data=snf (obs=10);
	var provid pmt_amt;
run;

proc contents data=snf; run;

/*Test how many MDS stays overlap with MedPAR SNF stays. Just a test, this is handled later in the code*/
proc sort data=snf nodupkey out=test_snf; by hicno admit provid; run;
proc sort data=mds nodupkey out=test_mds; by hicno admit provid; run;

data merge_snf_mds;
	merge test_snf (in=a)
		  test_mds (in=b);
	by hicno admit provid;
	if a & b then match=1;
	if a & ~b then match=2;
	if ~a & b then match=3;
run;

proc freq data=merge_snf_mds;
	tables match;
run;

/*Test out a fuzzy merge as well*/
proc sql;
	create table fuzzy_snf as
	select * from test_snf as snf join test_mds as mds
	on snf.hicno = mds.hicno  and (-1<=(snf.admit-mds.admit)<=1) and snf.provid=mds.provid;
quit;

data fuzzy_snf;
	set fuzzy_snf;
	if pmt_amt=. then miss_pmt=1;
	else miss_pmt=0;
run;

proc sort data=fuzzy_snf nodupkey; by hicno admit provid; run;

proc freq data=fuzzy_snf;
	tables miss_pmt*factype;
	where year(admit)>=2011; 
run;

/*Append the PAC datasets together*/
data data_sty.stay_dataset_all;
	length provid $10;
	set data_sty.&stay_dataset.
		mds
		irf
		snf;
run;


PROC SORT DATA=data_sty.stay_dataset_all;
     BY hicno admit disch provid;
RUN;

***-----------------------------------***;
***  Add nobs                         ***;
***-----------------------------------***;
DATA data_sty.stay_dataset_all; 
	SET data_sty.stay_dataset_all;
	nobs = _N_;
RUN;

proc contents data=out.pta_in_base_dataset; run;
proc contents data=data_sty.stay_dataset_all; run;
