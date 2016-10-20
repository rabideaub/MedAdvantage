/************************************************************************************************************
                               SAS Program Description

PROGRAM NAME: 2_add_Bene_data

FOR USE WITH: ALL MEASURES

PURPOSE:      add bene demographic fields and calculated fields 

OVERVIEW:    merge in bene demographic data and create calculated fields.  
			 run some data checks.

INPUT DATA: 
	dx_data.index01          *PRE-SORTED BY HICNO

OUTPUT FILES: 
	dx_data.index02

************************************************************************************************************/

OPTIONS COMPRESS=YES;

DATA _NULL_;
StartDate = "%SYSFUNC(DATE(),WORDDATE.)";
StartTime = "%SYSFUNC(TIME(),TIME.)";
PUT "===================================================================================";
PUT "  STARTING JOB INFORMATION:" /;
PUT "  Job Name: 2_add_Bene_data" /;
PUT "  Start Date: " StartDate ;
PUT "  Start Time: " StartTime ;
PUT "===================================================================================";
RUN;

%let include = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/MedAdvantage;
%include "&include./00_Assign_Macro_Variables_and_Libraries.sas";

********************************************************************; 
*ADD BENE DATA
********************************************************************; 
proc sort data=data_ben.&bene_dataset.; by hicno year; run;
proc sort data=dx_data.index01; by hicno year; run;

DATA index02;
	MERGE dx_data.index01 (IN=A)
		  data_ben.&bene_dataset. (IN=B KEEP=hicno birth death edbvdeth sex race year cstshr:
											  county zip bstate year AGE BUYIN_MO RTI_RACE_CD 
									RENAME=(death=cms_death));
	BY hicno year;
	IF A;

	/*CREATE FLAG FOR DIED IN HOSPITAL TO USE TO FLAG UNRELIABLE DEATHS*/          
	hospdead=(ddest=20);   


	/*ASSIGN CMS DATA - ONLY USE VERIFIED DEATH*/
	IF death=. AND edbvdeth='V' THEN death=cms_death; /*Change edbvdeth='Y' to edbvdeth='V'; BR 7/8/15*/

	/*ONLY KEEP CMS CASES IF THEY HAVE BENE DATA*/
	IF (A AND B) THEN OUTPUT index02;
RUN;

TITLE 'VALIDATE SEX FIELDS';
PROC FREQ DATA=index02;
	TABLE sex
		  /LIST MISSING;
RUN;

TITLE "VALIDATE BIRTH AND DEATH FIELDS FOR CMS CASES"; 
PROC PRINT DATA=index02 (OBS=10);                                             
	VAR birth edbvdeth cms_death death;              
	FORMAT death birth admit disch YYMMDD10.;   
RUN;                                                                            
                                                                    
**********************************************************************;



********************************************************************; 
*CREATE UNRELIABLE DEATH AND DEMOGRAPHICS INDICATORS
********************************************************************; 
PROC SORT DATA=index02                                                          
	OUT=index02_hosp;                                                              
	BY hicno case hospdead provid;                                                 
RUN;  


DATA index02_U(DROP=UNREL);                                           
	SET index02_hosp;                                                              
	BY hicno;                                                                      
	RETAIN unrel;                                                                  
	IF FIRST.hicno THEN unrel=0;                                                   
	IF hospdead=1 & NOT (LAST.hicno) THEN unrel=1;                                 
	                                                                            
	/*UNRELIABLE DEATH INDICATOR - USES EDB DEATH*/                                
	unreldth=0;                                                                    
	IF (unrel=1 & death=.) OR (death NE . AND death<admit) THEN unreldth=1;        
	ELSE IF (death>=admit AND death<disch AND hospdead=0) THEN unreldth=2;         
	                                                                            
	/*UNRELIABLE DEMOGRAPHICS INDICATOR*/                                          
	unreldmg=0;                                                                    
	IF (INT((admit-birth)/365.25)>115) THEN unreldmg=1;                            
	ELSE IF (sex NE '1' AND sex NE '2') THEN unreldmg=2;                           
RUN;  


TITLE "CHECK OF UNRELIABLE DEATH INDICATOR=1"; 
TITLE2 "ADMISSION DATE FOLLOWING DEATH DATE FROM EDB"; 
PROC PRINT DATA=index02_U (OBS=10);                                             
	VAR hicno case death birth sex admit disch provid ddest unreldth;              
	WHERE unreldth=1;                                                              
	FORMAT death birth admit disch YYMMDD10.;                                       
RUN;                                                                            
                                                                            
TITLE "CHECK OF UNRELIABLE DEATH INDICATOR=2";                                 
TITLE2 "DATE OF DEATH BEFORE DISCHARGE, BUT DISCHARGED ALIVE"; 
PROC PRINT DATA=index02_U (OBS=10);                                             
	VAR hicno case death birth sex admit disch provid ddest unreldth;              
	WHERE unreldth=2;                                                              
	FORMAT death birth admit disch YYMMDD10.;                                       
RUN;                                                                            
                                                                            
TITLE "CHECK OF UNRELIABLE AGE/GENDER INDICATOR=1";                            
TITLE2 " AGE > 115 YEARS AT TIME OF ADMIT"; 
PROC PRINT DATA=index02_U (OBS=10);                                             
	VAR hicno case death birth sex admit disch provid ddest unreldmg;              
	WHERE unreldmg=1;                                                              
	FORMAT death birth admit disch YYMMDD10.;                                       
RUN;                                                                            
                                                                                
TITLE "CHECK OF UNRELIABLE AGE/GENDER INDICATOR=2";                            
TITLE2 "SEX MISSING/INVALID"; 
PROC PRINT DATA=index02_U (OBS=10);                                             
	VAR hicno case death birth sex admit disch provid ddest unreldmg;              
	WHERE unreldmg=2;                                                              
	FORMAT death birth admit disch YYMMDD10.;                                       
RUN;                                                                            

TITLE "NUMBER OF UNRELIABLE DEATH/DEMO CASES";
TITLE2; 
PROC FREQ DATA = index02_U;
	TABLES unreldth unreldmg /LIST MISSING;
RUN;
**********************************************************************;


                                                                                
********************************************************************; 
*CHECK FOR DUPLICATES BY ADMIT, DISCH, PROVID
********************************************************************; 
PROC SORT DATA=index02_U;                                                       
	BY hicno admit disch provid trans_first;
RUN;                                                                            

DATA dx_data.index02(DROP=hospdead) dup;
	SET index02_u;
	BY hicno admit disch provid;
	IF LAST.provid THEN OUTPUT dx_data.index02;
	ELSE OUTPUT dup;
RUN;

TITLE "PRINT OF DUPLICATES IN INDEX02";
PROC PRINT DATA=dup;
RUN;  
**********************************************************************;



********************************************************************; 
*SORT FOR MERGE IN NEXT STEP
********************************************************************; 
PROC SORT DATA= dx_data.index02;
	BY hicno case;
RUN;

proc freq data=dx_data.index02;
	tables ma_claim year;
run;
**********************************************************************;



/*DELETE WORK AND INITIAL DATASET TO CLEAN UP THE EG PROCESS FLOW*/
PROC DELETE DATA=index02 index02_hosp index02_U dup; 
RUN;
TITLE;

DATA _NULL_;
EndDate = "%SYSFUNC(DATE(),WORDDATE.)";
EndTime = "%SYSFUNC(TIME(),TIME.)";
PUT "===================================================================================";
PUT "  ENDING JOB INFORMATION:" /;
PUT "  Job Name: 2_add_Bene_data" /;
PUT "  End Date: " EndDate ;
PUT "  End Time: " EndTime ;
PUT "===================================================================================";
RUN;
