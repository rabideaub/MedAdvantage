/************************************************************************************************************
                               SAS Program Description

PROGRAM NAME: 0C_Create_DX_codes_table

PURPOSE:      Create table with list of DX diag or proc codes to use in Analytic file programs 

OVERVIEW: 	  Hard code list of diag or proc codes for measure to use in other programs
			  so that list only needs to be created in one place.

INPUT DATA: 
	List of DX daig or proc codes

OUTPUT FILES: 
	dx_data.&DX._codes

************************************************************************************************************/

***************************************************************************************************
* Modified by Joe on 
* Changes made: 
* 1.) 
*
***************************************************************************************************;
OPTIONS COMPRESS=YES;

DATA _NULL_;
StartDate = "%SYSFUNC(DATE(),WORDDATE.)";
StartTime = "%SYSFUNC(TIME(),TIME.)";
PUT "===================================================================================";
PUT "  STARTING JOB INFORMATION:" /;
PUT "  Job Name: 0_Create_PN_diag_codes_table" /;
PUT "  Start Date: " StartDate ;
PUT "  Start Time: " StartTime ;
PUT "===================================================================================";
RUN;

%let include = /disk/agedisk3/medicare.work/goldman-DUA25731/rabideau/Programs/Vert_Int;
%include "&include./00_Assign_Macro_Variables_and_Libraries.sas";

****************************************************************************************
*BUILD TABLES WITH LIST OF DX CODES
****************************************************************************************;
DATA dx_data.MI_codes;
	INPUT diagcode $;
	DATALINES;
41000 
41001 
41010 
41011 
41020 
41021 
41030 
41031 
41040 
41041 
41050 
41051 
41060 
41061 
41070 
41071 
41080 
41081 
41090 
41091 
4111
41181
41189
4130
4131
4139
;
RUN;


DATA dx_data.HF_codes;
	INPUT diagcode $;
	DATALINES;
40201
40211
40291
40401
40403
40411
40413
40491
40493
4280
4281
42820
42821
42822
42823
42830
42831
42832
42833
42840
42841
42842
42843
4289
51881
51882
51884
7991
;
RUN;


DATA dx_data.PN_codes;
	INPUT diagcode $;
	DATALINES;
4800 
4801 
4802 
4803 
4808 
4809 
481  
4820 
4821 
4822 
48230
48231
48232
48239
48240
48241
48242
48249
48281
48282
48283
48284
48289
4829 
4830 
4831 
4838 
485  
486  
4870 
48811
488
48801
48242
99731
99739
507
5070
5071
5078
99591
99592
038
0380
0381
03810
03811
03812
03819
0382
0383
0384
03840
03841
03842
03843
03844
03849
0388
0389
78552
51881
51882
51884
7991
;
RUN;


DATA dx_data.CD_codes;
	INPUT diagcode $;
	DATALINES;
49121
49122
4918
4919
4928
49320
49321
49322
496
491
4910
4911
4912
49120
492
4920
494
4940
4941
51881
51882
51884
7991
4660
490
;
RUN;


DATA dx_data.SK_codes;
	INPUT diagcode $;
	DATALINES;
43301
43311
43321
43331
43381
43391
43401
43411
43491
436
36234
430
431
4320
4321
4329
43300
43310
43320
43330
43340
43350
43360
43370
43380
43390
43400
4350
4351
4352
4353
4358
4359
4380
43810
43811
43812
43819
43820
43821
43822
43830
43831
43832
43840
43841
43842
43850
43851
43852
43853
4386
4387
43881
43882
43883
43884
43885
43889
4389
;
RUN;


DATA dx_data.HKR_codes;
	INPUT proccode $;
	DATALINES;
8151
8154
8152
8153
0070
0071
0072
0073
0085
0086
0087
8155
8159
0080
0081
0082
0083
0084
;
RUN;


DATA dx_data.HKC_codes;
	INPUT proccode $;
	DATALINES;
8151
8154
8152
8153
0070
0071
0072
0073
0085
0086
0087
8155
8159
0080
0081
0082
0083
0084
;
RUN;



TITLE "&DX Measure Codes";
PROC PRINT DATA=dx_data.&DX._codes;
RUN;
TITLE;


DATA _NULL_;
EndDate = "%SYSFUNC(DATE(),WORDDATE.)";
EndTime = "%SYSFUNC(TIME(),TIME.)";
PUT "===================================================================================";
PUT "  ENDING JOB INFORMATION:" /;
PUT "  Job Name: 0_Create_PN_diag_codes_table.sas"/;
PUT "  End Date: " EndDate;
PUT "  End Time: " EndTime ;
PUT "===================================================================================";
RUN;
