/************************************************************************************************************
************************************************************************************************************
Program: gen_pta_in_datasets4
Created By: Brendan Rabideau
Created Date: 5/15/15
Updated Date: 11/24/15
Purpose: The purpose of this program is to to generate the raw datasets used in Yale's Analytic Files, 
  		 which create datasets to be used in Yale's 30day Unplanned Readmission Algorithim. The program creates
  		 the pta_in_base and pta_in_line datasets.

Notes: 
Updates:
	6/12/15 - Added years going back to 2002, updated to match Yale's inclusion criteria
    6/24/15 - Updated 2002-2005 to use EHIC-bene_id xwalked datasets
			- Stopped collapsing data down to the stay-level since Yale's code already does this (macro'd out code, %macro collapse)
			- Removed MedPAR merge and created desired variables according to Resdac specifications instead
	6/25/15 - clm_id (becomes hse_unique_id and merges line and base) likely claimindex pre-2005. Reformatted to match clm_id
	7/06/15 - Dropped obs with admissions before the 1st year of our data (2002) 
	7/29/15 - Updated to include IRF stays as well as SNF stays in the post acute care stay file
	11/24/15- Converting for the MedAdvantage project
	11/24/15- Making the process of reading in raw data more automated - macro-ing libnames, making start and end year set statements self-generating
	12/09/15- Added HHA and Outpatient to the pta_base_dataset_pac dataset
************************************************************************************************************
************************************************************************************************************/

options compress=yes mprint;

%let out = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Raw;
libname out "&out.";
libname mds "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MDS";

%let include = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/MedAdvantage;
%include "&include./00_Assign_Macro_Variables_and_Libraries.sas";
%let syr=20&YY.;
%let eyr=20&YYE.;

/************************************************************************************
PART A INPATIENT BASE DATASET (IP Claims)
************************************************************************************/

/*Use raw MedPAR files - standardize names as necessary - definitely different pre-2010*/

			libname med "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Processed";

			data med; 
				set med.med_mds3_temp (where=(bene_id~="")) /*2011-2013, MDS 3.0*/
					/*med.med_mds  (where=(bene_id~=""))*/; /*2009-2010, MDS 2.0*/
				/*Renaming variables that have the # in the middle. Only occurs post-2010*/
				array poa_dgns {25} $ poa_dgns_1_ind_cd  poa_dgns_2_ind_cd  poa_dgns_3_ind_cd  poa_dgns_4_ind_cd  poa_dgns_5_ind_cd
									  poa_dgns_6_ind_cd  poa_dgns_7_ind_cd  poa_dgns_8_ind_cd  poa_dgns_9_ind_cd  poa_dgns_10_ind_cd
	                                  poa_dgns_11_ind_cd poa_dgns_12_ind_cd poa_dgns_13_ind_cd poa_dgns_14_ind_cd poa_dgns_15_ind_cd
	                                  poa_dgns_16_ind_cd poa_dgns_17_ind_cd poa_dgns_18_ind_cd poa_dgns_19_ind_cd poa_dgns_20_ind_cd
									  poa_dgns_21_ind_cd poa_dgns_22_ind_cd poa_dgns_23_ind_cd poa_dgns_24_ind_cd poa_dgns_25_ind_cd;

				array poa_dgns_e {12} $ poa_dgns_e_1_ind_cd  poa_dgns_e_2_ind_cd  poa_dgns_e_3_ind_cd  poa_dgns_e_4_ind_cd
								        poa_dgns_e_5_ind_cd  poa_dgns_e_6_ind_cd  poa_dgns_e_7_ind_cd  poa_dgns_e_8_ind_cd
								        poa_dgns_e_9_ind_cd  poa_dgns_e_10_ind_cd poa_dgns_e_11_ind_cd poa_dgns_e_12_ind_cd;

				array dgns_e {12}	  $ dgns_e_1_cd dgns_e_2_cd dgns_e_3_cd dgns_e_4_cd  dgns_e_5_cd  dgns_e_6_cd
										dgns_e_7_cd dgns_e_8_cd dgns_e_9_cd dgns_e_10_cd dgns_e_11_cd dgns_e_12_cd;

				array POANCD {25}     $ POANCD1-POANCD25;
				array POAEND {12} 	  $ POAEND01-POAEND12;
				array EDGSCD {12}     $ EDGSCD01-EDGSCD12;

				do j=1 to 25;
					POANCD{j} = poa_dgns{j};
					if j <= 12 then do;
						POAEND{j} = poa_dgns_e{j};
						EDGSCD{j} = dgns_e{j};
					end;
				end;

				/*Standardize vartype for important vars; drop problematic, superfluous vars*/
 				c_drg_cd=put(DRG_CD,3.);
				c_DSTNTNCD=put(DSTNTNCD,2.);
				drop  ADMSNDAY PHRMCYCD TRNSPLNT ONCLGYSW DGNSTCSW THRPTCSW NUCLR_SW CTSCANSW 
					  IMGNG_SW OPSRVCCD DRG_CD DSTNTNCD OUTLR_CD ESRD_CD IPSBCD FILDTCD SMPLSIZE WRNGCD;
				rename c_drg_cd=DRG_CD
					   c_DSTNTNCD=DSTNTNCD
					   TOTCHRG=tot_chg
					   PRVDR_NUM=PRVDRNUM
					   admsn_dt=admsndt
					   dschrg_dt=dschrgdt;
			run;

data med (keep= dgnscd1-dgnscd25
				dgns_vrsn_cd_1-dgns_vrsn_cd_25
				poancd1-poancd25
				poaend01-poaend12
				edgscd01-edgscd12
				dgns_e_vrsn_cd_1-dgns_e_vrsn_cd_12
				prcdrcd1-prcdrcd25
				srgcl_prcdr_vrsn_cd_1-srgcl_prcdr_vrsn_cd_25
				prcdrdt1-prcdrdt25
			    /*Other Vars*/
			    bene_id medparid npi_at upin_at npi_op upin_op clm_type mdcl_rec admsour
				prvdrnum state_cd type_adm dschrgdt admsndt src_adms dstntncd dschrgcd drg_cd
				icuindcd crnry_cd sslssnf pmt_amt tot_chg 
				report_ma ma_claim entry_dt disch_dt final_dt first_rug first_cog last_cog disch_cd rug
				five_day_assess index_condition drop_obs qtr_assess ghopdcd RUG4 spclunit);

	set	med;	
		
		/*Vars we don't have and aren't important, but are required to exist to make the proceding programs run*/
		NPI_AT="";
		UPIN_AT="";
		NPI_OP="";
		UPIN_OP="";
		MDCL_REC="";
		ADMSOUR = "";

		if icuindcd ~in("","6") then icuind=1;
		if crnry_cd~="" then ccuind=1;
run;

proc freq data=med;
	tables sslssnf clm_type;
run;

proc sort data=med; by bene_id admsndt dschrgdt; run;

/*Subset and clean up the dataset to make it look like Yale's pta_in_base_dataset*/
data out.pta_in_base_dataset (keep = ADMIT ADMSOUR BENE_CLM_NUM CASE_TYPE CLM_ADMSN_DT DDEST DIAG1 DIAG2 DIAG3 
									 DIAG4 DIAG5 DIAG6 DIAG7 DIAG8 DIAG9 DIAG10 DIAG11 DIAG12 DIAG13 DIAG14 DIAG15 
									 DIAG16 DIAG17 DIAG18 DIAG19 DIAG20 DIAG21 DIAG22 DIAG23 DIAG24 DIAG25 DISCH 
									 DISST DRGCD DVRSND01 DVRSND02 DVRSND03 DVRSND04 DVRSND05 DVRSND06 DVRSND07 DVRSND08 
									 DVRSND09 DVRSND10 DVRSND11 DVRSND12 DVRSND13 DVRSND14 DVRSND15 DVRSND16 DVRSND17 
									 DVRSND18 DVRSND19 DVRSND20 DVRSND21 DVRSND22 DVRSND23 DVRSND24 DVRSND25 EDGSCD01 
									 EDGSCD02 EDGSCD03 EDGSCD04 EDGSCD05 EDGSCD06 EDGSCD07 EDGSCD08 EDGSCD09 EDGSCD10 
									 EDGSCD11 EDGSCD12 EVRSCD01 EVRSCD02 EVRSCD03 EVRSCD04 EVRSCD05 EVRSCD06 EVRSCD07 
									 EVRSCD08 EVRSCD09 EVRSCD10 EVRSCD11 EVRSCD12 HICNO HSE_UNIQUE_ID MDCL_REC MSCD 
									 NCH_CLM_TYPE_CD NPI_AT NPI_OP POAEND01 POAEND02 POAEND03 POAEND04 POAEND05 POAEND06 
									 POAEND07 POAEND08 POAEND09 POAEND10 POAEND11 POAEND12 POANCD1 POANCD2 POANCD3 POANCD4 
									 POANCD5 POANCD6 POANCD7 POANCD8 POANCD9 POANCD10 POANCD11 POANCD12 POANCD13 POANCD14 POANCD15 
									 POANCD16 POANCD17 POANCD18 POANCD19 POANCD20 POANCD21 POANCD22 POANCD23 POANCD24 POANCD25 
									 PROC1 PROC2 PROC3 PROC4 PROC5 PROC6 PROC7 PROC8 PROC9 PROC10 PROC11 PROC12 PROC13 PROC14 
									 PROC15 PROC16 PROC17 PROC18 PROC19 PROC20 PROC21 PROC22 PROC23 PROC24 PROC25 PROCDT1 PROCDT2
									 PROCDT3 PROCDT4 PROCDT5 PROCDT6 PROCDT7 PROCDT8 PROCDT9 PROCDT10 PROCDT11 PROCDT12 PROCDT13 
    								 PROCDT14 PROCDT15 PROCDT16 PROCDT17 PROCDT18 PROCDT19 PROCDT20 PROCDT21 PROCDT22 PROCDT23 
									 PROCDT24 PROCDT25 PROVID PSTATE_ALPHA PVSNCD01 PVSNCD02 PVSNCD03 PVSNCD04 PVSNCD05 
									 PVSNCD06 PVSNCD07 PVSNCD08 PVSNCD09 PVSNCD10 PVSNCD11 PVSNCD12 PVSNCD13 PVSNCD14 PVSNCD15 
									 PVSNCD16 PVSNCD17 PVSNCD18 PVSNCD19 PVSNCD20 PVSNCD21 PVSNCD22 PVSNCD23 PVSNCD24 PVSNCD25 
									 TYPEADM UPIN_AT UPIN_OP factype sslssnf pmt_amt tot_chg 
									 report_ma ma_claim entry_dt disch_dt final_dt first_rug first_cog last_cog disch_cd rug
									 five_day_assess index_condition drop_obs ghopdcd qtr_assess RUG4 spclunit);
	set med (where=(bene_id ~= "" & admsndt > MDY(01,01,20&YY.))); /*No admissions before the 1st date of our data*/

	label
		bene_id  = "Encrypted 723 Beneficiary ID"
		medparid = "Unique Key for CCW MedPAR Table"
		prvdrnum = "MEDPAR Provider Number"
		admsndt	 = "Date beneficiary admitted for Inpatient care or date care started"
		type_adm = "Type and priority of benes admission to facility for Inp hosp stay code"
		dschrgdt = "Date beneficiary was discharged or died"
		dschrgcd = "Code identifying status of patient as of CLM_THRU_DT"
		dstntncd = "Destination upon discharge from facility code"
		clm_type = "NCH Claim Type Code"
		crnry_cd = "Coronary care unit type code"
		icuindcd = "ICU type code";

	/*Rename variables to match the Yale programs*/
	rename dgnscd1-dgnscd25               				  = DIAG1-DIAG25
		   dgns_vrsn_cd_1-dgns_vrsn_cd_25     			  = DVRSND01-DVRSND25
		   prcdrcd1-prcdrcd25             				  = PROC1-PROC25
		   srgcl_prcdr_vrsn_cd_1-srgcl_prcdr_vrsn_cd_25   = PVSNCD01-PVSNCD25
		   prcdrdt1-prcdrdt25                    		  = PROCDT1-PROCDT25
		   dgns_e_vrsn_cd_1-dgns_e_vrsn_cd_12			  = EVRSCD01-EVRSCD12

			bene_id  	= hicno 
			medparid	= hse_unique_id
			prvdrnum	= PROVID
			admsndt  	= ADMIT
			type_adm 	= TYPEADM
			dschrgdt 	= DISCH
			dschrgcd 	= DISST
			dstntncd 	= DDEST
			clm_type 	= NCH_CLM_TYPE_CD
			crnry_cd 	= CCUIND
			icuindcd 	= ICUIND
			qlfyfrom 	= from_dt
			qlfythru	= thru_dt
			drg_cd      = DRGCD
			MS_CD       = MSCD
			state_cd    = PSTATE_ALPHA;

	CASE_TYPE = "CMS";
	factype="STA";

	/*Stay is inpatient, not PAC*/
	if 1<=(input(substr(PRVDRNUM,length(PRVDRNUM)-3,4),?? 4.))<=899 then output out.pta_in_base_dataset;
run;

proc sort data=out.pta_in_base_dataset; by HICNO ADMIT DISCH; run;

/************************************************************************************
PRINT CHECK
************************************************************************************/
proc contents data=out.pta_in_base_dataset out=in_base_cont (keep=NAME TYPE LENGTH VARNUM LABEL FORMAT);
run;
proc contents data=out.pta_in_base_dataset_pac out=cont_pac (keep=NAME TYPE LENGTH VARNUM LABEL FORMAT);
run;
/*Output a sample of each of the datasets to an excel workbook*/
ods tagsets.excelxp file="&out./look_pta_in.xml" style=sansPrinter;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="MedPAR on Inpatient" frozen_headers='yes');
proc print data=out.pta_in_base_dataset (obs=1000);
run;
ods tagsets.excelxp options(absolute_column_width='20' sheet_name="Inpatient Contents" frozen_headers='yes');
proc print data=in_base_cont (obs=1000);
run;
ods tagsets.excelxp close;
/*********************
CHECK END
*********************/


