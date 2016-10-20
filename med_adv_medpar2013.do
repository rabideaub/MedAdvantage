#delimit;
set more off;
clear;
capture: log close;

global dat "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data";
global medpar "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/PAC/Data/Raw/MedPAR";
global denom "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/PAC/Data/Raw/Denominator";
global out "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Output";
global temp "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/Temp";
global logdir "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/MedAdvantage";

log using "${logdir}/med_adv_medpar.log",replace;
capture: erase "${out}/medicare_advantage_comparison.xlsx";

/*This file should be unique on provider but has 108 duplicates (out of 6208). Keep the most recent according to 'fiscal year begin'*/

forvalues yr=2009/2013 {;
	/*This file is found here: http://www.nber.org/data/hcris.html*/
	use "${dat}/nber_ime_dsh_`yr'.dta",clear;
	gsort prvdr_num -fyb;
	duplicates drop prvdr_num,force;
	gen num_check = real(substr(prvdr_num,-4,4));
	gen stach=0;
	replace stach=1 if num_check<=899 & num_check>=1 & num_check!=.; /*Short stay, acute care*/
	rename prvdr_num PRVDR_NUM;
	save "${temp}/nber_ime_dsh_`yr'.dta",replace;
	gen report_ma = 0;
	summarize ime1, detail; /*Added 9-13-16 BR.*/
	summarize dsh1, detail; /*Added 9-13-16 BR.*/
	replace report_ma = 1 if ime1!=. | dsh1!=.;
	tab report_ma;
	tab2 report_ma stach;

	/*Read in the `yr' denominator information*/
	use "${denom}/mbsf_ab_summary`yr'",clear;
	describe, simple;
	keep if BENE_ID ~="";
	describe,simple; 
	keep BENE_ID HMOIND01-HMOIND12;

	/*Rename the variables to strip the leading 0 in single digits*/
	forvalues num = 1/9 {;
		rename HMOIND0`num' HMOIND`num';
	};

	sort BENE_ID; 
	duplicates drop BENE_ID, force;
	save "${temp}/mbsf_ab_summary`yr'.dta",replace;

	/*********************************************************************************
	Create a profile of Medicare Advantage Reporting vs Non-Reporting Hospitals
	*********************************************************************************/
	import excel "${dat}/imppuf08_103107.xls",sheet("imppuf08_103107") firstrow clear;
	gen num_check = real(substr(ProviderNumber,-4,4));
	*keep if num_check<=899 & num_check>=1 & num_check!=.; /*Short stay, acute care*/

	/*Identify teaching hospitals (IME, DME) and Disproportionate Share Adjustment hospital (DSH)*/
	sort ProviderNumber;
	rename ProviderNumber PRVDR_NUM;
	merge 1:1 PRVDR_NUM using "${temp}/nber_ime_dsh_`yr'.dta",keepusing(ime1 dsh1 stach);
	keep if _merge==3;
	drop _merge;

	/*Generate and format some variables of interest*/
	gen report_ma = 0;
	replace report_ma = 1 if ime1!=. | dsh1!=.;
	tab report_ma;

	gen RURAL = 0;
	replace RURAL = 1 if URSPA=="RURAL";
	gen URBAN = 0; 
	replace URBAN = 1 if regexm(URSPA,"URBAN");

	destring ADC MCR_PCT, replace;
	gen counter=1;
	preserve;

	/*Generate some summary stats comparing Medicare Advantage reporting hopsitals and non-reporting hospitals*/
	tabstat stach BEDS ADC RURAL URBAN MCR_PCT CMIV24 CMIV25, by(report_ma) stat(mean sd min max n) nototal;
	foreach stat in mean median sd min max {;
		restore,preserve;
		collapse (count) counter (`stat') BEDS ADC RURAL URBAN MCR_PCT CMIV24 CMIV25 stach, by(report_ma);
		xpose, varname clear;
		rename v1 Non_Reporting_`stat';
		rename v2 MA_Reporting_`stat';
		rename _varname Variable;
		order Variable MA_Reporting Non_Reporting;
		sort Variable;
		drop if Variable == "report_ma";
		save "${temp}/temp_`stat'",replace;
	};
	/*Format the summary stats*/
	restore, not;
	use "${temp}/temp_mean.dta",clear;
	merge Variable using "${temp}/temp_sd.dta" "${temp}/temp_sd.dta" "${temp}/temp_median.dta" "${temp}/temp_min.dta" "${temp}/temp_max.dta";
	drop _merge*;
	gsort -Variable;
	replace Variable = "Average Daily Census" if Variable == "ADC";
	replace Variable = "Medicare days as a percent of total inpatient days" if Variable == "MCR_PCT";
	replace Variable = "Case Mix Index under Grouper V24 for SCH providers paid under their hospital specific rate" if Variable == "CMIV24";
	replace Variable = "Case Mix Index under Grouper V25 for SCH providers paid under their hospital specific rate" if Variable == "CMIV25";
	export excel "${out}/medicare_advantage_comparison.xlsx", sheet("Hospital Comparisons `yr'") firstrow(variables);


	/*********************************************************************************
	Report MedPAR Claims Informtion Comparing MA Reporting vs Non-Reporting Hospitals 
	*********************************************************************************/
	/*Read in the MedPAR dataset - keep only STACHs*/
	use "${medpar}/medpar`yr'.dta",clear;
	describe, simple;
	rename prvdrnum PRVDR_NUM;
	rename admsndt ADMSN_DT;
	rename dschrgdt DSCHRG_DT;
	gen num_check = real(substr(PRVDR_NUM,-4,4));
	*keep if num_check >= 1 & num_check <=899;
	sort BENE_ID;

	/*Group MS-DRGs for conditions of interest*/
	gen index_condition = "Other";
	replace index_condition = "CHF" if inlist(DRG_CD,"291","292","293");  
	replace index_condition = "LEJR" if inlist(DRG_CD,"469","470");  
	replace index_condition ="Pneumonia" if inlist(DRG_CD,"177","178","179","193","194","195");  
	replace index_condition = "COPD"  if inlist(DRG_CD,"190","191","192","202","203"); 
	replace index_condition = "Femur Fracture" if inlist(DRG_CD,"533","534","535","536");  
	replace index_condition = "Hip Femur Proc" if inlist(DRG_CD,"480","481","482"); 
	replace index_condition = "Stroke" if inlist(DRG_CD,"061","062","063","064","065","066");  

	gen Other=index_condition=="Other";
	gen CHF=index_condition=="CHF";
	gen LEJR=index_condition=="LEJR";
	gen Pneumonia=index_condition=="Pneumonia";
	gen COPD=index_condition=="COPD";
	gen Femur_Fracture=index_condition=="Femur Fracture";
	gen Hip_Femure=index_condition=="Hip Femur Proc";
	gen Stroke=index_condition=="Stroke";

	/*Merge on the 2008 NBER IME and DSH info dataset*/
	sort PRVDR_NUM;
	merge m:1 PRVDR_NUM using "${temp}/nber_ime_dsh_`yr'.dta",keepusing(ime1 dsh1 stach);
	
	*Keep if MedPAR matches NBER IME DSH dataset;
	keep if _merge==3 & year(DSCHRG_DT)==`yr';
	drop _merge;

	/*Merge on the denominator files and make HMO flags*/
	sort BENE_ID;
	merge m:1 BENE_ID using "${temp}/mbsf_ab_summary`yr'";
	keep if _merge==3;
	drop _merge;
	gen adm_month=month(ADMSN_DT);
	gen disch_month=month(DSCHRG_DT);
	gen adm_hmo=0;
	gen stay_hmo=0;
	forvalues num = 1/12 {;
		/*If the bene had a positive HMO indicator for the admission month*/
		replace adm_hmo=1 if adm_month==`num' & inlist(HMOIND`num',"1","2","A","B","C");
	};
	/*Must be in 2 separate loops to account for the possibility for a december admission and january discharge*/
	forvalues num = 1/12 {;
		/*If the bene had a positive HMO indicator for both the admission and discharge months*/
		replace stay_hmo=1 if adm_hmo==1 & disch_month==`num' & inlist(HMOIND`num',"1","2","A","B","C");
	};

	/*Add Elixhauser*/
	/*Elixhaus - interactive only?*/
	egen _obs=seq();
	sort BENE_ID ADMSN_DT;
	save "${temp}/temp.dta", replace;
	destring DRG_CD,generate(DRG_NUM);
	keep _obs DRG_NUM DGNSCD1 DGNSCD2 DGNSCD3 DGNSCD4 DGNSCD5 DGNSCD6 DGNSCD7 DGNSCD8 DGNSCD9 DGNSCD10;
	rename (DGNSCD1 DGNSCD2 DGNSCD3 DGNSCD4 DGNSCD5 DGNSCD6 DGNSCD7 DGNSCD8 DGNSCD9 DGNSCD10)
	       (DGNS_CD1 DGNS_CD2 DGNS_CD3 DGNS_CD4 DGNS_CD5 DGNS_CD6 DGNS_CD7 DGNS_CD8 DGNS_CD9 DGNS_CD10); 
	save "${temp}/temp2.dta", replace;

	elixhaus "${temp}/temp2.dta" "${temp}/temp3.dta" 1 DGNS_CD DRG_NUM;

	use "${temp}/temp.dta", clear;
	merge 1:1 _obs using "${temp}/temp3.dta";
	drop _merge _obs;
	compress;
	describe,simple;

	/*Generate variables of interest*/
	gen report_ma = 0;
	replace report_ma = 1 if ime1!=. | dsh1!=.;
	tab report_ma;

	gen ma_claim = 0;
	replace ma_claim=1 if /*ghopdcd=="1" &*/ stay_hmo==1; /*& (pmt_amt==0 | pmt_amt==.)*/;
	gen los=(DSCHRG_DT-ADMSN_DT)+1;

	tab ma_claim;
	tab2 report_ma ma_claim;
	tab2 ma_claim adm_hmo;
	tab2 ma_claim stay_hmo;
	/*Tab the DRG codes for MA claims in MA Reporting Hospitals*/
	tab DRG_CD if ma_claim==1 & report_ma==1;


	/*Keep just the STACH hospitals*/
	tab ma_claim;
	tab2 report_ma ma_claim;
	keep if stach==1;
	tab ma_claim;
	tab2 report_ma ma_claim;

	gen claim=1;
	sort PRVDR_NUM;
	by PRVDR_NUM: gen num_claims=sum(claim);
	gen hospitals=0;
	replace hospitals=1 if num_claims==1;

	tab2 index_condition ma_claim if report_ma==1;
	tab2 index_condition ma_claim if report_ma==0;
	
	gen year=`yr';

	save "${temp}/med`yr'.dta",replace;
};

append using "${temp}/med2012.dta" "${temp}/med2011.dta" "${temp}/med2010.dta" "${temp}/med2009.dta";

#delimit ;
set more off;
save "${temp}/med2009_2013.dta",replace;

tab year;
tab2 report_ma year; 
tab2 ma_claim year;
tab2 ma_claim year if report_ma==1;
tab2 ghopdcd year;
tab2 ghopdcd year if report_ma==1;
tab2 stay_hmo year;
tab2 stay_hmo year if report_ma==1;

#delimit ;
set more off;

use "${temp}/med2009_2013.dta",clear;
levelsof index_condition, local(conditions);
preserve;

/*Compare variables of interest between MA Claims vs FFS claims at MA Reporting hospitals*/
collapse (sum) claim Other LEJR Stroke Pneumonia Hip_Femur Femur_Fracture CHF COPD if report_ma==1, by(ma_claim);
xpose, varname clear;
rename v1 FFS_Claim;
rename v2 MA_Claim;
rename _varname Variable;
gen Pct_MA=MA_Claim/(MA_Claim+FFS_Claim);
order Variable MA_Claim FFS_Claim Pct_MA;
sort Variable;
drop if Variable == "ma_claim";
export excel "${out}/medicare_advantage_comparison.xlsx", sheetmodify sheet("MA Reporting Comparisons") firstrow(variables);

restore, preserve;
/*Compare variables of interest between MA Claims vs FFS claims at MA Non-Reporting hospitals*/
collapse (sum) claim Other LEJR Stroke Pneumonia Hip_Femur Femur_Fracture CHF COPD if report_ma==0, by(ma_claim);
xpose, varname clear;
rename v1 FFS_Claim;
rename v2 MA_Claim;
rename _varname Variable;
gen Pct_MA=MA_Claim/(MA_Claim+FFS_Claim);
order Variable MA_Claim FFS_Claim Pct_MA;
sort Variable;
drop if Variable == "ma_claim";
export excel "${out}/medicare_advantage_comparison.xlsx", sheetmodify sheet("MA Non-Reporting Comparisons") firstrow(variables);
restore, not;

/*Create ttest datasets for each variable of interest*/
foreach var in elix_cnt PMT_AMT los {;
	clear;
	set obs 1;
	generate index_condition="";
	save "${temp}/`var'_ttest.dta",replace;
	
	foreach cond of local conditions {;
		use "${temp}/med2009_2013.dta",clear;
		ttest `var' if report_ma==1 & index_condition=="`cond'", by(ma_claim);
		
		clear;
		set obs 1;
		generate index_condition = "`cond'";
		generate `var'_pval = r(p);
		append using "${temp}/`var'_ttest.dta";
		drop if index_condition=="";
		save "${temp}/`var'_ttest.dta",replace;
	};
	use "${temp}/`var'_ttest.dta",clear;
	sort index_condition;
	save "${temp}/`var'_ttest.dta",replace;
};

/*Compare variables of interest between MA Claims vs FFS claims at MA Reporting hospitals, by Condition*/
use "${temp}/med2009_2013.dta",clear;
collapse (mean) elix_cnt PMT_AMT los if report_ma==1, by(index_condition ma_claim);
list;
reshape wide elix_cnt PMT_AMT los, j(ma_claim) i(index_condition);

/*Append the ttests for each variable of interest*/
sort index_condition;
merge index_condition using "${temp}/elix_cnt_ttest.dta" "${temp}/PMT_AMT_ttest.dta" "${temp}/los_ttest.dta";
drop _merge*;
export excel "${out}/medicare_advantage_comparison.xlsx", sheetmodify sheet("Condition Comparisons") firstrow(variables);

/*Make a list of the most frequent DRG_CD for MA claims in MA-Reporting STACHs*/
use "${temp}/med2009_2013.dta",clear;
gen freq=1;
egen drg_freq=sum(freq) if report_ma==1 & ma_claim==1,by(DRG_CD);
gsort -drg_freq DRG_CD;
duplicates drop DRG_CD,force;
keep DRG_CD drg_freq;
keep if _n<=10;
export excel "${out}/medicare_advantage_comparison.xlsx", sheetmodify sheet("Top DRGs") firstrow(variables);


/********************************************************************************
 This is just extra post-hoc analysis, not really related to the main output
********************************************************************************/

/*Determine the characteristics of the non-STACH hospitals that are not included in the analysis*/
use "${medpar}/med2009_2013.dta",clear;
sort PRVDR_NUM;
duplicates drop PRVDR_NUM, force;

gen num_check = real(substr(PRVDR_NUM,-4,4));
gen hosp_type="OTHER";
replace hosp_type="STACH" if num_check >= 1 & num_check <=899;
replace hosp_type="CAH" if num_check >= 1300 & num_check <=1399;
replace hosp_type="LTCH" if num_check >= 2000 & num_check <=2299;
replace hosp_type="HOSP DIALYSIS" if num_check >= 2300 & num_check <=2499;
replace hosp_type="IND DIALYSIS" if num_check >= 2500 & num_check <=2899;
replace hosp_type="IRF" if num_check >= 3025 & num_check <=3099;
replace hosp_type="CHILDREN" if num_check >= 3500 & num_check <=3699;
replace hosp_type="PSYCH HOSP" if num_check >= 4000 & num_check <=4499;
replace hosp_type="OPO" if inlist(upper(substr(PRVDR_NUM,3,1)),"P");
replace hosp_type="PSYCH UNIT" if inlist(upper(substr(PRVDR_NUM,3,1)),"M","S");
replace hosp_type="REHAB UNIT" if inlist(upper(substr(PRVDR_NUM,3,1)),"R","T");
replace hosp_type="SWING BED" if inlist(upper(substr(PRVDR_NUM,3,1)),"U","W","Y","Z");

keep if hosp_type!="STACH";
gen counter=1;
tab2 report_ma hosp_type;
collapse (sum) counter, by(hosp_type report_ma);

export excel "${out}/medicare_advantage_comparison.xlsx", sheetmodify sheet("Dropped Hospitals") firstrow(variables);

/*See the distribution of ma_claims per provider*/
use "${temp}/med2009_2013.dta",clear; 
gen claim=1;
sort PRVDR_NUM;
by PRVDR_NUM: gen num_claims=sum(claim);
gen hospitals=0;
replace hospitals=1 if num_claims==1;
collapse (sum) claim ma_claim, by(PRVDR_NUM report_ma);
gen pct_ma = ma_claim/claim;

export excel "${out}/medicare_advantage_comparison.xlsx", sheetmodify sheet("MA Claims by Provider") firstrow(variables);
