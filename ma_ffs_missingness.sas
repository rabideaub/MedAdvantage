/**************************************************************************************
 Purpose: To look at the missingness of key variables for the Medicare Advantage paper
		  and compare the distribution between MA and FFS patients in our MA-reporting
		  hospitals for our conditions of interest.
**************************************************************************************/

libname med "/schaeffer-b/sch-protected/VERTICAL-INTEGRATION/Data/2009-13_PAC/Raw/MedPAR";
libname dat "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data";
libname den "/schaeffer-b/sch-protected/VERTICAL-INTEGRATION/Data/2009-13_PAC/Raw/Denominator";

/*Keep just claims with our target index conditions from STACHs*/
data medpar;
	set med.medpar2011 (in=a)
		med.medpar2012 (in=b)
		med.medpar2013 (in=c);
	if 1<=(input(substr(prvdrnum,length(prvdrnum)-3,4),?? 4.))<=879;
	if a then year=2011;
	if b then year=2012;
	if c then year=2013;

	index_condition = "Other";
	if DRG_CD in("291","292","293") then index_condition = "CHF";    
	if DRG_CD in("469","470") then index_condition = "LEJR" ;  
	if DRG_CD in("177","178","179","193","194","195") then index_condition ="Pneumonia"  ;
	if DRG_CD in("190","191","192","202","203") then index_condition = "COPD"   ;
	if DRG_CD in("533","534","535","536") then index_condition = "Femur Fracture"  ; 
	if DRG_CD in("480","481","482") then  index_condition = "Hip Femur Proc" ;
	if put(input(DRG_CD,8.),z3.) in("061","062","063","064","065","066") then index_condition = "Stroke"  ;

	if trim(left(index_condition)) in("LEJR","Stroke","CHF");
run;

proc contents data=medpar; run;

proc freq data=medpar;
	tables index_condition*year / missing;
run;

/*Keep only MA-reporting hospitals*/
data cost_reports (rename=(prvdr_num=prvdrnum));
	set dat.nber_ime_dsh_2011 (in=a)
		dat.nber_ime_dsh_2012 (in=b)
		dat.nber_ime_dsh_2013 (in=c);
	report_ma=(ime1~=. | dsh1~=.);
	if a then year=2011;
	if b then year=2012;
	if c then year=2013;
run;

proc freq data=cost_reports;
	tables report_ma*year / missing;
run;

proc sort data=medpar; by prvdrnum year; run;
proc sort data=cost_reports nodupkey; by prvdrnum year; run;

data medpar;
	merge medpar (in=a)
		  cost_reports (in=b);
	by prvdrnum year;
	if a & report_ma=1;
run;

proc freq data=medpar;
	tables report_ma*year index_condition*year report_ma*index_condition / missing;
run;


/*Identify MA claims and FFS claims*/
data mbsf;
	set den.mbsf_ab_summary2011 (in=a)
		den.mbsf_ab_summary2012 (in=b)
		den.mbsf_ab_summary2013 (in=c);
	if a then year=2011;
	if b then year=2012;
	if c then year=2013;
run;

proc sort data=medpar; by bene_id year; run;
proc sort data=mbsf nodupkey; by bene_id year; run;

data medpar;
	merge medpar (in=a)
		  mbsf (in=b);
	by bene_id year; 
	if a;
	%macro coverage;
		ma_claim=0;
		%do i=1 %to 12;
			%if &i.<10 %then %do;
				rename HMOIND0&i.=HMOIND&i.; /*Strip the leading 0 from the number if it is 1-9*/
			%end;
			/*If the bene had a positive HMO indicator for the admission month*/
			if month(dschrgdt)=&i. & HMOIND&i. in("1","2","A","B","C") then ma_claim=1;
		%end;
	%mend;
	%coverage;

	miss_adm=(admsndt=.);
	miss_dis=(dschrgdt=.);
	miss_pmt=(pmt_amt=.);
	miss_dest=(dstntncd='');
	miss_dgn=(dgnscd1='');
run;

proc freq data=medpar;
	tables ma_claim*year ma_claim*index_condition;
run;

title "Variables of Interest";
proc freq data=medpar;
	tables ma_claim*(miss_adm miss_dis miss_pmt miss_dest miss_dgn dstntncd); 
run;
title;

proc univariate data=medpar;
	class ma_claim;
	var admsndt dschrgdt pmt_amt;
run;


