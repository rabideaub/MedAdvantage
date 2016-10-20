%let include = /sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Programs/MedAdvantage;
%include "&include./00_Assign_Macro_Variables_and_Libraries.sas";
libname dat "/sch-projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Final";
libname med "/schaeffer-b/sch-protected/VERTICAL-INTEGRATION/Data/2009-13_PAC/Raw/MedPAR";
libname hosp "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data/POS";
%let out=/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Output;

%macro collapse(DX);
	/*Create a hospital year level file for each index hospital in the dataset for a condition of interest*/
	data hosp_&DX.;
		set dat.freq_array_&DX.(keep=provid year);
	run;

	/*Calculate total discharges for our condition of interest*/
	proc sort data=hosp_&DX.; by provid year; run;

	data hosp_&DX.;
		set hosp_&DX.;
		by provid year;
		retain &DX._disch;
		if first.year then &DX._disch=0;
		&DX._disch+1;
		if last.year then output;
	run; 

	/*Calculate total yearly hospital discharges using MedPAR*/
	data medpar (rename=(prvdrnum=provid));
		set med.medpar2011 (in=a keep=prvdrnum)
			med.medpar2012 (in=b keep=prvdrnum)
			med.medpar2013 (in=c keep=prvdrnum);
		if a then year='2011';
		if b then year='2012';
		if c then year='2013';
		counter=1;
	run;

	proc sort data=medpar; by provid year; run;
			
	data hosp_&DX.;
		merge hosp_&DX. (in=a)
			  medpar (in=b);
		by provid year;
		if a;
		retain tot_disch;
		if first.year then tot_disch=0;
		tot_disch=sum(tot_disch,counter);
		if last.year then output;
	run;

	/*Merge on hospital characteristics from the impact files and POS files*/
	proc sort data=hosp.pos_other_dec2011_2013 out=pos; by prvdr_num pos_other_dec_year; run;
	proc sort data=hosp.cms_impact2011_2013 out=impact; by provider_number cms_impact_year; run;

	data pos;
		set pos;
		year=put(pos_other_dec_year,4.);
		provid=put(prvdr_num*1,z6.);
	run;

	proc sort data=pos; by provid year; run;

	data impact;
		set impact;
		year=put(cms_impact_year,4.);
		provid=put(provider_number*1,z6.);
	run;

	proc sort data=impact; by provid year; run;

	data hosp_&DX.;
		merge hosp_&DX. (in=a)
			  pos (in=b);
		by provid year;
		if a;
	run;

	data hosp_&DX.;
		merge hosp_&DX. (in=a)
			  impact (in=b);
		by provid year;
		if a;
	run;

	proc contents data=hosp_&DX.; run;
	proc print data=hosp_&DX. (obs=10); run;

	data dat.hosp_&DX.;
		set hosp_&DX.;
	run;

	/*Read in the data and quantify categorical variables*/
	data hosp_&DX.;
		set dat.hosp_&DX.;
		if cmiv30~=. then cmiv=cmiv30;
		else if cmiv29~=. then cmiv=cmiv29;
		else cmiv=cmiv28;
		urban=(index(upcase(urspa),"RURAL")=0);
		teaching=(resident_to_bed_ratio~=. & resident_to_bed_ratio>0);
		adc=input(average_daily_census,?? 8.);
	run;

	/*Summarize numeric measures by community*/
	proc means data=hosp_&DX.;
		class year;
		var &DX._disch tot_disch bed_cnt adc urban teaching cmiv;
		output out=&DX._summary mean=; 
	run;

	/*Transpose to make the measures readable*/
	proc transpose data=&DX._summary out=&DX._summary_wide;
		id year;
		var _FREQ_ &DX._disch tot_disch bed_cnt adc urban teaching cmiv;
	run;

	/*Make the output pretty*/
	data &DX._summary_wide (rename=(_NAME_=Variable));
		length _NAME_ $50;
		set &DX._summary_wide;
		if trim(left(_NAME_))="_FREQ_" then _NAME_="Total Hospitals";
		if trim(left(_NAME_))="&DX._disch" then _NAME_="Average &DX. Discharges";
		if trim(left(_NAME_))="tot_disch" then _NAME_="Average Discharges";
		if trim(left(_NAME_))="bed_cnt" then _NAME_= "Average Hospital Beds";
		if trim(left(_NAME_))="adc" then _NAME_= "Average Daily Census";
		if trim(left(_NAME_))="cmiv" then _NAME_= "Average Hospital Case Mix Index";
		if trim(left(_NAME_))="urban" then _NAME_= "% Urban Hospitals";
		if trim(left(_NAME_))="teaching" then _NAME_= "% Teaching Hospitals";
	run;

	proc print data=&DX._summary_wide; run;
%mend;
%collapse(JR);
%collapse(HP);
%collapse(SK);
%collapse(HF);
%collapse(FF);
%collapse(CD);

/*Output - each sheet is a different factype*/
ods tagsets.excelxp file="&out./hospital_characteristics.xml" style=sansPrinter;
%macro print_summary(DX);
	ods tagsets.excelxp options(absolute_column_width='20' sheet_name="&DX." frozen_headers='yes');
	proc print data=&DX._summary_wide noobs;
	run;
%mend;
%print_summary(JR);
%print_summary(HP);
%print_summary(SK);
%print_summary(HF);
%print_summary(FF);
%print_summary(CD);
ods tagsets.excelxp close;
