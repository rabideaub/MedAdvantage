libname in "/schaeffer-b/sch-protected/VERTICAL-INTEGRATION/Data/2009-13_PAC/Raw/MedPAR";

proc contents data=in.medpar2011; run;

data med;
	length index_condition $10;
	set in.medpar2011 (in=a)
		in.medpar2012 (in=b)
		in.medpar2013 (in=c);

	if a then year=2011;
	if b then year=2012;
	if c then year=2013;

	if 1<=(input(substr(PRVDRNUM,length(PRVDRNUM)-3,4),?? 4.))<=899;

	index_condition = "Other";
 	if DRG_CD in("291","292","293") then index_condition = "CHF" ;  
	if DRG_CD in("469","470") then index_condition = "LEJR";  
	if DRG_CD in("061","062","063","064","065","066") then index_condition = "Stroke" ; 

	if index_condition in("CHF","LEJR","Stroke");
run;

proc sort data=med nodupkey; by prvdrnum year index_condition; run;

proc freq data=med;
	tables index_condition*year; 
run;
