libname dat "/nfs/sch-data1/projects/dua-data-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Processed";

proc freq data=dat.stay_dataset_all;
	tables factype;
run;
