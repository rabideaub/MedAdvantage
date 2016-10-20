libname in "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Data/MedAdvantage/Final";
*%include "/schaeffer-b/sch-protected/from-projects/VERTICAL-INTEGRATION/rabideau/Programs/gen_ttest.sas";

data hf;
	set in.freq_array_hf;
	counter=1;
run;

data sk;
	set in.freq_array_sk;
		counter=1;
run;

data jr;
	set in.freq_array_jr;
		counter=1;
run;

proc sort data=hf; by provid; run;

data keep_provid;
	set hf;
	retain tot_disch;
	by provid;
	if first.provid then tot_disch=0;
	if SNF=1 | MDS=1 | IRF=1 then tot_disch+1;
	if last.provid & tot_disch>=5 then output; 
run;

data hf;
	merge hf (in=a)
		  keep_provid (in=b);
	by provid;
	if a & b;
run;



proc freq data=hf;
	tables ma_claim*(counter male rti_race_cd);
run;

proc means data=hf;
	class ma_claim;
	var CHF VALVE PULMCIRC PERIVASC HTN_C PARA NEURO CHRNLUNG DM
        DMCX HYPOTHY RENLFAIL LIVER ULCER AIDS LYMPH METS TUMOR ARTH COAG OBESE WGHTLOSS 
		LYTES BLDLOSS ANEMDEF ALCOHOL DRUG PSYCH DEPRESS elix_cnt hf_:;
run; 

*%gen_ttest(ds=hf,byvar=ma_claim);
*%gen_ttest(ds=sk,byvar=ma_claim);
*%gen_ttest(ds=jr,byvar=ma_claim);

