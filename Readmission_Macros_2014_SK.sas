/* MACRO PROGRAM UPDATED IN 2012 TO ACCEPT 5010 CHANGES
 - Additional Diagnosis and Procedure codes
 - Change in length of dx and proc codes
 - Additional of seperate E codes in the index file
 - JNG January 2012 
  
 MACRO PROGRAM UPDATED IN 2013
-Include Final Planned Readmission Algorithm
-Change the # of Diagnosis Codes to 25 and add e-codes
-Change the # of Procedure Codes to 25
-Maryland rehab/psych change
   CW 02-05-2013

UPDATED IN 2014
-took out "RANDOM _RESIDUAL_" from PROC GLIMMIX
CW 04-02-2014
*/

%MACRO RETRIEVE_HX(INDEXFILE, CDIAG, OUT_DIAG, OUT_PROC);

/* IDENTIFYING ADMISSIONS THAT BELONG TO A TRANSFER BUNDLE AND KEEPING
	ONLY ADMISSIONS BEFORE THE LAST ONE OF A TRANSFER BUNDLE */
PROC SQL;
CREATE TABLE BUNDLE AS
SELECT HICNO, CASEID, HISTORY_CASE, COUNT(HICNO) AS MAXCASE
FROM &INDEXFILE (RENAME=(CASE=CASEID))
GROUP BY HICNO, HISTORY_CASE
HAVING MAXCASE > 1;
QUIT;

DATA BUNDLE;
SET BUNDLE;
HICNO_HXCASE=HICNO||'_'||HISTORY_CASE;
RUN;

PROC SORT DATA=BUNDLE;
BY HICNO_HXCASE CASEID;
RUN;

DATA BUNDLE;
SET BUNDLE;
BY HICNO_HXCASE;
IF LAST.HICNO_HXCASE THEN DELETE;
RUN;

PROC SORT DATA=BUNDLE;
BY HICNO CASEID;
RUN;

PROC SORT DATA=&INDEXFILE out=one;   /* Added out=one CW 02-05-2013 */
BY HICNO CASE;
RUN;
/* add e codes from index - jng 2012 */
DATA &OUT_DIAG;
MERGE BUNDLE (IN=A RENAME=(HISTORY_CASE=CASE) DROP=MAXCASE HICNO_HXCASE) 
	/*&INDEXFILE*/ one (KEEP=HICNO CASE ADMIT DISCH DIAG1-DIAG25 EDGSCD01-EDGSCD12 YEAR  /* changed 1/24/11 zq*/
	RENAME=(CASE=CASEID ADMIT=FDATE DISCH=TDATE));
BY HICNO CASEID;
IF A;
diag26 = EDGSCD01; diag27 = EDGSCD02; diag28 = EDGSCD03; diag29 = EDGSCD04;
diag30 = EDGSCD05; diag31 = EDGSCD06; diag32 = EDGSCD07; diag33 = EDGSCD08;
diag34 = EDGSCD09; diag35 = EDGSCD10; diag36 = EDGSCD11; diag37 = EDGSCD12;

attrib diag length=$7.;   /* CHANGE to 7 JNG - 5010 update */
ARRAY ICD9(1:37) $ DIAG1-DIAG25 /*DIAG26-DIAG35*/ DIAG26-DIAG37;  /* changed by CW 03/13/13 */
/* for 2011 mm with VA data 1/24/2011 zq - more added 12/11 jng */
DO I=1 TO 37;
	IF I=1 THEN DO;
	SOURCE='0.0.1.0';
	DIAG=ICD9(I);
	OUTPUT;
	END;
	ELSE DO;
	SOURCE='0.0.2.0';
	DIAG=ICD9(I);
	OUTPUT;
	END;
END;
KEEP HICNO CASE DIAG FDATE TDATE SOURCE YEAR;
RUN;
DATA &OUT_DIAG;
SET &OUT_DIAG;
	IF &CDIAG THEN DIAG='';
	IF DIAG IN ('', ' ') THEN DELETE;
RUN;

DATA &OUT_PROC;
MERGE BUNDLE (IN=A RENAME=(HISTORY_CASE=CASE) DROP=MAXCASE HICNO_HXCASE) 
	/*&INDEXFILE*/ one (KEEP=HICNO CASE ADMIT DISCH PROC1-PROC25 YEAR 
	RENAME=(CASE=CASEID ADMIT=FDATE DISCH=TDATE));
BY HICNO CASEID;
IF A;
/* added 1/24/2011 zq  CHANGE FOR INCREASED VA JNG - 12/11 5010, length changed */
attrib proc length=$7.; 
ARRAY ICD9P(1:25) PROC1-PROC25;
DO J=1 TO 25;
	IF J=1 THEN DO;
	SOURCE='9.0.1.0';
	PROC=ICD9P(J);
	OUTPUT;
	END;
	ELSE DO;
	SOURCE='9.0.2.0';
	PROC=ICD9P(J);
	OUTPUT;
	END;
END;
KEEP HICNO CASE PROC FDATE TDATE SOURCE YEAR;
RUN;
DATA &OUT_PROC;
SET &OUT_PROC;
	IF PROC IN ('', ' ') THEN DELETE;
RUN;

%MEND;


**************************************************************************************;
* MACROS NEEDED FOR CREATING CC VARIABLES                                            *;
**************************************************************************************;

%MACRO CMS_HCC_GET(INDSN, OUTDSN, prefix); /* ZQ: add prefix 2/7/09 */
  DATA TEMP1;
    length ncase $5.;
    SET &INDSN;
	ncase=case;
	length nhic $18.;
	nhic=trim(hicno) || '_' || trim(ncase);  /*CW 04-19 */
    LENGTH ADDXG $6.;

    ADDXG = PUT(ICD9,$CCASS.);
   
    KEEP HICno CASE ADDXG SOURCE nhic ICD9 AGE SEX; /* ZQ: add age and sex */

  PROC SORT DATA=TEMP1 NODUP;
	BY nHIC ADDXG ICD9; 

/* End of codes from MPR----------------------------------------------------*/

  /*  Using a format, this step maps each addxg group for a person into a larger;
  *  group denoted by "CC".  The numeric variable IND is set to the value of ;
  *  the character variable CC. If ind is between 1 and 189 then ;
  *  the ind'th element of the array "C" is set to 1; if not it is set to 0 ;
  *  The array has variables CC1 through CC189. The array is retained as each ;
  *  ADDXG record is mapped for a person.  After last record for a person is;
  *  mapped, the macro at the top of the program is run to allow the presence of;
  *  high severity diseases to cancel out low severity versions of the disease. ;
  *  The final vector of 1's and 0's is stored in the variables HCC1 - HCC189.  ;
  *  The record is written to the output dataset and the CCs are reinitialized  ;
  *  the HCCs are the diagnosis groups that are used in the risk adjustment formula;
  */

  DATA &OUTDSN(KEEP=HICno CASE &prefix.CC1-&prefix.CC189 nhic) ERR;
    SET TEMP1;
	by nhic;
    length cc $4.;
/*------------------------------------------------------------------- 
	The fllowing codes are from MPR (Eric) HCC v2 received on July 09, 2007,
 Yale made no changes to these codes  
 YW 11-20-2007
------------------------------------------------------------------	*/

   cc=left(addxg);
/* ES - NEW LOGIC TO CONFORM TO 2006 HCC FORMATS 2/23 */
  _ICD9_3 = SUBSTR(ICD9,1,3);    *first 3 characters from ICD9;
    _ICD9_4 = SUBSTR(ICD9,1,4);    *first 4 characters from ICD9;

     * age restrictions;
     IF AGE < 18 THEN DO;
        /*emphysema chronic bronchitis */
        IF _ICD9_3 IN ('491','492','496') OR ICD9 IN ('5181','5182')
        THEN CC='109';
        ELSE
        IF  _ICD9_4 = '4932' THEN  CC='110';
        /*chronic obstructive asthma */
     END;

      *esophageal atresia/stenosis, oth cong GI anomalies age<2;
      IF AGE<2 THEN
         IF ICD9 IN ('7503', '7504', '7507', '7508', '7509', '751',
                      '7515', '7516', '75160','75162','75169','7517',
                      '7518','7519')
         THEN CC='170';

      * age/sex restrictions;
      SELECT;
        /* males only  */
         WHEN (( '185'<= _ICD9_3 <='187'
                 or _ICD9_3 = '257'
                 or '600'<= _ICD9_3 <='608'
                 or '7525'<=_ICD9_4<='7526'
                 or ICD9='7528')
               & SEX='2')                                  CC='-1.0';

         /* females only */
         WHEN ( (ICD9='1121' OR _ICD9_3='131'
                             OR '179'<=_ICD9_3<='184'
                             OR _ICD9_3='256'
                             OR '614'<=_ICD9_3<='627'
                             OR _ICD9_3='629'
                             OR ICD9='677')
               & SEX='1')                                  CC='-1.0';


        /*Infertility, Pregnancy DXGs Restricted to Females
          Between Ages 8 and 59 */
         WHEN ( (_ICD9_3='628' OR
               '630'<=_ICD9_3<='676' OR
			   '678' <= _ICD9_3 <='679' OR /*ZQ: add per RTI 2/7/09 */
                _ICD9_3 IN ('V22','V23','V24','V27','V28'))
                & (SEX='1' OR AGE<8 OR AGE>59))          CC='-1.0';

        /* newborns */
         WHEN((ICD9 IN ('0900 ','0901 ','0902 ','7485 '
                       '7505 ','7506 ','7511 ','7512 '
                       '7513 ','7514 ','75161','7566 ') OR
              '760' <=_ICD9_3<='770' OR
              '771 ' <=_ICD9_4<='7717' OR
              ICD9 IN ('7718','77182','77183','77189') OR
              '772' <=_ICD9_3<='779'  OR
              _ICD9_4='V213' OR
              'V29' <=_ICD9_3<='V39')
                &  AGE>=2)                                CC='-1.0';
         OTHERWISE;
      END; *SELECT;


    RETAIN &prefix.CC1-&prefix.CC189 0 ;
*    RETAIN HCC1-HCC189 0 ;

    ATTRIB &prefix.CC1-&prefix.CC189  LENGTH=3.;
    ARRAY C(189)  &prefix.CC1-&prefix.CC189;

*    ATTRIB HCC1-HCC189  LENGTH=3.;
*    ARRAY HCC(189) HCC1-HCC189;

    IF CC NOT IN ('0.0 ',' 0.0','-1.0',' -1.') THEN DO;
      *- to find index for the array for current PHCC -;
      IND = INPUT(CC,8.);
      IF 1<= IND <= 189 THEN C(IND)=1;
      ELSE OUTPUT ERR;
    END;

	/* The following code was added by MPR on early 2007 to address the changes 
	in HCC between the old and new mapping/format algorithms. */;
	
 IF ICD9 IN ('40403','40413','40493') THEN &prefix.CC80=1;
 
    
  /*----------------------End of codes from MPR -------------------------*/;
	IF LAST.nHIC THEN DO;
       OUTPUT &OUTDSN;
      DO I=1 TO 189;
        C(I)=0; 
      END;
    END;
      *revision7;
    label
      &prefix.CC1   ="HIV/AIDS                                "
      &prefix.CC2   ="Septicemia/Shock                        "
      &prefix.CC3   ="Central Nervous System Infection        "
      &prefix.CC4   ="Tuberculosis                            "
      &prefix.CC5   ="Opportunistic Infections                "
      &prefix.CC6   ="Other Infectious Diseases               "
      &prefix.CC7   ="Metastatic Cancer and Acute Leukemia    "
      &prefix.CC8   ="Lung, Upper Digestive Tract, and Other S"
      &prefix.CC9   ="Lymphatic, Head and Neck, Brain, and Oth"
      &prefix.CC10  ="Breast, Prostate, Colorectal and Other C"
      &prefix.CC11  ="Other Respiratory and Heart Neoplasms   "
      &prefix.CC12  ="Other Digestive and Urinary Neoplasms   "
      &prefix.CC13  ="Other Neoplasms                         "
      &prefix.CC14  ="Benign Neoplasms of Skin, Breast, Eye   "
      &prefix.CC15  ="Diabetes with Renal Manifestation       "
      &prefix.CC16  ="Diabetes with Neurologic or Peripheral C"
      &prefix.CC17  ="Diabetes with Acute Complications       "
      &prefix.CC18  ="Diabetes with Ophthalmologic Manifestati"
      &prefix.CC19  ="Diabetes with No or Unspecified Complica"
      &prefix.CC20  ="Type I Diabetes Mellitus                "
      &prefix.CC21  ="Protein-Calorie Malnutrition            "
      &prefix.CC22  ="Other Significant Endocrine and Metaboli"
      &prefix.CC23  ="Disorders of Fluid/Electrolyte/Acid-Base"
      &prefix.CC24  ="Other Endocrine/Metabolic/Nutritional Di"
      &prefix.CC25  ="End-Stage Liver Disease                 "
      &prefix.CC26  ="Cirrhosis of Liver                      "
      &prefix.CC27  ="Chronic Hepatitis                       "
      &prefix.CC28  ="Acute Liver Failure/Disease             "
      &prefix.CC29  ="Other Hepatitis and Liver Disease       "
      &prefix.CC30  ="Gallbladder and Biliary Tract Disorders "
      &prefix.CC31  ="Intestinal Obstruction/Perforation      "
      &prefix.CC32  ="Pancreatic Disease                      "
      &prefix.CC33  ="Inflammatory Bowel Disease              "
      &prefix.CC34  ="Peptic Ulcer, Hemorrhage, Other Specifie"
      &prefix.CC35  ="Appendicitis                            "
      &prefix.CC36  ="Other Gastrointestinal Disorders        "
      &prefix.CC37  ="Bone/Joint/Muscle Infections/Necrosis   "
      &prefix.CC38  ="Rheumatoid Arthritis and Inflammatory Co"
      &prefix.CC39  ="Disorders of the Vertebrae and Spinal Di"
      &prefix.CC40  ="Osteoarthritis of Hip or Knee           "
      &prefix.CC41  ="Osteoporosis and Other Bone/Cartilage Di"
      &prefix.CC42  ="Congenital/Developmental Skeletal and Co"
      &prefix.CC43  ="Other Musculoskeletal and Connective Tis"
      &prefix.CC44  ="Severe Hematological Disorders          "
      &prefix.CC45  ="Disorders of Immunity                   "
      &prefix.CC46  ="Coagulation Defects and Other Specified "
      &prefix.CC47  ="Iron Deficiency and Other/Unspecified An"
      &prefix.CC48  ="Delirium and Encephalopathy             "
      &prefix.CC49  ="Dementia                                "
      &prefix.CC50  ="Senility, Nonpsychotic Organic Brain Syn"
      &prefix.CC51  ="Drug/Alcohol Psychosis                  "
      &prefix.CC52  ="Drug/Alcohol Dependence                 "
      &prefix.CC53  ="Drug/Alcohol Abuse, Without Dependence  "
      &prefix.CC54  ="Schizophrenia                           "
      &prefix.CC55  ="Major Depressive, Bipolar, and Paranoid "
      &prefix.CC56  ="Reactive and Unspecified Psychosis      "
      &prefix.CC57  ="Personality Disorders                   "
      &prefix.CC58  ="Depression                              "
      &prefix.CC59  ="Anxiety Disorders                       "
      &prefix.CC60  ="Other Psychiatric Disorders             "
      &prefix.CC61  ="Profound Mental Retardation/Developmenta"
      &prefix.CC62  ="Severe Mental Retardation/Developmental "
      &prefix.CC63  ="Moderate Mental Retardation/Developmenta"
      &prefix.CC64  ="Mild/Unspecified Mental Retardation/Deve"
      &prefix.CC65  ="Other Developmental Disability          "
      &prefix.CC66  ="Attention Deficit Disorder              "
      &prefix.CC67  ="Quadriplegia, Other Extensive Paralysis "
      &prefix.CC68  ="Paraplegia                              "
      &prefix.CC69  ="Spinal Cord Disorders/Injuries          "
      &prefix.CC70  ="Muscular Dystrophy                      "
      &prefix.CC71  ="Polyneuropathy                          "
      &prefix.CC72  ="Multiple Sclerosis                      "
      &prefix.CC73  ="Parkinson's and Huntington's Diseases   "
      &prefix.CC74  ="Seizure Disorders and Convulsions       "
      &prefix.CC75  ="Coma, Brain Compression/Anoxic Damage   "
      &prefix.CC76  ="Mononeuropathy, Other Neurological Condi"
      &prefix.CC77  ="Respirator Dependence/Tracheostomy Statu"
      &prefix.CC78  ="Respiratory Arrest                      "
      &prefix.CC79  ="Cardio-Respiratory Failure and Shock    "
      &prefix.CC80  ="Congestive Heart Failure                "
      &prefix.CC81  ="Acute Myocardial Infarction             "
      &prefix.CC82  ="Unstable Angina and Other Acute Ischemic"
      &prefix.CC83  ="Angina Pectoris/Old Myocardial Infarctio"
      &prefix.CC84  ="Coronary Atherosclerosis/Other Chronic I"
      &prefix.CC85  ="Heart Infection/Inflammation, Except Rhe"
      &prefix.CC86  ="Valvular and Rheumatic Heart Disease    "
      &prefix.CC87  ="Major Congenital Cardiac/Circulatory Def"
      &prefix.CC88  ="Other Congenital Heart/Circulatory Disea"
      &prefix.CC89  ="Hypertensive Heart and Renal Disease or "
      &prefix.CC90  ="Hypertensive Heart Disease              "
      &prefix.CC91  ="Hypertension                            "
      &prefix.CC92  ="Specified Heart Arrhythmias             "
      &prefix.CC93  ="Other Heart Rhythm and Conduction Disord"
      &prefix.CC94  ="Other and Unspecified Heart Disease     "
      &prefix.CC95  ="Cerebral Hemorrhage                     "
      &prefix.CC96  ="Ischemic or Unspecified Stroke          "
      &prefix.CC97  ="Precerebral Arterial OCClusion and Trans"
      &prefix.CC98  ="Cerebral Atherosclerosis and Aneurysm   "
      &prefix.CC99  ="Cerebrovascular Disease, Unspecified    "
      &prefix.CC100 ="Hemiplegia/Hemiparesis                  "
      &prefix.CC101 ="Diplegia (Upper), Monoplegia, and Other "
      &prefix.CC102 ="Speech, Language, Cognitive, Perceptual "
      &prefix.CC103 ="Cerebrovascular Disease Late Effects, Un"
      &prefix.CC104 ="Vascular Disease with Complications     "
      &prefix.CC105 ="Vascular Disease                        "
      &prefix.CC106 ="Other Circulatory Disease               "
      &prefix.CC107 ="Cystic Fibrosis                         "
      &prefix.CC108 ="Chronic Obstructive Pulmonary Disease   "
      &prefix.CC109 ="Fibrosis of Lung and Other Chronic Lung "
      &prefix.CC110 ="Asthma                                  "
      &prefix.CC111 ="Aspiration and Specified Bacterial Pneum"
      &prefix.CC112 ="Pneumocoal Pneumonia, Empyema, Lung Ab"
      &prefix.CC113 ="Viral and Unspecified Pneumonia, Pleuris"
      &prefix.CC114 ="Pleural Effusion/Pneumothorax           "
      &prefix.CC115 ="Other Lung Disorders                    "
      &prefix.CC116 ="Legally Blind                           "
      &prefix.CC117 ="Major Eye Infections/Inflammations      "
      &prefix.CC118 ="Retinal Detachment                      "
      &prefix.CC119 ="Proliferative Diabetic Retinopathy and V"
      &prefix.CC120 ="Diabetic and Other Vascular Retinopathie"
      &prefix.CC121 ="Retinal Disorders, Except Detachment and"
      &prefix.CC122 ="Glaucoma                                "
      &prefix.CC123 ="Cataract                                "
      &prefix.CC124 ="Other Eye Disorders                     "
      &prefix.CC125 ="Significant Ear, Nose, and Throat Disord"
      &prefix.CC126 ="Hearing Loss                            "
      &prefix.CC127 ="Other Ear, Nose, Throat, and Mouth Disor"
      &prefix.CC128 ="Kidney Transplant Status                "
      &prefix.CC129 ="End Stage Renal Disease                 "
      &prefix.CC130 ="Dialysis Status                         "
      &prefix.CC131 ="Renal Failure                           "
      &prefix.CC132 ="Nephritis                               "
      &prefix.CC133 ="Urinary Obstruction and Retention       "
      &prefix.CC134 ="Incontinence                            "
      &prefix.CC135 ="Urinary Tract Infection                 "
      &prefix.CC136 ="Other Urinary Tract Disorders           "
      &prefix.CC137 ="Female Infertility                      "
      &prefix.CC138 ="Pelvic Inflammatory Disease and Other Sp"
      &prefix.CC139 ="Other Female Genital Disorders          "
      &prefix.CC140 ="Male Genital Disorders                  "
      &prefix.CC141 ="Ectopic Pregnancy                       "
      &prefix.CC142 ="Miscarriage/Abortion                    "
      &prefix.CC143 ="Completed Pregnancy With Major Complicat"
      &prefix.CC144 ="Completed Pregnancy With Complications  "
      &prefix.CC145 ="Completed Pregnancy Without Complication"
      &prefix.CC146 ="Uncompleted Pregnancy With Complications"
      &prefix.CC147 ="Uncompleted Pregnancy With No or Minor C"
      &prefix.CC148 ="Decubitus Ulcer of Skin                 "
      &prefix.CC149 ="Chronic Ulcer of Skin, Except Decubitus "
      &prefix.CC150 ="Extensive Third-Degree Burns            "
      &prefix.CC151 ="Other Third-Degree and Extensive Burns  "
      &prefix.CC152 ="Cellulitis, Local Skin Infection        "
      &prefix.CC153 ="Other Dermatological Disorders          "
      &prefix.CC154 ="Severe Head Injury                      "
      &prefix.CC155 ="Major Head Injury                       "
      &prefix.CC156 ="Concussion or Unspecified Head Injury   "
      &prefix.CC157 ="Vertebral Fractures                     "
      &prefix.CC158 ="Hip Fracture/Dislocation                "
      &prefix.CC159 ="Major Fracture, Except of Skull, Vertebr"
      &prefix.CC160 ="Internal Injuries                       "
      &prefix.CC161 ="Traumatic Amputation                    "
      &prefix.CC162 ="Other Injuries                          "
      &prefix.CC163 ="Poisonings and Allegic Reactions        "
      &prefix.CC164 ="Major Complications of Medical Care and "
      &prefix.CC165 ="Other Complications of Medical Care     "
      &prefix.CC166 ="Major Symptoms, Abnormalities           "
      &prefix.CC167 ="Minor Symptoms, Signs, Findings         "
      &prefix.CC168 ="Extremely Low Birthweight Neonates      "
      &prefix.CC169 ="Very Low Birthweight Neonates           "
      &prefix.CC170 ="Serious Perinatal Problem Affecting Newb"
      &prefix.CC171 ="Other Perinatal Problems Affecting Newbo"
      &prefix.CC172 ="Normal, Single Birth                    "
      &prefix.CC173 ="Major Organ Transplant                  "
      &prefix.CC174 ="Major Organ Transplant Status           "
      &prefix.CC175 ="Other Organ Transplant/Replacement      "
      &prefix.CC176 ="Artificial Openings for Feeding or Elimi"
      &prefix.CC177 ="Amputation Status, Lower Limb/Amputation"
      &prefix.CC178 ="Amputation Status, Upper Limb           "
      &prefix.CC179 ="Post-Surgical States/Aftercare/Elective "
      &prefix.CC180 ="Radiation Therapy                       "
      &prefix.CC181 ="Chemotherapy                            "
      &prefix.CC182 ="Rehabilitation                          "
      &prefix.CC183 ="Screening/Observation/Special Exams     "
      &prefix.CC184 ="History of Disease                      "
      &prefix.CC185 ="Oxygen                                  "
      &prefix.CC186 ="CPAP/IPPB/Nebulizers                    "
      &prefix.CC187 ="Patient Lifts, Power Operated Vehicles, "
      &prefix.CC188 ="Wheelchairs, Commodes                   "
      &prefix.CC189 ="Walkers                                 ";

  RUN;

%MEND;

/*--------------------------------------------------------------------------
* Create age, sex and source variables and re-arrange RTI dataset to one
  ICD-9 code per line in the index admission event file;

Yale University
YW 05-2003
----------------------------------------------------------------------------*/;
%MACRO HCCPAI(INDSN, OUTDSN);
  DATA &OUTDSN;
    SET &INDSN;
*	HIC=HICNO; /* ZQ: changes made on 2/7/09 */
*	FORMAT ADMDT DISDT MMDDYY10.;
*    ADMDT=ADMIT;
*    DISDT=DISCH;
*	need to add E-Code Fields - jng 1/10/12***;
*   Modified E-Code for Stroke --CW 03-13-2013 ;
diag26 = EDGSCD01; diag27 = EDGSCD02; diag28 = EDGSCD03; diag29 = EDGSCD04;
diag30 = EDGSCD05; diag31 = EDGSCD06; diag32 = EDGSCD07; diag33 = EDGSCD08;
diag34 = EDGSCD09; diag35 = EDGSCD10; diag36 = EDGSCD11; diag37 = EDGSCD12;

    AGE=INT((ADMIT-BIRTH)/365.25);
	SEX="" || CSEX;
    SOURCE='0.0.2.0'; /*ZQ: source changed 2/7/09 */
 	ARRAY ICD9CODE{1:37} $ DIAG1-DIAG25 /*DIAG26-DIAG35*/ DIAG26-DIAG37;  
	/* changed 1/24/11 zq - rev 1/4/12 jng  01/31/13: CW*/
    DO I=2 TO 37;
      ICD9=ICD9CODE[I];
      OUTPUT;
    END;
 
    IF ICD9='' THEN DELETE;
    KEEP HICno CASE AGE SEX ICD9 SOURCE;
  RUN;
%MEND;


***************************************************************;
* PROGRAM NAME: BOOTSTRAP_3b_altmethods.SAS                   *;
* SAS 9.1.3 WIN                                               *;
* ZQ LIN, YALE/YNHH CORE                                      *;
* 3/14/2008 (created for Heart Failure readmission measure)   *;
* 7/2/2008 (updated for AMI & Pneumonia readmission measures, *;
* incorporated changes made by JS from MPR.)                  *;
* *************************************************************;

%MACRO BOOTSTRAP_HOSPITAL_READMISSION(SFILE, TFILE, STARTPOINT, ENDPOINT, SEED);

PROC SORT DATA=&SFILE;
	BY PROVID;
RUN;

PROC SQL NOPRINT;
	CREATE TABLE HOSPITAL AS
	SELECT PROVID, COUNT(PROVID) AS VOLUME
	FROM &SFILE
	GROUP BY PROVID;
QUIT;

***************************************************************************;
* SAMPLE HOSPITAL WITH REPLACEMENT                                        *;
* ONE HOSPITAL MAY BE SAMPLED MORE THAN ONCE                              *;
* TOTLA NUMBER OF HOSPITAL WILL EQUAL TO THE NUMBER OF HOSPITAL IN THE    *;
* ORIGINAL DATASET. FOR HOSPITALS THAT APPEAR MORE THAN ONCE, THEY ARE    *;
* TREATED AS DISTINCT HOSPITAL.                                           *;
* ALL THE PATIENTS WITHIN EACH HOSPITAL ARE INCLUDED.                     *;
***************************************************************************;

%DO BS=&STARTPOINT %TO &ENDPOINT;

/* SAMPLING HOSPITALS */
PROC SURVEYSELECT DATA=HOSPITAL METHOD=URS SAMPRATE=1 OUT=H seed=%eval(&bs + &seed);
RUN;

DATA H2;
	SET H;
	DO I=1 TO NUMBERHITS;
			H_S_ID + 1;
		OUTPUT;
	END;
RUN;

PROC SORT DATA=H2;
	BY PROVID;
RUN;


/* CONSTRUCTING PATIENT LEVEL DATA BASED ON HOSPITAL LEVEL DATA FROM THE ABOVE STEP */
/* THE TOTAL SAMPLE SIZE MAY BE DIFFERENT FROM THE ORIGINAL SAMPLE SIZE             */

PROC SQL NOPRINT;
	CREATE TABLE BSHP AS
	SELECT B.H_S_ID, A.* FROM &SFILE AS A INNER JOIN H2 AS B
	ON A.PROVID=B.PROVID;
QUIT;

*****************************************************************************;
* FITTING HIERARCHICAL MODEL                                                *;
*****************************************************************************;

/* PARAMETERIZE OPTIMIZATION TECHNIQUE AND OFFER 2 OPTIONS JS 3/19/08 */

PROC GLIMMIX DATA=BSHP NOCLPRINT  MAXLMMUPDATE= &nbrits;
CLASS H_S_ID;
ODS OUTPUT SOLUTIONR=SOLUTIONR;
MODEL RADM30=&MODEL_VAR	/d=b link=logit solution;
XBETA=_XBETA_;
RANDOM INTERCEPT/SUBJECT=H_S_ID SOLUTION;
**RANDOM _RESIDUAL_;
OUTPUT OUT=PRED PRED(BLUP ILINK)=PREDPROB PRED(NOBLUP ILINK)=EXPPROB;
ID XBETA PSTATE PROVID H_S_ID HICNO CASE RADM30;
NLOPTIONS TECH=&firstmethod;
run;

* assuming that the one of the methods will converge;
%if %sysfunc(exist(solutionr)) %then %do;
        data _null_;
            file PRINT;
            put "DEBUG:  iteration # &bs converged with &firstmethod";
        run;
        %end;
    %else %do;
        PROC GLIMMIX DATA=BSHP NOCLPRINT MAXLMMUPDATE= &nbrits;
            CLASS H_S_ID;
            ODS OUTPUT SOLUTIONR=SOLUTIONR;
            MODEL RADM30=&MODEL_VAR	/d=b link=logit solution;
            XBETA=_XBETA_;
            RANDOM INTERCEPT/SUBJECT=H_S_ID SOLUTION;
            **RANDOM _RESIDUAL_;
            OUTPUT OUT=PRED PRED(BLUP ILINK)=PREDPROB PRED(NOBLUP ILINK)=EXPPROB;
            ID XBETA PSTATE PROVID H_S_ID HICNO CASE RADM30;
            NLOPTIONS TECH=&nextmethod;
        run;
        %if %sysfunc(exist(solutionr)) %then %do;
            data _null_;
                file PRINT;
                put "DEBUG:  iteration # &bs converged with &nextmethod";
            run;
            %end;
        %else %do;
            data _null_;
                file PRINT;
                put "DEBUG:  iteration # &bs failed to converge";
            run;                
            %end;
        %end;

%if %sysfunc(exist(solutionr)) %then %do;  /* Added on 05-23-2013 */
/* If iteration # &bs failed to converge, the following step will be skipped */

PROC SORT DATA=PRED;
	BY H_S_ID;
RUN;

DATA RANDOM_EFFECT;
	SET SOLUTIONR (KEEP=SUBJECT ESTIMATE STDERRPRED);
	LENGTH H_S_ID 8.;
	H_S_ID=SUBSTR(SUBJECT, 8);
	multiplier=rannor(&SEED + &bs);
	m_stderr=stderrpred*multiplier;
	DROP SUBJECT;
RUN;

PROC SORT DATA=RANDOM_EFFECT;
	BY H_S_ID;
RUN;

DATA ALL;
	MERGE PRED (IN=A) RANDOM_EFFECT;
	BY H_S_ID;
	IF A;
	LINP=XBETA + ESTIMATE;
	LINP_BS=LINP + m_stderr;
	P_XBETA=EXP(XBETA)/(1 + EXP(XBETA));
	P_LINP=EXP(LINP)/(1 + EXP(LINP));
	P_LINP_BS=EXP(LINP_BS)/(1 + EXP(LINP_BS));
	IF RADM30 ^=. AND P_LINP_BS ^=. AND P_XBETA ^=.;
	KEEP PROVID RADM30 P_XBETA P_LINP_BS H_S_ID;
RUN;;

PROC SQL NOPRINT;
	CREATE TABLE BSHP&BS AS
	SELECT DISTINCT H_S_ID, PROVID,
				&BS AS ITERATION, 
				RANNOR(&SEED+ &BS) AS SUBID,
				MEAN(P_XBETA) AS EXP_R,
				MEAN(P_LINP_BS) AS PRED_R,
				MEAN(RADM30) AS OBS_R,
				COUNT(PROVID) AS VOLUME,
				(CALCULATED PRED_R)/(CALCULATED EXP_R) AS SRR
	FROM ALL
	GROUP BY H_S_ID;
QUIT;

PROC SORT DATA=BSHP&BS;
BY PROVID SUBID;

DATA BSHP&BS;
	SET BSHP&BS;
	BY PROVID;
	IF FIRST.PROVID THEN OUTPUT;
RUN;

PROC APPEND BASE=&TFILE DATA=BSHP&BS;
RUN;

DM 'LOG; CLEAR';
DM 'OUTPUT; CLEAR';

/* Added the following step to clear data */
proc delete data= solutionr PRED RANDOM_EFFECT  ALL  BSHP&BS;
run;

  %END;

%END;

%MEND;
