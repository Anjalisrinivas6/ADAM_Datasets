/*Deriving ADSL*/

options validvarname = Upcase missing = '';


Data Adsl1;
Length Studyid Siteid $25 Usubjid Subjid $50 ageu armcd actarmcd $20 race ethnic $100 
arm  ACTARM TRT01P  TRT01A   $200 DMDTC $30 ;

Set Sdtm.Dm  ;
Studyid = Strip(Studyid);
Domain= 'ADSL';
Usubjid = Strip(Usubjid);
Subjid = Strip(Subjid);
Siteid = Strip(Siteid);

Age = Age;	
AgeU = strip(Ageu);
IF  AGE <50 THEN DO; 
AGEGRP="< 50 YEARS"; AGEGRPN=1;END;
ELSE IF AGE>=50 AND  AGE<=65 THEN DO; 
AGEGRP="50 TO 65 YEARS";AGEGRPN=2;END;
ELSE IF AGE>65 THEN DO; 
AGEGRP="> 65 YEARS";AGEGRPN=3;END;

Sex = Strip(Sex);
If Sex = 'M' then SEXN = 1;
Else if Sex = 'F'  then SEXN = 0 ;

Race = strip(Race);
IF Race = UPCASE( 'American Indian or Alaska Native') THEN RACEN = 1;
 ELSE IF RACE =  UPCASE( 'Asian') THEN RACEN = 2;
 ELSE IF RACE = UPCASE('Black ') THEN RACEN = 3;
  ELSE IF RACE =UPCASE( 'Native Hawaiian or Other Pacific Islander') THEN RACEN = 4;
ELSE IF RACE = UPCASE('White') THEN RACEN = 5;
ELSE IF RACE = ('OTHER: MIDDLE EASTERN.') THEN RACEN = 6;



ETHNIC = Strip (ETHNIC);
 IF Ethnic ='  Hispanic or Latino' Then ETHNICN = 1;
 IF Ethnic=  'Not Hispanic or Latino' Then ETHNICN = 2;
  IF Ethnic= 'Unknown' Then ETHNICN = 3;
  IF Ethnic= '' then ETHNICN = 4;



ARMCD = Strip(ARMCD);
ARM = Strip(ARM);

TRT01P = Strip(ARMCD);

IF ARMCD ='A' THEN TRT01PN = 1;
IF ARMCD ='B' THEN TRT01PN = 2;
IF ARMCD ='C' THEN TRT01PN = 3;
IF ARMCD ='D' THEN TRT01PN = 4;
IF ARMCD ='E' THEN TRT01PN = 5;
IF ARMCD ='F' THEN TRT01PN = 6;
IF ARMCD ='G' THEN TRT01PN = 7;
IF ARMCD ='J' THEN TRT01PN = 8;
IF ARMCD ='NOTTRT' THEN TRT01PN = 9;

ACTARMCD = Strip(Actarmcd);

ACTARM = strip(Actarm);
TRT01A = Strip(ACTARMCD);
IF ACTARMCD = 'A' THEN TRT01AN = 1;
IF ACTARMCD = 'B' THEN TRT01AN = 2;
IF ACTARMCD = 'C' THEN TRT01AN = 3;
IF ACTARMCD = 'D' THEN TRT01AN = 4;
IF ACTARMCD = 'E' THEN TRT01AN = 5;
IF ACTARMCD = 'F' THEN TRT01AN = 6;
IF ACTARMCD = 'G' THEN TRT01AN = 7;
IF ACTARMCD = 'J' THEN TRT01AN = 8;
IF ACTARMCD = 'NOTTRT' THEN TRT01AN = 9;




TRTSDT=INPUT(SUBSTR(RFXSTDTC,1,10),YYMMDD10.);
TRTSDTM=INPUT(RFXSTDTC , ANYDTDTM.);
TRTSTM = INPUT (SUBSTR(RFXSTDTC,12),TIME8.);



IF LENGTH(RFXENDTC)=10 THEN DO;
RFXENDTC_=STRIP(RFXENDTC)||"T"||"00:00:00";
end;

TRTEDT=INPUT(SUBSTR(RFXENDTC_,1,10),YYMMDD10.);
TRTEDTM=INPUT(RFXENDTC_,ANYDTDTM.);
TRTETM=INPUT(SUBSTR(RFXENDTC_,12),TIME8.);

Format   Trtsdt trtedt  is8601da. trtsdtm trtedtm is8601dt. trtstm trtetm time8. ;
If TRTEDT ne . and TRTSDT ne . then 
TRTDUR = (TRTEDT -TRTSDT) + 1;
else trtdur = .;

DMDTC =  strip(DMDTC);
DMDY =  DMDY ;
DTHFL = Strip( DTHFL) ;
If DTHDTC ne '' then  DTHDT = INPUT(DTHDTC,YYMMDD10.);
else DTHDT =  . ;



RFSTDT=INPUT(RFSTDTC, yymmdd10.);
RFENDT = INPUT(RFENDTC,YYMMDD10.);

RFPENDT=INPUT(RFPENDTC,YYMMDD10.);
RFICDT = Input(RFICDTC , YYMMDD10.);
 

Format RFSTDT RFENDT RFPENDT RFICDT DTHDT is8601da. ;

IF DTHDT NE . and  RFSTDT ne . then do;

IF DTHDT > RFSTDT THEN DTHDY =  (DTHDT - RFSTDT) + 1 ;
 else DTHDY = . ;
 END;


If RFXSTDTC  ne '' or TRTSDT ne . THEN  SAFFL = 'Y';
ELSE SAFFL = 'N' ;

If RFICDTC NE '' THEN SCRNFL = 'Y';

ELSE SCRNFL = '';

IF ARMCD= 'SCRNFAIL' THEN SCRNFAFL='Y';


Keep Studyid Domain Usubjid Subjid Siteid Age AgeU  AGEGRP AGEGRPN Sex Sexn Race Racen ETHNIC ETHNICN   Armcd Arm ActarmCD 
Actarm TRT01P TRT01PN  TRT01A TRT01AN  TRTSDT TRTEDT TRTDUR  TRTSDTM TRTSTM TRTEDTM TRTETM  DMDTC DMDY  
DTHDT DTHDY RFSTDT RFENDT RFPENDT RFICDT  SAFFL DTHFL  SCRNFAFL SCRNFL;
run;


/*USING SUPPDM DOMAIN */




Proc sort data = Sdtm.Suppdm Out = Suppdm1;
By usubjid;
run;




Proc Transpose data = Suppdm1 Out = Suppdm2;

BY usubjid ;
Var qval;
ID  QNAM;
run;


DATA SUPPDM;
LENGTH RACEOTH $40;
SET SDTM.SUPPDM;
WHERE QNAM = 'RACOTH' AND QVAL NE ''; 
RACEOTH = STRIP (QVAL);
KEEP USUBJID RACEOTH;
RUN;


proc sort data = suppdm;
by usubjid;
run;


DATA DM_SUPPDM;
MERGE ADSL1 (IN=A)SUPPDM;
BY USUBJID;
IF A ;
RUN;









/*USING THE DS DOMAIN */



Proc sort data = Sdtm.ds1 Out = Adsl2;
By Usubjid;
Where DSCAT = "DISPOSITION EVENT" and EPOCH = "TREATMENT"  ;
run;

Data Adsl3;
set Adsl2;

DSDECOD = Strip(DSDECOD);
DSSTDY = Strip(DSSTDY);
DSSTDT = INPUT (DSSTDTC ,YYMMDD10.);
Format dsstdt is8601da. ;

keep USUBJID DSDECOD DSSTDY DSSTDT; 
run;



PROC SORT DATA = SDTM.DS1 OUT = ADSL4;
BY USUBJID;
WHERE DSCAT = "DISPOSITION EVENT" and EPOCH = "FOLLOW-UP"   and DSDECOD  = "COMPLETED" ;

RUN;


DATA ADSL5;
SET ADSL4 ;
IF DSCAT = "DISPOSITION EVENT" and EPOCH = "FOLLOW-UP"   and DSDECOD  = "COMPLETED" THEN COMPFL = 'Y' ;
ELSE COMPFL = 'Y';
COMPLDT = INPUT(DSSTDTC,YYMMDD10.);
KEEP USUBJID COMPFL COMPLDT;

RUN;


PROC SORT DATA = SDTM.DS1 OUT = ADSL6;
BY USUBJID;
WHERE DSTERM='RANDOMIZED' AND DSCAT='PROTOCOL MILESTONE' AND DSSTDTC NE '' ;
RUN;


proc sort data=sdtm.ds out=rnd;
by usubjid;
where dsterm='RANDOMIZED' and Dscat='PROTOCOL MILESTONE' and Dsstdtc ne '';
run;



proc sort data=sdtm.ds1 out=ADSL6;
by usubjid;
where dsterm='RANDOMIZED' and Dscat='PROTOCOL MILESTONE' and Dsstdtc ne '';
run;

DATA ADSL7;
SET ADSL6;
If dsstdtc ne '' then do;
Randfl='Y';
Randdt=Input(Dsstdtc,yymmdd10.);
end;
Keep Usubjid Randfl Randdt;
run; 








DATA ADSL7;
SET ADSL6;
IF DSTERM='RANDOMIZED' AND DSCAT='PROTOCOL MILESTONE' AND DSSTDTC NE '' THEN RANDFL = 'Y';
IF RANDFL = 'Y' THEN ITTFL = 'Y';

KEEP USUBJID RANDFL ITTFL;
RUN;


PROC SORT DATA = Adsl3;
BY USUBJID;
RUN;


PROC SORT DATA = Adsl5;
BY USUBJID;
RUN;

PROC SORT DATA = Adsl7;
BY USUBJID;
RUN;





DATA ADSL8;
MERGE ADSL3 (IN =A) ADSL5 ADSL7;
BY USUBJID;
IF A ;
RUN;


DATA ADSL9;
SET SDTM.DS1;
RANDDT = INPUT(DSSTDTC,YYMMDD10.);
FORMAT RANDDT is8601da.;
run;


DATA ADSL10;
MERGE ADSL8 (IN=A) ADSL9;
BY USUBJID;
IF A;
keep USUBJID DSDECOD DSSTDY DSSTDT COMPFL COMPLDT RANDFL ITTFL RANDDT  ; 

RUN;







/*DERIVING THE BASELINES USING VS DOMAIN*/

DATA VS;
SET SDTM.VS;
WHERE VSBLFL='Y' AND VSSTRESN NE .;
RUN;

PROC TRANSPOSE DATA= VS OUT=VS1;
VAR VSSTRESN;
BY USUBJID ;
ID VSTESTCD;
RUN;

data vs2;
set vs1;
If height ne . then heightbl=height;
if weight ne . then weightbl=weight;
If sys_bp  ne .  then SYSBPBL = sys_bp; 
If dia_bp ne .  then DIABPBL = dia_bp;
if resp_rate ne . then RESPBL = resp_rate;
if heart_rate ne . then HRBL = heart_rate;
if temperature ne . then TEMPBL = temperature;
bmi=weight/(height*height); 
if BMI ne . then bmibl = BMI;
RUN;



/*DERIVING ONCOFL USING MH DOMAIN*/


PROC SORT DATA = SDTM.MH OUT= MH;
BY USUBJID;
WHERE MHCAT='CANCER' and MHOCCUR = 'Y'  ;

RUN;



DATA MH1;
SET MH;
IF MHCAT='CANCER' and MHOCCUR = 'Y' THEN ONCOFL = 'Y' ;
ELSE ONCOFL = 'N';
RUN;



/* USING EX DOMAIN TO DERIVE  EFFFL */




PROC SORT DATA = SDTM.EX OUT = EX;
BY USUBJID;
RUN;


PROC SORT DATA = ADSL1;
BY USUBJID;
RUN;



DATA DM_EX;
MERGE ADSL1 (IN = A)EX;
BY USUBJID;
IF A ;
RUN;



PROC SORT DATA =  DM_EX OUT  = DM_EX1 NODUPKEY DUPOUT = HJ;;
BY USUBJID ;

WHERE EXSTDTC NE ''  or TRTSDT ne .  And EXSTDY GE 77 ;
RUN;



DATA DM_EX2;

SET DM_EX1;
 IF   EXSTDTC NE ''  or TRTSDT ne . And EXSTDY GE 77 THEN EFFFL = 'Y';
 ELSE EFFFL = '';
 RUN;










/*MERGING ALL*/

 PROC SORT DATA = DM_SUPPDM ;
 BY USUBJID;
 RUN;


PROC SORT DATA = ADSL10 ;
 BY USUBJID;
 RUN;



 PROC SORT DATA = vs2 ;
 BY USUBJID;
 RUN;


 PROC SORT DATA = MH1 ;
 BY USUBJID;
 RUN;




 PROC SORT DATA = DM_EX2 ;
 BY USUBJID;
 RUN;


DATA ADSL;
MERGE DM_SUPPDM (IN=A) ADSL10 vs2 MH1 DM_EX2;
BY USUBJID;
IF A;
KEEP STUDYID DOMAIN USUBJID SUBJID SITEID AGE AGEU SEX SEXN RACE RACEN HEIGHTBL WEIGHTBL BMIBL SYSBPBL DIABPBL RESPBL HRBL 
TEMPBL ARMCD ARM TRT01P TRT01PN TRT01A TRT01AN TRTSDT TRTEDT TRTDUR SAFFL ETHNIC ETHNICN DMDTC DMDY TRTSDTM TRTSTM TRTEDTM TRTETM DTHFL DTHDT
DTHDY RACEOTH DSDECOD COMPFL COMPLDT DSSTDT DSSTDY RFSTDT RFENDT ACTARMCD ACTARM RFPENDT RFICDT RANDDT RANDFL AGEGRP ONCOFL 
SCRNFAFL AGEGRPN EFFFL ITTFL SCRNFL ;
RUN;
 

















DATA ADAM.ADSL;
SET ADSL;

ATTRIB 
 STUDYID LABEL = 'Study Identifier'
 DOMAIN  LABEL= 'Domain Abbreviation'

USUBJID LABEL = 'Unique Subject Identifier'
SUBJID LABEL = 'Subject Identifier for the Study'
SITEID LABEL = 'Study Site Identifier'
AGE LABEL = 'Age'
AGEU LABEL = 'Age Units' 
SEX LABEL = 'Sex'
SEXN LABEL = 'Sex in Numeric'
RACE LABEL = 'Race'
RACEN LABEL = 'Race in Numeric' 
HEIGHTBL LABEL = 'Height at Baseline'
WEIGHTBL LABEL = 'Weight at Baseline'
BMIBL LABEL = 'Body Mass Index at Baseline'
SYSBPBL LABEL = 'Systolic Blood Pressure at Baseline'
DIABPBL LABEL = 'Diastolic Blood Pressure at Baseline'
RESPBL LABEL = 'Respiratory at Baseline'
HRBL LABEL = 'Heart Rate at Baseline'
TEMPBL LABEL = 'Temperature at Baseline'
ARMCD LABEL = 'Planned Arm Code'
ARM LABEL = 'Description of Planned Arm'
TRT01P LABEL = 'Planned Treatment group'
TRT01PN LABEL = 'Planned Treatment group in numeric'
TRT01A LABEL = 'Analysis Treatment group'
TRT01AN LABEL = 'Analysis Treatment group in numeric'
TRTSDT LABEL = 'Date of first exposure to Treatment'
TRTEDT LABEL = 'Date of Last exposure to Treatment'
TRTDUR LABEL = 'Duration of Treatment Exposure'
SAFFL LABEL = 'Safety Population Flag'
ETHNIC LABEL = 'Ethnicity'
ETHNICN LABEL = 'Ethnicity in Numeric' 
DMDTC LABEL = 'Date/Time of Demography Collection'
DMDY LABEL = 'Study Day of Demography Collection'
TRTSDTM LABEL = 'Date of First Study Drug Exposure'
TRTSTM LABEL = 'Time of First Study Drug Exposure'
TRTEDTM LABEL = ' Date of last Study Drug Exposure'
TRTETM LABEL = 'DTime of Last Study Drug Exposure'
DTHFL LABEL = 'Death Flag' 
DTHDT LABEL = 'Death Date'
DTHDY LABEL = 'Study Day of Death'
RACEOTH LABEL = ' Race , Other' 
DSDECOD LABEL = 'Completed or Primary Discontinued Reason'
COMPFL LABEL = 'Completers Population Flag '
COMPLDT LABEL = 'Date/Time of Study Completion '
DSSTDT LABEL = 'Completed or Discontinued Date' 
DSSTDY LABEL = 'Completed or Discontinued Study Day'
RFSTDT LABEL = 'Date of Subject Reference Start'
RFENDT LABEL = 'Date of Subject Reference End '
ACTARMCD LABEL = 'Actual Arm Code'  
ACTARM LABEL = 'Decription of Arm Code'
RFPENDT LABEL = 'Date/Time of End of Participation' 
RFICDT  LABEL = 'Date/Time of Informed Consent'
RANDDT LABEL = 'Randomization Date'
RANDFL LABEL = 'Randomization Flag'
AGEGRP  LABEL = 'AGE Groups'
ONCOFL  LABEL = ' ONCOLOGY Flags'
SCRNFAFL LABEL = 'Screen Failure Flag' 
AGEGRPN  LABEL = ' AGE Groups in numeric'
EFFFL  LABEL = 'Efficacy Population Flag'
ITTFL  LABEL = 'Intent-to-treat Flag'
SCRNFL LABEL = 'Screened Flag' ;
RUN;














