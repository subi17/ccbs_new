/*-------------------------------------------
Updating DSS4 allowed Subscription types:
New Try&Buy tariff are compatible with DSS4.
/tmp_support/201807/YCO-597.p
jotorres 05.07.2018
--------------------------------------------*/

DEFINE VARIABLE lcNewCliTypes AS CHAR NO-UNDO INITIAL 
   "CONTDSLTB59,CONTFHTB59_50,CONTFHTB69_300,CONTFHTB89_1000,CONTFHNBTB69_300".
DEFINE VARIABLE liCount AS INTEGER NO-UNDO.

/* DSS4_PRIMARY_SUBS_TYPE */
FIND FIRST TMSParam EXCLUSIVE-LOCK WHERE
           TMSParam.Brand      EQ Syst.Var:gcBrand AND
           TMSParam.ParamGroup EQ "Bundles"        AND
           TMSParam.ParamCode  EQ "DSS4_PRIMARY_SUBS_TYPE" NO-ERROR.
IF NOT AVAILABLE TMSParam THEN 
   RETURN.
             
DO liCount = 1 TO NUM-ENTRIES(lcNewCliTypes):
   IF LOOKUP (ENTRY(liCount,lcNewCliTypes), TMSParam.CharVal) EQ 0 THEN 
      ASSIGN TMSParam.CharVal = TMSParam.CharVal + "," + 
                                ENTRY(liCount,lcNewCliTypes).            
END.   
RELEASE TMSParam.

/* DSS4_SUBS_TYPE */   
FIND FIRST TMSParam EXCLUSIVE-LOCK WHERE
           TMSParam.Brand      EQ Syst.Var:gcBrand AND
           TMSParam.ParamGroup EQ "Bundles"        AND
           TMSParam.ParamCode  EQ "DSS4_SUBS_TYPE" NO-ERROR.
IF NOT AVAILABLE TMSParam THEN 
   RETURN.
             
DO liCount = 1 TO NUM-ENTRIES(lcNewCliTypes):
   IF LOOKUP (ENTRY(liCount,lcNewCliTypes), TMSParam.CharVal) EQ 0 THEN 
      ASSIGN TMSParam.CharVal = TMSParam.CharVal + "," + 
                                ENTRY(liCount,lcNewCliTypes).            
END.   
RELEASE TMSParam.

