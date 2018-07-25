DEFINE VARIABLE lcNewCliTypes AS CHAR NO-UNDO INITIAL
   "CONTDSL48,CONTFH48_50,CONTFH58_300,CONTFH76_1000".
DEFINE VARIABLE liCount AS INTEGER NO-UNDO.

/* DSS4_PRIMARY_SUBS_TYPE */
FIND FIRST TMSParam EXCLUSIVE-LOCK WHERE
           TMSParam.Brand      EQ Syst.Var:gcBrand AND
           TMSParam.ParamGroup EQ "Bundles"        AND
           TMSParam.ParamCode  EQ "DSS2_PRIMARY_SUBS_TYPE" NO-ERROR.
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
           TMSParam.ParamCode  EQ "DSS2_SUBS_TYPE" NO-ERROR.
IF NOT AVAILABLE TMSParam THEN
   RETURN.

DO liCount = 1 TO NUM-ENTRIES(lcNewCliTypes):
   IF LOOKUP (ENTRY(liCount,lcNewCliTypes), TMSParam.CharVal) EQ 0 THEN
      ASSIGN TMSParam.CharVal = TMSParam.CharVal + "," +
                                ENTRY(liCount,lcNewCliTypes).
END.
RELEASE TMSParam.
