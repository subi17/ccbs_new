DEFINE VARIABLE ldaDate      AS DATE                                NO-UNDO.
DEFINE VARIABLE lcTableNames AS CHARACTER INITIAL "MobCDR,McdrDtl2" NO-UNDO.
DEFINE VARIABLE lii          AS INTEGER                             NO-UNDO.
DEFINE VARIABLE lcSourcePFFileName AS CHARACTER                     NO-UNDO.
DEFINE VARIABLE lcTargetPFFileName AS CHARACTER                     NO-UNDO.
DEFINE VARIABLE llCreatePF   AS LOGICAL   INITIAL TRUE              NO-UNDO.
DEFINE VARIABLE lcPFText     AS CHARACTER                           NO-UNDO.

ASSIGN
   lcSourcePFFilename = "/apps/yoigo/tms/pf/batch.pf"
   lcTargetPFFileName = SESSION:PARAMETER
   ldaDate            = TODAY
   .

IF NOT lcTargetPFFileName > ""
THEN RETURN.

DO lii = 1 TO NUM-ENTRIES(lcTableNames):
   FIND FIRST DBConfig NO-LOCK WHERE
      DBConfig.Brand     = "1"                     AND
      DBConfig.TableName = ENTRY(lii,lcTableNames) AND
      DBConfig.DBState   = 1                       AND
      DBConfig.ToDate   >= ldaDate
   NO-ERROR.
   
   IF NOT AVAILABLE DBConfig
   THEN DO:      
      llCreatePF = FALSE.
      LEAVE.
   END.
   
   ELSE lcPFText = lcPFText + (IF lcPFText = "" THEN "" ELSE CHR(10)) + "-db " +
                   DBConfig.DirectConnect + "/" + DBConfig.DBConnName +
                   " -ld " + DBConfig.LogicalName.
END.

IF llCreatePF
THEN DO:
   OS-COPY VALUE(lcSourcePFFilename) VALUE(lcTargetPFFileName).
   DEFINE STREAM outstr.
   OUTPUT STREAM outstr TO VALUE(lcTargetPFFileName) APPEND.
   PUT STREAM outstr UNFORMATTED lcPFText SKIP.
   OUTPUT STREAM outstr CLOSE.   
END.
ELSE OS-DELETE VALUE(lcTargetPFFileName).