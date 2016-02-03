DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand      AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE ldeEndStamp  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lcKeyValue   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel2       AS CHARACTER NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldaFromDate  AS DATE      NO-UNDO.

{Func/cparam2.i}
{Func/timestamp.i}
{Func/ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/customermservicelpool_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldaFromDate  = DATE(MONTH(TODAY),1,YEAR(TODAY))
       ldeEndStamp  = fMake2Dt(ldaFromDate,0)
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       lcDel2       = CHR(255)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH ServiceLimit WHERE
         ServiceLimit.GroupCode BEGINS "DSS" NO-LOCK,
    EACH MServiceLPool WHERE
         MServiceLPool.SlSeq  = ServiceLimit.SlSeq AND
         MServiceLPool.EndTS >= ldeEndStamp NO-LOCK,
   FIRST MobSub WHERE
         MobSub.MsSeq = MServiceLPool.MsSeq NO-LOCK,
   FIRST Customer WHERE
         Customer.CustNum = MServiceLPool.CustNum NO-LOCK:

   /* Exclude subscription based bundles */
   IF MServiceLPool.CustNum = 0 OR MServiceLPool.CustNum = ? THEN NEXT.

   liEvents = liEvents + 1.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "MServiceLPool" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   lcKeyValue = STRING(MServiceLPool.CustNum) + lcDel2 +
                STRING(MServiceLPool.SLSeq)   + lcDel2 +
                STRING(MServiceLPool.FromTS).

   PUT STREAM slog UNFORMATTED
       "CustomerMServiceLPool"                  lcDel
       "CREATE"                                 lcDel
       fNotNull(STRING(RECID(MServiceLPool)))   lcDel
       fNotNull(lcKeyValue)                     lcDel
       fNotNull(STRING(ldtTimeStamp))           lcDel
       fNotNull(STRING(MServiceLPool.MsSeq))    lcDel
       fNotNull(STRING(MServiceLPool.SLSeq))    lcDel
       fNotNull(STRING(MServiceLPool.LimitAmt)) lcDel
       fNotNull(STRING(MServiceLPool.FromTS))   lcDel
       fNotNull(STRING(MServiceLPool.EndTS))    lcDel
       fNotNull(STRING(MServiceLPool.CustNum))  SKIP.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).


