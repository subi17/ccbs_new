DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand      AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE ldeEndStamp  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldaFromDate  AS DATE      NO-UNDO.
DEFINE VARIABLE liCustNum    AS INTEGER   NO-UNDO.

{cparam2.i}
{timestamp.i}
{ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/mservicelimit_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldaFromDate  = DATE(MONTH(TODAY),1,YEAR(TODAY))
       ldeEndStamp  = fMake2Dt(ldaFromDate,0)
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH MobSub WHERE
         MobSub.Brand = gcBrand NO-LOCK,
    EACH MServiceLimit WHERE
         MServiceLimit.MsSeq  = MobSub.MsSeq AND
         MServiceLimit.EndTS >= ldeEndStamp NO-LOCK:

   liEvents = liEvents + 1.

   /* Exclude customer based bundles */
   IF MServiceLimit.CustNum > 0 THEN NEXT.

   IF MServiceLimit.CustNum = ? THEN liCustNum = 0.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "MServiceLimit" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   PUT STREAM slog UNFORMATTED
       "MServiceLimit"                          lcDel
       "CREATE"                                 lcDel
       fNotNull(STRING(RECID(MServiceLimit)))   lcDel
       fNotNull(STRING(MServiceLimit.MSID))     lcDel
       fNotNull(STRING(ldtTimeStamp))           lcDel
       fNotNull(STRING(MServiceLimit.MSID))     lcDel
       fNotNull(STRING(MServiceLimit.MsSeq))    lcDel
       fNotNull(STRING(MServiceLimit.SLSeq))    lcDel
       fNotNull(STRING(MServiceLimit.DialType)) lcDel
       fNotNull(STRING(MServiceLimit.InclUnit)) lcDel
       fNotNull(STRING(MServiceLimit.InclAmt))  lcDel
       fNotNull(STRING(MServiceLimit.FromTS))   lcDel
       fNotNull(STRING(MServiceLimit.EndTS))    lcDel
       fNotNull(STRING(liCustNum))              SKIP.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).



