DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand      AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldaFromDate  AS DATE      NO-UNDO.

{Func/cparam2.i}
{Func/timestamp.i}
{Func/ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/dccli_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldaFromDate  = DATE(MONTH(TODAY),1,YEAR(TODAY))
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH MobSub WHERE
         MobSub.Brand = gcBrand NO-LOCK,
    EACH DCCLI WHERE
         DCCLI.MsSeq    = MobSub.MsSeq AND
         DCCLI.ValidTo >= ldaFromDate NO-LOCK:

   liEvents = liEvents + 1.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "DCCLI" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   PUT STREAM slog UNFORMATTED
       "DCCLI"                               lcDel
       "CREATE"                              lcDel
       fNotNull(STRING(RECID(DCCLI)))        lcDel
       fNotNull(STRING(DCCLI.PerContractID)) lcDel
       fNotNull(STRING(ldtTimeStamp))        lcDel
       fNotNull(STRING(DCCLI.MsSeq))         lcDel
       fNotNull(STRING(DCCLI.PerContractID)) lcDel
       fNotNull(DCCLI.DCEvent)               lcDel
       fNotNull(STRING(DCCLI.ValidFrom))     lcDel
       fNotNull(STRING(DCCLI.ValidTo))       lcDel
       fNotNull(STRING(DCCLI.ValidToOrig))   lcDel
       fNotNull(STRING(DCCLI.RenewalDate))   lcDel
       fNotNull(STRING(DCCLI.Amount))        SKIP.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).
