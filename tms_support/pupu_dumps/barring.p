DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE ldtTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTariffBundle AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOldCLIType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOldtariffBundle AS CHARACTER NO-UNDO.

{commpaa.i}
katun = "Cron".
gcbrand = "1".
{cparam2.i}
{timestamp.i}
{ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/barring_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).


FOR EACH Barring NO-LOCK:

   liEvents = liEvents + 1.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "Barring"
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   PUT STREAM slog UNFORMATTED
       "Barring"                              lcDel
       "CREATE"                               lcDel
       fNotNull(STRING(RECID(Barring)))       lcDel
       fNotNull(STRING(Barring.MsSeq) + CHR(255) +
                Barring.BarringCode + CHR(255) +
                Barring.BarringStatus + CHR(255) +
                STRING(Barring.EventTS))      lcDel
       fNotNull(STRING(ldtTimeStamp))         lcDel
       fNotNull(STRING(Barring.MsSeq))        lcDel
       fNotNull(Barring.BarringCode)          lcDel
       fNotNull(Barring.BarringStatus)        lcDel
       fNotNull(Barring.UserCode)             lcDel
       fNotNull(STRING(Barring.EventTS))      SKIP.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).

