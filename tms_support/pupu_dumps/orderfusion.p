DEFINE VARIABLE lcLogFile           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcSpoolDir          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcOutDir            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcDel               AS CHARACTER  NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand             AS CHARACTER  NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp        AS DATETIME   NO-UNDO INIT ?.
DEFINE VARIABLE liEvents            AS INTEGER    NO-UNDO.
DEFINE VARIABLE lcMessage           AS CHARACTER  NO-UNDO.

{cparam2.i}
{timestamp.i}
{ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/orderfusion_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fToUTF8 RETURNS CHARACTER 
         (INPUT pcText AS CHARACTER):

   RETURN  CODEPAGE-CONVERT(pcText,"UTF-8",SESSION:CHARSET).

END FUNCTION.

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH OrderFusion WHERE
         OrderFusion.Brand = gcBrand NO-LOCK:
 
   ASSIGN liEvents  = liEvents + 1
          lcMessage = "OrderFusion"                              + lcDel +
                      "CREATE"                                   + lcDel +
                      fNotNull(STRING(RECID(OrderFusion)))       + lcDel +
                      fNotNull(STRING(OrderFusion.OrderID))      + lcDel +
                      fNotNull(STRING(ldtTimeStamp))             + lcDel +
                      fNotNull(STRING(OrderFusion.OrderId))      + lcDel +
                      fNotNull(OrderFusion.FusionStatus)         + lcDel +
                      fNotNull(OrderFusion.FixedNumber)          + lcDel +
                      fNotNull(OrderFusion.FixedNumberType)      + lcDel +
                      fNotNull(OrderFusion.FixedStatus).

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "OrderFusion" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   lcMessage = fToUTF8(lcMessage).

   PUT STREAM slog UNFORMATTED lcMessage SKIP.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).
