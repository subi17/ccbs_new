DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand      AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE liPeriod     AS INTEGER   NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.

{cparam2.i}
{timestamp.i}
{ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/substerminal" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldtTimeStamp = DATETIME(TODAY,MTIME)
       liPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).
FOR EACH Order WHERE
         Order.Brand = gcBrand NO-LOCK,
    EACH SubsTerminal WHERE
         SubsTerminal.Brand = gcBrand AND
         SubsTerminal.OrderId = Order.OrderId NO-LOCK:

   liEvents = liEvents + 1.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "SubsTerminal" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   PUT STREAM slog UNFORMATTED
       "SubsTerminal"                              lcDel
       "CREATE"                                    lcDel
       fNotNull(STRING(RECID(SubsTerminal)))       lcDel
       fNotNull(STRING(SubsTerminal.MSSeq))        lcDel
       fNotNull(STRING(ldtTimeStamp))              lcDel
       fNotNull(STRING(SubsTerminal.MSSeq))        lcDel
       fNotNull(STRING(SubsTerminal.OrderId))      lcDel
       fNotNull(STRING(SubsTerminal.TerminalId))   lcDel
       fNotNull(STRING(SubsTerminal.TerminalType)) lcDel
       fNotNull(SubsTerminal.Model)                lcDel
       fNotNull(SubsTerminal.IMEI)                 lcDel
       fNotNull(SubsTerminal.BillCode)             lcDel
       fNotNull(STRING(SubsTerminal.PurchaseTS))   lcDel
       fNotNull(STRING(SubsTerminal.PerContractID)) SKIP.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).


