DEFINE VARIABLE lcLogFile           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel               AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE ldtTimeStamp        AS DATETIME  NO-UNDO.
DEFINE VARIABLE liEvents            AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTariffBundle      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOldCLIType        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOldtariffBundle   AS CHARACTER NO-UNDO.
DEFINE VARIABLE llDeviceStart       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llDeviceScreen      AS LOGICAL   NO-UNDO.

{Syst/commpaa.i}
katun = "Cron".
gcbrand = "1".
{Func/cparam2.i}
{Func/timestamp.i}
{Func/ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/termreturn_" +
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


FOR EACH TermReturn NO-LOCK:

   liEvents = liEvents + 1.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "TermReturn"
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   IF TermReturn.DeviceStart = ? AND TermReturn.DeviceScreen = ? THEN
     ASSIGN llDeviceStart = TRUE
            llDeviceScreen = TRUE.
   ELSE ASSIGN llDeviceStart = TermReturn.DeviceStart
               llDeviceScreen = TermReturn.DeviceScreen.

   PUT STREAM slog UNFORMATTED
       "TermReturn"                                lcDel
       "CREATE"                                    lcDel
       fNotNull(STRING(RECID(TermReturn)))         lcDel
       fNotNull(STRING(TermReturn.OrderId)  + CHR(255) +
                STRING(TermReturn.ReturnTS))       lcDel
       fNotNull(STRING(ldtTimeStamp))              lcDel
       fNotNull(STRING(TermReturn.OrderId))        lcDel
       fNotNull(TermReturn.BillCode)               lcDel
       fNotNull(TermReturn.IMEI)                   lcDel
       fNotNull(TermReturn.MSISDN)                 lcDel
       fNotNull(STRING(llDeviceStart))             lcDel
       fNotNull(STRING(llDeviceScreen))            lcDel
       fNotNull(TermReturn.Salesman)               lcDel
       fNotNull(TermReturn.TerminalType)           lcDel
       fNotNull(TermReturn.EnvelopeNumber)         lcDel
       fNotNull(STRING(TermReturn.ReturnTS))       lcDel      
       fNotNull(TermReturn.ReturnChannel)          lcDel
       fNotNull(TermReturn.ContractID)             SKIP.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).
