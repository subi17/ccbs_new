DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand      AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE ldtTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldaPeriod    AS DATE      NO-UNDO.

{cparam2.i}
{timestamp.i}
{ftransdir.i}
{date.i}

DEFINE TEMP-TABLE ttMsRequest NO-UNDO
       FIELD MsSeq       AS INT.

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/imsi_" +
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

FOR EACH MobSub NO-LOCK,
   FIRST IMSI WHERE IMSI.IMSI = MobSub.IMSI NO-LOCK:

   RUN pCollectIMSI.
END.

ldaPeriod = ADD-INTERVAL(DATE(MONTH(TODAY),1,YEAR(TODAY)), -6, "months").

EMPTY TEMP-TABLE ttMsRequest.

FOR EACH MsRequest WHERE MsRequest.Brand      = gcBrand AND
                         MsRequest.ReqType    = 18      AND
                         MsRequest.ReqStatus  = 2       AND
                         MsRequest.ActStamp  >= fDate2TS(ldaPeriod) NO-LOCK:

   IF NOT CAN-FIND (FIRST ttMsRequest WHERE
                          ttMsRequest.MsSeq = MsRequest.MsSeq) THEN DO:
      CREATE ttMsRequest.
      ASSIGN ttMsRequest.MsSeq = MsRequest.MsSeq.
   END. 
END.

FOR EACH ttMsRequest NO-LOCK,
   FIRST TermMobSub WHERE TermMobSub.MsSeq = ttMsRequest.MsSeq NO-LOCK,
      FIRST IMSI WHERE IMSI.IMSI = TermMobSub.IMSI NO-LOCK:

   RUN pCollectIMSI.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).


PROCEDURE pCollectIMSI:

   liEvents = liEvents + 1.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "IMSI" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   PUT STREAM slog UNFORMATTED
       "IMSI"                          lcDel
       "CREATE"                        lcDel
       fNotNull(STRING(RECID(IMSI)))   lcDel
       fNotNull(IMSI.IMSI)             lcDel
       fNotNull(STRING(ldtTimeStamp))  lcDel
       fNotNull(IMSI.IMSI)             lcDel
       fNotNull(IMSI.ICC)              lcDel
       fNotNull(IMSI.PUK1)             lcDel
       fNotNull(IMSI.PUK2)             lcDel
       fNotNull(IMSI.PIN1)             lcDel
       fNotNull(IMSI.PIN2)             SKIP.

END PROCEDURE.
