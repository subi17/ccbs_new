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

ASSIGN lcLogFile = lcSpoolDir + "/fixedfee_" +
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

FOR EACH MobSub WHERE
         MobSub.Brand = gcBrand NO-LOCK,
    EACH FixedFee WHERE
         FixedFee.Brand = gcBrand AND
         FixedFee.CustNum = MobSub.CustNum AND
         FixedFee.HostTable = "MobSub" AND
         FixedFee.KeyValue  = STRING(MobSub.MsSeq) AND
         FixedFee.EndPeriod >= liPeriod NO-LOCK USE-INDEX CustNum:

   liEvents = liEvents + 1.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "FixedFee" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   PUT STREAM slog UNFORMATTED
       "FixedFee"                           lcDel
       "CREATE"                             lcDel
       fNotNull(STRING(RECID(FixedFee)))    lcDel
       fNotNull(STRING(FixedFee.FFNum))     lcDel
       fNotNull(STRING(ldtTimeStamp))       lcDel
       fNotNull(STRING(FixedFee.FFNum))     lcDel
       fNotNull(STRING(FixedFee.CustNum))   lcDel
       fNotNull(FixedFee.HostTable)         lcDel
       fNotNull(FixedFee.KeyValue)          lcDel
       fNotNull(FixedFee.FeeModel)          lcDel
       fNotNull(FixedFee.BillCode)          lcDel
       fNotNull(FixedFee.CalcObj)           lcDel
       fNotNull(STRING(FixedFee.Amt))       lcDel
       fNotNull(STRING(FixedFee.BegPeriod)) lcDel
       fNotNull(STRING(FixedFee.EndPeriod)) lcDel
       fNotNull(STRING(FixedFee.CustPP))    lcDel
       fNotNull(STRING(FixedFee.CalcAmt))   lcDel
       fNotNull(STRING(FixedFee.BegDate))   lcDel
       fNotNull(FixedFee.SourceTable)       lcDel
       fNotNull(FixedFee.SourceKey)         lcDel
       fNotNull(STRING(FixedFee.OrderId))   lcDel
       fNotNull(FixedFee.FinancedResult)    lcDel
       fNotNull(FixedFee.TFBank)            SKIP.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).


