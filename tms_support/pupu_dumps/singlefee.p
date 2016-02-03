DEFINE VARIABLE lcLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDel        AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE gcBrand      AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE liPeriod     AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldtCurrStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE liEvents     AS INTEGER   NO-UNDO.

{Func/cparam2.i}
{Func/timestamp.i}
{Func/ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir").

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/singlefee_" +
                   STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") +
                   STRING(DAY(TODAY),"99") + "_" +
                   REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
       ldtCurrStamp = DATETIME(TODAY,MTIME)
       liPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
       SESSION:NUMERIC-FORMAT = "AMERICAN".

FUNCTION fNotNull RETURNS CHAR (INPUT icInput AS CHAR):

   IF icInput = ? THEN icInput = "".
   
   RETURN icInput.

END. /* FUNCTION fNotNull RETURNS CHAR (INPUT): */

OUTPUT STREAM slog TO VALUE(lcLogFile).

FOR EACH MobSub WHERE
         MobSub.Brand = gcBrand NO-LOCK,
    EACH SingleFee WHERE
         SingleFee.Brand = gcBrand AND
         SingleFee.CustNum = MobSub.CustNum AND
         SingleFee.HostTable = "MobSub" AND
         SingleFee.KeyValue  = STRING(MobSub.MsSeq) AND
         SingleFee.BillPeriod >= liPeriod NO-LOCK USE-INDEX CustNum:

   liEvents = liEvents + 1.

   IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liEvents LABEL "SingleFee" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

   PUT STREAM slog UNFORMATTED
       "SingleFee"                              lcDel
       "CREATE"                                 lcDel
       fNotNull(STRING(RECID(SingleFee)))       lcDel
       fNotNull(STRING(SingleFee.FMItemId))     lcDel
       fNotNull(STRING(ldtCurrStamp))           lcDel
       fNotNull(STRING(SingleFee.FMItemId))     lcDel
       fNotNull(STRING(SingleFee.CustNum))      lcDel
       fNotNull(SingleFee.HostTable)            lcDel
       fNotNull(SingleFee.KeyValue)             lcDel
       fNotNull(SingleFee.FeeModel)             lcDel
       fNotNull(SingleFee.BillCode)             lcDel
       fNotNull(SingleFee.CalcObj)              lcDel
       fNotNull(STRING(SingleFee.Amt))          lcDel
       fNotNull(STRING(SingleFee.BillPeriod))   lcDel
       fNotNull(STRING(SingleFee.Concerns[1]))  lcDel
       fNotNull(SingleFee.SourceTable)          lcDel
       fNotNull(SingleFee.SourceKey)            lcDel
       fNotNull(STRING(SingleFee.SourceKey))    SKIP.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).

