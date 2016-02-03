DEF VAR lcLogFile    AS CHARACTER NO-UNDO.
DEF VAR lcSpoolDir   AS CHARACTER NO-UNDO.
DEF VAR lcOutDir     AS CHARACTER NO-UNDO.
DEF VAR lcDel        AS CHARACTER NO-UNDO INIT "|".
DEF VAR gcBrand      AS CHARACTER NO-UNDO INIT "1".
DEF VAR ldtTimeStamp AS DATETIME  NO-UNDO.
DEF VAR liEvents     AS INTEGER   NO-UNDO.
DEF VAR liPPstatus AS INT NO-UNDO.
DEF VAR ldeFrom AS DEC NO-UNDO. 

{Func/cparam2.i}
{Func/timestamp.i}
{Func/ftransdir.i}

DEFINE STREAM slog.

ASSIGN lcSpoolDir = fCParam("HPD","DumpSpoolDir")
       lcOutDir   = fCParam("HPD","DumpOutDir")
       ldeFrom    = fMake2Dt(ADD-INTERVAL(TODAY,-6,"months"),0).

IF lcSpoolDir = "" OR lcOutDir = "" THEN RETURN.

ASSIGN lcLogFile = lcSpoolDir + "/prepaidrequest_" +
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

DO liPPstatus = 0 TO 100:
   FOR EACH PrepaidRequest NO-LOCK WHERE
            PrepaidRequest.Brand = gcBrand AND
            PrepaidRequest.PPStatus = liPPstatus AND
            PrepaidRequest.TsRequest >= ldeFrom:

      liEvents = liEvents + 1.

      IF NOT SESSION:BATCH AND liEvents MOD 1000 = 0 THEN DO:
         PAUSE 0.
         DISP liEvents LABEL "PrepaidRequest" 
         WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
            TITLE " Collecting " FRAME fQty.
      END. /* IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN DO: */

      PUT STREAM slog UNFORMATTED
          "PrepaidRequest"                                  lcDel
          "CREATE"                                          lcDel
          fNotNull(STRING(RECID(PrepaidRequest)))           lcDel
          fNotNull(STRING(PrepaidRequest.PPrequest))        lcDel
          fNotNull(STRING(ldtTimeStamp))                    lcDel
          fNotNull(STRING(PrepaidRequest.PPrequest))        lcDel
          fNotNull(STRING(PrepaidRequest.MsSeq))            lcDel
          fNotNull(PrepaidRequest.CLI)                      lcDel
          fNotNull(PrepaidRequest.Request)                  lcDel
          fNotNull(PrepaidRequest.Source)                   lcDel
          fNotNull(STRING(PrepaidRequest.TSRequest))        lcDel
          fNotNull(STRING(PrepaidRequest.RespCode))         lcDel
          fNotNull(STRING(PrepaidRequest.PPStatus))         lcDel
          fNotNull(STRING(PrepaidRequest.TopUpAmt / 100))   lcDel
          fNotNull(STRING(PrepaidRequest.VatAmt / 100))     lcDel
          fNotNull(PrepaidRequest.Reference)                SKIP.
   END.
END.

OUTPUT STREAM slog CLOSE.

UNIX SILENT VALUE("gzip " + lcLogFile).
lcLogFile = lcLogFile + ".gz".

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.

/* Move the report to Transfer directory */
fMove2TransDir(lcLogFile, ".txt", lcOutDir).

