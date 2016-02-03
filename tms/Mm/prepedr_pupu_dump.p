/* ----------------------------------------------------------------------
  MODULE .......: prepedr_pupu_dump.p
  TASK .........: Prepaid EDR dump for PUPU
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 30.08.13
  Version ......: Yoigo
----------------------------------------------------------------------- */
{Syst/commali.i}
{Func/detailvalue.i}
{Syst/dumpfile_run.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF INPUT  PARAMETER ilAppend      AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric                  AS CHAR NO-UNDO.
DEF VAR lcDelimiter                AS CHAR NO-UNDO.
DEF VAR ldaReadDate                AS DATE NO-UNDO.
DEF VAR liTime                     AS INT  NO-UNDO.
DEF VAR ldtCurrStamp               AS DATETIME NO-UNDO.
DEF VAR lcKeyValue                 AS CHAR NO-UNDO.
DEF VAR lcDel2                     AS CHAR NO-UNDO.

DEF STREAM sFile.

ASSIGN ldtCurrStamp = DATETIME(TODAY,MTIME)
       lcDel2       = CHR(255)
       lcNumeric    = SESSION:NUMERIC-FORMAT
       icFile       = REPLACE(icFile,".gz","").

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).
   
   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END.
ELSE DO:
   ASSIGN 
      lcDelimiter = CHR(9)
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END.

fSplitTs(idLastDump,ldaReadDate,liTime).

IF ldaReadDate = ? THEN ldaReadDate = TODAY.

IF ilAppend THEN 
   OUTPUT STREAM sFile TO VALUE(icFile) APPEND.
ELSE OUTPUT STREAM sFile TO VALUE(icFile).

FOR EACH PrepEDR NO-LOCK USE-INDEX ReadDate WHERE
         PrepEDR.ReadDate   >= ldaReadDate AND
         PrepEDR.ReadInTS    > idLastDump  AND
         PrepEDR.ErrorCode   = 0          
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   ASSIGN
      oiEvents   = oiEvents + 1
      lcKeyValue = PrepEDR.CLI + lcDel2 + STRING(PrepEDR.DtlSeq) + lcDel2 +
                   STRING(PrepEDR.DateSt).

   PUT STREAM sFile UNFORMATTED
      "PrepEDR"                            lcDelimiter
      "CREATE"                             lcDelimiter
      STRING(RECID(PrepEDR))               lcDelimiter
      lcKeyValue                           lcDelimiter
      STRING(ldtCurrStamp)                 lcDelimiter
      PrepEDR.MsSeq                        lcDelimiter
      PrepEDR.CLI                          lcDelimiter
      PrepEDR.CustNum                      lcDelimiter
      STRING(PrepEDR.DateSt,"99.99.9999")  lcDelimiter
      STRING(PrepEDR.TimeStart,"HH:MM:SS") lcDelimiter
      PrepEDR.ReadInTS                     lcDelimiter
      PrepEDR.SubscriberFee                lcDelimiter
      PrepEDR.SuccessCode                  lcDelimiter
      PrepEDR.BalanceAfter SKIP.

   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "EDRs" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
END.                       

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

UNIX SILENT VALUE("gzip " + icFile).
icFile = icFile + ".gz".

SESSION:NUMERIC-FORMAT = lcNumeric.
