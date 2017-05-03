/* ----------------------------------------------------------------------
  Create a dump file for prepaid cdrs
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/detailvalue.i}
{Syst/dumpfile_run.i}
{Func/msisdn_prefix.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idaFromDate   AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaToDate     AS DATE NO-UNDO.
DEF INPUT  PARAMETER ilAppend      AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric   AS CHAR NO-UNDO.
DEF VAR lcCallID    AS CHAR NO-UNDO.
DEF VAR lcDelimiter AS CHAR NO-UNDO.

DEF STREAM sFile.

lcNumeric = SESSION:NUMERIC-FORMAT.

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

IF ilAppend THEN 
   OUTPUT STREAM sFile TO VALUE(icFile) APPEND.
ELSE OUTPUT STREAM sFile TO VALUE(icFile).

FOR EACH PrepCDR NO-LOCK USE-INDEX ReadDate WHERE
         PrepCDR.ReadDate >= idaFromDate AND
         PrepCDR.ReadDate <= idaToDate   AND
         PrepCDR.ErrorCode = 0          
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.
   
   lcCallID = "".
   FIND FIRST mcdrdtl2 WHERE
              mcdrdtl2.datest = PrepCDR.DateSt AND
              mcdrdtl2.dtlseq = PrepCDR.DtlSeq NO-LOCK NO-ERROR.
   IF AVAIL MCDRDtl2 THEN 
      lcCallID = fGetValue(MCDRDtl2.Version,
                           "Call identification number",
                           MCDRDtl2.DateSt,
                           MCDRDtl2.DtlSeq).
   IF lcCallID = "" THEN 
      lcCallID = PrepCDR.CLI + PrepCDR.GsmBnr +
                 STRING(YEAR(PrepCDR.DateSt),"9999") + 
                 STRING(MONTH(PrepCDR.DateSt),"99")  +
                 STRING(DAY(PrepCDR.DateSt),"99")    + 
                 REPLACE(STRING(PrepCDR.TimeSt,"hh:mm:ss"),":","").
   
   PUT STREAM sFile UNFORMATTED
      PrepCDR.MsSeq                        lcDelimiter
      PrepCDR.CLI                          lcDelimiter
      PrepCDR.CLIType                      lcDelimiter
      PrepCDR.InvCust                      lcDelimiter
      lcCallID                            lcDelimiter
      STRING(PrepCDR.DateSt,"99.99.9999")  lcDelimiter
      STRING(PrepCDR.TimeSt,"hh:mm:ss")    lcDelimiter
      PrepCDR.ReadInTS                     lcDelimiter
      PrepCDR.GsmBnr                       lcDelimiter
      INTEGER(fIsYoigoCLI(PrepCDR.GsmBnr) OR fIsMasmovilCLI(PrepCDR.GsmBnr)) lcDelimiter
      PrepCDR.BillCode                     lcDelimiter
      PrepCDR.CCN                          lcDelimiter
      PrepCDR.BillDur                      lcDelimiter
      PrepCDR.DataIn + PrepCDR.DataOut      lcDelimiter
      TRIM(STRING(PrepCDR.Charge,"->>>>>>>>>>>9.9<<<<<")) SKIP.
   
   oiEvents = oiEvents + 1.

   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "CDRs" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END.                       

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.


