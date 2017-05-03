/* ----------------------------------------------------------------------
  MODULE .......: mobcdr_dump.p
  TASK .........: Create a dump file for cdrs
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 06.04.09
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/detailvalue.i}
{Syst/dumpfile_run.i}
{Syst/tmsconst.i}
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
DEF VAR liCase      AS INT  NO-UNDO.
DEF VAR lcVersion   AS CHAR NO-UNDO.
DEF VAR liPos       AS INT  NO-UNDO.

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

fFillTempTable().

FOR EACH MobCDR NO-LOCK USE-INDEX ReadDate WHERE
         MobCDR.ReadDate >= idaFromDate   AND
         MobCDR.ReadDate <= idaToDate     AND
         MobCDR.ErrorCode = 0  
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   RUN pWrite2File.
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.


PROCEDURE pWrite2File:

   ASSIGN
      lcCallID = ""
      liPos    = 0.
      
   FIND FIRST McdrDtl2 WHERE
              McdrDtl2.datest = MobCDR.DateSt AND
              McdrDtl2.dtlseq = MobCDR.DtlSeq NO-LOCK NO-ERROR.
   IF AVAIL McdrDtl2 THEN DO:
      lcVersion = McdrDtl2.Version.
      IF INDEX(lcVersion,"YC") = 0 THEN 
         lcVersion = lcVersion + ENTRY(4,McdrDtl2.Detail,"|").
    
      liPos = fGetPosition(lcVersion,
                           "Call identification number").
      IF liPos > 0 THEN 
         lcCallID = ENTRY(liPos,McdrDtl2.Detail,"|").
   END.                        
   
   IF lcCallID = "" THEN 
      lcCallID = MobCDR.CLI + MobCDR.GsmBnr +
                 STRING(YEAR(MobCDR.DateSt),"9999") + 
                 STRING(MONTH(MobCDR.DateSt),"99")  +
                 STRING(DAY(MobCDR.DateSt),"99")    + 
                 REPLACE(STRING(MobCDR.TimeSt,"hh:mm:ss"),":","").
   
   PUT STREAM sFile UNFORMATTED
      MobCDR.MsSeq                        lcDelimiter
      MobCDR.CLI                          lcDelimiter
      MobCDR.CLIType                      lcDelimiter
      MobCDR.InvCust                      lcDelimiter
      lcCallID                            lcDelimiter
      STRING(MobCdr.DateSt,"99.99.9999")  lcDelimiter
      STRING(MobCdr.TimeSt,"hh:mm:ss")    lcDelimiter
      MobCDR.ReadInTS                     lcDelimiter
      MobCDR.GsmBnr                       lcDelimiter
      INTEGER((fIsYoigoCLI(MobCDR.GsmBnr) OR fIsMasmovilCLI(MobCDR.GsmBnr))) lcDelimiter
      MobCDR.BillCode                     lcDelimiter
      MobCDR.CCN                          lcDelimiter
      MobCDR.BillDur                      lcDelimiter
      MobCDR.DataIn + MobCDR.DataOut      lcDelimiter
      TRIM(STRING(MobCDR.Amount,"->>>>>>>>>>>9.9<<<<<")) lcDelimiter
      MobCDR.OrigRecordType SKIP.
   
   oiEvents = oiEvents + 1.

   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "CDRs" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
    
END PROCEDURE.               


