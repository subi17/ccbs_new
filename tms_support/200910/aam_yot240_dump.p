/* ----------------------------------------------------------------------
  MODULE .......: mobcdr_dump.p
  TASK .........: Create a dump file for cdrs
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 06.04.09
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/detailseq.i}
{Syst/dumpfile_run.i}

def input parameter idadate as date no-undo.

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR ldaFromDate AS DATE NO-UNDO.
DEF VAR ldaToDate   AS DATE NO-UNDO.
DEF VAR ldaBaseDate AS DATE NO-UNDO.
DEF VAR ldeStartTS  AS DEC  NO-UNDO.
DEF VAR ldeEndTS    AS DEC  NO-UNDO.
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

ASSIGN 
   ldaBaseDate  = idadate
   ldaFromDate  = ldaBaseDate - 1
   ldaToDate    = ldaBaseDate - 1
   ldeStartTS   = fMake2DT(ldaFromDate,0)
   ldeEndTS     = fMake2DT(ldaToDate,86399).
 
OUTPUT STREAM sFile TO VALUE(icFile).

FOR EACH MobCDR NO-LOCK USE-INDEX Date WHERE
         MobCDR.DateSt >= ldaFromDate - 2 AND
         MobCDR.DateSt <= ldaToDate       AND
         MobCDR.ErrorCode = 0             AND
         MobCDR.ReadInTS >= ldeStartTS    AND
         MobCDR.ReadInTS <= ldeEndTS
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   fFindMcdrDtl2(MobCDR.DateSt, MobCDR.DtlSeq).

   lcCallID = "".

   IF AVAILABLE McdrDtl2 THEN DO:
      /* id depends on cdr version */
      CASE ENTRY(4,McdrDtl2.detail,"|"):
      WHEN "yc" THEN lcCallID = ENTRY(8,McdrDtl2.detail,"|").
      WHEN "mm" THEN lcCallID = ENTRY(33,McdrDtl2.detail,"|").
      END CASE.                     
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
      INTEGER(MobCDR.GsmBnr BEGINS "622" OR 
              MobCDR.GsmBnr BEGINS "633") lcDelimiter
      MobCDR.BillCode                     lcDelimiter
      MobCDR.CCN                          lcDelimiter
      MobCDR.BillDur                      lcDelimiter
      MobCDR.DataIn + MobCDR.DataOut      lcDelimiter
      TRIM(STRING(MobCDR.Amount,"->>>>>>>>>>>9.9<<<<<")) SKIP.
   
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


