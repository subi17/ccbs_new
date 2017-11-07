/* ----------------------------------------------------------------------
  MODULE .......: mnp_cancellation_log.p
  TASK .........: Dumps MNP IN cancellation events. YDR-579
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 19.9.2012
  Version ......: Yoigo
----------------------------------------------------------------------- */
{Syst/commali.i}
{Syst/tmsconst.i}
{Syst/dumpfile_run.i}
{Func/cparam2.i}
{Func/ftransdir.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF STREAM sdump.

DEF VAR ldaDate AS DATE NO-UNDO. 
DEF VAR ldeFrom AS DEC NO-UNDO. 
DEF VAR ldeTo AS DEC NO-UNDO.

ASSIGN
   ldaDate = TODAY - 1
   ldeFrom = Func.Common:mHMS2TS(ldaDate, "00:00:00")
   ldeTo = Func.Common:mHMS2TS(ldaDate,"23:59:59").

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
    TITLE " Collecting " FRAME fColl.

OUTPUT STREAM sdump TO value (icFile).

ERROR_LOOP:
FOR EACH MNPProcess NO-LOCK WHERE
         MNPProcess.Brand = Syst.Var:gcBrand AND
         MNPProcess.MNPType = {&MNP_TYPE_IN} AND
         MNPProcess.UpdateTS >= ldeFrom AND
         MNPProcess.UpdateTS <= ldeTo AND
         MNPProcess.StatusCode = {&MNP_ST_ACAN} USE-INDEX UpdateTS,
   FIRST Order NO-LOCK WHERE
         Order.Brand = Syst.Var:gcBrand AND
         Order.OrderID = MNPProcess.OrderID:

   /* it's presumed that the latest memo is related to cancellation */
   FIND FIRST Memo NO-LOCK WHERE
              Memo.Brand = Syst.Var:gcBrand AND
              Memo.HostTable = "MNPProcess" AND
              Memo.KeyValue = STRING(MNPProcess.MNPSeq)
              USE-INDEX HostTable NO-ERROR.
      
   put stream sdump unformatted 
       Order.CLI "|"
       MNPProcess.PortRequest "|"
       MNPProcess.FormRequest "|"
      (IF AVAIL Memo THEN Memo.Source ELSE "") "|"
      (IF AVAIL Memo THEN Memo.CreUser ELSE "") "|"
      Func.Common:mTS2HMS(MNPProcess.UpdateTS) skip.
      
   oiEvents = oiEvents + 1.
   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents WITH FRAME fColl.
   END.

END.

OUTPUT STREAM sdump close.
HIDE FRAME fColl NO-PAUSE.
