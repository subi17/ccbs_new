/* ----------------------------------------------------------------------
  MODULE .......: icc_unblock_report.p
  TASK .........: Report for MNP POS SIM release. YDR-16 
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 05.10.2009
  Version ......: yoigo
----------------------------------------------------------------------- */
{commpaa.i}
katun = "Cron".
gcBrand = "1".

{tmsconst.i}
{ftransdir.i}
{cparam2.i}
{eventlog.i}
{date.i}

DEFINE VARIABLE idaDate AS DATE NO-UNDO. 
DEFINE VARIABLE lcParam AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSep AS CHARACTER NO-UNDO INIT "|".
DEFINE VARIABLE lcLogFile AS CHAR NO-UNDO. 
DEFINE VARIABLE lcSpoolDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldaOrderDate AS DATE NO-UNDO. 
DEFINE VARIABLE liOrderTime AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldeTimeFrom AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldeTimeTo AS DECIMAL NO-UNDO. 
DEF STREAM sLog.

lcParam = SESSION:PARAMETER.

IF lcParam NE "" THEN DO: 
  idaDate = DATE(lcParam) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO: 
     MESSAGE "Wrong date: " + lcParam.
     PAUSE 0.
     RETURN.
  END.
END.
ELSE idaDate = TODAY - 1.

ASSIGN
   lcSpoolDir = fCParam("SIMRelease","OutSpoolDir")
   lcOutDir   = fCParam("SIMRelease","OutDir").

ldeTimeFrom = fDate2TS(idaDate).
ldeTimeTo = fDate2TS(idaDate + 1).

lcLogFile = lcSpoolDir + "icc_unblock_" + 
            STRING(YEAR(idaDate),"9999") +
            STRING(MONTH(idaDate),"99") +
            STRING(DAY(idaDate),"99") + ".txt".
OUTPUT STREAM sLog TO VALUE(lcLogFile).

fBatchLog("START", lcLogFile).

FOR EACH ActionLog WHERE
   ActionLog.Brand = gcBrand AND
   ActionLog.ActionID = "SIMRELEASE" AND
   ActionLog.ActionTS >= ldeTimeFrom AND
   ActionLog.ActionTS < ldeTimeTo AND
   ActionLog.ActionStatus = 2 NO-LOCK:

   FIND Order WHERE
      Order.Brand = gcBrand AND
      Order.OrderId = INT(ActionLog.KeyValue) NO-LOCK NO-ERROR.
   IF NOT AVAIL Order THEN NEXT.

   FIND SIM WHERE
      SIM.ICC = ENTRY(1,ActionLog.ActionChar,"|") NO-LOCK NO-ERROR.
   IF NOT AVAIL SIM THEN NEXT.
   
   fSplitTS(Order.CrStamp, OUTPUT ldaOrderDate, OUTPUT liOrderTime).

   PUT STREAM sLog UNFORMATTED 
       ActionLog.KeyValue lcSep
       ENTRY(1,ActionLog.ActionChar,"|") lcSep
       STRING(ldaOrderDate,"99-99-9999") lcSep
       Order.Salesman lcSep 
       SIM.SimStat SKIP.
END.
   
lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
fBatchLog("FINISH", lcReportFileOut).

OUTPUT STREAM sLog CLOSE.
