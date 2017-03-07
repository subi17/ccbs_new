/* ----------------------------------------------------------------------
  MODULE .......: stc_balance_query_err.p
  TASK .........: Report failed prepaid to postpaid balance transfers. YOT-627
                  (2.x - 1.y, x = previous month, y = current month)
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 08/2010
  Version ......: yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "Cron".
gcbrand = "1".
{Func/timestamp.i}
{Func/cparam2.i}
{Func/email.i}

DEF VAR liMsRequest AS INT NO-UNDO. 
DEF VAR ldeFrom AS DEC NO-UNDO. 
DEF VAR ldeTo AS DEC NO-UNDO. 
DEF VAR ldaExecDate AS DATE NO-UNDO. 
DEF VAR liSecs AS INT NO-UNDO. 
DEF VAR lcReportDir AS CHAR NO-UNDO. 
DEF VAR lcFileName AS CHAR NO-UNDO. 
DEF VAR lcConfDir AS CHAR NO-UNDO. 
DEF VAR liCases AS INT NO-UNDO. 
DEF VAR lcReportFileOut AS CHAR NO-UNDO. 

DEF STREAM sout.

lcReportDir = fCParam("Reports", "stc_balance_query_err").
IF lcReportDir = ? OR lcReportDir = "" THEN RETURN.

lcConfDir = fCParamC("RepConfDir").
lcFileName = "stc_balance_query_err_" +
             STRING(YEAR(TODAY) * 100 + MONTH(TODAY)) + ".txt".

lcReportFileOut = lcReportDir + lcFileName.
OUTPUT STREAM sout TO VALUE(lcReportFileOut).

IF MONTH(TODAY) = 1 THEN ASSIGN
   ldeFrom = fMake2DT(DATE(12, 2, YEAR(TODAY) - 1), 0).
ELSE ASSIGN
   ldeFrom = fMake2DT(DATE(MONTH(TODAY) - 1, 2, YEAR(TODAY)), 0).

ldeTo = fMake2DT(DATE(MONTH(TODAY), 2, YEAR(TODAY)), 0).
   
FOR EACH ErrorLog WHERE
         ErrorLog.Brand = gcBrand AND
         ErrorLog.ActionID = "STCBalanceQuery" AND
         ErrorLog.ActionTS >= ldeFrom AND
         ErrorLog.ActionTS < ldeTo NO-LOCK:

   liMsRequest = INT(ErrorLog.KeyValue) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN NEXT.

   FIND MsRequest WHERE
        MsRequest.MsRequest = liMsRequest NO-LOCK NO-ERROR.
   IF NOT AVAIL MsRequest THEN NEXT.

   fSplitTS(ErrorLog.ActionTS, OUTPUT ldaExecDate, OUTPUT liSecs).

   PUT STREAM sout UNFORMATTED
      MsRequest.CLI ";"
      MsRequest.MsSeq ";"
      MsRequest.ReqCParam1 ";"
      MsRequest.ReqCParam2 ";"
      ldaExecDate skip.
                   
   liCases = liCases + 1.
END.

OUTPUT STREAM sout close.
   
IF liCases > 0 THEN DO:
   /* mail recipients */
   GetRecipients(lcConfDir + "stc_balance_query_err.email").
   /* send via mail */
   SendMail(lcReportFileOut,"").
END.
