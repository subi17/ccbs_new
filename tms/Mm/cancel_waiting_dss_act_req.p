/* ----------------------------------------------------------------------
  module .......: Mm/cancel_waiting_dss_act_req.p
  task .........: Cancel DSS activation request that has been in waiting
                  queue more than 14 days
  application ..: tms
  author .......: vikas
  created ......: 16.01.12
  version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{timestamp.i}
{cparam2.i}
{tmsconst.i}
{fsendsms.i}
{msreqfunc.i}

DEF VAR liConfDays       AS INT  NO-UNDO.
DEF VAR ldMsActDate      AS DATE NO-UNDO.
DEF VAR liMsActTime      AS INT  NO-UNDO.

liConfDays = fCParamI("WaitingCancelDSSDays").
IF liConfDays = 0 OR liConfDays = ? THEN liConfDays = 14.

FOR EACH MsRequest WHERE
         MsRequest.Brand      = gcBrand AND
         MsRequest.ReqType    = {&REQTYPE_DSS} AND
         MsRequest.ReqStatus  = 19 AND
         MsRequest.ReqCParam1 = "CREATE" NO-LOCK:

    fSplitTS(MsRequest.ActStamp,ldMsActDate,liMsActTime).
    IF liConfDays >= (TODAY - ldMsActDate) THEN NEXT.

    /* Cancel DSS request */
    fReqStatus(4,"dss_criteria_not_matched").

    /* If subs. is active then only send a SMS */
    RUN pSendSMS(INPUT MsRequest.MsSeq, INPUT 0, INPUT "DSSActCancel",
                 INPUT 9, INPUT "622", INPUT "").

END. /* FOR EACH MsRequest WHERE */

