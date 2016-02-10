/* ----------------------------------------------------------------------
  module .......: Mm/cancel_waiting_einvoice_act_req.p
  task .........: Cancel eInvoice activation request that has been in
                  waiting queue more than 30 days
                  YOT-2591
  application ..: tms
  author .......: vikas
  created ......: 11.04.12
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "Cron".
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/msreqfunc.i}
{Syst/eventval.i}

DEF VAR liConfDays       AS INT  NO-UNDO.
DEF VAR ldMsActDate      AS DATE NO-UNDO.
DEF VAR liMsActTime      AS INT  NO-UNDO.
DEF VAR lhCustomer     AS HANDLE NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {Func/lib/eventlog.i}
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).
END. /* IF llDoEvent THEN DO: */

liConfDays = fCParamI("WaitingCanceleInvoiceDays").
IF liConfDays = 0 OR liConfDays = ? THEN liConfDays = 90.

FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand      = gcBrand AND
         MsRequest.ReqType    = {&REQTYPE_ACTIVATE_EMAIL_INVOICE} AND
         MsRequest.ReqStatus  = {&REQUEST_STATUS_CONFIRMATION_PENDING}:

    fSplitTS(MsRequest.UpdateStamp,ldMsActDate,liMsActTime).
    IF liConfDays >= (TODAY - ldMsActDate) THEN NEXT.

    FIND Customer NO-LOCK WHERE
         Customer.Custnum  = MsRequest.Custnum NO-ERROR.

    IF AVAIL Customer AND
             Customer.DelType EQ {&INV_DEL_TYPE_EMAIL_PENDING} THEN DO:
      FIND CURRENT Customer EXCLUSIVE-LOCK NO-ERROR.
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer).
      Customer.DelType = {&INV_DEL_TYPE_SMS}.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustomer).
      RELEASE Customer.
    END.
    
    /* Cancel eInvoice Activation request */
    fReqStatus(4,"Activation link for eInvoice is not valid after " +
               STRING(liConfDays) + " days.").

END. /* FOR EACH MsRequest WHERE */

IF llDoEvent THEN fCleanEventObjects().
