/* ----------------------------------------------------------------------
  module .......: Mm/cancel_pending_icc_req.p
  task .........: Cancel pending ICC change request that has been in
                  pending queue more than 60 days
  application ..: tms
  author .......: vikas
  created ......: 12.10.12
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/msreqfunc.i}
{Syst/eventval.i}

DEF VAR liConfDays       AS INT  NO-UNDO.
DEF VAR ldMsActDate      AS DATE NO-UNDO.
DEF VAR liMsActTime      AS INT  NO-UNDO.

liConfDays = fCParamI("WaitingCancelICCDays").
IF liConfDays = 0 OR liConfDays = ? THEN liConfDays = 60.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhSIM AS HANDLE NO-UNDO.
   lhSIM = BUFFER SIM:HANDLE.
   RUN StarEventInitialize(lhSIM).
END.

FOR EACH MsRequest WHERE
         MsRequest.Brand      = gcBrand AND
         MsRequest.ReqType    = {&REQTYPE_ICC_CHANGE} AND
         MsRequest.ReqStatus  = {&REQUEST_STATUS_CONFIRMATION_PENDING} NO-LOCK:

    fSplitTS(MsRequest.ActStamp,ldMsActDate,liMsActTime).
    IF liConfDays >= (TODAY - ldMsActDate) THEN NEXT.

    /* Cancel pending ICC request */
    fReqStatus(4,"Cancel " + STRING(liConfDays) + " days old pending ICC change request").

    /* Mark ICC to Lost status */
    FIND FIRST SIM WHERE
               SIM.ICC  = MsRequest.ReqCparam2 EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL SIM THEN DO:
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSIM).
       Sim.SimStat = 14.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSIM).
    END. /* IF AVAIL SIM THEN DO: */
END. /* FOR EACH MsRequest WHERE */

fCleanEventObjects().
