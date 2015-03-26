/* ----------------------------------------------------------------------
  MODULE .......: bankrequest.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: 
  CREATED ......: 
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{msreqfunc.i}
{fmakemsreq.i}
{tmsconst.i}
{eventval.i}

DEFINE INPUT PARAMETER iiReqId AS INTEGER   NO-UNDO.

DEFINE BUFFER agrCust FOR Customer.
DEFINE BUFFER bMsRequest FOR MsRequest.

DEF VAR ocResult      AS CHAR NO-UNDO.

FIND MsRequest WHERE 
     MsRequest.MsRequest = iiReqId AND
     MsRequest.Brand     = gcBrand
NO-LOCK NO-ERROR.

IF NOT AVAIL MsRequest THEN RETURN "ERROR, request lost!".

IF MsRequest.ReqType NE 24 OR
   (MSRequest.ReqStatus NE 0 AND MsRequest.Reqstatus NE 15) THEN RETURN.

/* request is under work */
IF NOT fReqStatus(1,"") THEN DO:
   RETURN "ERROR".
END.

FIND Customer WHERE
     Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.

IF NOT AVAIL Customer THEN DO:
   fReqStatus(3,"Customer not found").
   RETURN.
END.
            
FIND AgrCust WHERE
     AgrCust.CustNum = Customer.CustNum EXCLUSIVE-LOCK NO-ERROR.

IF NOT AVAIL agrCust THEN DO:
   fReqStatus(3,"Agreement customer not found").
   RETURN.
END.

CREATE memo.
ASSIGN
memo.CreStamp  = fMakeTS()
memo.MemoSeq   = NEXT-VALUE(MemoSeq)
Memo.Custnum   = AgrCust.CustNum
memo.HostTable = "Customer"
memo.KeyValue  = STRING(AgrCust.CustNum)
memo.CreUser   = katun
memo.MemoTitle = "NEW BANK ACCOUNT NUMBER"
Memo.memotext  = "RequestID" + STRING(msrequest.msrequest) + 
                 " Bank account update:" + msrequest.ReqCparam1.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}
   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER AgrCust:HANDLE.
   RUN StarEventInitialize(lhCustomer).
   RUN StarEventSetOldBuffer(lhCustomer).
END.

AgrCust.BankAcc = MSREquest.ReqCparam2.

IF llDoEvent THEN DO:
   RUN StarEventMakeModifyEvent(lhCustomer).
   fCleanEventObjects().
END.

RELEASE AgrCust.

/* Create possible Credit Check request */
FIND bMsRequest NO-LOCK WHERE
     bMsRequest.MsRequest = MsRequest.OrigRequest NO-ERROR.

/* Do not perform post credit check with STCs originating from web. YDR-323 */
IF AVAIL bMsRequest AND
         bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
         bMsrequest.ReqIParam1 = 1 /* credit check */ AND
         bMsRequest.ReqSource NE {&REQUEST_SOURCE_NEWTON} AND
         bMsRequest.ReqSource NE {&REQUEST_SOURCE_FUSION_ORDER}
THEN DO:
   
   fSubRequest
   (INPUT  bMSRequest.MSSeq,
           bMSRequest.CLI,
           bMSRequest.CustNum,
           FALSE,               /* Fees */
           FALSE,               /* SendSMS */
           bMSRequest.UserCode,
           fMakeTS(),
           "CREDITCHECK",
           "",
           33,
           bMSRequest.MSrequest,
           1,                   /* 1 = MAndatory */
    OUTPUT ocResult ).
END.

fReqStatus(2,""). /* bank request */
