
/* ----------------------------------------------------------------------
  MODULE .......: **_FILENAME_**
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 19.03.10
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{commpaa.i}
katun  = "anttis".
gcBrand = "1".
{tmsconst.i}
{msreqfunc.i}
{orderfunc.i}

find mnpprocess where
   mnpprocess.portrequest = "00500111100512114906719" EXCLUSIVE-LOCK. 

find order where
   order.brand = "1" and
   order.orderid = mnpprocess.orderid EXCLUSIVE-LOCK.
         
/* ACAN cannot come after porting */ 
FIND FIRST MsRequest WHERE
           MsRequest.MsSeq     = Order.MsSeq AND
           MsRequest.ReqType   = ({&REQTYPE_SUBSCRIPTION_CREATE}) AND
           MsRequest.ReqStatus = ({&REQUEST_STATUS_NEW})
NO-LOCK NO-ERROR.
IF AVAIL MsRequest THEN DO:
   fReqStatus(4,"Cancelled MNP Process").
END.

/* Cancel pending SMS messages */
FOR EACH CallAlarm WHERE
         CallAlarm.Brand = gcBrand AND
         CallAlarm.CLI = Order.CLI AND
         CallAlarm.DeliStat = 1 AND
         CallAlarm.CreditType = 12 EXCLUSIVE-LOCK:
    CallAlarm.DeliStat = 4. /* CANCELLED */
END.
         
fSetOrderStatus(Order.OrderId,"7").

ASSIGN         
   MNPProcess.UpdateTS = fMakeTS()
   MNPProcess.StatusCode = {&MNP_ST_ACAN}
   MNPProcess.statusreason = "CANC_ABONA".
   Order.MNPStatus = MNPProcess.StatusCode + 1.
