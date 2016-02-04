/* ----------------------------------------------------------------------
  MODULE .......: postpaid_chgcomp.p
  TASK .........: Changes and Compensation Handler for PostPaid
  APPLICATION ..: 
  AUTHOR .......: rafaeldv
  CREATED ......: 
  CHANGED ......: 
                  
  Version ......: 
  ---------------------------------------------------------------------- */

{Func/timestamp.i}
{Syst/commali.i}
{Func/msreqfunc.i}
{Syst/tmsconst.i}

DEF INPUT  PARAMETER  iiMSrequest AS INT  NO-UNDO.

DEF VAR lcResponse AS CHAR NO-UNDO.
DEF VAR lDateCr    AS DATE NO-UNDO.
DEF VAR lTimeCr    AS INTEGER NO-UNDO.
DEF VAR lcResp AS CHARACTER NO-UNDO.
DEF VAR ldCharge AS DECIMAL NO-UNDO. 
DEF VAR lcBillEvent AS CHARACTER NO-UNDO. 

FIND MSRequest WHERE
     MSRequest.MSRequest = iiMSRequest  AND
     MSRequest.ReqType = {&REQTYPE_CHARGE_AND_COMPENSATION}  AND
     MSRequest.ReqStatus = {&REQUEST_STATUS_NEW} NO-LOCK NO-ERROR.

IF NOT AVAIL MSRequest  THEN DO:
   RETURN "ERROR:Unknown MSRequest " + STRING(iiMSRequest).
END.

 /* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

ASSIGN
   lcBillEvent = MSRequest.ReqCParam1
   ldCharge = MSRequest.ReqDParam1.

FIND MobSub WHERE 
     MobSub.MsSeq = MSRequest.MsSeq NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN DO:
   fReqError("Unknow MobSub: " + STRING(MSRequest.MsSeq)).
   RETURN.
END.

IF MobSub.PayType THEN DO:
   fReqError("MobSub " + STRING(MobSub.MsSeq) + " is PrePaid").
   RETURN.
END.
                                                
/* general fees */
IF lcBillEvent = "" THEN DO:
   fReqError("Not input billing event value ").
   RETURN.
END.
 
fSplitTS(MsRequest.CreStamp, OUTPUT lDateCr, OUTPUT lTimeCr).

RUN Mc/creasfee.p(MobSub.CustNum,
              MobSub.MsSeq,
              lDateCr,
              "FeeModel",         /* FeeModel where using CHARGE */
              lcBillEvent,     /* FeeModel.FeeModel  where using ServFee.ServFeekey  */
              9,                /* 9 where using 1*/
              ldCharge,
              "",
              FALSE,
              MsRequest.UserCode,
              "C&C",
              0,
              "",
              "",
              OUTPUT lcResp).

IF lcResp BEGINS "ERROR" THEN DO:
   fReqError("Create Fee: " + lcResp).
   RETURN.
END.
ELSE fReqStatus( {&REQUEST_STATUS_DONE} ,"").
