/*-----------------------------------------------------------------------------
  MODULE .......: smsrequest.p
  FUNCTION .....: handle requests for sms creation
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 12.06.08
  Version ......: M15
  -------------------------------------------------------------------------- */


{Func/msreqfunc.i}
{Func/fgettxt.i}
{Func/fmakesms.i}


DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 30 THEN 
   RETURN "ERROR:Unknown request".

RUN pCreateSMS.

RETURN "". 


/* create sms to callalarm queue */
PROCEDURE pCreateSMS:

   DEF VAR liCreditType  AS INT  NO-UNDO.

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   /* this check should be futile, but who knows .. */
   IF MsRequest.SendSMS NE 1 THEN DO:
      fReqError("SMS sending not allowed").
      RETURN.
   END.
   
   FIND FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE
              MsOwner.MsSeq  = MsRequest.MsSeq AND
              MsOwner.TsEnd >= 99999999 NO-ERROR.
   IF NOT AVAILABLE MsOwner THEN 
   FIND FIRST MsOwner NO-LOCK WHERE
              MsOwner.MsSeq  = MsRequest.MsSeq NO-ERROR.
      
   IF NOT AVAILABLE MsOwner THEN DO:
      fReqError("Subscription not found").
      RETURN.
   END.

   FIND Customer WHERE Customer.CustNum = MsOwner.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   fSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).

   lcSMSText = "".

   /* ReqCParam1 = message source:
      - 'free' = ReqCParam2 contains a ready text
      - empty or 'invtext' = get the text from InvText using SMSText as
        InvText.KeyValue
   */

   IF MsRequest.ReqCParam1 = "free" THEN 
      lcSMSText = MsRequest.ReqCParam2.
    
   ELSE IF MsRequest.SMSText > "" THEN
      lcSMSText = fGetSMSTxt(MsRequest.SMSText,
                             TODAY,
                             Customer.Language,
                             OUTPUT ldReqStamp).

   IF lcSMSText = "" THEN DO:                    
      fReqError("Message text not found").
      RETURN.
   END.
   
   ASSIGN 
      lcSMSText = REPLACE(lcSMSText,"#MSISDN",MsRequest.CLI)
      lcSMSText = REPLACE(lcSMSText,"#DATE",STRING(ldtActDate,"99.99.9999")).
              
   /* message type, use 'info' if not given */ 
   IF MsRequest.ReqIParam1 > 0 
   THEN liCreditType = MsRequest.ReqIParam1.
   ELSE liCreditType = 9.
   
   fMakeSchedSMS2(MsOwner.CustNum,
                 MsRequest.CLI,
                 liCreditType,
                 lcSMSText,
                 ldReqStamp,
                 MsRequest.ReqCParam3,
                 "").

   IF AVAILABLE CallAlarm THEN RELEASE CallAlarm.
    
   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.


