/* ----------------------------------------------------------------------
  MODULE .......: pushinvoice.p 
  TASK .........: Handles push notification invoice request (type 94)
  APPLICATION ..: TMS
  AUTHOR .......: ivekov 
  CREATED ......: 01/2017 
  CHANGE .......: 
  Version ......: Yoigo
----------------------------------------------------------------------- */

{commali.i}
{tmsconst.i}
{fmakemsreq.i}
{cparam2.i}
{femailinvoice.i}
{email.i}
{smsnotify.i}
{heartbeat.i}
{log.i}

DEF INPUT PARAMETER iiMSRequest AS INT  NO-UNDO.

DEF VAR ldaDateFrom        AS DATE      NO-UNDO. 
DEF VAR liMonth            AS INTEGER   NO-UNDO. 
DEF VAR lcMessageMq        AS CHARACTER NO-UNDO.
DEF VAR lcLoginMq          AS CHARACTER NO-UNDO.
DEF VAR lcPassMq           AS CHARACTER NO-UNDO.
DEF VAR liPortMq           AS INTEGER   NO-UNDO.
DEF VAR lcServerMq         AS CHARACTER NO-UNDO.
DEF VAR liTimeOutMq        AS INTEGER   NO-UNDO.
DEF VAR lcQueueMq          AS CHARACTER NO-UNDO.
DEF VAR lcPushLogDir       AS CHARACTER NO-UNDO.
DEF VAR lcPushLogFile      AS CHARACTER NO-UNDO.
DEF VAR lcLogManagerFile   AS CHARACTER NO-UNDO.
DEF VAR liPushCount        AS INTEGER   NO-UNDO.
DEF VAR liLogLevel         AS INTEGER   NO-UNDO.
DEF VAR liLogTreshold      AS INTEGER   NO-UNDO.

DEF VAR lMsgPublisher AS CLASS Gwy.MqPublisher NO-UNDO.

DEF STREAM sEmail.
DEF STREAM sPushLog.

FIND MSRequest WHERE 
     MSRequest.MSRequest = iiMSRequest
NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR 
                 MsRequest.ReqType NE ({&REQTYPE_PUSH_INVOICE}) THEN 
   RETURN "ERROR".

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

ASSIGN 
   ldaDateFrom      = MsRequest.ReqDtParam1
   liMonth          = MONTH(ldaDateFrom)
   lcLoginMq        = fCParamC("InvPushMqLogin")
   lcPassMq         = fCParamC("InvPushMqPassCode")
   liPortMq         = fCParamI("InvPushMqPort")
   lcServerMq       = fCParamC("InvPushMqServer")
   liTimeOutMq      = fCParamI("InvPushMqTimeOut")
   lcQueueMq        = fCParamC("InvPushMqToQueue")
   lcPushLogDir     = fCParamC("InvPushLogDir")
   liLogLevel       = fCParamI("InvPushLogLevel")
   liLogTreshold    = fCParamI("InvPushLogTreshold").

FUNCTION fGetMessageMq RETURNS CHARACTER
   (icCLI AS CHAR):

   DEF VAR lcRequestId   AS CHAR NO-UNDO.
   DEF VAR lcMsg         AS CHAR NO-UNDO.

   lcRequestId = "101" + SUBSTRING(BASE64-ENCODE(GENERATE-UUID), 1, 22).

   IF INDEX(lcRequestId,"|") > 0 THEN
      lcRequestId = REPLACE(lcRequestId,"|","0").

   lcMsg = lcRequestId            + "|" +  /* 1  request_id           */
           "Invoice"              + "|" +  /* 2  message_category     */
           ""                     + "|" +  /* 3  sms_recipient        */
           ""                     + "|" +  /* 4  email_recipient      */
           icCLI                  + "|" +  /* 5  push_recipient       */
           "Invoice/Paper"        + "|" +  /* 6  message_template_id  */
           ""                     + "|" +  /* 7  message_body         */
           ""                     + "|" +  /* 8  message_inapp_link   */
           ""                     + "|" +  /* 9  message_type_id      */
           ""                     + "|" +  /* 10 scheduling_policy    */
           ""                     + "|" +  /* 11 product_id           */
           ""                     + "|" +  /* 12 first_name           */
           "".                             /* 13 last_name            */

   RETURN lcMsg.

END FUNCTION.

/* Push notification for paper invoices */
ASSIGN
   lcPushLogFile = lcPushLogDir + "InvPush_" + STRING(YEAR(TODAY),"9999") +
                                               STRING(MONTH(TODAY),"99")  +
                                               STRING(DAY(TODAY),"99")    +
                                               ".log"
   lcLogManagerFile = lcPushLogDir + "InvPus_LogManager_" + 
                                               STRING(YEAR(TODAY),"9999") +
                                               STRING(MONTH(TODAY),"99")  +
                                               STRING(DAY(TODAY),"99")    +
                                               ".log".
IF liLogLevel = 0 OR liLogLevel = ? THEN
   liLogLevel = 2. /* default */

IF lcLogManagerFile > "" THEN DO:
   fSetLogFileName(lcLogManagerFile).
   fSetGlobalLoggingLevel(liLogLevel).
   fSetLogTreshold(liLogTreshold).
END.

lMsgPublisher = NEW Gwy.MqPublisher(lcServerMq,liPortMq,
                                    liTimeOutMq,lcQueueMq,
                                    lcLoginMq,lcPassMq).

IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
   IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
      LOG-MANAGER:WRITE-MESSAGE("RabbitMQ Publisher handle not found","ERROR").
END.

OUTPUT STREAM sPushLog TO VALUE(lcPushLogFile).

PUSH_INVOICE_LOOP:
FOR EACH Invoice WHERE
         Invoice.Brand    = gcBrand AND
         Invoice.InvType  = 1 AND
         Invoice.InvDate >= ldaDateFrom AND
         Invoice.InvAmt  >= 0 NO-LOCK:

   IF Invoice.DelType <> {&INV_DEL_TYPE_PAPER} THEN NEXT PUSH_INVOICE_LOOP. 

   IF Invoice.InvCfg[1] THEN NEXT PUSH_INVOICE_LOOP.

   IF MONTH(Invoice.InvDate) NE liMonth THEN NEXT PUSH_INVOICE_LOOP.

   PUSH_SUBINVOICE_LOOP:
   FOR EACH SubInvoice OF Invoice NO-LOCK:
   
      FIND FIRST MobSub WHERE
                 MobSub.MsSeq = SubInvoice.MsSeq NO-LOCK NO-ERROR.
      IF NOT AVAIL MobSub OR 
         MobSub.CustNum NE Invoice.CustNum THEN NEXT PUSH_SUBINVOICE_LOOP.

      ASSIGN lcMessageMq = fGetMessageMq(MobSub.CLI)
             liPushCount = liPushCount + 1.

      IF lcMessageMq = ? THEN lcMessageMq = "".

      IF NOT lMsgPublisher:send_message(lcMessageMq) THEN DO:
         IF LOG-MANAGER:LOGFILE-NAME <> ? AND
            LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
            LOG-MANAGER:WRITE-MESSAGE("Message sending failed","ERROR").
      END.

      PUT STREAM sPushLog UNFORMATTED
         STRING(MobSub.MsSeq)     + "|" +
         STRING(Invoice.Custnum)  + "|" +
         STRING(Invoice.InvNum)   + "|" +
         MsRequest.UserCode       SKIP.

      IF liPushCount = 50000 THEN DO:
         PAUSE 120.
         liPushCount = 0.
      END.

   END. /* PUSH_SUBINVOICE_LOOP: */
END. /* PUSH_INVOICE_LOOP: */   

OUTPUT STREAM sPushLog CLOSE.
IF VALID-OBJECT(lMsgPublisher) THEN DELETE OBJECT(lMsgPublisher).

fReqStatus(2,""). /* request handled succesfully */

