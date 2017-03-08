/* ----------------------------------------------------------------------
  Module .......: migration.i.i
  Task .........: Functions that are used in migration related needs
  Application ..: TMS
  Author .......: ilsavola
  Created ......: 10.2.2017
  Version ......: Yoigo
---------------------------------------------------------------------- */
&IF "{&migrationi}" NE "YES"
&THEN
&GLOBAL-DEFINE migrationi YES
{Syst/commali.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/fmakemsreq.i}
{Func/orderfunc.i}
{log.i}
/*Message queue related global variables.*/
DEF VAR lcLoginMq          AS CHAR NO-UNDO.
DEF VAR lcPassMq           AS CHAR NO-UNDO.
DEF VAR liPortMq           AS INTEGER   NO-UNDO.
DEF VAR lcServerMq         AS CHAR NO-UNDO.
DEF VAR liTimeOutMq        AS INTEGER   NO-UNDO.
DEF VAR lcQueueMq          AS CHAR NO-UNDO.
DEF VAR liLogLevel         AS INT NO-UNDO.
DEF VAR lcLogManagerFile   AS CHAR NO-UNDO.
DEF VAR lcMigrationLogDir  AS CHAR NO-UNDO.
DEF VAR liLogTreshold      AS IN  NO-UNDO.
DEF VAR lMsgPublisher AS CLASS Gwy.MqPublisher NO-UNDO.


/*Temp table for sending NC response info to WEB.*/
DEF TEMP-TABLE NCResponse NO-UNDO
   FIELD Order AS CHAR
   FIELD MSISDN AS CHAR
   FIELD StatusCode AS CHAR
   FIELD Comment AS CHAR.

DEF TEMP-TABLE OrderInfo NO-UNDO
   FIELD Order AS CHAR
   FIELD MSISDN AS CHAR
   FIELD StatusCode AS CHAR.

{Func/freacmobsub.i}
/*Function checks that customer given:
   -does not have active subscrition
   -does not have subscription that can be reactivated
   RETURN:
   "" OK
   Error text in failure cases
*/
FUNCTION fMigrationCheckCustomer RETURNS CHAR
   (INPUT icBrand AS CHAR,
    INPUT icCustID AS CHAR):
   DEF BUFFER Customer FOR Customer.
   DEF BUFFER MobSub FOR MobSub.
   DEF BUFFER TermMobSub FOR TermMobSub.
   DEF BUFFER Order FOR Order.
   DEF VAR lcResult AS CHAR NO-UNDO.
  
   FOR EACH Customer NO-LOCK WHERE
            Customer.Brand EQ icBrand AND
            Customer.OrgID EQ icCustID:
      FIND FIRST MobSub NO-LOCK WHERE
                 MobSub.Brand EQ icBrand AND
                 MobSub.CustNum EQ Customer.CustNum.
      IF AVAIL MobSub THEN DO:         
         /*TODO Ilkka seek order and ensure that this is not old one.*/
         FOR EACH Order NO-LOCK WHERE 
                  Order.MsSeq EQ MsSeq:
            /* IF NOT fMigrationTariff THEN TODO implement function*/
               RETURN "ERROR: Mobsub for the customer exists".
         END.         
         
      END.
      FOR EACH TermMobSub NO-LOCK WHERE
               TermMobSub.Brand EQ icBrand AND
               TermMobSub.CustNum EQ Customer.Custnum:
         lcResult = freacprecheck(TermMobSub.MsSeq,
                                  "username",
                                  FALSE). 
         if lcResult NE "" THEN RETURN lcResult.
      END.

   END.

RETURN "".
END.


FUNCTION fIsNumberInMigration RETURNS LOGICAL
   (icCLI AS CHAR):
   DEF BUFFER Order FOR Order.
   FIND FIRST Order NO-LOCK WHERE
              Order.CLI EQ icCLI AND
              (Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_PENDING} OR /*60*/
               Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_ONGOING}).  /*61*/
   IF AVAIL Order THEN RETURN TRUE.

   RETURN FALSE.

END.   

/*Function creates Json entry containing Nodo Central response related data*/
FUNCTION fGenerateNCResponseInfo RETURNS CHAR
   (iiOrderID AS INT,
    icMSISDN AS CHAR,
    icStatusCode AS CHAR,
    icComment AS CHAR):
   DEF VAR lcTargetType AS CHAR NO-UNDO.
   DEF VAR llcMessage  AS LONGCHAR NO-UNDO.
   DEF VAR llgOK AS LOGICAL NO-UNDO.


   CREATE NCResponse.
   ASSIGN
      NCResponse.Order = STRING(iiOrderID)
      NCResponse.MSISDN = icMSISDN
      NCResponse.Statuscode = icStatusCode
      NCResponse.Comment = icComment.
   llgOK = TEMP-TABLE NCResponse:WRITE-JSON("LONGCHAR", /*writing type*/
                                             llcMessage, /*target*/
                                             TRUE). /*formatted to readabale*/

   EMPTY TEMP-TABLE NCResponse.
   IF llgOK EQ TRUE THEN RETURN STRING( llcMessage  ).


RETURN "".
END.


/*Function creates Json entry containing Order status information*/
FUNCTION fGenerateOrderInfo RETURNS CHAR
   (iiOrderID AS INT,
    icMSISDN AS CHAR,
    icStatusCode AS CHAR):
   DEF VAR lcTargetType AS CHAR NO-UNDO.
   DEF VAR llcMessage  AS LONGCHAR NO-UNDO.
   DEF VAR llgOK AS LOGICAL NO-UNDO.

  CREATE OrderInfo.
   ASSIGN
      OrderInfo.Order = STRING(iiOrderID)
      OrderInfo.MSISDN = icMSISDN
      OrderInfo.Statuscode = icStatusCode
   llgOK = TEMP-TABLE OrderInfo:WRITE-JSON("LONGCHAR", /*writing type*/
                                             llcMessage, /*target*/
                                             TRUE). /*formatted to readabale*/

   EMPTY TEMP-TABLE NCResponse.
   IF llgOK EQ TRUE THEN RETURN STRING( llcMessage  ).


RETURN "".
END.   

/*Function creates Json entry containing Operational data  status information*/
FUNCTION fGenerateOPDataInfo RETURNS CHAR
   (iiOrderID AS INT,
    icMSISDN AS CHAR,
    icStatusCode AS CHAR, 
    icComment AS CHAR):
   DEF VAR lcTargetType AS CHAR NO-UNDO.
   DEF VAR llcMessage  AS LONGCHAR NO-UNDO.
   DEF VAR llgOK AS LOGICAL NO-UNDO.

  CREATE NCResponse.
   ASSIGN
      NCResponse.Order = STRING(iiOrderID)
      NCResponse.MSISDN = icMSISDN
      NCResponse.Statuscode = icStatusCode
      NCResponse.Comment = icComment.
   llgOK = TEMP-TABLE NCResponse:WRITE-JSON("LONGCHAR", /*writing type*/
                                             llcMessage, /*target*/
                                             TRUE). /*formatted to readabale*/

   EMPTY TEMP-TABLE NCResponse.
   IF llgOK EQ TRUE THEN RETURN STRING( llcMessage  ).


RETURN "".
END.   

FUNCTION fCreateMigrationSub RETURNS CHAR
   (iiOrderID AS INT,
    ilgChangeSim AS LOGICAL):
   DEF BUFFER Order FOR Order.
   DEF VAR ocResult AS CHAR NO-UNDO.
   DEF VAR ldeSwitchTS AS DECIMAL NO-UNDO.
   DEF VAR lcCreateOption AS CHAR NO-UNDO.
   DEF VAR llOrdStChg AS LOGICAL NO-UNDO.
   FIND FIRST Order NO-LOCK WHERE
              Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_ONGOING}.
   IF NOT AVAIL Order THEN RETURN "Migration order not found " +
                                   STRING(iiOrderID).           
   

   fSubscriptionRequest(INPUT  Order.MSSeq,
                        INPUT  Order.Cli,
                        INPUT  Order.CustNum,
                        INPUT  1,
                        INPUT  katun,
                        INPUT  ldeSwitchTS,
                        INPUT  "CREATE",
                        INPUT  STRING(Order.OrderId),
                        INPUT  "", /*for old SIM*/
                        INPUT  "", /*for Reason info*/
                        INPUT  "", /*for ContractID*/
                        INPUT  FALSE,
                        INPUT  0,
                        INPUT  {&REQUEST_SOURCE_NEWTON},
                        OUTPUT ocResult).   
   IF ocResult > "" THEN DO:
      llOrdStChg = fSetOrderStatus(Order.OrderId,"2").
      RETURN "ERROR: Subscription creation failed, order " +
             STRING(Order.OrderID).
   END.
/*TODO*/
   

RETURN "".
END.   


/*Migration message queuearation related functions and procedures*/
FUNCTION fInitMigrationMQ RETURNS CHAR
   (icLogIdentifier AS CHAR): /*migration event type: NC response,...*/

ASSIGN
   lcLoginMq        = fCParamC("MigrationMqLogin")
   lcPassMq         = fCParamC("MigrationMqPassCode")
   liPortMq         = fCParamI("MigrationMqPort")
   lcServerMq       = fCParamC("MigrationMqServer")
   liTimeOutMq      = fCParamI("MigrationMqTimeOut")
   liLogTreshold    = fCParamI("InvPushLogTreshold")
   lcQueueMq        = fCParamC("MigrationToQueue") 
   lcLogManagerFile = lcMigrationLogDir + "Migration_LogManager_" +
                                               icLogIdentifier + "_" +
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
   IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
         LOG-MANAGER:WRITE-MESSAGE("RabbitMQPub handle not found","ERROR").
      RETURN "MQ creation failed".
   END.

   RETURN "".
END.

FUNCTION fWriteToMQ RETURNS CHAR
   (icMessage AS CHAR):
   DEF VAR lcError AS CHAR NO-UNDO.

   IF NOT VALID-OBJECT(lMsgPublisher) THEN DO:
      lcError = "MQ Publisher handle not found".
      IF LOG-MANAGER:LOGGING-LEVEL GE 1 THEN 
         LOG-MANAGER:WRITE-MESSAGE(lcError,"ERROR").
      RETURN lcError.
   END.

   IF NOT lMsgPublisher:send_message(icMessage) THEN DO:
      lcError = "Message sending failed".
      IF LOG-MANAGER:LOGFILE-NAME <> ? AND
      LOG-MANAGER:LOGGING-LEVEL GE 1 THEN
         LOG-MANAGER:WRITE-MESSAGE(lcError,"ERROR").
      RETURN lcError.
   END.


   RETURN "".
END.   

FUNCTION fFinalizeMQ RETURNS CHAR
   ():
   IF VALID-OBJECT(lMsgPublisher) THEN DELETE OBJECT(lMsgPublisher).
END.


&ENDIF

