/**
 * Send Renewal Order Contract Id(s) by SMS 
 *
 * @input  transaction_id;string;mandatory;transaction id
           msisdn;string;mandatory;MSISDN for finding the corresponding orders
           delivery_type;string;mandatory;SMS
 * @output     struct;mandatory;response struct
 * @response   transaction_id;string;transaction id
               result;boolean;True
 * @exceptions  1;Application Id does not match
                2;Invalid Delivery Type
                3;Format of the MSISDN number is not correct
                4;Renewal order does not exist with given MSISDN
                5;Renewal order not available
                6;Renewal order already cancelled
                7;Subscription is cancelled
                8;Missing SMS Template
                9;Email sending is failed
                10.No cusomer for order

 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
ASSIGN katun = ghAuthLog::UserName + "_" + ghAuthLog::EndUserId
       gcBrand = "1".
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/forderstamp.i}
{Func/fgettxt.i}
{Func/fmakesms.i}
{Func/smsmessage.i}
{Func/fmakemsreq.i}
{Func/fexternalapi.i}

DEFINE VARIABLE pcTransId               AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcCLI                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcDelType               AS CHARACTER NO-UNDO.
DEFINE VARIABLE top_struct              AS CHARACTER NO-UNDO.

DEFINE VARIABLE pcReqList               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcReplaceText           AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCount                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE liTotalCount            AS INTEGER   NO-UNDO.
DEFINE VARIABLE liOrderTime             AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldOrderDate             AS DATE      NO-UNDO.
DEFINE VARIABLE liFinalOrderTime        AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldFinalOrderDate        AS DATE      NO-UNDO.
DEFINE VARIABLE ldeOrderStamp           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE liRequest               AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcResult                AS CHARACTER NO-UNDO.
DEFINE VARIABLE llOngoing               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llDelivered             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llClose                 AS LOGICAL   NO-UNDO.
DEF VAR lcApplicationId AS CHAR NO-UNDO. 
DEFINE VARIABLE liCustNum               AS INTEGER   NO-UNDO.
DEFINE VARIABLE liMsSeq                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE liOrderId               AS INTEGER   NO-UNDO.


pcReqList = validate_request(param_toplevel_id, "string,string,string,[string]").
IF pcReqList EQ ? THEN RETURN.

ASSIGN pcTransId    = get_string(param_toplevel_id, "0")
       pcCLI        = get_string(param_toplevel_id, "1")
       pcDelType    = get_string(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcApplicationId = substring(pcTransId,1,3).

IF NOT fchkTMSCodeValues(ghAuthLog::UserName, lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").

katun = lcApplicationId + "_" + ghAuthLog::EndUserId.

IF LOOKUP(pcDelType,"SMS") = 0 THEN
   RETURN appl_err("Invalid Delivery Type").

/* Renewal should be allowed only for mobile subscriptions */ 
IF LENGTH(pcCLI) <> 9 OR
   NOT (pcCLI BEGINS "6" OR pcCLI BEGINS "7") THEN
   /*Incorrect format*/
   RETURN appl_err("Format of the MSISDN number is not correct").

FOR EACH Order WHERE
         Order.CLI = pcCLI AND
         Order.OrderType EQ {&ORDER_TYPE_RENEWAL} NO-LOCK:

   IF liCount >= 3 THEN LEAVE.
   liTotalCount = liTotalCount + 1.

   fSplitTS(Order.CrStamp,ldOrderDate,liOrderTime).

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand EQ gcBrand AND
              Ordercustomer.OrderID EQ Order.OrderId AND
              OrderCustomer.Rowtype EQ 1 NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN 
      RETURN appl_err("No customer for order").

   IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0 THEN DO:
      IF Order.StatusCode = {&ORDER_STATUS_DELIVERED} THEN
         ldeOrderStamp = fGetOrderStamp(Order.OrderId,"Delivery").
      ELSE
         ldeOrderStamp = fGetOrderStamp(Order.OrderId,"Close").

      fSplitTS(ldeOrderStamp,ldFinalOrderDate,liFinalOrderTime).

      IF ldFinalOrderDate <> ? THEN DO:
         IF ldFinalOrderDate < (TODAY - 30) THEN NEXT.
      END. /* IF ldFinalOrderDate <> ? THEN DO: */
      ELSE IF ldOrderDate < (TODAY - 30) THEN NEXT.
   END. /* IF LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0 THEN DO: */
   ELSE llOngoing = TRUE.

   liCount = liCount + 1.

   IF liCount = 1 THEN DO:
      IF Order.StatusCode = "6" THEN llDelivered = TRUE.
      ELSE IF LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 THEN llClose = TRUE.
      liMsSeq = Order.MsSeq.
      liOrderId = Order.OrderID.
      liCustNum = OrderCustomer.Custnum.
   END. /* IF liCount = 1 THEN DO: */

   lcReplaceText = lcReplaceText +
                   (IF lcReplaceText > "" THEN " - " ELSE "") +
                   Order.ContractID + ", del " + STRING(ldOrderDate) +
                   ", del número " + Order.CLI.

END. /* FOR EACH Order WHERE */

IF liTotalCount = 0 THEN
    RETURN appl_err("Renewal order does not exist with given MSISDN"). 
ELSE IF liCount = 0 THEN
   RETURN appl_err("Renewal order not available"). 
ELSE IF NOT llOngoing THEN DO:
   IF llClose THEN
      RETURN appl_err("Renewal order already cancelled").
   ELSE IF llDelivered THEN DO:
      FIND FIRST MobSub WHERE
                 MobSub.Brand = gcBrand AND
                 MobSub.CLI   = pcCLI NO-LOCK NO-ERROR.
      IF NOT AVAIL MobSub THEN
         RETURN appl_err("Subscription is cancelled"). 
   END. /* ELSE IF llDelivered THEN DO: */
END. /* ELSE IF NOT llOngoing THEN DO: */

IF pcDelType = "SMS" THEN DO:
   lcSMSText = fGetSMSTxt((IF liCount > 1 THEN "GetMultiConsultID"
                           ELSE "GetConsultID"),
                           TODAY,
                           1,
                           OUTPUT ldeOrderStamp).
   IF lcSMSText = "" THEN
      RETURN appl_err("Missing SMS Template").

   lcSMSText = REPLACE(lcSMSText,"#INFO",lcReplaceText).
   
   fCreateSMS(liCustnum,
              pcCLI,
              liMsSeq,
              liOrderId,
              lcSMSText,
              "Yoigo info",
              {&SMS_TYPE_CONSULT}).

END. /* IF pcDelType = "SMS" THEN DO: */

/* add values to the response if no error */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct,"transaction_id",pcTransId).
add_boolean(top_struct,"result",True).

FINALLY:
   /* Store the transaction id */
   ghAuthLog::TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
