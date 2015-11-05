/**
 * Send MNP Order Contract Id(s) by SMS or Email
 *
 * @input  transaction_id;string;mandatory;transaction id
           msisdn;string;mandatory;MSISDN for finding the corresponding orders
           delivery_type;string;mandatory;SMS/EMAIL
 * @output     struct;mandatory;response struct
 * @response   transaction_id;string;transaction id
               result;boolean;True
 * @exceptions  1;Application Id does not match
                2;Invalid Delivery Type
                3;Format of the MSISDN number is not correct
                4;MNP order does not exist with given MSISDN
                5;MNP order not available
                6;MNP order already cancelled
                7;Subscription is cancelled
                8;MNP order does not have email address
                9;Missing SMS Template
               10;Email sending is failed
 */

{xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.
{commpaa.i}
ASSIGN katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId
       gcBrand = "1".
{timestamp.i}
{tmsconst.i}
{forderstamp.i}
{fgettxt.i}
{fmakesms.i}
{fmakemsreq.i}
{fexternalapi.i}

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
DEFINE VARIABLE lcEmail                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE llOngoing               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llDelivered             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llClose                 AS LOGICAL   NO-UNDO.
DEF VAR lcApplicationId AS CHAR NO-UNDO. 

pcReqList = validate_request(param_toplevel_id, "string,string,string,[string]").
IF pcReqList EQ ? THEN RETURN.

ASSIGN pcTransId    = get_string(param_toplevel_id, "0")
       pcCLI        = get_string(param_toplevel_id, "1")
       pcDelType    = get_string(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcApplicationId = substring(pcTransId,1,3).

IF NOT fchkTMSCodeValues(gbAuthLog.UserName, lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").

katun = lcApplicationId + "_" + gbAuthLog.EndUserId.

IF LOOKUP(pcDelType,"EMAIL,SMS") = 0 THEN
   RETURN appl_err("Invalid Delivery Type").

IF LENGTH(pcCLI) <> 9 OR
   NOT (pcCLI BEGINS "6" OR pcCLI BEGINS "7") THEN
   RETURN appl_err("Format of the MSISDN number is not correct").

FOR EACH Order WHERE
         Order.CLI = pcCLI AND
         Order.MNPStatus > 0 NO-LOCK:

   IF liCount >= 3 THEN LEAVE.
   liTotalCount = liTotalCount + 1.

   fSplitTS(Order.CrStamp,ldOrderDate,liOrderTime).

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
   END. /* IF liCount = 1 THEN DO: */

   IF pcDelType = "EMAIL" THEN DO:
      IF lcEmail = "" THEN DO:
         FIND FIRST OrderCustomer WHERE
                    OrderCustomer.Brand   = gcBrand AND
                    OrderCustomer.OrderId = Order.OrderId AND
                    OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
         IF AVAIL OrderCustomer AND OrderCustomer.Email > "" THEN
            lcEmail = OrderCustomer.Email.
      END. /* IF lcEmail = "" THEN DO: */
      lcReplaceText = lcReplaceText +
                      (IF lcReplaceText > "" THEN CHR(10) ELSE "") +
                      "Número de Pedido: " + Order.ContractID + ", del " +
                      STRING(ldOrderDate) + ", del número " + Order.CLI + ".".
   END. /* IF pcDelType = "EMAIL" THEN DO: */
   ELSE
      lcReplaceText = lcReplaceText +
                      (IF lcReplaceText > "" THEN " - " ELSE "") +
                      Order.ContractID + ", del " + STRING(ldOrderDate) +
                      ", del número " + Order.CLI.

END. /* FOR EACH Order WHERE */

IF liTotalCount = 0 THEN
   RETURN appl_err("MNP order does not exist with given MSISDN").
ELSE IF liCount = 0 THEN
   RETURN appl_err("MNP order not available").
ELSE IF NOT llOngoing THEN DO:
   IF llClose THEN
      RETURN appl_err("MNP order already cancelled").
   ELSE IF llDelivered THEN DO:
      FIND FIRST MobSub WHERE
                 MobSub.Brand = gcBrand AND
                 MobSub.CLI   = pcCLI NO-LOCK NO-ERROR.
      IF NOT AVAIL MobSub THEN
         RETURN appl_err("Subscription is cancelled").
   END. /* ELSE IF llDelivered THEN DO: */
END. /* ELSE IF NOT llOngoing THEN DO: */

IF pcDelType = "EMAIL" AND lcEmail = "" THEN
   RETURN appl_err("MNP order does not have email address").

IF pcDelType = "SMS" THEN DO:
   lcSMSText = fGetSMSTxt((IF liCount > 1 THEN "GetMultiConsultID"
                           ELSE "GetConsultID"),
                           TODAY,
                           1,
                           OUTPUT ldeOrderStamp).
   IF lcSMSText = "" THEN
      RETURN appl_err("Missing SMS Template").

   lcSMSText = REPLACE(lcSMSText,"#INFO",lcReplaceText).

   fMakeSchedSMS2(0,
                  pcCLI,
                  9,
                  lcSMSText,
                  ldeOrderStamp,
                  "Yoigo info",
                  "").

END. /* IF pcDelType = "SMS" THEN DO: */
ELSE DO:
   liRequest = fEmailSendingRequest(INPUT fMakeTS(),
                                    INPUT katun,
                                    INPUT 0, /* custnum */
                                    INPUT pcCLI,
                                    INPUT lcEmail,
                                    INPUT (IF liCount > 1 THEN "GetMultiConsultID"
                                           ELSE "GetConsultID"),
                                    INPUT lcReplaceText,
                                    INPUT 0,
                                    INPUT {&REQUEST_SOURCE_EXTERNAL_API},
                                    OUTPUT lcResult).
   IF liRequest = 0 THEN
      RETURN appl_err("Email sending is failed").
END. /* ELSE DO: */

/* add values to the response if no error */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct,"transaction_id",pcTransId).
add_boolean(top_struct,"result",True).

FINALLY:
   /* Store the transaction id */
   gbAuthLog.TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
