/**
 * Send New Order Contract Id(s) by SMS or Email
 *
 * @input  transaction_id;string;mandatory;transaction id
           dni_type;string;mandatory;order dni type
           dni;string;mandatory;order dni value
           delivery_type;string;mandatory;SMS/EMAIL
 * @output     struct;mandatory;response struct
 * @response   transaction_id;string;transaction id
               result;boolean;True
 * @exceptions  1;Application Id does not match
                2;Invalid Delivery Type
                3;Order does not exist with given DNI
                4;Order not available
                5;Order already cancelled
                6;Subscription is cancelled
                7;Order does not have email address
                8;Order does not have contact number
                9;Missing SMS Template
               10;Email sending is failed
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
{Func/multitenantfunc.i}

DEFINE VARIABLE pcTransId               AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcDNIType               AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcDNI                   AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE lcCLI                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDelValue              AS CHARACTER NO-UNDO.
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
       pcDNIType    = get_string(param_toplevel_id, "1")
       pcDNI        = get_string(param_toplevel_id, "2")
       pcDelType    = get_string(param_toplevel_id, "3").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcApplicationId = substring(pcTransId,1,3).

IF NOT fchkTMSCodeValues(ghAuthLog::UserName, lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").

katun = lcApplicationId + "_" + ghAuthLog::EndUserId.

IF LOOKUP(pcDelType,"EMAIL,SMS") = 0 THEN
   RETURN appl_err("Invalid Delivery Type").

IF NOT fsetEffectiveTenantForAllDB({&TENANT_YOIGO}) THEN RETURN
   int_err("Tenant change failed").

FOR EACH OrderCustomer WHERE 
         OrderCustomer.Brand      = gcBrand   AND
         OrderCustomer.CustIdType = pcDNIType AND
         OrderCustomer.CustId     = pcDNI     AND
         OrderCustomer.Rowtype    = 1 NO-LOCK,
    EACH Order WHERE
         Order.Brand     = gcBrand AND
         Order.OrderId   = OrderCustomer.OrderId AND
         Order.OrderType = 0 NO-LOCK BY Order.CrStamp DESC:

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

      lcCLI = Order.CLI.
      liMsSeq = Order.MsSeq.
      liOrderId = Order.OrderID.
      liCustNum = OrderCustomer.Custnum.

   END. /* IF liCount = 1 THEN DO: */

   IF pcDelType = "EMAIL" THEN DO:
      IF lcDelValue = "" AND OrderCustomer.Email > "" THEN
         lcDelValue = OrderCustomer.Email.
      ASSIGN
         
         lcReplaceText = lcReplaceText +
                         (IF lcReplaceText > "" THEN CHR(10) ELSE "") +
                         "Numero de Pedido: " + Order.ContractID + ", del " +
                         STRING(ldOrderDate) + ", del número " + Order.CLI + ".".
   END. /* IF pcDelType = "EMAIL" THEN DO: */
   ELSE DO:
      IF lcDelValue = "" AND OrderCustomer.MobileNumber > "" THEN
         lcDelValue = OrderCustomer.MobileNumber.
      lcReplaceText = lcReplaceText +
                      (IF lcReplaceText > "" THEN " - " ELSE "") +
                      Order.ContractID + ", del " + STRING(ldOrderDate) +
                      ", del " + Order.CLI.
   END. /* ELSE DO: */
END. /* FOR EACH Order WHERE */

IF liTotalCount = 0 THEN
   RETURN appl_err("Order does not exist with given DNI").
ELSE IF liCount = 0 THEN
   RETURN appl_err("Order not available").
ELSE IF NOT llOngoing THEN DO:
   IF llClose THEN
      RETURN appl_err("Order already cancelled").
   ELSE IF llDelivered THEN DO:
      FIND FIRST MobSub WHERE
                 MobSub.Brand = gcBrand AND
                 MobSub.CLI   = lcCLI NO-LOCK NO-ERROR.
      IF NOT AVAIL MobSub THEN
         RETURN appl_err("Subscription is cancelled").
   END. /* ELSE IF llDelivered THEN DO: */
END. /* ELSE IF NOT llOngoing THEN DO: */

IF lcDelValue = "" THEN DO:
   IF pcDelType = "EMAIL" THEN
      RETURN appl_err("Order does not have email address").
   ELSE RETURN appl_err("Order does not have contact number").
END. /* ELSE IF lcDelValue = "" THEN DO: */

IF pcDelType = "SMS" THEN DO:
   lcSMSText = fGetTxt("SMS",
                       (IF liCount > 1 THEN "GetMultiConsultID"
                        ELSE "GetConsultID"),
                       TODAY,
                       1).
   IF lcSMSText = "" THEN
      RETURN appl_err("Missing SMS Template").

   lcSMSText = REPLACE(lcSMSText,"#INFO",lcReplaceText).

   /* don't send messages before 8 am. */
   ldeOrderStamp = DYNAMIC-FUNCTION("fMakeOfficeTS" in ghFunc1).
   IF ldeOrderStamp = ? THEN ldeOrderStamp = fMakeTS().

   fCreateSMS(liCustnum,
              lcDelValue,
              liMsSeq,
              liOrderId,
              lcSMSText,
              "Yoigo info",
              {&SMS_TYPE_CONSULT}).

END. /* IF pcDelType = "SMS" THEN DO: */
ELSE DO:
   liRequest = fEmailSendingRequest(INPUT fMakeTS(),
                                    INPUT katun,
                                    INPUT 0, /* custnum */
                                    INPUT lcCLI,
                                    INPUT lcDelValue,
                                    INPUT (IF liCount > 1 THEN "GetMultiConsultID"
                                           ELSE "GetConsultID"),
                                    INPUT lcReplaceText,
                                    INPUT 0,
                                    INPUT {&REQUEST_SOURCE_EXTERNAL_API},
                                    OUTPUT lcResult).
   IF liRequest = 0 THEN
      RETURN appl_err("Email sending is failed").
END. /* ELSE DO: */

/*  add values to the response if no error  */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct,"transaction_id",pcTransId).
add_boolean(top_struct,"result",True).

FINALLY:
   /* Store the transaction id */
   ghAuthLog::TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
