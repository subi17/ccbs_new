/**
 * Cancel ongoing MNP order
 *
 * @input transaction_id;string;mandatory;transaction id
          order_id;int;mandatory;order id
          mnp_process_id;string;mandatory;mnp process reference id
          cancel_code;string;optional;4 digit code send by SMS
 * @output     struct;mandatory;response struct
 * @response   transaction_id;string;transaction id
               result;boolean;True
 * @exceptions 1;Order does not exist
               2;Active MNP process does not exist
               3;Cancellation rules are not met
               4;Application Id does not match
               5;Incorrect cancellation code
               6;cancellation code SMS was not found
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
ASSIGN katun = ghAuthLog::UserName + "_" + ghAuthLog::EndUserId
       gcBrand = "1".
{Func/fexternalapi.i}
{Syst/tmsconst.i}

DEF VAR piOrderId      AS INT  NO-UNDO.
DEF VAR pcPortRequest  AS CHAR NO-UNDO.
DEF VAR pcTransId      AS CHAR NO-UNDO.
DEF VAR top_struct     AS CHAR NO-UNDO.
DEF VAR lcApplicationId AS CHAR NO-UNDO.
DEF VAR pcCancel_code   AS CHAR NO-UNDO INIT "".
DEF VAR liCancel_code   AS INT  NO-UNDO.
DEF VAR top_array       AS CHAR NO-UNDO.

top_array = validate_request(param_toplevel_id, "string,int,string,[string]").
IF top_array EQ ? THEN RETURN.

ASSIGN
   pcTransId = get_string(param_toplevel_id, "0")
   piOrderId = get_int(param_toplevel_id, "1")
   pcPortRequest = get_string(param_toplevel_id, "2").

IF NUM-ENTRIES(top_array) > 3 THEN
    pcCancel_code = get_string(param_toplevel_id, "3").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcApplicationId = substring(pcTransId,1,3).

IF NOT fchkTMSCodeValues(ghAuthLog::UserName, lcApplicationId) THEN
   RETURN appl_err("Application Id does not match").

katun = lcApplicationId + "_" + ghAuthLog::EndUserId.

FIND FIRST Order WHERE
           Order.Brand   = gcBrand   AND
           Order.OrderId = piOrderId NO-LOCK NO-ERROR.
IF NOT AVAILABLE Order THEN
   RETURN appl_err("Order does not exist").

IF NOT fchkOrderCancelRule(INPUT Order.OrderChannel,
                           INPUT "api") THEN
   RETURN appl_err("Cancellation rules are not met").

FIND FIRST MNPProcess NO-LOCK WHERE
           MNPProcess.PortRequest = pcPortRequest AND
           MNPProcess.OrderId = Order.OrderId NO-ERROR.

IF NOT AVAIL MNPProcess THEN
   RETURN appl_err("Mnp process does not exist").

/* If cancel code inserted then it must be the same that was send by last sms */
IF pcCancel_code NE "" THEN DO:
   liCancel_code = INT(pcCancel_code). /* Check that number is in correct range */
   IF ERROR-STATUS:ERROR THEN 
      RETURN appl_err("Incorrect cancellation code").

   IF liCancel_code < 1000 OR liCancel_code > 9999 THEN 
      RETURN appl_err("Incorrect cancellation code").

   FOR EACH CallAlarm NO-LOCK WHERE
            CallAlarm.Brand      = gcBrand AND
            CallAlarm.CLI        = Order.CLI   AND
            CallAlarm.CreditType = {&SMSTYPE_MNP_CANCEL} BY ActStamp DESC:
      LEAVE.
   END.
   IF NOT AVAILABLE CallAlarm THEN
      RETURN appl_err("SMS was not found").

   IF INDEX(CallAlarm.DeliMsg,pcCancel_code,1) = 0 THEN
      RETURN appl_err("Incorrect cancellation code").
END.

RUN Mnp/mnp_operation.p(MNPProcess.MNPSeq,"cancel","CANC_ABONA").

IF RETURN-VALUE NE "OK" THEN
   RETURN appl_err("Cancellation rules are not met").

/* Create Memo */
CREATE Memo.
ASSIGN
    Memo.CreStamp  = {&nowTS}
    Memo.Brand     = gcBrand
    Memo.HostTable = "MNPProcess"
    Memo.KeyValue  = STRING(MNPProcess.MNPSeq)
    Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
    Memo.CreUser   = ghAuthLog::EndUserId
    Memo.MemoTitle = "By customer's request (Self Service)"
    Memo.MemoText  = "MNP process with request id " + MNPProcess.FormRequest +
                     " was successfully cancelled"
    Memo.Source    = "Self Service".

CREATE Memo.
ASSIGN
   Memo.CreStamp  = {&nowTS}
   Memo.Brand     = gcBrand
   Memo.HostTable = "Order"
   Memo.KeyValue  = STRING(MNPProcess.OrderId)
   Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
   Memo.CreUser   = ghAuthLog::EndUserId
   Memo.MemoTitle = "By customer's request (Self Service)"
   Memo.MemoText  = "MNP process with request id " + MNPProcess.FormRequest +
                    " was successfully cancelled"
   Memo.CustNum   = Order.Custnum WHEN AVAIL Order
   Memo.Source    = "Self Service".

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_boolean(top_struct, "result", True).

FINALLY:
   /* Store the transaction id */
   ghAuthLog::TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
