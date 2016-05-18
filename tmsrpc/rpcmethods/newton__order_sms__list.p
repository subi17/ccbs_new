/**
 * Get a list of order SMS IDs.
 *
 * @input order_id;int;mandatory;order id
          limit;int;optional;
          offset;int;optional;
 * @output results;array of int;list of sms message ids 
           total_amount;int;total sms message amount
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}

DEF VAR gcBrand AS CHAR NO-UNDO INIT "1".
DEF VAR lcResultStruct AS CHARACTER NO-UNDO. 
DEF VAR pcStruct AS CHARACTER NO-UNDO. 
DEF VAR lcStruct AS CHARACTER NO-UNDO. 
DEF VAR lcIdStruct AS CHARACTER NO-UNDO. 
DEF VAR liOrderId AS INTEGER NO-UNDO. 
DEF VAR liLimit AS INTEGER NO-UNDO INIT 10000000. 
DEF VAR liOffSet AS INTEGER NO-UNDO. 
DEF VAR liCount AS INTEGER NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_struct(pcStruct, "order_id!,limit,offset").
IF gi_xmlrpc_error NE 0 THEN RETURN.

liOrderId = get_int(pcStruct,"order_id").
IF LOOKUP("limit",lcStruct) > 0 THEN 
  liLimit = get_int(pcStruct,"limit").
IF LOOKUP("offset",lcStruct) > 0 THEN 
  liOffSet = get_int(pcStruct,"offset").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST Order NO-LOCK WHERE
           Order.Brand = gcBrand AND
           Order.OrderId = liOrderId NO-ERROR.

IF NOT AVAIL Order THEN
   RETURN appl_err("Order not found").

lcResultStruct = add_struct(response_toplevel_id, "").
lcIdStruct = add_array(lcResultStruct, "results").

FOR EACH SMSMessage NO-LOCK WHERE
         SMSMessage.OrderID = Order.OrderID:

   IF SMSMessage.DeliStat NE {&CA_DELISTAT_SENT} THEN NEXT.

   liCount = liCount + 1.
   IF liCount <= liOffSet THEN NEXT.
   IF liCount > liLimit + liOffSet THEN NEXT.

   add_string(lcIdStruct, "", STRING(SMSMessage.SMSSeq)). 
END.

add_int(lcResultStruct, "total_amount", liCount). 
