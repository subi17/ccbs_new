/* ----------------------------------------------------------------------
  MODULE .......: newton__get_msisdn.p
  TASK .........: gets info related to Orders and this MSISDN details
  APPLICATION ..:
  AUTHOR .......: subhash sanjeevi
  CREATED ......: 07.01.2018
  CHANGED ......:
  Version ......:
  ---------------------------------------------------------------------- */

/* @input string;mandatory;TenantId
          int;mandatory;OrderId   */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/cparam2.i}

DEF VAR pcTenant       AS CHAR NO-UNDO.
DEF VAR piOrderId      AS INT  NO-UNDO.
DEF VAR lcError        AS CHAR NO-UNDO.
DEF VAR lcResultStruct AS CHAR NO-UNDO.
DEF VAR lcCliStruct    AS CHAR NO-UNDO. 
DEF VAR lcCustName     AS CHAR NO-UNDO. 
DEF VAR lcCliArray     AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "string,int") EQ ? THEN RETURN.

pcTenant  = get_string(param_toplevel_id, "0").
piOrderId = get_int(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF piOrderId EQ 0 THEN RETURN appl_err("OrderId is null").

{newton/src/settenant.i pcTenant}

FIND FIRST Order NO-LOCK WHERE 
           Order.Brand   = Syst.Var:gcBrand AND 
           Order.OrderId = piOrderId        NO-ERROR.

IF NOT AVAIL Order THEN 
   RETURN appl_err("OrderId is not available"). 

IF LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 THEN 
   RETURN appl_err("Order is already closed or cancelled").

FIND FIRST OrderCustomer NO-LOCK WHERE
           OrderCustomer.Brand   = Syst.Var:gcBrand                   AND
           OrderCustomer.OrderId = Order.OrderId                      AND
           OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.

IF NOT AVAIL OrderCustomer THEN 
   RETURN appl_err("OrderCustomer not available").

FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK.

IF NOT AVAIL Customer THEN 
   RETURN appl_err("Customer is not available"). 

lcCustName = Func.Common:mDispCustName(BUFFER Customer).

lcResultStruct = add_struct(response_toplevel_id, "").

add_string(lcResultStruct,"OrderId",STRING(Order.OrderId)).
add_string(lcResultStruct,"Email",Customer.Email).
add_int(lcResultStruct,"Phone",INT(Customer.SMSNumber)).
add_string(lcResultStruct,"Person",lcCustName).

lcCliArray = add_array(lcResultStruct, "MSISDN").

lcCliStruct = add_struct(lcCliArray,"").

add_string(lcCliStruct, "ICC", Order.ICC).
add_string(lcCliStruct, "TariffCode", Order.CLIType).

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
END.



