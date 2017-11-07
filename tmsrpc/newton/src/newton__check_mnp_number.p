/** Check if MNP number is used already in different order/subscription.
 * @input  brand;string;mandatory
           cli;string;mandatory;
 * @output orderstatus;string;Order's statuscode
           error;string;Error text 
           success;boolean;true or error
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{Syst/tmsconst.i}

/* Input parameters */
DEF VAR pcTenant      AS CHAR NO-UNDO.
DEF VAR pcCli         AS CHAR NO-UNDO.
DEF VAR lcText        AS CHAR NO-UNDO.
DEF VAR top_struct    AS CHAR NO-UNDO.
DEF VAR lcOrderStatus AS CHAR NO-UNDO.
DEF VAR llResult      AS LOG  NO-UNDO INIT TRUE.

IF validate_request(param_toplevel_id, "string!,string!") EQ ? THEN RETURN.
pcTenant = get_string(param_toplevel_id, "0").
pcCli = get_string(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

ASSIGN top_struct = add_struct(response_toplevel_id, "").

FIND FIRST MobSub NO-LOCK WHERE
           MobSub.Brand EQ Syst.Var:gcBrand AND
           MobSub.CLI   EQ pcCLI                   NO-ERROR.
IF AVAILABLE MobSub THEN
   ASSIGN lcText        = "Subscription exists for this number"
          lcOrderStatus = "6"
          llResult      = FALSE.

IF lcText = "" THEN DO:
   FIND FIRST Order NO-LOCK WHERE
              Order.brand EQ Syst.Var:gcBrand AND
              Order.CLI   EQ pcCLI                   AND
              LOOKUP(STRING(Order.StatusCode),{&ORDER_INACTIVE_STATUSES}) EQ 0 NO-ERROR.
   IF AVAILABLE Order THEN
      ASSIGN lcText        = "Order exists for this number"
             lcOrderStatus = Order.StatusCode
             llResult      = FALSE.
END.

add_string(top_struct, "orderstatus",lcOrderStatus).
add_string(top_struct, "Error",lcText).
add_boolean(top_struct, "Result", llResult).
