/** Check if MNP number is used already in different order/subscription.
 * @input cli;string;mandatory;
 * @output success;boolean;true or error
 */
{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{tmsconst.i}

/* Input parameters */
DEF VAR pcCli AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcCli = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF CAN-FIND(FIRST MobSub
            WHERE MobSub.brand EQ "1"
              AND MobSub.CLI EQ pcCli) THEN
    RETURN appl_err("Subscription exists for this number").

IF CAN-FIND(FIRST Order
            WHERE Order.brand EQ "1"
              AND Order.CLI EQ pcCLI
              AND LOOKUP(STRING(Order.StatusCode),{&ORDER_INACTIVE_STATUSES}) EQ 0) THEN
    RETURN appl_err("Order exists for this number").

add_boolean(response_toplevel_id, "", TRUE).
