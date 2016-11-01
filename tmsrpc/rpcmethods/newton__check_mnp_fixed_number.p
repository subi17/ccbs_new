/** Check if fixed line MNP number is used already in different order/subscription.
 * @input string;mandatory;fixed numebr
 * @output success;boolean;true or error
 */
{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{Syst/tmsconst.i}

/* Input parameters */
DEF VAR pcFixedNumber AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcFixedNumber = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF CAN-FIND(FIRST MobSub WHERE
                  MobSub.brand EQ Syst.Parameters:gcBrand AND
                  MobSub.fixedNumber EQ pcFixedNumber) THEN
    RETURN appl_err("Subscription exists for this number").

FOR EACH OrderFusion NO-LOCK WHERE
         OrderFusion.FixedNumber EQ pcFixedNumber,
    EACH Order NO-LOCK WHERE
         Order.brand    EQ Syst.Parameters:gcBrand AND
         Order.OrderId  EQ OrderFusion.OrderId AND
         LOOKUP(Order.statuscode,{&ORDER_INACTIVE_STATUSES}) EQ 0:
    RETURN appl_err("Order exists for this number").
END.

add_boolean(response_toplevel_id, "", TRUE).
