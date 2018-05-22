/** Check if fixed number is exist or not.
 * @input string;mandatory;fixed number
 * @output success;boolean;true or false
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{Syst/tmsconst.i}

/* Input parameters */
DEF VAR pcFixedNumber AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcFixedNumber = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF CAN-FIND(FIRST MobSub WHERE
                  MobSub.brand EQ Syst.Var:gcBrand AND
                  MobSub.fixedNumber EQ pcFixedNumber) THEN DO:
   add_boolean(response_toplevel_id, "", TRUE).
   RETURN.
END.

FOR EACH OrderFusion NO-LOCK WHERE
         OrderFusion.FixedNumber EQ pcFixedNumber,
    EACH Order NO-LOCK WHERE
         Order.brand    EQ Syst.Var:gcBrand AND
         Order.OrderId  EQ OrderFusion.OrderId AND
         LOOKUP(Order.statuscode,{&ORDER_INACTIVE_STATUSES}) EQ 0:
    add_boolean(response_toplevel_id, "", TRUE).
    RETURN.
END.

add_boolean(response_toplevel_id, "", FALSE).




