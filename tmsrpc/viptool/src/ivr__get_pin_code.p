/**
 * Get subscription PIN code 
 *
 * @input       string;mandatory;MSISDN
 * @output      string;Subscription PIN code
 * @Exceptions  1;Subscription not found
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR pcCLI AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcCLI = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{viptool/src/findtenant.i NO Ordercanal MobSub CLI pcCli}

add_string(response_toplevel_id,"",MobSub.IDCode).
