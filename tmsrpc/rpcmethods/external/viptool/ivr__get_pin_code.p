/**
 * Get subscription PIN code 
 *
 * @input       string;mandatory;MSISDN
 * @output      string;Subscription PIN code
 * @Exceptions  1;Subscription not found
 */
{xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR pcCLI AS CHAR NO-UNDO.
DEF VAR gcBrand AS CHARACTER NO-UNDO INIT "1".

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcCLI = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST mobsub NO-LOCK WHERE 
           mobsub.brand = gcBrand AND
           mobsub.cli = pcCli NO-ERROR.
IF NOT AVAILABLE mobsub THEN RETURN appl_err("Subscription not found").

add_string(response_toplevel_id,"",MobSub.IDCode).
