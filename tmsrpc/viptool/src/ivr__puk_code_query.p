/**
 * Get PUK codes
 *
 * @input       string;mandatory;MSISDN
 * @output      puk1;string
                puk2;string
 * @Exceptions  1;Subscription not found
                2;IMSI not found
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR pcCLI AS CHAR NO-UNDO.

DEF VAR lcResultStruct AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcCLI = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{viptool/src/findtenant.i NO Ordercanal MobSub CLI pcCli}

FIND IMSI WHERE
     IMSI.IMSI = MobSub.IMSI NO-LOCK NO-ERROR.

IF NOT AVAIL IMSI THEN RETURN appl_err("IMSI not found").

lcResultStruct = add_struct(response_toplevel_id, "").

add_string(lcResultStruct,"puk1",IMSI.PUK1).
add_string(lcResultStruct,"puk2",IMSI.PUK2).
