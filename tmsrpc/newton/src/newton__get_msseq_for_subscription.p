/**
 * Returns subscription ID (msseq) for a given MSISDN
 *
 * @input       string;mandatory;MSISDN
 * @output      int;subscription ID
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR pcMSISDN AS CHAR NO-UNDO.
IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcMSISDN = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO ordercanal MobSub CLI pcMSISDN}

add_int(response_toplevel_id, "", MobSub.MsSeq).

