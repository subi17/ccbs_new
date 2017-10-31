/**
 * Get subscription id
 *
 * @input       msisdn;string;mandatry;msisdn of subscription
 * @output      id;int;subscription id
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR pcCLI    AS CHAR NO-UNDO.
DEF VAR Syst.CUICommon:gcBrand  AS CHARACTER NO-UNDO INIT "1".

DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

pcCLI    = get_string(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{cctool/src/findtenant.i NO ordercanal MobSub Cli pcCLI}

add_int(response_toplevel_id,"id",MobSub.MsSeq).
