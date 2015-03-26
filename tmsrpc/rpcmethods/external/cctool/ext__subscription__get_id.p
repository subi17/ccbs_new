/**
 * Get subscription id
 *
 * @input       msisdn;string;mandatry;msisdn of subscription
 * @output      id;int;subscription id
 */
{xmlrpc/xmlrpc_access.i}

/* Input parameters */
DEF VAR pcCLI AS CHAR NO-UNDO.
DEF VAR gcBrand AS CHARACTER NO-UNDO INIT "1".
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcCLI = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND mobsub NO-LOCK WHERE 
     mobsub.cli = pcCli NO-ERROR.
IF NOT AVAILABLE mobsub THEN
    RETURN appl_err(SUBST("Subscription &1 was not found", pcCLI)).

add_int(response_toplevel_id,"id",MobSub.MsSeq).
