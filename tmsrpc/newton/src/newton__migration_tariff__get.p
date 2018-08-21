/**
 * Changes a request status.
 *
 * @input   msrequest;int;mandatory;msrequest id
            newstatus;int;mandatory;new status for msrequest
            usercode;string;optional;login id 
 * @output  success;boolean
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "Newton".
{Func/profunc_request.i}

/* Input parameters */
DEF VAR pcTenant      AS CHAR NO-UNDO.
DEF VAR pcCliTypeFrom AS CHAR NO-UNDO.
DEF VAR pcCliTypeTo   AS CHAR NO-UNDO.
DEF VAR top_array     AS CHAR NO-UNDO. 

/* Local variables */
DEFINE VARIABLE liLoop AS INTEGER NO-UNDO.

/* Parse and validate request */
top_array = validate_request(param_toplevel_id, "string,string").
IF top_array EQ ? THEN RETURN.

pcTenant      = get_string(param_toplevel_id, "0").
pcCliTypeFrom = get_string(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

fCheckSubscriptionTypeAllowedForProMigration(pcCliTypeFrom, OUTPUT pcCliTypeTo).

add_string(response_toplevel_id, "", pcCliTypeTo).

RETURN.

FINALLY:
END.
