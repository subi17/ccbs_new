/**
 * Creates SMS invoice request 
 *
 * @input string;mandatory;username
 * @output boolean;true
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand  = "1".
{Syst/tmsconst.i}
{Func/einvoice.i}

DEFINE VARIABLE pcTenant    AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcUsername  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liRequestID AS INTEGER   NO-UNDO. 

IF validate_request(param_toplevel_id, "string,string") EQ ? THEN RETURN.
pcTenant = get_string(param_toplevel_id, "0").
pcUserName = get_string(param_toplevel_id, "1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "" THEN RETURN appl_err("username is empty").

/*katun = "VISTA_" + pcUserName.*/

{newton/src/settenant.i pcTenant}

liRequestID = fEInvoiceRequest
    (Func.Common:mMakeTS(),  /* when request should be handled */
    DATE(MONTH(TODAY),1,YEAR(TODAY)),
    "", /* creator */
    ({&REQUEST_SOURCE_NEWTON}),
    OUTPUT lcError).

IF liRequestID = 0 THEN DO:
   RETURN appl_err(lcError).
END.

add_boolean(response_toplevel_id,?,TRUE).

