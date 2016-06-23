/**
 * Create EMAIL invoice request 
 *
 * @input string;mandatory;username
 * @output boolean;true
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcUsername  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liRequestID AS INTEGER   NO-UNDO. 

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcUserName = get_string(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "" THEN RETURN appl_err("username is empty").

{Syst/commpaa.i}
gcBrand = "1".
katun = "VISTA_" + pcUserName.
{Syst/tmsconst.i}
{Func/femailinvoice.i}

liRequestID = fSendeInvoiceRequest(
                      INPUT fMakeTS(),
                      INPUT DATE(MONTH(TODAY),1,YEAR(TODAY)),
                      INPUT pcUserName, /* creator */
                      INPUT ({&REQUEST_SOURCE_NEWTON}),
                      OUTPUT lcError).

IF liRequestID = 0 THEN RETURN appl_err(lcError).

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
