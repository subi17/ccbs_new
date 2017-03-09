/**
 * Create terminal financing bank file request. YPR-485
 *
 * @input string;mandatory;brand
 		  string;mandatory;username	
 * @output boolean;true
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
gcBrand = "1".
{Syst/tmsconst.i}
{Func/terminal_financing.i}

DEFINE VARIABLE pcTenant    AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcUsername  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liRequestID AS INTEGER   NO-UNDO. 
DEFINE VARIABLE pcBankCode  AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,string") EQ ? THEN RETURN.

ASSIGN
   pcTenant   = get_string(param_toplevel_id, "0")	
   pcUserName = get_string(param_toplevel_id, "1")
   pcBankCode = get_nonempty_string(param_toplevel_id, "2").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "" THEN RETURN appl_err("username is empty").

{newton/src/settenant.i pcTenant}

katun = "VISTA_" + pcUserName.

IF LOOKUP(pcBankCode,{&TF_BANK_CODES}) EQ 0 THEN
   RETURN appl_err(SUBST("Incorrect bank code: &1", pcBankCode)).

liRequestID = fCreateBankFileRequest(
                   pcBankCode,
                   pcUserName, /* creator */
                   ({&REQUEST_SOURCE_NEWTON}),
                   OUTPUT lcError).

IF liRequestID = 0 THEN RETURN appl_err(lcError).

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
