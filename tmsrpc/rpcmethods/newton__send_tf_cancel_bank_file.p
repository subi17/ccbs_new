/**
 * Create terminal financing cancellation bank file request. YRD-2025
 * Yoigo can now press button to AND CREATE terminal finance cancellation report * OR terminal finance termination report for banks SABADELL AND UNOE.
 *
 * @input string;mandatory;username
 * @output boolean;true
 */

{xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcUsername  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liRequestID AS INTEGER   NO-UNDO. 
DEFINE VARIABLE pcBankCode  AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string,string") EQ ? THEN RETURN.

ASSIGN
   pcUserName = get_string(param_toplevel_id, "0")
   pcBankCode = get_nonempty_string(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "" THEN RETURN appl_err("username is empty").

{commpaa.i}
gcBrand = "1".
katun = "VISTA_" + pcUserName.
{tmsconst.i}
{terminal_financing.i}

IF LOOKUP(pcBankCode,{&TF_BANK_CODES}) EQ 0 THEN
   RETURN appl_err(SUBST("Incorrect bank code: &1", pcBankCode)).

liRequestID = fCreateTFBankFileRequest(
                   pcBankCode,
                   pcUserName, /* creator */
                   ({&REQUEST_SOURCE_NEWTON}),
                   OUTPUT lcError).

IF liRequestID = 0 THEN RETURN appl_err(lcError).

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
