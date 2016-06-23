/**
 * Create terminal financing cancellation bank file request. YRD-2025
 * Yoigo can now press button to AND CREATE terminal finance cancellation report * OR terminal finance termination report for banks SABADELL AND UNOE.
 *
 * @input string;mandatory;username
 *        string;mandatory;bankcode
 * @output boolean;true
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

DEFINE VARIABLE pcUsername  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liRequestID AS INTEGER   NO-UNDO. 
DEFINE VARIABLE pcBankCode  AS CHAR NO-UNDO.
DEF VAR ldeMonthBeg AS DEC NO-UNDO.
DEF VAR ldeMonthEnd AS DEC NO-UNDO. 


IF validate_request(param_toplevel_id, "string,string") EQ ? THEN RETURN.

ASSIGN
   pcUserName = get_string(param_toplevel_id, "0")
   pcBankCode = get_nonempty_string(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "" THEN RETURN appl_err("username is empty").

{Syst/commpaa.i}
gcBrand = "1".
katun = "VISTA_" + pcUserName.
{Syst/tmsconst.i}
{Func/terminal_financing.i}
{Func/timestamp.i}

IF LOOKUP(pcBankCode,{&TF_BANK_CODES}) EQ 0 THEN
   RETURN appl_err(SUBST("Incorrect bank code: &1", pcBankCode)).

/* Checking if files are sent to certain bank at this month already.
   In case yes, exception is returned. */
fMonthlyStamps(TODAY, ldeMonthBeg, ldeMonthEnd).
IF CAN-FIND( FIRST MsRequest NO-LOCK WHERE
         MsRequest.Brand = gcBrand AND
         MsRequest.ReqType = {&REQTYPE_TERMINAL_FINANCE_CAN_TER_BANK_FILE} AND
         MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
         MsRequest.ActStamp >= ldeMonthBeg AND
         MsRequest.ActStamp < ldeMonthEnd AND
         MsRequest.ReqCParam1 = pcBankCode) THEN
   RETURN appl_err(SUBST("Files generated already at this month for bank: &1", pcBankCode)).


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
