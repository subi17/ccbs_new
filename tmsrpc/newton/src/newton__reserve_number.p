/** Reserve a single number for 60 minutes.
 * @input msisdn;string;mandatory;number to be reserved
          reserve;bool;should be true when called 1st time false otherwise 
 * @output success;boolean;true or error
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}

/* Input parameters */
DEF VAR pcCli AS CHAR NO-UNDO.
DEF VAR pcFor AS CHAR NO-UNDO.
DEF VAR plReserve AS LOG NO-UNDO.
{Syst/tmsconst.i}
{Func/timestamp.i}

IF validate_request(param_toplevel_id, "string,boolean") EQ ? THEN RETURN.
pcCli = get_string(param_toplevel_id, "0").
plReserve = get_bool(param_toplevel_id, "1"). /* true = really reserved, when called 1st time*/
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcFor = {&MSISDN_STOCK_ONLINE}.

FIND FIRST msisdn EXCLUSIVE-LOCK
WHERE MSISDN.Brand EQ "1"
  AND msisdn.statuscode EQ 1
  AND MSISDN.ValidTo GE {&nowts}
  AND MSISDN.POS EQ pcFor
  AND msisdn.cli EQ pcCLI NO-ERROR.

IF NOT AVAIL MSISDN OR (plReserve AND msisdn.LockedTo > {&nowts}) THEN DO:
   RETURN appl_err(SUBST("Number &1 not found or not free", pcCli)).
END.

msisdn.LockedTo = fOffSet({&nowts},1). /* reserve number for 1 hour */
RELEASE msisdn.

add_boolean(response_toplevel_id, "", true).
