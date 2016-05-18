/**
 * Consumption of the previous month
 *
 * @input       string;mandatory;MSISDN
 * @output      int;previous month consumption in cents
 * @Exceptions  1;Subscription not found
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.

/* Input parameters */
DEF VAR pcMSISDN AS CHAR NO-UNDO.

{Syst/commpaa.i}
ASSIGN
   katun = "IVR_" + gbAuthLog.EndUserId.
   gcBrand = "1".

{Syst/tmsconst.i}

DEF VAR liPeriod AS INT NO-UNDO. 
DEF VAR ldaDate AS DATE NO-UNDO. 
DEF VAR liAmount AS INT NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcMSISDN = get_string(param_toplevel_id,"0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST mobsub NO-LOCK WHERE 
           mobsub.brand = gcBrand AND
           mobsub.cli = pcMSISDN NO-ERROR.
IF NOT AVAILABLE mobsub THEN RETURN appl_err("Subscription not found").

ldaDate = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
liPeriod = YEAR(ldaDate) * 100 + MONTH(ldaDate). 

FIND FIRST SaldoCounter WHERE
           SaldoCounter.MsSeq = MobSub.MsSeq AND
           SaldoCounter.Period = liPeriod NO-LOCK NO-ERROR.
IF AVAIL SaldoCounter THEN liAmount = INT(SaldoCounter.amt * 100).
ELSE liAmount = 0.

add_int(response_toplevel_id, "", liAmount).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
