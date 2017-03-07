/**
 * Fetch a balance of currect month for postpaid subscription
 *
 * @input   msisdn;string
 *
 * @output  balance;double;,
            msisdn;string;
 *
 */

{xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{Syst/tmsconst.i}

DEFINE VARIABLE gcBrand AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE response_array AS CHARACTER NO-UNDO. 

DEF VAR pcCLI AS CHAR NO-UNDO.
DEF VAR ldBalance AS DECIMAL NO-UNDO.
DEF VAR liCurrentPeriod AS INT NO-UNDO. 

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcCLI = get_string(param_toplevel_id, "0").
if gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST mobsub WHERE
     mobsub.brand = gcBrand AND
     mobsub.cli = pcCli 
NO-LOCK NO-ERROR.

IF NOT AVAILABLE mobsub THEN DO:
   add_int(response_toplevel_id, "", 1).
   RETURN.
END.

/* Cannot be prepaid */
IF mobsub.paytype = TRUE THEN DO:
   add_int(response_toplevel_id, "", 2).
   RETURN.
END.
    
liCurrentPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).

/* get postpaid balance of the current month */
FIND FIRST saldocounter WHERE
           saldocounter.msseq = mobsub.msseq AND
           saldocounter.period = liCurrentPeriod NO-LOCK NO-ERROR.

IF AVAIL saldocounter THEN ldbalance = ldBalance + saldocounter.amt.

response_array = add_array(response_toplevel_id, "").
add_double(response_array, "", ldBalance).
add_string(response_array, "", MobSub.CLI).
