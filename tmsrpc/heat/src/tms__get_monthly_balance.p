/**
 * Fetch a balance of currect month for postpaid subscription
 *
 * @input   msisdn;string
 *
 * @output  balance;double;,
            msisdn;string;
 *
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i &NOTIMEINCLUDES=1}
{Syst/tmsconst.i}

DEFINE VARIABLE gcBrand AS CHARACTER NO-UNDO INIT "1".
DEFINE VARIABLE response_array AS CHARACTER NO-UNDO. 

DEF VAR pcCLI           AS CHAR    NO-UNDO.
DEF VAR ldBalance       AS DECIMAL NO-UNDO.
DEF VAR liCurrentPeriod AS INT     NO-UNDO. 
DEF VAR liDBCount		AS INT     NO-UNDO.
DEF VAR lcTenant	    AS CHAR    NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.
pcCLI = get_string(param_toplevel_id, "0").
if gi_xmlrpc_error NE 0 THEN RETURN.

FOR FIRST MobSub WHERE MobSub.Brand = gcBrand AND MobSub.Cli = pcCLI TENANT-WHERE TENANT-ID() > -1 NO-LOCK:
    ASSIGN lcTenant = BUFFER-TENANT-NAME(Order).                
END.

IF NOT AVAIL MobSub OR lcTenant = "" THEN DO:
   add_int(response_toplevel_id, "", 1).
   RETURN.
END.

DO liDBCount = 1 TO NUM-DBS
   ON ERROR UNDO, THROW:
    SET-EFFECTIVE-TENANT(lcTenant, LDBNAME(liDBCount)).
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
