/* ----------------------------------------------------------------------
  MODULE .......: newton__yoicard_get_status.p
  TASK .........: get status of yoicard ( creditcard )
  AUTHOR .......: ashok
  CREATED ......: 04.05.2018
  ---------------------------------------------------------------------- */
{Syst/commpaa.i}
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}
Syst.Var:gcBrand = "1".

DEFINE VARIABLE liCustNum       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcResultStruct  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAvailCard      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcStats         AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "int") EQ ? THEN RETURN.
liCustNum = get_int(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST Customer WHERE
           Customer.brand   EQ Syst.Var:gcBrand  AND 
           Customer.custnum EQ liCustNum NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN
   RETURN appl_err("Customer not found").

lcStats = "Desactivada,Emitida,Activada".
lcResultStruct = add_struct(response_toplevel_id, "").
FOR EACH MobSub NO-LOCK 
   WHERE mobsub.brand = Customer.brand AND  
         MobSub.CustNum = liCustNum :
    FIND FIRST DCCli WHERE 
           DCCLi.brand = MobSub.Brand          AND 
           Dccli.Dcevent = {&YOICARD_DCEvent}  AND 
           DCCLi.msseq = mobsub.msseq          NO-LOCK NO-ERROR.
    IF NOT AVAILABLE DCCLi THEN NEXT.
    add_string(lcResultStruct,"status", ENTRY(DCCli.ServiceStatus + 1 , lcStats )).
    lAvailCard = TRUE.
    LEAVE.
END.
IF lAvailCard = FALSE THEN 
    add_string(lcResultStruct,"status","Sin tarjeta").


