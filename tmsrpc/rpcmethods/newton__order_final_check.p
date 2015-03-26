/**
 * Final order checks
 *
 * @input: msisdn;string;mandatory;Subscription msisdn 
           number_type;string;mandatory;Order type (new,mnp,renewal,stc)
 * @output: boolean;True=OK/False=NOT OK
 */
{xmlrpc/xmlrpc_access.i}

{commpaa.i}
katun = "NewtonRPC".
gcBrand = "1".
{tmsconst.i}
{date.i}
{orderchk.i}

/* Input parameters */
DEF VAR pcStruct AS CHARACTER NO-UNDO. 
DEF VAR pcCLI AS CHAR NO-UNDO.
DEF VAR pcNumberType AS CHAR NO-UNDO INIT ?.
/* Output parameters */
DEF VAR llAllow AS LOG NO-UNDO INIT TRUE. 

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.

pcstruct = get_struct(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

validate_request(pcstruct,"msisdn!,number_type!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   pcCLI = get_string(pcStruct, "msisdn")
   pcNumberType = get_string(pcStruct, "number_type").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF LOOKUP(pcNumberType,"new,mnp,renewal,stc") = 0 THEN RETURN
   appl_err(SUBST("Incorrect number type: &1",pcNumberType)).

IF fOngoingOrders(pcCli, pcNumberType) THEN llAllow = FALSE.
ELSE IF pcNumberType EQ "stc" THEN DO:

   FIND FIRST MobSub NO-LOCK WHERE
              MobSub.Brand = gcBrand AND
              MobSub.CLI = pcCLI NO-ERROR.
   IF NOT AVAIL Mobsub THEN
      RETURN appl_err("Subscription not found").

   /* Check ongoing STC request */
   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.MsSeq = Mobsub.MsSeq AND
              MsRequest.ReqType = 0 AND
              LOOKUP(STRING(MsRequest.ReqStat),
               {&REQ_INACTIVE_STATUSES}) = 0  NO-ERROR.
   IF AVAILABLE MsRequest THEN llAllow = FALSE.
END.

add_boolean(response_toplevel_id, "", llAllow).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
