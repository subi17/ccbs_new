/**
 * Final order checks
 *
 * @input: msisdn;string;mandatory;Subscription msisdn 
           fixed_number;string;optional;Fixed line number 
           number_type;string;mandatory;Order type (new,mnp,renewal,stc)
           channel;string;optional;order channel
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
DEF VAR pcFixedNumber AS CHAR NO-UNDO INIT "".
DEF VAR pcNumberType AS CHAR NO-UNDO INIT ?.
DEF VAR pcChannel AS CHAR NO-UNDO. 
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR lcError AS CHAR NO-UNDO.
/* Output parameters */
DEF VAR llAllow AS LOG NO-UNDO INIT TRUE. 

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcstruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcstruct,"msisdn!,fixed_number,number_type!,channel").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   pcCLI = get_string(pcStruct, "msisdn")
   pcFixedNumber = get_string(pcStruct, "fixed_number") WHEN 
      LOOKUP("fixed_number",lcStruct) > 0
   pcNumberType = get_string(pcStruct, "number_type")
   pcChannel = get_string(pcStruct,"channel") WHEN
      LOOKUP("channel",lcStruct) > 0.

pcChannel = REPLACE(pcChannel,"order","").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF LOOKUP(pcNumberType,"new,mnp,renewal,stc") = 0 THEN RETURN
   appl_err(SUBST("Incorrect number type|&1",pcNumberType)).

IF fOngoingOrders(pcCli, (IF pcChannel BEGINS "retention"
                          THEN "retention"
                          ELSE pcNumberType)) THEN 
   RETURN appl_err("Ongoing order for number|" + pcCli).


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
   IF AVAILABLE MsRequest THEN 
      RETURN appl_err("Ongoing STC found").
END.

/* Check Fixed number existence and orders */
IF pcFixedNumber > "" THEN DO:
   FIND FIRST MobSub WHERE
              MobSub.Brand = gcBrand AND
              MobSub.FixedNumber = pcFixedNumber NO-LOCK NO-ERROR. 
   IF AVAIL MobSub THEN
      RETURN appl_err("Subscription already exists with number|" + 
                       pcFixedNumber).
   
   lcError = fOngoingFixedOrders(pcFixedNumber, 
                     (IF pcChannel BEGINS "retention"
                          THEN "retention"
                     ELSE pcNumberType)).

   IF lcError > "" THEN RETURN appl_err(lcError).
END.
ELSE IF pcNumberType EQ "MNP" OR
        pcNumberType EQ "NEW" THEN DO:
      FIND FIRST MobSub WHERE
                 MobSub.Brand = gcBrand AND
                 MobSub.CLI = pcCLI NO-LOCK NO-ERROR. 
      IF AVAIL MobSub THEN
         RETURN appl_err("Subscription already exists with number|" + pcCLI).
END.

add_boolean(response_toplevel_id, "", llAllow).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
