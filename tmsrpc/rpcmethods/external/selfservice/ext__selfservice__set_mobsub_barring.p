/**
 * Change the status of given only customer level barring code
 *
 * @input       transaction_id;string;mandatory;transaction id
                msisdn;string;mandatory;subscription msisdn number
                barr_code;string;mandatory;barring code
                barr_status;string;mandatory;barring status (on/off)
 * @output      struct;mandatory;response struct
 * @response    transaction_id;string;transaction id
                result;boolean;True
 * @Examples    1;C_BPAC;CLB Premium and Content
                2;C_BRAIC;CLB Roaming and International
                3;C_BRIAP;CLB Roaming and Int.calls + Premium
                4;C_LOS;CLB Lost or Stolen
 * @exceptions  1;Subscription not found
                2;CLIType Service Package not found
                3;Service Package not found
                4;Ongoing network command
                5;Operator or debt level barring is on
                6;Barring is not allowed
                7;Barring is already active
                8;Barring is already inactive
                9;Application Id does not match
 */

{xmlrpc/xmlrpc_access.i}
DEFINE SHARED BUFFER gbAuthLog FOR AuthLog.
{commpaa.i}
katun = gbAuthLog.UserName + "_" + gbAuthLog.EndUserId.
gcBrand = "1".
{tmsconst.i}
{timestamp.i}
{barrfunc.i}
{fexternalapi.i}

/* Input parameters */
DEF VAR pcCLI           AS CHAR NO-UNDO.
DEF VAR pcServiceCode   AS CHAR NO-UNDO.
DEF VAR pcServiceStatus AS CHAR NO-UNDO.
DEF VAR pcSetServiceId  AS CHAR NO-UNDO.
DEF VAR pcTransId       AS CHAR NO-UNDO.
DEF VAR top_struct      AS CHAR NO-UNDO.

DEF VAR lcBarrStatus    AS CHAR NO-UNDO.
DEF VAR lcBarrComList   AS CHAR NO-UNDO.
DEF VAR lcStatus        AS CHAR NO-UNDO.
DEF VAR lcAppId         AS CHAR NO-UNDO. 
DEF VAR lcAccessRight   AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,string,string") EQ ?
THEN RETURN.

ASSIGN pcTransId = get_string(param_toplevel_id, "0")
       pcCLI     = get_string(param_toplevel_id,"1")
       pcServiceCode = get_string(param_toplevel_id,"2")
       pcServiceStatus = get_string(param_toplevel_id,"3").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcAppId = substring(pcTransId,1,3).

IF NOT fchkTMSCodeValues(gbAuthLog.UserName, lcAppId) THEN
   RETURN appl_err("Application Id does not match").

IF pcServiceCode EQ "Y_HURP_P" THEN DO:
   If lcAppId NE "650" THEN
      RETURN appl_err("Application Id does not match").
   lcAccessRight = "NewtonAd".
END.
ELSE lcAccessRight = "NewtonCC".

FIND MobSub NO-LOCK WHERE
     MobSub.CLI = pcCLI NO-ERROR.
IF NOT AVAILABLE MobSub THEN
   RETURN appl_err("Subscription not found").

IF LOOKUP(pcServiceStatus,"on,off") = 0 THEN
   RETURN appl_err("Invalid service status").

FIND FIRST CtServPac NO-LOCK WHERE
           CtServPac.Brand    = gcBrand        AND
           CtServPac.CliType  = MobSub.CliType AND
           CtServPac.ServPac  = pcServiceCode  AND
           CtServPac.ToDate   >= TODAY NO-ERROR.
IF NOT AVAIL CtServPac THEN
   RETURN appl_err("CLIType Service Package not found").

FIND FIRST ServPac  WHERE
           ServPac.Brand   = CTServPac.Brand  AND
           ServPAc.ServPac = CTServPac.ServPac NO-LOCK NO-ERROR.
IF NOT AVAIL ServPac THEN
   RETURN appl_err("Service Package not found").

/* Check ongoing service requests */
IF CAN-FIND(FIRST MsRequest WHERE
                  MsRequest.MsSeq      = MobSub.MsSeq AND
                  MsRequest.ReqType    = {&REQTYPE_SERVICE_CHANGE} AND
                  MsRequest.ReqCParam1 = ServPAc.ServPac AND
                  LOOKUP(STRING(MsRequest.ReqStatus),
                         {&REQ_INACTIVE_STATUSES}) = 0) THEN
   RETURN appl_err("Ongoing network command").

RUN checkmsbarring(INPUT MobSub.MsSeq,
                   INPUT lcAccessRight,
                   OUTPUT lcBarrComList,
                   OUTPUT lcBarrStatus).
CASE lcBarrStatus:
   WHEN "NAD" THEN RETURN appl_err("Operator or debt level barring is on").
   WHEN "ONC" THEN RETURN appl_err("Ongoing network command").
END. /* CASE lcBarrStatus: */
  
/* Check that barring is allowed */
/* Do not allow set/unset D_ barrings from newton */
IF INDEX(lcBarrComList,ServPAc.ServPac) = 0 OR
   ServPAc.ServPac BEGINS "D_" THEN 
RETURN appl_err("Barring is not allowed").

CASE pcServiceStatus:
   WHEN "on" THEN pcSetServiceId = ServPAc.ServPac. 
   WHEN "off" THEN pcSetServiceId = "UN" + ServPAc.ServPac. 
END.

/* Check that barring is already with same status */
IF LOOKUP(pcSetServiceId,lcBarrComList,"|") = 0 THEN 
   RETURN appl_err("Barring is already " + (IF pcServiceStatus = "on" then "active" ELSE "inactive")).
   
RUN barrengine(MobSub.MsSeq,
               pcSetServiceId,
               {&REQUEST_SOURCE_EXTERNAL_API},
               "",
               fMakeTS(),
               "",
               OUTPUT lcStatus).

CASE lcStatus:
   WHEN "ONC" THEN RETURN appl_err("Ongoing network command").
END.

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_boolean(top_struct, "result", True).

FINALLY:
   /* Store the transaction id */
   gbAuthLog.TransactionId = pcTransId.

   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
