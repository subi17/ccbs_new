/** IVR MNP Status query
 *
 * @input       string;mandatory;IVR MNP Status query
 * @output      struct;response or response_error
 * @response    creation_date;datetime
                status;string
                sub_status;string
                porting_date;datetime
 * @response_error errors;string

 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}

/* Input parameters */
DEF VAR pcMSISDN AS CHAR NO-UNDO.

DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR lcCode AS CHAR NO-UNDO.
DEF VAR ldeCreated AS DECIMAL NO-UNDO INIT 20000101.
DEF VAR lrMNPProcess AS ROWID NO-UNDO.

IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

pcMSISDN = get_string(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FOR EACH MNPSub WHERE MNPSub.CLI = pcMSISDN TENANT-WHERE TENANT-ID() > -1 NO-LOCK:

   FIND MNPProcess WHERE
        MNPProcess.MNPSeq = MNPSub.MNPSeq AND
        MNPProcess.MNPType = {&MNP_TYPE_IN} NO-LOCK NO-ERROR.

   IF NOT AVAIL MNPProcess THEN NEXT.

   IF ldeCreated < MNPProcess.CreatedTS THEN DO:
      ASSIGN
         ldeCreated = MNPProcess.CreatedTS
         lrMNPProcess = ROWID(MNPProcess).
   END.
      
END.

lcStruct = add_struct(response_toplevel_id, "").

IF lrMNPProcess EQ ? THEN DO:
   add_string(lcStruct, "errors", "Error Consulta").
   RETURN.
END.

FIND MNPProcess WHERE
     ROWID(MNPProcess) = lrMNPProcess NO-LOCK.

FIND TMSCodes WHERE
     TMSCodes.TableName = "MNPProcess" AND
     TMSCodes.FieldName = "StatusCode" AND
     TMSCodes.CodeGroup = "MNP" AND
     TMSCodes.CodeValue = STRING(MNPProcess.StatusCode) NO-LOCK NO-ERROR.

IF AVAIL TMSCodes THEN
   lcCode = (IF TMSCodes.CodeName = "AREC_CLOSED" THEN "AREC" ELSE TMSCodes.CodeName).
ELSE lccode = STRING(MNPProcess.StatusCode).

add_timestamp(lcStruct, "creation_date", mnpprocess.createdts).
add_string(lcStruct, "status", lcCode).
add_string(lcStruct, "sub_status", MNPProcess.StatusReason).
add_timestamp(lcStruct, "porting_date", MNPProcess.PortingTime).
