/** 
 * RPC to test sending of SMS.
 *
 * @input       clinmbr;string;Number to send test SMS
                smscontent;string;Contain sms

 * @output      success;boolean;Result status of sending test SMS
*/
{xmlrpc/xmlrpc_access.i}
{commpaa.i}
{fmakesms.i}
{tmsconst.i}
gcBrand = "1".


DEFINE VARIABLE pcSmsContent AS CHAR NO-UNDO.
DEFINE VARIABLE pcCLI AS CHAR NO-UNDO.
DEFINE VARIABLE pcReqList AS CHAR NO-UNDO. 
DEFINE VARIABLE liSmsSuccess AS INT NO-UNDO.

pcReqList = validate_request(param_toplevel_id, "string,string").
IF pcReqList EQ ? THEN RETURN.

pcCLI        = get_string(param_toplevel_id, "0").
pcSmsContent = get_string(param_toplevel_id, "1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND MobSub NO-LOCK WHERE
     MobSub.Brand = gcBrand AND
     Mobsub.CLI   = pcCLI NO-ERROR.
IF NOT AVAIL MobSub THEN
   RETURN appl_err("Not found requested subscriber").

/* Sending test SMS */
liSmsSuccess = fMakeSMS(MobSub.CustNum,Mobsub.CLI,{&SMSTYPE_INFO},pcSmsContent).

IF  liSmsSuccess > 0 THEN
   add_boolean(response_toplevel_id,?,TRUE).
ELSE add_boolean(response_toplevel_id,?,FALSE).


FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.

