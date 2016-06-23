/**
 * CF service management for voice mail and MCA
 *
 * @input       string;mandatory;msisdn
                struct;mandatory;CF Services
 * @struct      cfb;string;mandatory;0,MCA,VM
                cfnrc;string;mandatory;0,MCA,VM
                cfnry;string;mandatory;0,VM
 * @output      boolean;true
 * @Exceptions  1;Incorrect CFB value
                2;Incorrect CFNRC value
                3;Incorrect CFNRY value
                4;Subscription not found
                5;Subscription service not found
                6;Service component not found
                7;Service request failed
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO. 

/* Input parameters */
DEF VAR pcMSISDN AS CHAR NO-UNDO.
DEF VAR pcVMSSetting AS CHAR NO-UNDO. 
DEF VAR lcInfo AS CHAR NO-UNDO.

DEF VAR liRequest AS INT NO-UNDO. 

DEF VAR pcCFB AS CHAR NO-UNDO. 
DEF VAR pcCFNRC AS CHAR NO-UNDO. 
DEF VAR pcCFNRY AS CHAR NO-UNDO. 
DEF VAR lcSetting AS CHAR NO-UNDO. 

{Syst/commpaa.i}
ASSIGN
   katun = "IVR_" + ghAuthLog::EndUserId.
   gcBrand = "1".

{Func/fmakemsreq.i}

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.
pcMSISDN = get_string(param_toplevel_id,"0").
pcVMSSetting = get_struct(param_toplevel_id,"1").
IF gi_xmlrpc_error NE 0 THEN RETURN.

DEF VAR i AS INT NO-UNDO. 

validate_request(pcVMSSetting, "cfb!,cfnrc!,cfnry!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcCFB = get_string(pcVMSSetting,"cfb").
pcCFNRC = get_string(pcVMSSetting,"cfnrc").
pcCFNRY = get_string(pcVMSSetting,"cfnry").
IF gi_xmlrpc_error NE 0 THEN RETURN.

DEF VAR liLookUp AS INT NO-UNDO. 

liLookUp = LOOKUP(pcCFB,"0,VM,MCA").
IF liLookUp = 0 THEN RETURN appl_err("Incorrect CFB value").
lcSetting = lcSetting + STRING(liLookUp - 1).

liLookUp = LOOKUP(pcCFNRC,"0,VM,MCA").
IF liLookUp = 0 THEN RETURN appl_err("Incorrect CFNRC value").
lcSetting = lcSetting + STRING(liLookUp - 1).

liLookUp = LOOKUP(pcCFNRY,"0,VM").
IF liLookUp = 0 THEN RETURN appl_err("Incorrect CFNRY value").
lcSetting = lcSetting + STRING(liLookUp - 1).

FIND FIRST mobsub NO-LOCK WHERE 
           mobsub.brand = gcBrand AND
           mobsub.cli = pcMSISDN NO-ERROR.
IF NOT AVAILABLE mobsub THEN RETURN appl_err("Subscription not found").

FIND FIRST SubSer NO-LOCK WHERE
           SubSer.MsSeq = Mobsub.MsSeq AND
           SubSer.ServCom = "CF" USE-INDEX ServCom NO-ERROR.
IF NOT AVAIL SubSer THEN RETURN appl_err("Subscription service not found").

FIND FIRST ServCom NO-LOCK WHERE 
           ServCom.Brand = gcBrand AND
           ServCom.ServCom = SubSer.ServCom NO-ERROR.
IF NOT AVAIL ServCom THEN RETURN appl_err("Service component not found").
             
liRequest = fServiceRequest(MobSub.MsSeq,
                            Subser.ServCom,
                            3,
                            lcSetting,
                            fMakeTS(),
                            "",     /* salesman */
                            FALSE,  /* fees */
                            FALSE,  /* sms */
                            "",     /* creator */
                            {&REQUEST_SOURCE_EXTERNAL_API},
                            0,
                            FALSE,
                            OUTPUT lcInfo).

IF liRequest = 0 THEN DO:

   FIND FIRST MsRequest WHERE
              MsRequest.MsSeq = MobSub.MsSeq  AND
              MsRequest.ReqType = {&REQTYPE_SERVICE_CHANGE} AND
              MsRequest.ReqCParam1 = "CF" AND
              LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9,99") = 0
   NO-LOCK USE-INDEX MsSeq NO-ERROR.

   IF AVAIL MsRequest THEN DO:
      IF MsRequest.ReqStatus = {&REQUEST_STATUS_REJECTED}
      THEN lcInfo = "Failed request".
      ELSE lcInfo = "Ongoing request".
   END.
   ELSE lcInfo = "Service request failed".

   RETURN appl_err(lcInfo).
END.

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
