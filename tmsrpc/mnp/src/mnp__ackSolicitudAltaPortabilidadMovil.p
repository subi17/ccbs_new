/**
 * Get data bundle status 
 *
 * @input  codigoReferencia;string;mandatory;MNP OUT process reference code
           action;string;mandatory;Action value (confirm or cancel)
 * @output boolean;True
 * @Exceptions  1; Incorrect action &1
                2; MNP process not found: &1
                3: Incorrect MNP type: &1
                4: Incorrect current MNP status: &1
                5: Confirmation message not found: &1
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}

DEFINE INPUT PARAMETER icrequest AS LONGCHAR NO-UNDO.

DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR lcPortRequest AS CHAR NO-UNDO. 
DEF VAR lcAction AS CHAR NO-UNDO. 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.
pcStruct = get_struct(param_toplevel_id, "0").
validate_struct(pcStruct, "codigoReferencia!,action!").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   lcPortRequest = get_string(pcStruct,"codigoReferencia")
   lcAction = get_string(pcStruct,"action").
IF gi_xmlrpc_error NE 0 THEN RETURN.

IF LOOKUP(lcAction,"confirm,cancel") = 0 THEN
   RETURN appl_err(SUBST("Incorrect action &1", lcAction)).

FIND MNPProcess EXCLUSIVE-LOCK WHERE
     MNPProcess.PortRequest = lcPortRequest NO-ERROR.
IF NOT AVAIL MNPProcess THEN 
   RETURN appl_err(SUBST("MNP process not found: &1", lcPortRequest)).

IF MNPProcess.MNPType NE {&MNP_TYPE_OUT} THEN
   RETURN appl_err(SUBST("Incorrect MNP type: &1",
                         MNPProcess.MNPType)).

FIND FIRST MNPOperation EXCLUSIVE-LOCK WHERE
     MNPOperation.MNPSeq = MNPProcess.MNPSeq AND
     MNPOperation.MessageType = "confirmarSolicitudAltaPortabilidadMovil" AND
    (MNPOperation.StatusCode EQ {&MNP_MSG_WAITING_CONFIRM} OR
     MNPOperation.StatusCode EQ {&MNP_MSG_WAITING_RESPONSE_HANDLE} OR
     MNPOperation.StatusCode EQ {&MNP_MSG_HANDLED}) NO-ERROR.
IF NOT AVAIL MNPOperation THEN
   RETURN appl_err(SUBST("Confirmation message not found: &1", lcPortRequest)).

IF MNPOperation.StatusCode EQ {&MNP_MSG_WAITING_CONFIRM} AND
   MNPProcess.StatusCode NE {&MNP_ST_ASOL} THEN
   RETURN appl_err(SUBST("&1: Incorrect current MNP process status: &2",
                          MNPProcess.PortRequest,
                          MNPProcess.StatusCode)).
   
FOR EACH MNPSub EXCLUSIVE-LOCK WHERE
         MNPSub.MNPSeq = MNPProcess.MNPSeq AND
         MNPSub.StatusReason = "":
   MNPSub.StatusReason = UPPER(lcAction).
END.

ASSIGN
   MNPProcess.StateFlag = {&MNP_STATEFLAG_CONFIRM}
   MNPOperation.StatusCode = {&MNP_MSG_WAITING_RESPONSE_HANDLE} WHEN
      MNPOperation.StatusCode EQ {&MNP_MSG_WAITING_CONFIRM}.
      
CREATE MNPOperation.
ASSIGN MNPOperation.MNPOperationID = NEXT-VALUE(MNPOperSeq)
       MNPOperation.MNPSeq = MNPProcess.MNPSeq
       MNPOperation.CreatedTS = {&nowts} /*ttInput.statusTS */
       MNPOperation.Sender = {&MNP_SENDER_ADAPTER}
       MNPOperation.SentTS = {&nowts} /* tms receive time */
       MNPOperation.MessageType = "ackSolicitudAltaPortabilidadMovil"
       MNPOperation.StatusCode = {&MNP_MSG_HANDLED}.
COPY-LOB FROM icrequest to MNPOperation.XMLRequest.

add_boolean(response_toplevel_id,"",TRUE).
