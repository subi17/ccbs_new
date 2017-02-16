/**
 * Set subscription terminal
 *
 * @input   id;string;terminal id
            sub_terminal;struct;mandatory;subscription terminal data
 * @sub_terminal reason;string;mandatory;
            sim_lock_code_viewable;boolean;mandatory;
            username;string;mandatory;newton username
 * @output  result;struct;empty
 */
 {Syst/commpaa.i}
{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Func/fmakemsreq.i}
{Func/msreqfunc.i}
{Func/fpcmaintreq.i}
{Func/fcreatereq.i}
gcBrand = "1".

DEF VAR pcStruct AS CHAR NO-UNDO. 
DEF VAR pcUsername AS CHAR NO-UNDO.
DEF VAR pcReason AS CHAR NO-UNDO.
DEF VAR pcId AS CHAR NO-UNDO.
DEF VAR piId AS INT NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO. 
DEF VAR liCustnum AS INT NO-UNDO.
DEF VAR plSimChecked AS LOGICAL NO-UNDO. 
DEF VAr pcIMEI AS CHAR NO-UNDO INITIAL "".
DEF VAR lcOldIMEI AS CHAR NO-UNDO.
DEF VAR pcTenant AS CHAR NO-UNDO.

IF validate_request(param_toplevel_id, "string,string,struct") EQ ? THEN RETURN.

pcTenant = get_string(param_toplevel_id, "0").
pcId     = get_string(param_toplevel_id, "1").
pcStruct = get_struct(param_toplevel_id, "2").

lcstruct = validate_struct(pcStruct, "username!,reason,sim_lock_code_viewable,imei").

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcUsername = "VISTA_" + get_string(pcStruct, "username").

IF LOOKUP("sim_lock_code_viewable",lcStruct) > 0 THEN DO:
   plSimChecked = get_bool(pcStruct, "sim_lock_code_viewable").
   pcReason = get_string(pcStruct, "reason").
END.

IF LOOKUP("imei",lcStruct) > 0 THEN DO:
   pcIMEI = get_string(pcStruct, "imei").
   IF LENGTH(pcIMEI,"CHARACTER") NE 15 THEN 
    RETURN appl_err("IMEI code doesn't contain 15 characters").
END.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUsername) EQ "VISTA_" THEN RETURN appl_err("username is empty").

piId = INTEGER(pcId) NO-ERROR.
IF ERROR-STATUS:ERROR THEN 
   RETURN appl_err(SUBST("Id &1 is not numerical", pcId)).

{newton/src/settenant.i pcTenant}

FIND SubsTerminal WHERE SubsTerminal.TerminalID = piId NO-LOCK NO-ERROR.
IF NOT AVAIL SubsTerminal then 
    return appl_err("Subscription terminal was not found").


FIND FIRST Order NO-LOCK WHERE Order.Brand EQ gcBrand AND Order.OrderId EQ Substerminal.OrderId NO-ERROR.
IF NOT AVAIL Order THEN
    appl_err("Order for device was not found").
   
/* if nothing will be updated then return empty */
IF ( LOOKUP("sim_lock_code_viewable",lcStruct) = 0 OR 
      Substerminal.SimChecked = plSimChecked ) AND
   LOOKUP("imei",lcStruct) = 0 THEN DO: 
   add_struct(response_toplevel_id, "").
   RETURN.
END.

{Syst/eventval.i}
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER pcUsername 
   {Func/lib/eventlog.i}
   DEF VAR lhSubsTerminal AS HANDLE NO-UNDO.
   lhSubsTerminal = BUFFER SubsTerminal:HANDLE.
   RUN StarEventInitialize(lhSubsTerminal).
   RUN StarEventSetOldBuffer(lhSubsTerminal).
END.
lcOldIMEI = SubsTerminal.IMEI.
FIND CURRENT SubsTerminal EXCLUSIVE-LOCK.
   ASSIGN 
    Substerminal.SimChecked = plSimChecked WHEN Substerminal.SimChecked NE plSimChecked
    Substerminal.IMEI = pcIMEI WHEN LOOKUP("imei",lcStruct) > 0 .

IF llDoEvent THEN DO:
   RUN StarEventMakeModifyEvent(lhSubsTerminal).
   fCleanEventObjects().
END.

/* create memo */
find mobsub where
   mobsub.msseq = SubsTerminal.msseq NO-LOCK no-error.
if not avail mobsub then do:
   find termmobsub where
      termmobsub.msseq = SubsTerminal.msseq NO-LOCK no-error.
   if avail termmobsub then liCustnum = termmobsub.custnum.
end.
else liCustnum = mobsub.custnum.

/* sim_lock_code_viewable memo */
IF LOOKUP("reason",lcStruct) > 0 THEN DO: 
   CREATE Memo.
   ASSIGN
       Memo.CreStamp  = {&nowTS}
       Memo.Brand     = gcBrand
       Memo.HostTable = "MobSub"
       Memo.KeyValue  = STRING(SubsTerminal.MsSeq)
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = pcUsername
       Memo.MemoTitle = "Sim lock code checked"
       Memo.MemoText  = pcReason
       Memo.CustNum   = licustnum.
END. 

RELEASE SubsTerminal.

/*IMEI change needs request because Document Management reads IMEI changes
from MsRequest.*/
IF LOOKUP("imei",lcStruct) > 0 THEN DO:

   fCreateRequest({&REQTYPE_IMEI_CHANGE}, /* heat balance query request */
                  0 , /* chgstamp */
                  "", /* creator */
                  FALSE, /* create fees */
                  FALSE). /* send sms */

   ASSIGN
      bCreaReq.msseq = Order.msseq
      bCreaReq.custnum = Order.custnum
      bCreaReq.CLI = Order.cli
      bCreaReq.reqcparam1 = lcOldIMEI 
      bCreaReq.reqcparam2 = pcIMEI /*new IMEI*/
      bCreaReq.reqcparam3 = Order.Offer
      bCreaReq.reqiparam1 = Order.OrderId
      bCreaReq.ReqSource  = {&REQUEST_SOURCE_NEWTON}.

   FIND MSRequest WHERE
        ROWID(MsRequest) EQ ROWID(bCreaReq) NO-LOCK.

   fReqStatus(2,"").
END.
   
add_struct(response_toplevel_id, "").
