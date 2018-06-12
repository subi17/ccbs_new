/**
 * Request service change(s).
 *
 * @input   msseq;int;mandatory;the subscription identifier
            userlevel;string;mandatory;user level (CustomerCare|Operator),
            user;string;mandatory;username to used with created MsRequest
            services;array;mandatory;array of service-structs
 * @service service_id;string;mandatory;newton alias for the service,
            value;string;mandatory;new value for the service
 * @output success;boolean
 *
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Mm/subser.i}
{Func/barrfunc.i}
{Mm/fbundle.i}
{Func/service.i}
{Func/profunc_request.i}
{Func/orderfunc.i}

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.
DEF VAR pcUserLevel AS CHAR NO-UNDO.
DEF VAR pcInputArray AS CHAR NO-UNDO.

DEF VAR pcStruct     AS CHAR NO-UNDO.
DEF VAR pcServiceId  AS CHAR NO-UNDO.
DEF VAR pcWebContractId AS CHAR NO-UNDO.
DEF VAR pcValue      AS CHAR NO-UNDO.
DEF VAR pcParam2     AS CHAR NO-UNDO.
DEF VAR pcParamOffer AS CHAR NO-UNDO.
DEF VAR pcParam      AS CHAR NO-UNDO.
DEF VAR lcStruct     AS CHAR NO-UNDO.
DEF VAR plSendSMS    AS LOGICAL NO-UNDO INITIAL TRUE.

DEF VAR liInputCounter AS INT NO-UNDO.
DEF VAR lcStatus AS CHAR NO-UNDO.
DEF VAR lcBarrComList AS CHARACTER NO-UNDO.
DEF VAR lcBarrStatus AS CHARACTER NO-UNDO. 
DEF VAR llRequest AS LOG NO-UNDO.
DEF VAR liValue AS INT NO-UNDO.
DEF VAR liValue2 AS INT NO-UNDO.
DEF VAR ldActStamp AS DEC NO-UNDO.
DEF VAR liReq AS INT NO-UNDO.
DEF VAR lcSalesman AS CHAR NO-UNDO.
DEF VAR lcInfo AS CHAR NO-UNDO.
DEF VAR lcServCom AS CHAR NO-UNDO.
DEF VAR ocError AS CHAR NO-UNDO.
DEF VAR lcErr AS CHAR NO-UNDO.
DEF VAR liValidate AS INT  NO-UNDO.
DEF VAR pcSetServiceId AS CHAR  NO-UNDO.
DEF VAR pcReqList      AS CHAR  NO-UNDO.
DEF VAR lcFromStat     AS CHAR  NO-UNDO.
DEF VAR lcToStat       AS CHAR  NO-UNDO.
DEF VAR lcBarringCode  AS CHAR  NO-UNDO.
DEF VAR orBarring      AS ROWID NO-UNDO.
DEF VAR lcOnOff        AS CHAR  NO-UNDO.
DEF VAR llOngoing      AS LOG   NO-UNDO.
DEF VAR liParams       AS INT   NO-UNDO.
DEF VAR liSVARequest   AS INT   NO-UNDO.
DEF VAR lcBundleType   AS CHAR  NO-UNDO.

DEF BUFFER bReq  FOR MsRequest.
DEF BUFFER bSubReq FOR MsRequest.

pcReqList = validate_request(param_toplevel_id, "int,array").
IF pcReqList EQ ? THEN RETURN.

piMsSeq = get_int(param_toplevel_id, "0").
pcInputArray = get_array(param_toplevel_id, "1").

IF NUM-ENTRIES(pcReqList) >= 3 THEN 
    RETURN appl_err("too many parameters").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST Mobsub WHERE Mobsub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE Mobsub THEN
    RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

ASSIGN Syst.Var:katun = "Newton".

DO liInputCounter = 1 TO 1 /*get_paramcount(pcInputArray) - 1*/:
   pcStruct = get_struct(pcInputArray, STRING(liInputCounter - 1)).

   lcStruct = validate_request(pcStruct,"service_id!,contract_id,value!,param,param2").
   IF lcStruct EQ ? THEN RETURN.

   pcServiceId = get_string(pcStruct, "service_id").
   IF LOOKUP("contract_id",lcStruct) > 0 THEN 
      pcWebContractId = get_string(pcStruct, "contract_id").
   pcValue = get_string(pcStruct, "value").

   IF LOOKUP('param', lcStruct) GT 0 THEN
    pcParam = get_string(pcStruct, "param").
   IF LOOKUP('param2', lcStruct) GT 0 THEN
    pcParam2 = get_string(pcStruct, "param2").

   IF gi_xmlrpc_error NE 0 THEN RETURN.

   FIND FIRST DayCampaign WHERE DayCampaign.Brand = Syst.Var:gcBrand AND DayCampaign.DCEvent = pcServiceId NO-LOCK NO-ERROR.
   IF AVAIL DayCampaign THEN 
   DO:
       CASE DayCampaign.BundleTarget:
           WHEN {&TELEVISION_BUNDLE} THEN
           DO:
               ASSIGN lcBundleType = (IF DayCampaign.BundleTarget = {&TELEVISION_BUNDLE} THEN "Television" ELSE "").

               CASE pcValue:
                   WHEN "off" THEN 
                       RUN pDeActivateTVService.
                   WHEN "on" THEN
                       RUN pActivateTVService.
                   OTHERWISE  
                       RETURN appl_err("Invalid parameter 'value': " + pcValue).
               END CASE.            
           END.
           WHEN {&DC_BUNDLE_TARGET_SVA} THEN
           DO: /*'off', 'on', 'cancel activation', 'cancel deactivation'*/
               IF pcValue BEGINS "activate" THEN 
               DO:
                   CASE DayCampaign.DCEvent:
                       WHEN "FAXTOEMAIL" THEN 
                       DO:
                           IF pcParam EQ "" OR pcParam2 EQ "" THEN  
                               RETURN appl_err("Missing SVA parameter"). 
                       END.
                       WHEN "OFFICE365" THEN 
                       DO:
                           IF pcParam EQ "" THEN  
                               RETURN appl_err("Missing SVA parameter").  
                       END.
                   END CASE.
               END.

               liSVARequest = fMakeProActRequest(MobSub.MsSeq,
                                                 pcServiceId,
                                                 0,
                                                 pcParam,
                                                 pcParam2,
                                                 pcValue,
                                                 lcErr). 
               
               IF lcErr NE "" OR liSVARequest EQ 0 OR liSVARequest EQ ? THEN 
                   RETURN appl_err("SVA request failure " + lcErr).
               /* Email is generated from percontr.p    
               lcErr = fSendEmailByRequest(liSVARequest,"SVA_" + pcServiceId).

               IF lcErr NE "" THEN 
                   RETURN appl_err("SVA email request failure " + lcErr).
               */
               FIND MsRequest WHERE MsRequest.MsRequest = liSVARequest EXCLUSIVE-LOCK NO-ERROR.
               ASSIGN MsRequest.Memo = MsRequest.Memo + (IF MsRequest.Memo > "" THEN ", "  ELSE "") + "WebContractID=" + pcWebContractId.  
               CREATE Memo.
               ASSIGN
                   Memo.CreStamp  = {&nowTS}
                   Memo.Brand     = Syst.Var:gcBrand
                   Memo.HostTable = "MobSub"
                   Memo.KeyValue  = STRING(Mobsub.MsSeq)
                   Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
                   Memo.CreUser   = "VISTA"
                   Memo.MemoTitle = "SVA operation"
                   Memo.MemoText  = pcValue + " " + pcServiceId
                   Memo.CustNum   = mobsub.custnum.

           END.
       END CASE.
   END.
END.   

add_boolean(response_toplevel_id, "", TRUE).

PROCEDURE pDeActivateTVService:
    DEFINE VARIABLE liRequest   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcResult    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE liServSeq   AS INTEGER   NO-UNDO.

    IF CAN-FIND(FIRST TPService WHERE TPService.MsSeq      = piMsSeq             AND 
                                      TPService.Operation  = {&TYPE_ACTIVATION}  AND 
                                      TPService.ServType   = lcBundleType        AND
                                      TPService.ServStatus <> {&STATUS_CANCELED} AND 
                                      TPService.ServStatus <> {&STATUS_ERROR}    NO-LOCK) THEN
    DO:
        IF NOT fDeactivateTVService(piMsSeq, pcParam2) THEN 
            RETURN appl_err("Cancellation failed.").  
    END.
    ELSE 
        RETURN appl_err("No tv service active for cancellation").

    RETURN "".

END PROCEDURE.

PROCEDURE pActivateTVService:
    DEF VAR liServSeq AS INT NO-UNDO.

    FIND FIRST TPService WHERE TPService.MsSeq       = piMsSeq            AND 
                               TPService.Operation   = {&TYPE_ACTIVATION} AND  
                               TPService.ServType    = lcBundleType       AND 
                               TPService.ServStatus <> {&STATUS_HANDLED}  AND 
                               TPService.ServStatus <> {&STATUS_CANCELED} AND 
                               TPService.ServStatus <> {&STATUS_ERROR}    NO-LOCK NO-ERROR.
    IF AVAIL TPService THEN 
        RETURN appl_err("There exists an ongoing tv service request.").

    ASSIGN liServSeq = fCreateNewTPService(piMsSeq, 
                                           pcServiceId, 
                                           "Huawei", 
                                           lcBundleType, 
                                           {&TYPE_ACTIVATION}, 
                                           {&STATUS_NEW}, 
                                           pcParam,   /* OfferId */
                                           pcParam2). /* UserCode */
    
    IF liServSeq > 0 THEN 
        fCreateTPServiceMessage(piMsSeq, liServSeq , {&SOURCE_TMS}, {&STATUS_NEW}).

    RETURN "".

END PROCEDURE.
