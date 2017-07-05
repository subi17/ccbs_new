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
gcBrand = "1".
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Mm/subser.i}
{Func/barrfunc.i}
{Mm/fbundle.i}
{Func/service.i}
{Func/vasfunc.i}
{Func/profunc.i}

/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.
DEF VAR pcUserLevel AS CHAR NO-UNDO.
DEF VAR pcInputArray AS CHAR NO-UNDO.

DEF VAR pcStruct AS CHAR NO-UNDO.
DEF VAR pcServiceId AS CHAR NO-UNDO.
DEF VAR pcValue AS CHAR NO-UNDO.
DEF VAR pcParam2 AS CHAR NO-UNDO.
DEF VAR pcParam AS CHAR NO-UNDO.
DEF VAR lcStruct AS CHAR NO-UNDO.
DEF VAR plSendSMS AS LOGICAL NO-UNDO INITIAL TRUE.

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
DEF VAR pcSetServiceId AS CHAR NO-UNDO.
DEF VAR pcReqList      AS CHAR NO-UNDO.
DEF VAR lcFromStat     AS CHAR NO-UNDO.
DEF VAR lcToStat       AS CHAR NO-UNDO.
DEF VAR lcBarringCode  AS CHAR NO-UNDO.
DEF VAR orBarring      AS ROWID NO-UNDO.
DEF VAR lcOnOff        AS CHAR NO-UNDO.
DEF VAR llOngoing      AS LOG NO-UNDO.
DEF VAR liParams       AS INT NO-UNDO.
DEF VAR liSVARequest   AS INT NO-UNDO.

DEF BUFFER bReq  FOR MsRequest.
DEF BUFFER bSubReq FOR MsRequest.

pcReqList = validate_request(param_toplevel_id, "int,array").
IF pcReqList EQ ? THEN RETURN.

piMsSeq = get_int(param_toplevel_id, "0").
pcInputArray = get_array(param_toplevel_id, "1").

IF NUM-ENTRIES(pcReqList) >= 3 THEN RETURN appl_err("too many parameters").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND Mobsub NO-LOCK
WHERE Mobsub.MsSeq = piMsSeq NO-ERROR.
IF NOT AVAILABLE Mobsub THEN
    RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

katun = "Newton".

DO liInputCounter = 1 TO 1 /*get_paramcount(pcInputArray) - 1*/:
   pcStruct = get_struct(pcInputArray, STRING(liInputCounter - 1)).

   lcStruct = validate_request(pcStruct,"service_id!,value!,param,param2").
   IF lcStruct EQ ? THEN RETURN.

   pcServiceId = get_string(pcStruct, "service_id").
   pcValue = get_string(pcStruct, "value").
   IF LOOKUP('param', lcStruct) GT 0 THEN
    pcParam = get_string(pcStruct, "param").
   IF LOOKUP('param2', lcStruct) GT 0 THEN
    pcParam2 = get_string(pcStruct, "param2").

   IF gi_xmlrpc_error NE 0 THEN RETURN.

   /*YPRO*/
   /*SVAs*/
   /*'off', 'on', 'cancel activation', 'cancel deactivation'*/
   IF fIsSVA(pcServiceId, OUTPUT liParams) THEN DO:
      IF liParams EQ 2 AND (pcValue BEGINS "activate") THEN DO:
         IF pcParam EQ "" OR pcParam2 EQ "" THEN
            RETURN appl_err("Missing SVA parameter").
      END.
      ELSE IF liParams EQ 1 AND (pcValue BEGINS "activate") THEN DO:
         IF pcParam EQ "" THEN
            RETURN appl_err("Missing SVA parameter").
      END.
      ELSE IF liParams EQ 0 THEN DO:
      END.
      liSVARequest = fMakeProActRequest(MobSub.MsSeq,
                                        pcServiceId,
                                        0,
                                        pcParam,
                                        pcParam2,
                                        pcValue,
                                        lcErr). 
      IF lcErr NE "" OR liSVARequest EQ 0 OR 
         liSVARequest EQ ? THEN RETURN appl_err("SVA request failure " + lcErr).
      lcErr = fSendEmailByRequest(liSVARequest,
                                  "SVA_" + pcServiceId).
      IF lcErr NE "" THEN RETURN appl_err("SVA email request failure " + lcErr).

      CREATE Memo.
      ASSIGN
          Memo.CreStamp  = {&nowTS}
          Memo.Brand     = gcBrand
          Memo.HostTable = "MobSub"
          Memo.KeyValue  = STRING(Mobsub.MsSeq)
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = "VISTA"
          Memo.MemoTitle = "SVA operation"
          Memo.MemoText  = pcValue + " " + pcServiceId
          Memo.CustNum   = mobsub.custnum.
   END. /*YPRO*/
END.   

add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.



