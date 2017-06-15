/**
 * Request service change(s).
 *
 * @input   msseq;int;mandatory;the subscription identifier
            userlevel;string;mandatory;user level (CustomerCare|Operator),
            user;string;mandatory;username to used with created MsRequest
            services;array;mandatory;array of service-structs
            Memo;struct;optional
 * @service service_id;string;mandatory;newton alias for the service,
            value;string;mandatory;new value for the service
 * @Memo    title;string;mandatory
            content;string;mandatory
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
DEF VAR liValidate AS INT  NO-UNDO.
DEF VAR pcSetServiceId AS CHAR NO-UNDO.
DEF VAR pcUser AS CHAR NO-UNDO.
DEF VAR cCheckMsBarringKatun AS CHAR NO-UNDO. 
DEF VAR pcMemoStruct   AS CHAR NO-UNDO.
DEF VAR lcMemoTitle    AS CHAR NO-UNDO.
DEF VAR lcMemoContent  AS CHAR NO-UNDO.
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

FUNCTION fLocalMemo RETURNS LOGIC
   (icHostTable AS CHAR,
    icKey       AS CHAR,
    iiCustNum   AS INT,
    icTitle     AS CHAR,
    icText      AS CHAR):

   CREATE Memo.
   ASSIGN
      Memo.Brand     = gcBrand
      Memo.CreStamp  = fMakeTS()
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.Custnum   = iiCustNum
      Memo.HostTable = icHostTable
      Memo.KeyValue  = icKey
      Memo.MemoType  = "service"
      Memo.CreUser   = katun
      Memo.MemoTitle = icTitle
      Memo.Memotext  = icText.
END FUNCTION.


pcReqList = validate_request(param_toplevel_id, "int,string,string,array,[struct]").
IF pcReqList EQ ? THEN RETURN.

piMsSeq = get_int(param_toplevel_id, "0").
pcUserLevel = get_string(param_toplevel_id, "1").
pcUser = "VISTA_" + get_string(param_toplevel_id, "2").
pcInputArray = get_array(param_toplevel_id, "3").

IF NUM-ENTRIES(pcReqList) >= 5 THEN DO:
   pcMemoStruct = get_struct(param_toplevel_id, "4").
   IF validate_request(pcMemoStruct,"title!,content!") EQ ? THEN RETURN.

   lcMemoTitle   = get_string(pcMemoStruct, "title").
   lcMemoContent = get_string(pcMemoStruct, "content").
END. /* IF NUM-ENTRIES(pcReqList) >= 5 THEN DO: */

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcUser) EQ "VISTA_" THEN RETURN appl_err("username is empty").

FIND Mobsub NO-LOCK
WHERE Mobsub.MsSeq = piMsSeq NO-ERROR.
IF NOT AVAILABLE Mobsub THEN
    RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

katun = pcUser.
IF pcUserLevel EQ "Operator" THEN 
   cCheckMsBarringKatun = "NewtonAd". 
ELSE 
   cCheckMsBarringKatun = "NewtonCC". 
/*YPR-4773*/
/*Activation is not allowed if fixed line provisioning is pending*/
IF (MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG}    /*16*/ OR 
    MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) /*17*/ THEN  
   RETURN appl_err("Mobile line provisioning is not complete").


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
   IF fIsSVA(pcServiceId, OUTPUT liParams) THEN DO:
      IF liParams EQ 2 THEN DO:
         IF pcParam EQ "" OR pcParam2 EQ "" THEN
            RETURN appl_err("Missing SVA parameter").
      END.
      ELSE IF liParams EQ 1 THEN DO:
         IF pcParam EQ "" OR pcParam2 EQ "" THEN
            RETURN appl_err("Missing SVA parameter").
      END.
      ELSE IF liParams EQ 0 THEN DO:
      END.
      liSVARequest = fMakeProActRequest(MobSub.MsSeq,
                                        pcServiceId,
                                        0,
                                        0).
   END. /*YPRO*/
   
   /* SERVICES */
   FOR FIRST SubSer NO-LOCK WHERE
     SubSer.MsSeq = Mobsub.MsSeq AND
     SubSer.ServCom = pcServiceId,
   FIRST ServCom NO-LOCK WHERE 
      ServCom.Brand = gcBrand AND
      ServCom.ServCom = pcServiceID:

      /* Check ongoing service requests */
      llRequest = CAN-FIND(FIRST MsRequest WHERE
                                 MsRequest.MsSeq      = MobSub.MsSeq AND
                                 MsRequest.ReqType    = 1       AND
                                 MsRequest.ReqCParam1 = SubSer.ServCom AND
                                 LOOKUP(STRING(MsRequest.ReqStatus),
                                        {&REQ_INACTIVE_STATUSES}) = 0).
      IF llRequest THEN RETURN appl_err("Ongoing network command").
     
      /* Check new value */ 
      CASE pcValue:
         WHEN "off" THEN liValue = 0. 
         WHEN "on"  THEN liValue = 1.
         OTHERWISE DO:
            liValue = INT(pcValue) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN   
               RETURN appl_err(SUBST("Unknown service value: &1", pcValue)).
         END.
      END.
 
      liValidate = fSubSerValidate(
         INPUT MobSub.MsSeq,
         INPUT Subser.ServCom,
         INPUT liValue,
         OUTPUT ocError).

      IF liValidate NE 0 THEN 
      CASE liValidate:
         WHEN 3 THEN RETURN appl_err("Ongoing network command").
         OTHERWISE appl_err("Service change is not allowed").
      END.

      liValidate = fSubSerSSStat(
         INPUT MobSub.MsSeq,
         INPUT Subser.ServCom,
         INPUT liValue,
         OUTPUT ocError).
     
      IF liValidate NE 0 THEN
         RETURN appl_err(SUBST("Unknown service value: &1", pcValue)).

      /* Return error if new value is same as existing value */ 
      IF SubSer.ServCom = "BB" AND
         (liValue = SubSer.SSStat OR (SubSer.SSStat = 2 AND liValue = 0)) THEN
         RETURN appl_err("Service is already " +
                         (IF liValue = 1 THEN "active" ELSE "suspended")).

      /* check the validity of change date */
      ldActStamp = fServiceActStamp(SubSer.MsSeq,
                                    SubSer.ServCom,
                                    liValue).
      IF ldActStamp > 0 THEN DO:
         fSplitTS(ldActStamp,
                  OUTPUT ldtActDate,
                  OUTPUT liReq).

         IF ldtActDate > SubSer.SSDate OR
            (DAY(ldtActDate) = 1 AND liReq < TIME - 120 AND
             DAY(SubSer.SSDate) NE 1)
         THEN .
         ELSE ldActStamp = fMakeTS().
      END.
      ELSE ldActStamp = fMakeTS().

      IF ldtActDate = TODAY
      THEN ldActStamp = fMakeTS().
      ELSE ldActStamp = fMake2DT(ldtActDate,1).

      IF pcParam EQ "" AND
         SubSer.ServCom NE "CF" AND /* YBU-2004 */
         LOOKUP(pcValue,"off") = 0 THEN pcParam = SubSer.SSParam.

      /* Special handling for Black Berry service */
      IF Subser.ServCom = "BB" THEN DO:
         /* Suspended */
         IF liValue = 0 AND SubSer.SSStat = 1 THEN
            liValue = 2.
         /* Resume - Pass the new status in parameter */
         ELSE IF liValue = 1 AND SubSer.SSStat = 2 THEN
            pcParam = "3".
      END. /* IF Subser.ServCom = "BB" THEN DO: */

      /* Extra pre-caution, if fraud barring is applied then */
      /* don't allow to activate the BB service              */
      IF Subser.ServCom = "BB" AND liValue = 1 THEN DO:
         llOngoing = fCheckBarrStatus(INPUT MobSub.MsSeq,
                                          OUTPUT lcBarringCode,
                                          OUTPUT orBarring).
         IF llOngoing EQ TRUE THEN DO:
            FIND FIRST MsRequest WHERE
                 ROWID(MsRequest) = orBarring AND
                 fIsInList(MsRequest.ReqCParam1, {&FRAUD_BARR_CODES}) EQ TRUE
                 NO-LOCK NO-ERROR.
            IF AVAILABLE MsRequest THEN
               RETURN appl_err("Ongoing Fraud Barring Request").
         END. /* IF lcBarringCode = "91" THEN DO: */
         IF fIsInList(lcBarringCode, {&FRAUD_BARR_CODES}) EQ TRUE THEN
            RETURN appl_err("BB service can not be activated since " +
                            "subscription has fraud barring").

         IF NOT fIsBBAllowed(Mobsub.MsSeq,ldActStamp)
         THEN RETURN appl_err("BB service can not be activated since " +
                     "subscription does not have active data bundle").
      END. /* IF liValue = 1 THEN DO: */
      /*YPR-1965 starts*/
      
      liReq = fServiceRequest(MobSub.MsSeq,
                              Subser.ServCom,
                              liValue,
                              pcParam,
                              ldActStamp,
                              lcSalesman,
                              TRUE,      /* fees */
                              plSendSMS,      /* sms */
                              "",
                              {&REQUEST_SOURCE_NEWTON},
                              0, /* father request */
                              false, /* mandatory for father request */
                              OUTPUT lcInfo).

      IF liReq = 0 THEN DO:
          RETURN appl_err("Change request was not accepted for service"
              + SubSer.ServCom + "; " + lcInfo).
      END.

      /* Create Memo */
      IF lcMemoTitle > "" THEN DO:
         IF SubSer.SSStat = 0 THEN lcFromStat = "cancelled".
         ELSE IF SubSer.SSStat = 1 THEN lcFromStat = "active".
         ELSE IF SubSer.SSStat = 2 THEN lcFromStat = "suspended".
         
         IF liValue = 0 THEN lcToStat = "cancelled".
         ELSE IF liValue = 1 THEN lcToStat = "active".
         ELSE IF liValue = 2 THEN lcToStat = "suspended".
 
         fLocalMemo(INPUT "MobSub",
                    INPUT STRING(MobSub.MsSeq),
                    INPUT MobSub.CustNum,
                    INPUT lcMemoTitle,
                    INPUT lcMemoContent + ". " + UPPER(pcServiceID) +
                          " service status is changed from " + lcFromStat +
                          " to " + lcToStat).
      END. /* IF lcMemoTitle > "" THEN DO: */
      add_boolean(response_toplevel_id, "", TRUE).
      RETURN.
   END.
   
   /* Additional logic to add the new BB/LTE service to the subscription */
   IF LOOKUP(pcServiceID,"BB,LTE") > 0 AND pcValue = "ON" THEN DO:
   
      FIND FIRST ServCom WHERE
                 ServCom.Brand = gcBrand AND
                 ServCom.ServCom = pcServiceID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ServCom THEN RETURN appl_err("Invalid Service Id").

      IF CAN-FIND(FIRST MsRequest WHERE
                        MsRequest.MsSeq      = MobSub.MsSeq AND
                        MsRequest.ReqType    = 1       AND
                        MsRequest.ReqCParam1 = ServCom.ServCom AND
                        LOOKUP(STRING(MsRequest.ReqStatus),
                               {&REQ_INACTIVE_STATUSES}) = 0) THEN
         RETURN appl_err("Ongoing network command").

      /* Extra logic for BB service */
      IF pcServiceID = "BB" THEN DO:

         /* Check BB allowed */
         IF NOT fIsBBAllowed(Mobsub.MsSeq,ldActStamp) THEN
            RETURN appl_err("BB service can not be activated since " +
                            "subscription does not have active data bundle").

         /* Extra pre-caution, if fraud barring is applied then */
         /* don't allow to activate the BB service              */

         llOngoing = fCheckBarrStatus(INPUT MobSub.MsSeq,
                                      OUTPUT lcBarringCode,
                                      OUTPUT orBarring).
         IF llOngoing EQ TRUE THEN DO:
            FIND FIRST MsRequest WHERE
                 ROWID(MsRequest) = orBarring AND
                 fIsInList(MsRequest.ReqCparam1, {&FRAUD_BARR_CODES}) EQ TRUE
                 NO-LOCK NO-ERROR.
            IF AVAILABLE MsRequest THEN
               RETURN appl_err("Ongoing Fraud Barring Request").
         END. /* IF lcBarringCode = "91" THEN DO: */
         IF fIsInList(lcBarringCode,  {&FRAUD_BARR_CODES}) EQ TRUE THEN
            RETURN appl_err("BB service can not be activated since " +
                            "subscription has fraud barring").

      END. /* IF pcServiceID = "BB" THEN DO: */

      liReq = fServiceRequest(MobSub.MsSeq,
                              ServCom.ServCom,
                              1,
                              pcParam,
                              fMakeTS(),
                              lcSalesman,
                              TRUE,      /* fees */
                              plSendSMS,      /* sms */
                              "",
                              {&REQUEST_SOURCE_NEWTON},
                              0, /* father request */
                              false, /* mandatory for father request */
                              OUTPUT lcInfo).
      IF liReq = 0 THEN
         RETURN appl_err("New request was not accepted for service" +
                         SubSer.ServCom + "; " + lcInfo).

      IF lcMemoTitle > "" THEN
         fLocalMemo(INPUT "MobSub",
                    INPUT STRING(MobSub.MsSeq),
                    INPUT MobSub.CustNum,
                    INPUT lcMemoTitle,
                    INPUT lcMemoContent + ". " + UPPER(pcServiceID) +
                          " service status is changed to active.").

      add_boolean(response_toplevel_id, "", TRUE).
      RETURN.
   END. /* IF LOOKUP(pcServiceID,"BB,LTE") > 0 AND pcValue = "ON" THEN DO: */
   /* SERVICES END */

END. 

add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.



