
/* ----------------------------------------------------------------------
  MODULE .......: susprequest.p 
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: jukka 
  CREATED ......: 30.10.07
  CHANGED ......: 05.02.08 "pending main request" as a part of new
                           barring handling
  Version ......: xfera
----------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{msreqfunc.i}
{fmakemsreq.i}
{eventval.i}
{tmsconst.i}
{service.i}
{main_add_lines.i}

DEFINE INPUT PARAMETER iiRequest AS INTEGER   NO-UNDO.

DEF VAR liReq           AS INT  NO-UNDO.
DEF VAR ocResult        AS CHAR NO-UNDO.
DEF VAR lcValidate      AS CHAR NO-UNDO.
DEF VAR liMasterRequest AS INT  NO-UNDO.
DEF VAR ldActStamp      AS DEC  NO-UNDO.
DEF VAR ldaActDate      AS DATE NO-UNDO.
DEF VAR liTime          AS INT  NO-UNDO.

/*For servicerequest handling */
DEF BUFFER bMsRequest FOR MsRequest. 
/* For Checking mandatory unbarring */
DEF BUFFER bMsReq     FOR MsRequest.

FIND MsRequest WHERE
     MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.

IF NOT AVAIL MsRequest OR MsRequest.ReqType NE 35 THEN RETURN "ERROR".

/* Store Master request request Id to be used in subrequest */
liMasterRequest = MsRequest.MsRequest.

FIND FIRST Mobsub WHERE
           Mobsub.MSSeq = MSRequest.MsSeq NO-LOCK NO-ERROR.

IF NOT AVAILABLE MobSub THEN DO:
   fReqError("MobSub not found"). 
   RETURN.
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}
END.

/* mainly for test purposes when dates are manipulated .. */
IF MsRequest.ActStamp > fMakeTS() THEN DO:
   ldActStamp = MsRequest.ActStamp.
   fSplitTS(MsRequest.ActStamp,
            OUTPUT ldaActDate,
            OUTPUT liTime).
END.
ELSE ASSIGN
   ldActStamp = fMakeTS()
   ldaActDate = TODAY.

RUN pMain.

fCleanEventObjects().

RETURN RETURN-VALUE.

/* note: ReqIParam3 and ReqIParam4 are in use for fraud tool limits */


PROCEDURE pMain:

   /* Request handling by status: */
   CASE MsRequest.ReqStat:

   /* New request, create subrequest for servicepac */
   WHEN 0 THEN DO:
      /* Request cannot be handled until barring removal has been 
      handled */
      IF MsRequest.ReqIParam2 > 0 THEN DO: 
         /* Is this mandatory for next barring package to be activated */
         FIND bMsReq NO-LOCK WHERE
              bMsReq.MsRequest = MsRequest.ReqIParam2
         NO-ERROR.
         
         CASE bMsReq.ReqStat:
          
            WHEN 2 THEN DO:
               /* Mandatory unbarring complete, run pending */
               RUN pNew(MsRequest.ReqCParam1,
                        ldaActDate,
                        ldActStamp,
                        MobSub.CliType,
                        MobSub.MsSeq).

               RETURN.
            END.
            WHEN 3 THEN DO:
               fReqStatus(3,"Package change failed see requests "  + 
                             STRING(MsRequest.MsRequest) + " and " +
                             STRING(MsRequest.ReqIParam2)).
               /* Unarring failed, discontinue further handling */
            END.
            OTHERWISE RETURN. /* Handling not done yet */
         END.
      END.
      /* No mandatory requests, handle immediately */ 
      ELSE RUN pNew(MsRequest.ReqCParam1, 
                        ldaActDate,
                        ldActStamp,
                        MobSub.CliType,
                        MobSub.MsSeq).
   END.
   WHEN 7 OR WHEN 8 THEN RUN pDone.

   END CASE.
   
END PROCEDURE.
   

PROCEDURE pNew:
    
   DEFINE INPUT PARAMETER icServPac  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER idtDate    AS DATE      NO-UNDO.
   DEFINE INPUT PARAMETER ideActTime AS DECIMAL   NO-UNDO.
   DEFINE INPUT PARAMETER icCliType  AS CHAR      NO-UNDO.
   DEFINE INPUT PARAMETER iiMsSeq    AS INTEGER   NO-UNDO. 
   
   DEFINE VARIABLE lcDefParam AS CHAR NO-UNDO. 

   IF idtDate = TODAY THEN ideActTime = fMakeTS().
   ELSE ideActTime = fMake2Dt(idtDate,10800).
   
   /* Assign master request (35) to 1 */
   IF NOT fReqStatus(1,"") THEN RETURN.
   
   /* Package from MsRequest */
   FIND FIRST CTServPac NO-LOCK WHERE
              CTServPac.Brand     = gcBrand   AND
              CTServPac.CLIType   = icCLIType AND
              CTServPac.ServPac   = icServPac AND
              CTServPac.FromDate <= idtDate   AND
              CTServPac.ToDate   >= idtDate
   NO-ERROR.
   
   IF NOT AVAIL CtServPac THEN DO:
      fReqError("ERROR, Unknown servpac!"). 
      RETURN.
   END.

   /* Now create components from service pac */
   FOR EACH CTServEl NO-LOCK WHERE
            CTServEl.Brand     = gcBrand            AND
            CTServEl.CLIType   = CTServPac.CLIType  AND
            CTServEl.ServPac   = CTServPac.ServPac  AND
            CTServEl.FromDate >= CTServPac.FromDate AND
            CTServEl.FromDate <= CTServPac.ToDate   AND
            CTServEl.FromDate <= idtDate,
         FIRST ServCom NO-LOCK WHERE
               ServCom.Brand    = gcBrand          AND
               ServCom.ServCom  = CTServEl.ServCom AND
               ServCom.Target   = 0
         BREAK BY CTServEl.ServPac
               BY ServCom.ScPosition
               BY CTServEl.ServCom
               BY CTServEl.FromDate DESC:

      /* use newest */
      IF NOT FIRST-OF(CTServEl.ServCom) THEN NEXT.

      lcDefParam = CTServEl.DefParam.
      /* Keep NAM service active if it has been activated manually  */
      /* hot-fix for YTS-4471 */
      IF ServCom.ServCom = "BARRING" THEN DO:

         FOR EACH bMsRequest WHERE
                  bMsRequest.MsSeq      = MsRequest.MsSeq AND
                  bMsRequest.ReqType    = {&REQTYPE_SERVICE_CHANGE} AND
                  bMsRequest.ReqStatus  = {&REQUEST_STATUS_DONE} AND
                  bMsRequest.ReqCparam1 = "NAM" AND
                  bMsRequest.ActStamp  <= MsRequest.ActStamp AND
                  bMsRequest.UserCode  <> "barr" NO-LOCK
            USE-INDEX MsSeq BY bMsRequest.UpdateStamp DESC:

            IF bMsRequest.ReqIparam1 EQ 1 THEN
               lcDefParam = SUBSTRING(CTServEl.DefParam,1,6) + "1".
            
            LEAVE.
         END. /* FOR FIRST bMsRequest WHERE */

         IF MobSub.CLIType EQ "CONTM2" THEN OVERLAY(lcDefParam,2) = "11".

      END. /* IF icServPac = "UNC_LOS" THEN DO: */

      /* Create subrequests (set mandataory and orig request) */ 
      liReq = fServiceRequest (iiMsSeq,
                               CTServEl.ServCom,
                               CTServEl.DefValue,
                               lcDefParam,
                               ideActTime + 0.00005, /* 5 sec delay */ 
                               "",                /* SalesMan */
                               FALSE,             /* Set fees */
                               FALSE,             /* SMS */
                               "",
                               "",
                               liMasterRequest,
                               TRUE,
                               OUTPUT ocResult).
      /* Change this request to subrequest */ 
      
      /* Creation of subrequests failed, "fail" master request too */
      IF liReq = 0 OR liReq = ? THEN DO:
         
         fReqStatus(3,"ServiceRequest failure: " + ocResult).
         RETURN.
      END.

   END.
   
   /* Subrequests created, assign Master request 
      to status 7 (it has new unhandled requests */
   fReqStatus(7,"").

END PROCEDURE.
 

PROCEDURE pDone.
 
   DEF VAR lcSMSText  AS CHAR NO-UNDO.
   DEF VAR ldSMSStamp AS DEC  NO-UNDO.

   DEF VAR liBBStat   AS INT  NO-UNDO.
   DEF VAR liReq      AS INT  NO-UNDO.
   DEF VAR lcResult   AS CHAR NO-UNDO.
   DEF VAR liBarr AS INT NO-UNDO. 
   DEF VAR llCancelSTC AS LOG NO-UNDO.
   DEF VAR lcBONOContracts  AS CHAR NO-UNDO.
   DEF VAR lcSender AS CHAR NO-UNDO.  
    
   DEF BUFFER bOLBRequest FOR MsRequest.
   DEF BUFFER lbMobsub FOR MobSub.

   /* All other statuses keep mainrequest waiting until HLR
      is done correctly */
   IF fGetSubRequestState(MsRequest.MsRequest) NE 2 THEN DO:
      IF MsRequest.ReqStat NE 7 THEN DO:
         /* back to waiting mode */
         fReqStatus(7,"").
      END.   
      RETURN.
   END.

   IF llDoEvent THEN DO:
      DEFINE VARIABLE lhMobsub AS HANDLE NO-UNDO.
      lhMobsub = BUFFER Mobsub:HANDLE.
      RUN StarEventInitialize(lhMobsub).
   END.

   /* subrequest handled succesfully */
      
   FIND MobSub WHERE
        MobSub.MsSeq = MsRequest.MsSeq
   EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

   IF LOCKED(MobSub) THEN RETURN.         
         
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobsub).
   IF MsRequest.ReqCParam1 BEGINS "UN" 
   THEN ASSIGN MobSub.MsStatus = 4
               MobSub.BarrCode = "".
   ELSE ASSIGN MobSub.MsStatus = 8
               MobSub.BarrCode = MsRequest.ReqCParam1.
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobsub).
   
   /* send SMS */
   IF MsRequest.SendSMS = 1 AND MsRequest.SMSText > "" THEN DO:

      FIND Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK.
            
      lcSMSText = fGetSMSTxt(MsRequest.SMSText,
                          TODAY,
                          Customer.Language,
                          OUTPUT ldSMSStamp).

      IF lcSMSText > "" THEN DO:                    
             
         lcSMSText = REPLACE(lcSMSText,"#MSISDN",MsRequest.CLI).
                
         fGetSMSSendRule(MsRequest.SMSText,
                         TODAY,
                         1,
                         OUTPUT lcSender).

         fMakeSchedSMS2(MobSub.CustNum,
                        MobSub.CLI,
                        (IF MsRequest.ReqCParam1 EQ "Y_HURP_P"
                         THEN {&SMSTYPE_PREMIUM} ELSE {&SMSTYPE_BARRING}),
                        lcSMSText,
                        ldSMSStamp,
                        lcSender,
                        "").
      END.          
   END. 
          
   /* remove old olb tags if this a new one */
   IF MsRequest.ReqCParam2 = "OLB" THEN
   FOR EACH bOLBRequest EXCLUSIVE-LOCK USE-INDEX MsSeq WHERE
            bOLBRequest.MsSeq      = MsRequest.MsSeq AND
            bOLBRequest.Reqtype    = 35              AND
            bOLBRequest.ReqStat    = 2               AND
            bOLBRequest.ReqCParam2 = "OLB"           AND
            RECID(bOLBRequest) NE RECID(MsRequest):
      bOLBRequest.ReqCParam2 = "".      
   END.
         
   /* memo for ifs events */
   IF MsRequest.ReqSource = {&REQUEST_SOURCE_IFS} THEN DO:
      CREATE Memo.
      ASSIGN 
         Memo.Brand     = gcBrand
         Memo.HostTable = "MobSub"
         Memo.KeyValue  = STRING(MsRequest.MsSeq)
         Memo.CustNum   = MsRequest.CustNum
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CreUser   = "IFS" 
         Memo.MemoType  = "service"
         Memo.MemoTitle = "Collection Action"
         Memo.MemoText  = IF MsRequest.ReqCParam1 BEGINS "UN"
                          THEN SUBSTRING(MsRequest.ReqCParam1,3) + " released"
                          ELSE MsRequest.ReqCParam1 + " applied".
         Memo.CreStamp  = fMakeTS().
   END.
   /* YDR-150 */
   ELSE IF MsRequest.UserCode = "Cron / TMQueue" AND
       NOT MsRequest.ReqCParam1 BEGINS "UN" AND
       CAN-FIND(FIRST TMRule NO-LOCK WHERE 
                      TMRule.TMRuleSeq = MsRequest.ReqIParam3 AND
                      TMRule.TicketType = {&TICKET_TYPE_FRAUD}) THEN DO:
      CREATE Memo.
      ASSIGN 
         Memo.Brand     = gcBrand
         Memo.HostTable = "MobSub"
         Memo.KeyValue  = STRING(MsRequest.MsSeq)
         Memo.CustNum   = MsRequest.CustNum
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CreUser   = "FRONT" 
         Memo.MemoType  = "service"
         Memo.MemoTitle = "Accion para prevencion de fraude en Roaming"
         Memo.MemoText  = MsRequest.ReqCParam1 + " aplicado".
         Memo.CreStamp  = fMakeTS().
   END.
      
   /* MobSub status update ok, Master request OK */
   fReqStatus(2,"").

   /* Activate/Suspend the BB service based on the fraud barring */
   IF LOOKUP(MsRequest.ReqCParam1,{&FRAUD_BARR_CODES}) > 0 OR
      LOOKUP(MsRequest.ReqCParam1,{&UNFRAUD_BARR_CODES}) > 0 THEN DO:
      FOR FIRST SubSer WHERE SubSer.ServCom = "BB" AND
                             SubSer.MsSeq   = MsRequest.MsSeq AND
                             SubSer.SsDate <= TODAY NO-LOCK:
         IF LOOKUP(MsRequest.ReqCParam1,{&UNFRAUD_BARR_CODES}) > 0 AND
            SubSer.SSStat = 2 AND
            fIsBBAllowed(MsRequest.MsSeq,ldActStamp)
         THEN liBBStat = 1.
         ELSE IF LOOKUP(MsRequest.ReqCParam1,{&FRAUD_BARR_CODES}) > 0 AND
            SubSer.SSStat = 1 THEN liBBStat = 2.

         IF liBBStat > 0 THEN DO:
            liReq = fServiceRequest(INPUT MsRequest.MsSeq,
                                 INPUT SubSer.ServCom,
                                 INPUT liBBStat,
                                 INPUT (IF liBBStat = 1 THEN "3" ELSE ""),
                                 INPUT ldActStamp,
                                 INPUT "",                /* SalesMan */
                                 INPUT TRUE,              /* Set fees */
                                 INPUT FALSE,             /* SMS      */
                                 INPUT "",
                                 INPUT "",
                                 INPUT liMasterRequest,   /* Father request */
                                 INPUT FALSE,
                                 OUTPUT lcResult).
            IF liReq = 0 THEN
               /* Write memo */
               DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                "MobSub",
                                STRING(MsRequest.MsSeq),
                                MsRequest.CustNum,
                                "Service Package",
                                "Error in BB service request: " + lcResult).
         END. /* IF liBBStat > 0 THEN DO: */
      END. /* FOR FIRST SubSer WHERE SubSer.ServCom = "BB" AND AND */

      /* Cancel Ongoing STC or subs. type change BTC request */
      IF LOOKUP(MsRequest.ReqCParam1,{&FRAUD_BARR_CODES}) > 0 THEN DO:
         FIND FIRST MsRequest WHERE
                    MsRequest.MsSeq = Mobsub.MSSeq AND
                    MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                    LOOKUP(STRING(MsRequest.ReqStatus),
                           {&REQ_INACTIVE_STATUSES}) = 0 AND
                    MsRequest.ActStamp >= ldActStamp NO-LOCK NO-ERROR.
         IF AVAIL MsRequest THEN DO:
            fReqStatus(4,"Cancelled due to fraud barring").
            llCancelSTC = TRUE.

            fAdditionalSimTermination(MsRequest.MsSeq,
                                     {&REQUEST_SOURCE_STC}).
         END. /* IF AVAIL MsRequest THEN DO: */

         IF NOT llCancelSTC THEN DO:
            lcBONOContracts = fCParamC("BONO_CONTRACTS").

            FIND FIRST MsRequest WHERE
                       MsRequest.MsSeq = Mobsub.MSSeq AND
                       MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
                       LOOKUP(STRING(MsRequest.ReqStatus),
                              {&REQ_INACTIVE_STATUSES}) = 0 AND
                       MsRequest.ActStamp >= ldActStamp AND
                       LOOKUP(MsRequest.ReqCparam1,lcBONOContracts) = 0
                 NO-LOCK NO-ERROR.
            IF AVAIL MsRequest THEN DO:
               fReqStatus(4,"Cancelled due to fraud barring").
               llCancelSTC = TRUE.

               fAdditionalSimTermination(MsRequest.MsSeq,
                                        {&REQUEST_SOURCE_BTC}).
            END. /* IF AVAIL MsRequest THEN DO: */
         END. /* IF NOT llCancelSTC THEN DO: */

         /* Trigger SMS */
         IF llCancelSTC THEN DO:
            FIND Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK.

            lcSMSText = fGetSMSTxt("CancelledSTCDueToBarring",
                                   TODAY,
                                   Customer.Language,
                                   OUTPUT ldSMSStamp).
            IF lcSMSText > "" THEN
               fMakeSchedSMS(MobSub.CustNum,
                             MobSub.CLI,
                             9,
                             lcSMSText,
                             ldSMSStamp).
         END. /* IF llCancelSTC THEN DO: */

         /* Find original request back */
         FIND FIRST MsRequest WHERE
                    MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.

      END. /* IF LOOKUP(MsRequest.ReqCParam1,{&FRAUD_BARR_CODES}) > 0 */
   END. /* IF LOOKUP(MsRequest.ReqCParam1,{&FRAUD_BARR_CODES}) > 0 THEN DO: */
     
   /* If primary MultiSIM subscription is barred,
      the secondary must be barred as well */
   IF NOT MsRequest.ReqCParam1 BEGINS "UN" AND
      MobSub.MultiSimID > 0 AND
      MobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} THEN DO:
      
      FIND FIRST lbMobsub NO-LOCK USE-INDEX MultiSimID WHERE
                 lbMobsub.Brand = gcBrand AND
                 lbMobsub.MultiSimID = MobSub.MultiSimID AND
                 lbMobsub.MultiSimType = {&MULTISIMTYPE_SECONDARY} AND
                 lbMobsub.Custnum = MobSub.Custnum
      NO-ERROR.

      IF AVAIL lbMobsub THEN DO:

         RUN barrengine.p(lbMobSub.MsSeq,
                          MsRequest.ReqCparam1,
                          {&REQUEST_SOURCE_MULTISIM},
                          "", /* creator */
                          fMakeTS(),
                          "", /* sms text */
                          OUTPUT lcResult).
          
         liBarr = 0.
         liBarr = INTEGER(lcResult) NO-ERROR. 
         
         IF liBarr = 0 THEN
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                             "MobSub",
                             STRING(lbMobsub.MsSeq),
                             lbMobsub.CustNum,
                             "Barring failed",
                             SUBST("Error: &1", lcResult)).
      END.
   END.
END PROCEDURE.

