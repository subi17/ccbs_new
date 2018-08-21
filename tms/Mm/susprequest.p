
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

{Syst/commali.i}
{Func/msreqfunc.i}
{Func/fmakemsreq.i}
{Syst/eventval.i}
{Syst/tmsconst.i}
{Func/service.i}
{Func/add_lines_request.i}
{Func/barrfunc.i}

DEFINE INPUT PARAMETER iiRequest AS INTEGER   NO-UNDO.

DEF VAR lcValidate      AS CHAR NO-UNDO.
DEF VAR liMasterRequest AS INT  NO-UNDO.
DEF VAR ldActStamp      AS DEC  NO-UNDO.
DEF VAR ldaActDate      AS DATE NO-UNDO.
DEF VAR liTime          AS INT  NO-UNDO.
DEF VAR liRemHRLPReq    AS INT  NO-UNDO.
DEF VAR liLinkReq       AS INT  NO-UNDO.
/*For servicerequest handling */
DEF BUFFER bMsRequest FOR MsRequest. 
/* For Checking mandatory unbarring */
DEF BUFFER bMsReq     FOR MsRequest.

FIND MsRequest WHERE
     MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.

IF NOT AVAIL MsRequest OR MsRequest.ReqType NE 35 THEN RETURN "ERROR".
IF MsRequest.MsSeq EQ 0 THEN RETURN "ERROR".

/* Store Master request request Id to be used in subrequest */
liMasterRequest = MsRequest.MsRequest.

FIND FIRST Mobsub WHERE
           Mobsub.MSSeq = MSRequest.MsSeq NO-LOCK NO-ERROR.

IF NOT AVAILABLE MobSub THEN DO:
   fReqError("MobSub not found"). 
   RETURN.
END.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
END.

/* mainly for test purposes when dates are manipulated .. */
IF MsRequest.ActStamp > Func.Common:mMakeTS() THEN DO:
   ldActStamp = MsRequest.ActStamp.
   Func.Common:mSplitTS(MsRequest.ActStamp,
            OUTPUT ldaActDate,
            OUTPUT liTime).
END.
ELSE ASSIGN
   ldActStamp = Func.Common:mMakeTS()
   ldaActDate = TODAY.

RUN pMain.

RETURN RETURN-VALUE.

FINALLY:
   fCleanEventObjects().
   EMPTY TEMP-TABLE ttProvCommand NO-ERROR.
   EMPTY TEMP-TABLE ttMergedBarring NO-ERROR.
   EMPTY TEMP-TABLE ttBarringCmd NO-ERROR.
END.

/* note: ReqIParam3 and ReqIParam4 are in use for fraud tool limits */

PROCEDURE pMain:

   /* Request handling by status: */
   CASE MsRequest.ReqStat:

   /* New request, create subrequest for servicepac */
   WHEN 0 then do:
   
      IF MsRequest.ReqIParam2 > 0 THEN DO:
         /* Is this mandatory for next barring package to be activated */
         FIND bMsReq NO-LOCK WHERE
              bMsReq.MsRequest = MsRequest.ReqIParam2
         NO-ERROR.

         CASE bMsReq.ReqStat:

            WHEN 2 THEN DO TRANS:
               /* Mandatory unbarring complete, run pending */
               RUN pNew(MsRequest.ReqCParam1,
                        ldaActDate,
                        ldActStamp).
               IF INDEX(RETURN-VALUE, "Error") > 0 THEN
                   fReqStatus(3, RETURN-VALUE).
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
      ELSE DO TRANS:
         RUN pNew(MsRequest.ReqCParam1, 
                  ldaActDate,
                  ldActStamp).
         IF INDEX(RETURN-VALUE, "Error") > 0 THEN
            fReqStatus(3, RETURN-VALUE).
         RETURN.
      END.              
   END.
   WHEN 7 OR WHEN 8 THEN RUN pDone.

   END CASE.
   
END PROCEDURE.

PROCEDURE pNew:

   DEFINE INPUT PARAMETER icBarrings AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER idtDate    AS DATE      NO-UNDO.
   DEFINE INPUT PARAMETER ideActTime AS DECIMAL   NO-UNDO.

   DEF VAR lcError AS CHAR NO-UNDO.
   DEF VAR liReq AS INT  NO-UNDO.
   DEF VAR lcFinalMask AS CHAR NO-UNDO.
   DEF VAR liUnbarrReq AS INT NO-UNDO.
   DEF VAR liDelay AS INT NO-UNDO.

   IF idtDate = TODAY THEN ideActTime = Func.Common:mMakeTS().
   ELSE ideActTime = Func.Common:mMake2DT(idtDate,10800).

   /* Assign master request (35) to 1 */
   IF NOT fReqStatus(1,"") THEN RETURN "".

   IF icBarrings BEGINS "#REFRESH" THEN DO:

      fRefreshBarrings(MobSub.MsSeq,
                       icBarrings,
                       OUTPUT TABLE ttMergedBarring).

   END.
   ELSE DO:

      IF fBarringListToTT(icBarrings,
                          OUTPUT TABLE ttBarringCmd,
                          OUTPUT lcError) EQ FALSE THEN DO:
         fReqError(lcError).
         RETURN "".
      END.

      IF fMergeBarrings(MobSub.MsSeq,
                        INPUT TABLE ttBarringCmd BY-REFERENCE,
                        OUTPUT TABLE ttMergedBarring,
                        OUTPUT lcError) EQ FALSE THEN DO:
          MESSAGE lcError VIEW-AS ALERT-BOX.
         fReqError(lcError).
         RETURN "".
      END.
   END.

   fBuildBarringCommand(MobSub.CLIType,
                        MsRequest.ReqSource,
                        INPUT TABLE ttMergedBarring BY-REFERENCE,
                        OUTPUT TABLE ttProvCommand,
                        OUTPUT lcFinalMask,
                        OUTPUT lcError).

   FOR EACH ttProvCommand:

      FIND ServCom NO-LOCK WHERE
           ServCom.Brand   = Syst.Var:gcBrand AND
           ServCom.ServCom = ttProvCommand.component NO-ERROR.

      IF NOT AVAILABLE ServCom THEN DO:
         fReqError("SYSTEM ERROR:BARRING service not defined").
         RETURN "".
      END.

      FIND FIRST CTServEl NO-LOCK WHERE
                 CTServEl.Brand     = Syst.Var:gcBrand   AND
                 CTServEl.ServCom   = ServCom.ServCom AND
                 CTServEl.CLIType   = MobSub.CLIType AND
                 CTServEl.FromDate <= TODAY NO-ERROR.

      IF NOT AVAIL CTServEl THEN DO:
         fReqError("SYSTEM ERROR:BARRING service not defined for " +
            MobSub.CLIType).
         RETURN "".
      END.

      lcError = fChkRequest(MobSub.MsSeq,
                            1,
                            CTServEl.ServCom,
                            "").
      IF lcError > "" THEN DO:
         fReqStatus(3,SUBST("Barring component &1: &2",
                             CTServEl.ServCom,
                             lcError)).
         RETURN "".
      END.

   END.

   FOR EACH ttProvCommand:
  
      /* a quick fix to send provisioning commands in correct order */
      IF MobSub.CLIType EQ "CONTM2" AND
         MsRequest.ReqCParam1 EQ "#REFRESH" AND
         ttProvCommand.Component NE "HOTLINE" AND
         CAN-FIND(FIRST ttProvCommand WHERE
                        ttProvCommand.Component EQ "HOTLINE") THEN
          liDelay = 10.
      ELSE liDelay = 5.
      
      /*YPR-3866: ensure that HRLP redirection removal is done before
      anothe LP activation*/
      IF ttProvCommand.DropService EQ "HRLP" THEN liDelay = liDelay + 5.
      
      /* Create subrequests (set mandataory and orig request) */
      liReq = fServiceRequest (MobSub.MsSeq,
                               ttProvCommand.Component,
                               (IF ttProvCommand.ComponentParam NE "" THEN 1
                                ELSE ttProvCommand.ComponentValue),
                               ttProvCommand.ComponentParam,
                               Func.Common:mSecOffSet(ideActTime,liDelay),
                               "",                /* SalesMan */
                               FALSE,             /* Set fees */
                               FALSE,             /* SMS */
                               "",
                               {&REQUEST_SOURCE_BARRING},
                               liMasterRequest,
                               TRUE,
                               OUTPUT lcError).

      /* Creation of subrequests failed, "fail" master request too */
      IF liReq = 0 OR liReq = ? THEN DO:
         UNDO, RETURN SUBST("ERROR:Base ServiceRequest failure: &1", lcError).
      END.

      /* extra request to reset barring status before hotline activation */
      liUnbarrReq = 0.
      IF ttProvCommand.BarringCmd NE "" THEN DO:

         /* Create subrequests (set mandataory and orig request) */
         liUnbarrReq = fServiceRequest (MobSub.MsSeq,
                                  "BARRING",
                                  1,
                                  ttProvCommand.BarringCmd,
                                  Func.Common:mSecOffSet(ideActTime,5), /* 5 sec delay */
                                  "",                /* SalesMan */
                                  FALSE,             /* Set fees */
                                  FALSE,             /* SMS */
                                  "",
                                  {&REQUEST_SOURCE_BARRING},
                                  liReq,
                                  TRUE,
                                 OUTPUT lcError).
         IF liUnbarrReq = 0 OR liUnbarrReq = ? THEN DO:
            UNDO, RETURN SUBST("ERROR:Unbarr ServiceRequest failure: &1", 
                                lcError).
         END.         
      END.
      
      
      liRemHRLPReq = 0.
      IF ttProvCommand.DropService EQ "HRLP" THEN DO:
         FIND FIRST SubSer NO-LOCK WHERE
                    SubSer.MsSeq EQ Mobsub.MsSeq AND
                    SubSer.ServCom EQ "LP" NO-ERROR.
         IF AVAIL SubSer AND SubSer.SSParam EQ "REDIRECTION_HIGHRISKCUSTOMER_1"
         THEN DO:
            liRemHRLPReq =  fServiceRequest (MobSub.MsSeq,
                                     "LP",
                                     1,
                                     "remove",
                                     Func.Common:mSecOffSet(ideActTime,5), /* 5 sec delay, 
                                                must be before LP activation*/
                                     "",                /* SalesMan */
                                     FALSE,             /* Set fees */
                                     FALSE,             /* SMS */
                                     "",
                                     {&REQUEST_SOURCE_BARRING},
                                     liReq,
                                     TRUE,
                                     OUTPUT lcError).
            IF liRemHRLPReq = 0 OR liRemHRLPReq = ? THEN DO:
               UNDO, RETURN SUBST("ERROR:HRLP ServiceRequest failure").
            END.
         END.   
      END.
   END.

   FIND CURRENT MsRequest EXCLUSIVE-LOCK.
   ASSIGN
      MsRequest.ReqCParam5 = lcFinalMask.

   IF liReq > 0 THEN fReqStatus(7,""). /* wait provisioning sub requests */
   ELSE fReqStatus(8,""). /* provisioning not required */
   RETURN "".
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
   DEF VAR lrCurrentBarring AS ROWID NO-UNDO.
   DEF VAR lcError AS CHAR NO-UNDO. 
   DEF VAR llRefreshBarring AS LOG NO-UNDO. 
   DEF VAR lcUserName  AS CHAR NO-UNDO.
   DEF VAR lcMemoTitle AS CHAR NO-UNDO.
    
   DEF BUFFER bMsRequest FOR MsRequest.
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
   
   /* subrequest handled succesfully */
   IF llDoEvent THEN DO:
      DEFINE VARIABLE lhMobsub AS HANDLE NO-UNDO.
      lhMobsub = BUFFER Mobsub:HANDLE.
      RUN StarEventInitialize(lhMobsub).
   END.
      
   /* refresh affects only to network  */
   IF MsRequest.ReqCParam1 BEGINS "#REFRESH" THEN DO:

      IF MsRequest.ReqCParam1 EQ "#REFRESH" AND
         MobSub.MsStatus = 8 AND
         MsRequest.ReqCParam5 ne "" THEN DO:

         FIND CURRENT MobSub EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF LOCKED(MobSub) THEN RETURN.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobsub).
         MobSub.BarrCode = MsRequest.ReqCParam5.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobsub).

         FIND CURRENT MobSub NO-LOCK.
      END.
      fReqStatus(2,"").
      RETURN.
   END.

   IF fBarringListToTT(MsRequest.ReqCParam1,
                       OUTPUT TABLE ttBarringCmd,
                       OUTPUT lcError) EQ FALSE THEN DO:
      fReqError(lcError).
      RETURN.
   END.

   IF fMergeBarrings(MobSub.MsSeq,
                     INPUT TABLE ttBarringCmd BY-REFERENCE,
                     OUTPUT TABLE ttMergedBarring,
                     OUTPUT lcError) EQ FALSE THEN DO:
      fReqError(lcError).
      RETURN.
   END.

   FIND CURRENT MobSub EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
   IF LOCKED(MobSub) THEN RETURN.         
         
   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobsub).
   IF INDEX(MsRequest.ReqCParam5,"1") = 0
   THEN DO: 
      IF NOT (MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} OR
              MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) THEN
         ASSIGN MobSub.MsStatus = {&MSSTATUS_ACTIVE}.
      ASSIGN  MobSub.BarrCode = "".
   END.
   ELSE DO:
      IF NOT (MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} OR
              MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) THEN
         ASSIGN MobSub.MsStatus = {&MSSTATUS_BARRED}.
      ASSIGN MobSub.BarrCode = MsRequest.ReqCParam5.
   END.
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobsub).
   
   FIND CURRENT MobSub NO-LOCK.

   FOR EACH ttMergedBarring:
      
      IF ttMergedBarring.NWStatus EQ "EXISTING" THEN NEXT.

      CREATE Barring.
      ASSIGN
         Barring.MsSeq = MobSub.MsSeq
         Barring.BarringCode = ttMergedBarring.BarrCode
         Barring.BarringStatus = ttMergedBarring.BarrStatus
         Barring.EventTS = Func.Common:mMakeTS() 
         Barring.UserCode = MsRequest.UserCode
         Barring.MSrequest = MSrequest.MSrequest.
   END.

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
                        (IF MsRequest.ReqCParam1 EQ "Limits_TotalPremium_Off=1"
                         THEN {&SMSTYPE_PREMIUM} ELSE {&SMSTYPE_BARRING}),
                        lcSMSText,
                        ldSMSStamp,
                        lcSender,
                        "").
      END.          
   END. 
          
   /* YDR-150 - Limit rules barring */
   IF MsRequest.UserCode = "Cron / TMQueue" AND
      INDEX(MsRequest.ReqCParam1,"=1") > 0  AND
      MsRequest.ReqIParam3 > 0 THEN DO:
      IF CAN-FIND(FIRST TMRule NO-LOCK WHERE 
                        TMRule.TMRuleSeq = MsRequest.ReqIParam3 AND
                        TMRule.TicketType = {&TICKET_TYPE_FRAUD}) THEN
         ASSIGN lcUserName = "FRONT"
                lcMemoTitle = "Accion para prevencion de fraude en Roaming".
      ELSE
         ASSIGN lcUserName = "LIMITS"
                lcMemoTitle = "Control de de riesgo".
 
      CREATE Memo.
      ASSIGN 
         Memo.Brand     = Syst.Var:gcBrand
         Memo.HostTable = "MobSub"
         Memo.KeyValue  = STRING(MsRequest.MsSeq)
         Memo.CustNum   = MsRequest.CustNum
         Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
         Memo.CreUser   = lcUserName
         Memo.MemoType  = "service"
         Memo.MemoTitle = lcMemoTitle
         Memo.MemoText  = ENTRY(1,ENTRY(1,MsRequest.ReqCParam1),"=") +
                          " aplicado".
         Memo.CreStamp  = Func.Common:mMakeTS().
   END.

   /* MobSub status update ok, Master request OK */
   fReqStatus(2,"").
   
   IF (LOOKUP("Debt_HOTL=0", MsRequest.ReqCParam1) > 0 OR
       LOOKUP("Debt_HOTLP=0",MsRequest.ReqCParam1) > 0) AND
      NOT CAN-FIND(FIRST bMsRequest NO-LOCK WHERE
                         bMsRequest.MsSeq = MobSub.MsSeq AND
                         bMsRequest.ReqType = 35 AND
           LOOKUP(STRING(bMsRequest.ReqStat),{&REQ_INACTIVE_STATUSES} + ",3") = 0)
      THEN DO:
               
      IF MobSub.CLIType EQ "CONTM2" AND
         MsRequest.ReqCParam1 EQ "Debt_HOTL=0" THEN llRefreshBarring = FALSE.
      ELSE IF MobSub.CLIType EQ "CONTM2" THEN llRefreshBarring = TRUE.
      ELSE DO:
         BARR_CHECK:
         FOR EACH Barring NO-LOCK WHERE
                  Barring.MsSeq = MsRequest.MsSeq 
            USE-INDEX MsSeq BREAK BY Barring.BarringCode:
            
            IF FIRST-OF(Barring.BarringCode) AND
               Barring.BarringStatus EQ {&BARR_STATUS_ACTIVE} THEN DO:
               
               FIND FIRST BarringConf NO-LOCK WHERE
                          BarringConf.BarringCode = Barring.BarringCode AND
                          BarringConf.NWComponent = "BARRING" NO-ERROR.
               IF AVAIL BarringConf THEN DO:
                  llRefreshBarring = TRUE.
                  LEAVE BARR_CHECK.
               END.
            END.
         END.
      END.

      IF llRefreshBarring THEN DO:

         /* create barring request */
         RUN Mm/barrengine.p(Mobsub.MsSeq,
                         "#REFRESH",
                         "5",                /* source  */
                         Syst.Var:katun,              /* creator */
                         Func.Common:mMakeTS(),
                         "",                 /* SMS */
                         OUTPUT lcResult).
               
         liReq = INTEGER(lcResult) NO-ERROR. 
         IF liReq EQ ? OR NOT liReq > 0 THEN DO:     
            /* Write memo */
            Func.Common:mWriteMemo("MobSub",
                             STRING(MsRequest.MsSeq),
                             MsRequest.CustNum,
                             "Barring",
                             "Automatic barring refresh request failed: " + lcResult).
         END.

      END.

   END.

   DEF VAR lcActiveBarrings AS CHAR NO-UNDO. 
   lcActiveBarrings = Func.BarrMethod:mGetActiveBarrings(MobSub.MsSeq).

   /* Activate/Suspend the BB service based on the fraud barring */
   IF fIsInList(MsRequest.ReqCParam1,{&FRAUD_BARR_CODES}) THEN DO:

      FOR FIRST SubSer WHERE SubSer.ServCom = "BB" AND
                             SubSer.MsSeq   = MsRequest.MsSeq AND
                             SubSer.SsDate <= TODAY NO-LOCK:

         IF NOT fIsInList(lcActiveBarrings,{&FRAUD_BARR_CODES}) THEN DO:
            IF SubSer.SSStat = 2 AND fIsBBAllowed(MsRequest.MsSeq,ldActStamp)
               THEN liBBStat = 1.
         END.
         ELSE IF SubSer.SSStat = 1 THEN liBBStat = 2.

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
               Func.Common:mWriteMemo("MobSub",
                                STRING(MsRequest.MsSeq),
                                MsRequest.CustNum,
                                "Service Package",
                                "Error in BB service request: " + lcResult).
         END. /* IF liBBStat > 0 THEN DO: */
      END. /* FOR FIRST SubSer WHERE SubSer.ServCom = "BB" AND AND */

      /* Cancel Ongoing STC or subs. type change BTC request */
      IF fIsInList(lcActiveBarrings,{&FRAUD_BARR_CODES}) THEN DO:

         FIND FIRST MsRequest WHERE
                    MsRequest.MsSeq = Mobsub.MSSeq AND
                    MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                    LOOKUP(STRING(MsRequest.ReqStatus),
                           {&REQ_INACTIVE_STATUSES}) = 0 AND
                    MsRequest.ActStamp >= ldActStamp NO-LOCK NO-ERROR.
         IF AVAIL MsRequest AND
            NOT ((MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} OR
                  MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) AND
                  fIsFixedOnly(MsRequest.ReqCParam2)) THEN DO: /* Allow STC to 2P */
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
     
END PROCEDURE. /*pDone*/
