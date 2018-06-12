/* ----------------------------------------------------------------------
  MODULE .......: bundle_change_request.p  
  TASK .........: Change bundle to another
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 10.11.10   
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/service.i}
{Rate/rerate_request.i}
{Func/fsendsms.i}
{Func/fbtc.i}
{Mc/invoicetarget.i}
{Func/dss_matrix.i}
{Func/dss_request.i}
{Mc/dpmember.i}
{Func/add_lines_request.i}

DEF INPUT  PARAMETER iiMSrequest AS INT  NO-UNDO.

DEF VAR liOrigStatus      AS INT  NO-UNDO.
DEF VAR ldEndStamp        AS DEC  NO-UNDO.
DEF VAR ldaActivationDate AS DATE NO-UNDO.
DEF VAR liActivationTime  AS INT  NO-UNDO.

DEF TEMP-TABLE ttContract NO-UNDO
    FIELD DCEvent AS CHAR.

DEF TEMP-TABLE ttAdditionalSIM NO-UNDO
    FIELD MsSeq    AS INT
    FIELD CustNum  AS INT
    FIELD CLI      AS CHAR.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMsOwner AS HANDLE NO-UNDO.
   lhMsOwner = BUFFER MsOwner:HANDLE.

   RUN StarEventInitialize(lhMsOwner).

END.

/******** Main start *********/

FIND FIRST MSRequest WHERE
      MSRequest.MSRequest = iiMSRequest  AND
      MSRequest.ReqType   = {&REQTYPE_BUNDLE_CHANGE} NO-LOCK NO-ERROR.

IF NOT AVAIL MSRequest THEN DO:
   RETURN "ERROR:Unknown MSRequest " + STRING(iiMSRequest).
END.

liOrigStatus = MsRequest.ReqStatus.          

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

FIND FIRST MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN DO:
   fReqError("ERROR:Subscription not available").
   RETURN "ERROR:Subscription not available".
END. /* IF NOT AVAILABLE MobSub THEN DO: */

Func.Common:mSplitTS(MsRequest.ActStamp,
         OUTPUT ldaActivationDate,
         OUTPUT liActivationTime).

IF liActivationTime > 0 THEN 
   ldEndStamp = Func.Common:mMake2DT(ldaActivationDate,liActivationTime - 1).
ELSE ldEndStamp = Func.Common:mMake2DT(ldaActivationDate - 1,86399).

CASE liOrigStatus:
WHEN {&REQUEST_STATUS_NEW} THEN DO:
   RUN pBundleChange.
END.
WHEN {&REQUEST_STATUS_SUB_REQUEST_DONE} THEN DO:
   RUN pFinalize.
END.
OTHERWISE RETURN "ERROR:Current status is not handled".
END CASE. 

IF RETURN-VALUE BEGINS "ERROR:" THEN DO:
   fReqError(RETURN-VALUE).
END.

RETURN RETURN-VALUE.

/******** Main end *********/


PROCEDURE pBundleChange:

   DEF VAR lcError           AS CHAR NO-UNDO. 
   DEF VAR liActivate        AS INT  NO-UNDO.
   DEF VAR liTerminate       AS INT  NO-UNDO.
   DEF VAR lcResult          AS CHAR NO-UNDO.

   DEF BUFFER bActRequest FOR MsRequest.
   
   /* current bundle (per.contract) is terminated and the new one is 
      activated */
     
   /* STC may prevent BTC */
   IF NOT fIsBTCAllowed(MsRequest.MsSeq,
                        MsRequest.ReqCParam2,
                        OUTPUT lcResult) THEN  
      RETURN "ERROR:" + lcResult.

   IF NOT fIsBundle(MsRequest.ReqCParam1) THEN 
      RETURN "ERROR:Not a bundle".

   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand   = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = MsRequest.ReqCParam1 AND
              LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DayCampaign THEN 
      RETURN "ERROR:Current bundle is unknown".

   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand   = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = MsRequest.ReqCParam2 AND
              LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 
      NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DayCampaign THEN 
      RETURN "ERROR:New bundle is unknown".

   IF NOT fIsBTCBundleAllowed(MsRequest.MsSeq,
                              MobSub.CLIType,  
                              MsRequest.ReqCParam1,
                              MsRequest.ReqCParam2,
                              ldaActivationDate,
                              OUTPUT lcResult) THEN 
      RETURN "ERROR:" + lcResult.

   /* terminate current periodical contract */
   liTerminate = fPCActionRequest(MsRequest.MsSeq,
                                  MsRequest.ReqCParam1,
                                  "term",
                                  ldEndStamp,
                                  TRUE,   /* create fee */
                                  {&REQUEST_SOURCE_BTC},
                                  "",
                                  MsRequest.MsRequest,
                                  TRUE,
                                  "",
                                  0,
                                  0,
                                  "",
                                  OUTPUT lcError).
   IF liTerminate = 0 OR liTerminate = ? THEN 
      RETURN "ERROR: Bundle termination request creation failed; " + 
             lcError.

   /* activate new periodical contract */
   liActivate = fPCActionRequest(MsRequest.MsSeq,
                                 MsRequest.ReqCParam2, 
                                 "act",
                                 (IF MobSub.ActivationTS > MsRequest.ActStamp THEN
                                  MobSub.ActivationTS ELSE MsRequest.ActStamp),
                                 TRUE,   /* create fee */
                                 {&REQUEST_SOURCE_BTC},
                                 "",
                                 MsRequest.MsRequest,
                                 TRUE,
                                 "",
                                 0,
                                 0,
                                 "",
                                 OUTPUT lcError).
   IF liActivate = 0 THEN 
      RETURN "ERROR: Bundle activation request creation failed; " + 
             lcError.
   
   /* termination must be completed before starting activation */
   DO TRANS:
      FIND FIRST bActRequest WHERE bActRequest.MsRequest = liActivate
         EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE bActRequest THEN 
         bActRequest.ReqIParam2 = liTerminate.
   END.      

   /* Close unsupported contracts and services */
   RUN pCloseContracts(ldaActivationDate,
                       ldEndStamp,
                       {&REQUEST_SOURCE_BTC}).
   
   /* wait for subrequests */
   fReqStatus(7,""). 

   RETURN "".
   
END PROCEDURE.

PROCEDURE pFinalize:

   DEF VAR lcError                 AS CHAR NO-UNDO.
   DEFINE VARIABLE lhDPMember AS HANDLE NO-UNDO.

   DEF BUFFER bDPMember  FOR DPMember.

   EMPTY TEMP-TABLE ttContract.

   /* check that subrequests really are ok */
   IF fGetSubRequestState(MsRequest.MsRequest) NE 2 THEN DO:
      IF MsRequest.ReqStat NE 7 THEN DO:
         /* back to waiting mode */
         fReqStatus(7,"").
      END.   
      RETURN "".
   END.

   /* BTC between pos=>pos bundle except bono */
   IF NOT (MsRequest.ReqCparam1 BEGINS "MDUB" OR
           MsRequest.ReqCparam1 BEGINS "DATA") THEN
      RUN pUpdateSubscription.

   /* re-activate/terminate service packages etc. */
   RUN Mm/requestaction_exec.p(MsRequest.MsRequest,
                            MobSub.CLIType,       /* CLI Type */
                            0,                    /* order */
                            MsRequest.ActStamp,
                            ldEndStamp,
                            YES,                    /* create fees */
                            {&REQUEST_SOURCE_BTC},  /* req.source */
                            {&REQUEST_ACTIONLIST_ALL}).

   RUN pReRate(MobSub.MsSeq,
               MobSub.InvCust,
               ldaActivationDate).

   IF MobSub.MultiSIMID > 0 AND
      LOOKUP(MsRequest.ReqCParam1,"CONTS39,CONTS32") > 0 THEN
      RUN pMultiSimBTC (INPUT ldaActivationDate).

   fReqStatus(2,"").

   /* Send SMS "STC_DONE" for IPL -> IPL and Flat Tariffs */
   RUN Mm/requestaction_sms.p(INPUT MsRequest.MsRequest,
                           INPUT MobSub.CLIType,
                           INPUT {&REQUEST_SOURCE_BTC}).

   /* Activate DSS2 if allowed and additional line STC to CONT9 if possible */
   IF MsRequest.ReqCParam2 BEGINS "CONTS" THEN DO:

      fAdditionalLineSTC(MsRequest.MsRequest,
                         MSRequest.ActStamp,
                         "").
     
      RUN pActivateDSS2(INPUT MsRequest.MsRequest,
                        INPUT MsRequest.ActStamp,
                        INPUt ldaActivationDate).
   END. /* IF MsRequest.ReqCParam2 BEGINS "CONTS" THEN DO: */


   /* Change InvoiceTarget if old CONTS30 or CONTS39 or Migrated CONTS32 to new La Infinita */
   IF LOOKUP(MsRequest.ReqCParam1,"CONTS30,CONTS39,CONTS32") > 0 THEN DO:
      fSTCInvoiceTarget(MsRequest.MsSeq,
                        MsRequest.ReqCParam1,
                        MsRequest.ReqCParam2,
                        OUTPUT lcError).
      IF lcError NE "" THEN 
         Func.Common:mWriteMemo("MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.CustNum,
                          "Bundle type change",
                          "Invoice target creation failed: " + lcError).
   END. /* IF LOOKUP(MsRequest.ReqCParam1,"CONTS30,CONTS39") > 0 THEN DO: */

   /* Close existing InvoiceRowCounter if BTC pos=>pos except bono */
   IF DAY(ldaActivationDate) <> 1 AND
      NOT (MsRequest.ReqCparam1 BEGINS "MDUB" OR
           MsRequest.ReqCparam1 BEGINS "DATA") THEN DO:
      FIND FIRST MSOwner WHERE 
                 MSOwner.MsSeq   = Mobsub.MsSeq AND
                 MSOwner.TsEnd  >= 99999999 NO-LOCK NO-ERROR.
      IF AVAIL MSOwner AND MSOwner.CLIEvent BEGINS "iS" THEN
         FOR EACH InvRowCounter WHERE
                  InvRowCounter.MsSeq   = MobSub.MsSeq      AND
                  InvRowCounter.InvCust = MSOwner.Custnum AND
                  InvRowCounter.ToDate >= ldaActivationDate EXCLUSIVE-LOCK:
            IF InvRowCounter.InvNum > 0 THEN DO:
               FIND FIRST Invoice NO-LOCK WHERE
                          Invoice.InvNum = InvRowCounter.InvNum NO-ERROR.
               IF NOT AVAIL Invoice OR 
                            Invoice.InvType NE 99 THEN NEXT.
            END.
            InvRowCounter.ToDate = (ldaActivationDate - 1).
         END. /* FOR EACH InvRowCounter WHERE */
   END. /* IF DAY(ldtActDate) <> 1 THEN DO: */

   /* close discounts that are not allowed for the new type */
   IF INDEX(MsRequest.ReqCParam2,"MDUB") = 0 AND
      INDEX(MsRequest.ReqCParam2,"DATA") = 0 THEN
      FOR EACH DPMember NO-LOCK WHERE
               DPMember.HostTable = "MobSub" AND
               DPMember.KeyValue = STRING(MsRequest.MsSeq) AND
               DPMember.ValidTo >= ldaActivationDate,
         FIRST DiscountPlan OF DPMember NO-LOCK WHERE
               DiscountPlan.SubjectType = "List":

         IF NOT CAN-FIND(FIRST DPSubject WHERE
                               DPSubject.DPId      = DiscountPlan.DPId AND
                               MsRequest.ReqCParam2 MATCHES DPSubject.DPSubject
                               AND
                               DPSubject.ValidFrom <= ldaActivationDate AND
                               DPSubject.ValidTo   >= ldaActivationDate)
         THEN DO TRANS:
            fCloseDPMember(DPMember.DPMemberID,
                           ldaActivationDate - 1).
         END.
      END.

END PROCEDURE.

PROCEDURE pReRate:

   DEF INPUT PARAMETER iiMsSeq    AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiInvCust  AS INT  NO-UNDO.
   DEF INPUT PARAMETER idtActDate AS DATE NO-UNDO.

   DEF VAR ldtFrom  AS DATE NO-UNDO.
   DEF VAR ldtTo    AS DATE NO-UNDO.
   DEF VAR lcResult AS CHAR NO-UNDO.
   
   /* from the beginning of change month */
   ldtFrom = DATE(MONTH(idtActDate),1,YEAR(idtActDate)).
      
   /* to the end of it */
   ldtTo = IF MONTH(idtActDate) = 12
           THEN DATE(12,31,YEAR(idtActDate))
           ELSE DATE(MONTH(idtActDate) + 1,1,YEAR(idtActDate)) - 1.

   IF ldtTo > ldtFrom THEN DO:
         
      fRerateRequest(iiInvCust,
                     iiMsSeq,
                     ldtFrom,
                     ldtTo,
                     TRUE,    /* wait for other possible subrequests */
                     FALSE,   /* double check */
                     0,       /* activate now */
                     "",      /* creator */
                     {&REQUEST_SOURCE_BTC},     /* source */
                     MsRequest.MsRequest,
                     0,       /* not mandatory */
                     OUTPUT lcResult).
   END.
 
END PROCEDURE.

PROCEDURE pMultiSimBTC:

   DEF INPUT PARAMETER idaActivationDate AS DATE NO-UNDO.

   DEF VAR liQuarTime         AS INT  NO-UNDO.
   DEF VAR liSimStat          AS INT  NO-UNDO.
   DEF VAR liMSISDNStat       AS INT  NO-UNDO.
   DEF VAR liRequest          AS INT  NO-UNDO.
   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR ldaBTCCreateDate   AS DATE NO-UNDO.
   DEF VAR liBTCCreateTime    AS INT  NO-UNDO.
   DEF VAR ldaSecSIMTermDate  AS DATE NO-UNDO.
   DEF VAR ldeSecSIMTermStamp AS DEC  NO-UNDO. 

   DEF BUFFER lbMobSub        FOR Mobsub.

   IF NOT AVAIL Mobsub THEN RETURN.

   IF NOT (Mobsub.MultiSimType EQ {&MULTISIMTYPE_PRIMARY} AND
           Mobsub.MultiSimID > 0) THEN RETURN.

   FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
              lbMobSub.Brand  = Syst.Var:gcBrand AND
              lbMobSub.MultiSimID = MobSub.MultiSimID AND
              lbMobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} AND
              lbMobSub.Custnum = MobSub.Custnum NO-ERROR.
   IF NOT AVAIL lbMobSub THEN DO:
      /* Terminate MultiSIM Link */
      FIND CURRENT Mobsub EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN MobSub.MultiSimID = 0
             MobSub.MultiSimType = 0.
      FIND CURRENT Mobsub NO-LOCK NO-ERROR.

      RETURN.
   END.

   CREATE ActionLog.
   ASSIGN
      ActionLog.ActionTS     = Func.Common:mMakeTS()
      ActionLog.Brand        = Syst.Var:gcBrand
      ActionLog.TableName    = "Customer"
      ActionLog.KeyValue     = STRING(MobSub.Custnum)
      ActionLog.UserCode     = Syst.Var:katun
      ActionLog.ActionID     = "MultiSIMTermination"
      ActionLog.ActionPeriod = YEAR(idaActivationDate - 1) * 100 +
                               MONTH(idaActivationDate - 1)
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}
      ActionLog.ActionChar   = SUBST("BTC subscription: &1, " + "
                                      MultiSIMID: &2, MultiSimType &3",
                               MobSub.MsSeq, MobSub.MultiSimID,
                               MobSub.MultiSIMType).

   /* Terminate MultiSIM Link */
   FIND CURRENT Mobsub EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN MobSub.MultiSimID = 0
          MobSub.MultiSimType = 0.
   FIND CURRENT Mobsub NO-LOCK NO-ERROR.

   IF CAN-FIND (FIRST MsRequest WHERE
      MsRequest.MsSeq   = lbMobSub.Msseq AND
      MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
      LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0) OR
      CAN-FIND (FIRST MsRequest WHERE
      MsRequest.MsSeq   = lbMobSub.Msseq AND
      MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
      LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0) OR
      Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT lbMobSub.CLI)
   THEN RETURN.

   fTermAdditionalSim(lbMobSub.MsSeq,
                      lbMobSub.CLI,
                      lbMobSub.CustNum,
                      {&SUBSCRIPTION_TERM_REASON_MULTISIM},
                      idaActivationDate,
                      {&REQUEST_SOURCE_BTC},
                      iiMsRequest,
                      OUTPUT lcError).

   IF lcError = "" THEN    
      RUN pSendSMS(
            MobSub.MsSeq,
            MsRequest.MsRequest,
            "MultiSIMPrimarySTC",
            {&SMSTYPE_STC},
            "22622",
            "").

END PROCEDURE. 

PROCEDURE pActivateDSS2:

   DEF INPUT PARAMETER iiMainRequest AS INT  NO-UNDO.
   DEF INPUT PARAMETER ideActStamp   AS DEC  NO-UNDO.
   DEF INPUT PARAMETER idActDate     AS DATE NO-UNDO.

   DEF VAR liDSSMsSeq                AS INT  NO-UNDO.
   DEF VAR ldeEndStamp               AS DEC  NO-UNDO.
   DEF VAR ldEndDate                 AS DATE NO-UNDO.
   DEF VAR liRequest                 AS INT  NO-UNDO.
   DEF VAR lcError                   AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType     AS CHAR NO-UNDO.

   DEF BUFFER bMobSub  FOR MobSub.

   /* end old bundles to the end of previous month */
   IF DAY(idActDate) = 1 THEN
      ASSIGN ldeEndStamp = Func.Common:mMake2DT(idActDate - 1,86399)
             ldEndDate   = (idActDate - 1).
   ELSE
      ASSIGN ldEndDate   = Func.Common:mLastDayOfMonth(idActDate)
             ldeEndStamp = Func.Common:mMake2DT(ldEndDate,86399).

   /* If ongoing DSS termination request then return */
   IF fOngoingDSSTerm(INPUT MobSub.CustNum,
                      INPUT ldeEndStamp) THEN RETURN.

   lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

   IF LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0 AND
      NOT fIsDSSActive(MobSub.CustNum,ideActStamp) AND
      NOT fOngoingDSSAct(MobSub.CustNum) AND
      fIsDSSActivationAllowed(MobSub.CustNum,
                              0,
                              ideActStamp,
                              {&DSS2},
                              OUTPUT liDSSMsSeq,
                              OUTPUT lcError)
   THEN DO:
      FIND FIRST bMobSub WHERE
                 bMobSub.MsSeq = liDSSMsSeq NO-LOCK NO-ERROR.
      IF AVAIL bMobSub THEN DO:
         liRequest = fDSSRequest(bMobSub.MsSeq,
                                 bMobSub.CustNum,
                                 "CREATE",
                                 "",
                                 "DSS2",
                                 ideActStamp,
                                 {&REQUEST_SOURCE_BTC},
                                 "",
                                 TRUE, /* create fees */
                                 iiMainRequest,
                                 FALSE,
                                 OUTPUT lcError).
         IF liRequest = 0 THEN
            /* write possible error to a memo */
            Func.Common:mWriteMemo("MobSub",
                             STRING(MobSub.MsSeq),
                             MobSub.Custnum,
                             "DSS2 activation failed",
                             lcError).
      END.
   END.

END PROCEDURE.

PROCEDURE pCloseContracts:

   DEF INPUT PARAMETER idaActDate    AS DATE NO-UNDO.
   DEF INPUT PARAMETER idEndStamp    AS DEC  NO-UNDO.
   DEF INPUT PARAMETER icReqSource   AS CHAR NO-UNDO.

   DEF VAR lcBonoBundle              AS CHAR NO-UNDO.
   DEF VAR lcBONOContracts           AS CHAR NO-UNDO. 
   DEF VAR lcOnlyVoiceContracts      AS CHAR NO-UNDO.
   DEF VAR lcAllowedBonoSTCContracts AS CHAR NO-UNDO. 
   DEF VAR lcError                   AS CHAR NO-UNDO. 
   DEF VAR liRequest                 AS INT  NO-UNDO.
   DEF VAR lcNativeVoipTariffs      AS CHAR NO-UNDO.
   DEF VAR lcNativeVoipBundles      AS CHAR NO-UNDO.
   
   ASSIGN lcAllowedBonoSTCContracts = fCParamC("ALLOWED_BONO_STC_CONTRACTS")
          lcBONOContracts = fCParamC("BONO_CONTRACTS")
          lcNativeVoipTariffs    = fCParamC("NATIVE_VOIP_BASE_BUNDLES")
          lcNativeVoipBundles    = fCParamC("NATIVE_VOIP_BUNDLES").

   IF LOOKUP(MsRequest.ReqCparam2,lcBonoContracts) = 0 THEN DO:

   lcBonoBundle = fGetActiveSpecificBundle(MobSub.MsSeq,
                                           MsRequest.ActStamp,
                                           "BONO").

   IF LOOKUP(lcBonoBundle,lcBonoContracts) > 0 AND
      LOOKUP(lcBonoBundle,lcAllowedBonoSTCContracts) = 0 THEN DO:

      /* terminate periodical contract */
      liRequest = fPCActionRequest(MsRequest.MsSeq,
                                   lcBonoBundle,
                                   "term",
                                   idEndStamp,
                                   TRUE,   /* create fee */
                                   icReqSource,
                                   "",
                                   MsRequest.MsRequest,
                                   FALSE,   /* mandatory subreq. */
                                   "", 
                                   0,
                                   0,
                                   "",
                                   OUTPUT lcError).
      IF liRequest = 0 THEN
         Func.Common:mWriteMemo("MobSub",
                          STRING(MsRequest.MsSeq),
                          MsRequest.CustNum,
                          "Bundle type change",
                          "Per.contract termination request creation failed " +
                          lcError).
   END. /* IF LOOKUP(lcBonoBundle,lcBonoContracts) > 0 AND */

   /* Modify/terminate the BB service for FLAT Tariffs */
   IF INDEX(MsRequest.ReqCParam2,"CONTF") > 0 THEN DO:
      lcOnlyVoiceContracts = fCParamC("ONLY_VOICE_CONTRACTS").
      IF LOOKUP(MsRequest.ReqCParam2,lcOnlyVoiceContracts) > 0 THEN DO:
         IF LOOKUP(lcBonoBundle,lcBONOContracts) = 0 OR
            liRequest > 0 THEN DO:
            RUN pChangedBBStatus(INPUT 2,
                                 INPUT MsRequest.ActStamp,
                                 INPUT {&REQUEST_SOURCE_BTC},
                                 BUFFER MsRequest,
                                 BUFFER MobSub).

            /* Deactivate BONO_VOIP bundle if there is no active data bundle */
            IF fGetActiveSpecificBundle(Mobsub.MsSeq,MsRequest.ActStamp,
                                        "BONO_VOIP") > "" THEN DO:
               CREATE ttContract.
                      ttContract.DCEvent = "BONO_VOIP".
            END. /* IF fGetActiveSpecificBundle(Mobsub.MsSeg */
         END. /* IF LOOKUP(lcBonoBundle,lcBONOContracts) = 0 THEN DO: */
         ELSE
            RUN pChangedBBStatus(INPUT 1,
                                 INPUT MsRequest.ActStamp,
                                 INPUT {&REQUEST_SOURCE_BTC},
                                 BUFFER MsRequest,
                                 BUFFER MobSub).
      END. /* IF LOOKUP(MsRequest.ReqCParam2,lcOnlyVoiceContracts) > 0 */
   END. /* IF INDEX(MsRequest.ReqCParam2,"CONTF") > 0 THEN DO: */
   END. /* IF LOOKUP(MsRequest.ReqCparam2,lcBonoContracts) = 0 THEN DO: */
   
   IF (LOOKUP(MsRequest.ReqCParam2,lcNativeVoipTariffs) > 0 OR 
       LOOKUP(MsRequest.ReqCParam2,lcNativeVoipBundles) > 0) AND
      NOT CAN-FIND(FIRST ttContract WHERE
                         ttContract.DCEvent  = "BONO_VOIP") AND
      fGetActiveSpecificBundle(Mobsub.MsSeq,
                               MsRequest.ActStamp,
                               "BONO_VOIP") > "" THEN DO:
      CREATE ttContract.
             ttContract.DCEvent = "BONO_VOIP".
   END.

   FOR EACH ttContract:
      FIND FIRST DayCampaign WHERE
                 DayCampaign.Brand   = Syst.Var:gcBrand AND
                 DayCampaign.DCEvent = ttContract.DCEvent AND
                 DayCampaign.ValidTo >= Today NO-LOCK NO-ERROR.
      IF NOT AVAIL DayCampaign THEN DO:
         Func.Common:mWriteMemo("MobSub",
                          STRING(Mobsub.MsSeq),
                          Mobsub.CustNum,
                          "Periodical Contract",
                          ttContract.DCEvent +
                          ": Periodical contract information is missing!").
         DELETE ttContract.
         NEXT.
      END. /* IF NOT AVAIL DayCampaign THEN DO: */

      liRequest = fPCActionRequest(MobSub.MsSeq,
                       ttContract.DCEvent,
                       "term",
                       ldEndStamp,
                       TRUE,             /* create fees */
                       {&REQUEST_SOURCE_BTC},
                       "",
                       MsRequest.MsRequest, /* Father Request */
                       FALSE,
                       "",
                       0,
                       0,
                       "",
                       OUTPUT lcError).
      IF liRequest = 0 THEN
         /* Write memo */
         Func.Common:mWriteMemo("MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.CustNum,
                          "Periodical Contract",
                          ttContract.DCEvent +
                          ": Periodical contract is not closed: " + lcError).
   END. /* FOR EACH ttContract: */

   EMPTY TEMP-TABLE ttContract NO-ERROR.

END PROCEDURE.

PROCEDURE pUpdateSubscription:

   DEF BUFFER bOwner        FOR MsOwner.

   FOR FIRST MSOwner WHERE 
             MSOwner.MsSeq  = Mobsub.MsSeq AND
             MSOwner.TsEnd >= 99999999 EXCLUSIVE-LOCK:

      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsOwner).

      MSOwner.TsEnd = ldEndStamp.
   
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsOwner).
   
      CREATE bOwner.
      BUFFER-COPY MsOwner EXCEPT TsBegin TsEnd CLIEvent TO bOwner.
      ASSIGN bOwner.TsBegin      = Func.Common:mSecOffSet(MsOwner.TsEnd,1)
             bOwner.TsEnd        = 99999999.99999
             bOwner.TariffBundle = MsRequest.ReqCParam2.

      IF DAY(ldaActivationDate) <> 1 THEN
         bOwner.CLIEvent = "iS".
      ELSE
         bOwner.CLIEvent = "S".
         
      IF llDoEvent THEN fMakeCreateEvent((BUFFER bOwner:HANDLE),
                                         "",
                                         Syst.Var:katun,
                                         "").

      RELEASE bOwner.
      RELEASE MsOwner. 
   END.

   /* update subscription */
   FIND CURRENT Mobsub EXCLUSIVE-LOCK NO-ERROR.
   
   ASSIGN Mobsub.TariffActDate = ldaActivationDate
          MobSub.TariffActTS   = ldEndStamp. 

   FIND CURRENT Mobsub NO-LOCK NO-ERROR.

END PROCEDURE.

FINALLY:
   fCleanEventObjects().
END.
