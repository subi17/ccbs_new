/* ----------------------------------------------------------------------
  Module .......: Mm/reacmobsub.p
  Task .........: Reactivate the terminated Subscription
  Application ..: TMS
  Author .......: Vikas
  Created ......: 03.10.11
  Version ......: Yoigo
---------------------------------------------------------------------- */

{commali.i}
{eventval.i}
{timestamp.i}
{cparam2.i}
{fmakemsreq.i}
{msreqfunc.i}
{msisdn.i}
{tmsconst.i}
{freacmobsub.i}
{contract_end_date.i}
{service.i}
{fdss.i}
{forderstamp.i}
{orderfunc.i}
{ftopup.i}
{ftaxdata.i}
{fsubstermreq.i}
{fbankdata.i}
{fbtc.i}

DEFINE INPUT PARAMETER iiMSrequest  AS INTEGER   NO-UNDO.

DEFINE VARIABLE ldCurrTS            AS  DECIMAL   NO-UNDO.

DEFINE BUFFER bMSRequest FOR MsRequest.

FIND FIRST MSRequest WHERE
           MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR
   MsRequest.ReqType NE {&REQTYPE_SUBSCRIPTION_REACTIVATION} THEN
   RETURN "ERROR".

DEF TEMP-TABLE ttContract NO-UNDO
   FIELD DCEvent   AS CHAR
   FIELD PerContID AS INT.

ldCurrTS = fMakeTS().

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}

   DEFINE VARIABLE lhMSISDN    AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhSIM       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhIMSI      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhMSOWNER   AS HANDLE    NO-UNDO.

   lhMSISDN  = BUFFER MSISDN:HANDLE.
   lhSIM     = BUFFER SIM:HANDLE.
   lhIMSI    = BUFFER IMSI:HANDLE.
   lhMSOWNER = BUFFER MSOWNER:HANDLE.

   RUN StarEventInitialize(lhMSISDN).
   RUN StarEventInitialize(lhSIM).
   RUN StarEventInitialize(lhIMSI).
   RUN StarEventInitialize(lhMSOWNER).

END. /* IF llDoEvent THEN DO: */

RUN pReactivate.

fCleanEventObjects().

RETURN RETURN-VALUE.


PROCEDURE pReactivate:

DEFINE VARIABLE liRequest           AS  INTEGER   NO-UNDO.
DEFINE VARIABLE lcResult            AS  CHARACTER NO-UNDO.
DEFINE VARIABLE lcError             AS  CHARACTER NO-UNDO.
DEFINE VARIABLE ldeTopupAmount      AS  DECIMAL   NO-UNDO.
DEFINE VARIABLE lcTaxZone           AS  CHARACTER NO-UNDO.
DEFINE VARIABLE ldaSecSIMTermDate   AS  DATE      NO-UNDO.
DEFINE VARIABLE liSecSIMTermTime    AS  INTEGER   NO-UNDO.
DEFINE VARIABLE llMultiSIMActive    AS  LOGICAL   NO-UNDO.
DEFINE VARIABLE liMultiSIMType      AS  INTEGER   NO-UNDO.
DEFINE VARIABLE liMsisdnStat        AS  INTEGER   NO-UNDO.
DEFINE VARIABLE liSimStat           AS  INTEGER   NO-UNDO.
DEFINE VARIABLE liQuarTime          AS  INTEGER   NO-UNDO.
DEFINE VARIABLE llDSSActive         AS  LOGICAL   NO-UNDO.
DEFINE VARIABLE lcDSSResult         AS  CHARACTER NO-UNDO.
DEFINE VARIABLE ldeCurrMonthLimit   AS  DECIMAL   NO-UNDO.
DEFINE VARIABLE ldeConsumedData     AS  DECIMAL   NO-UNDO.
DEFINE VARIABLE ldeOtherMonthLimit  AS  DECIMAL   NO-UNDO.
DEFINE VARIABLE ldeTermStamp        AS  DECIMAL   NO-UNDO.

DEFINE VARIABLE llReStoreDefaultShaper AS LOGICAL NO-UNDO INIT TRUE.
DEFINE VARIABLE lcPostpaidDataBundles  AS CHAR    NO-UNDO.
DEFINE VARIABLE lcPrePaidDataBundles   AS CHAR    NO-UNDO.
DEFINE VARIABLE lcALLPostpaidBundles   AS CHAR    NO-UNDO.
DEFINE VARIABLE liDSSMsSeq             AS INT     NO-UNDO.
DEFINE VARIABLE lcAllowedDSS2SubsType  AS CHAR    NO-UNDO.
DEFINE VARIABLE lcBundleId             AS CHAR    NO-UNDO.
DEFINE VARIABLE lcBankAccount          AS CHAR    NO-UNDO.
DEF    VAR      lcInfo                 AS CHARACTER NO-UNDO.
DEF VAR lbolSTCRequestFound        AS LOGICAL NO-UNDO.
DEF VAR lbolBTCRequestFound        AS LOGICAL NO-UNDO.
DEF VAR lbolIsUpgradeUpsell        AS LOGICAL NO-UNDO.

DEFINE BUFFER bSubMsRequest  FOR MsRequest.
DEFINE BUFFER bTermMsRequest FOR MsRequest.
DEFINE BUFFER bMsRequest     FOR MsRequest.
DEFINE BUFFER bOrder         FOR Order.
DEFINE BUFFER bRenewalOrder  FOR Order.
DEFINE BUFFER lbMobSub       FOR MobSub.

IF MsRequest.ReqStatus <> 6 THEN RETURN.

/* Request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

lcResult = freacprecheck(INPUT MSRequest.MSSeq, 
                         INPUT MSRequest.UserCode, INPUT TRUE).
IF lcResult > "" THEN DO:
   fReqError(lcResult).
   RETURN.
END. /* IF lcResult > "" THEN DO: */

TRANS_BLK:
DO TRANSACTION:
   FIND FIRST termMobSub WHERE
              termMobSub.MsSeq = MSRequest.MSSeq EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE termMobSub THEN DO:
      fReqError("Invalid Subscription Id").
      RETURN.
   END. /* IF NOT AVAILABLE termMobSub THEN DO: */

   FIND FIRST MSISDN WHERE
              MSISDN.Brand    = gcBrand        AND
              MSISDN.CLI      = termMobSub.CLI EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE MSISDN THEN DO:
      fReqError("MSISDN not found").
      RETURN.
   END. /* IF NOT AVAILABLE MSISDN THEN DO: */
   ELSE DO:
      fMakeMsidnHistory(INPUT RECID(MSISDN)).
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSISDN).
      ASSIGN MSISDN.Stat        = 3
             MSISDN.CustNum     = termMobSub.custnum
             MSISDN.PortingDate = ?
             MSISDN.OutOperator = "".
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSISDN).
   END. /* ELSE DO: */

   FIND FIRST SIM WHERE
              SIM.ICC = termMobSub.ICC EXCLUSIVE-LOCK NO-ERROR.
   /* Change SIM card status to active */
   IF NOT AVAILABLE SIM THEN DO:
      fReqError("SIM not found").
      UNDO TRANS_BLK, RETURN.
   END. /* IF NOT AVAILABLE SIM THEN DO: */
   ELSE DO:
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSIM).
      ASSIGN
         SIM.SimStat = 4
         SIM.CustNum = termMobSub.custnum.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSIM).
   END. /* ELSE DO: */

   FIND FIRST IMSI WHERE
              IMSI.IMSI = termMobSub.IMSI EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE IMSI THEN DO:
      fReqError("IMSI not found").
      UNDO TRANS_BLK, RETURN.
   END. /* IF NOT AVAILABLE IMSI THEN DO: */
   ELSE DO:
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhIMSI).
      IMSI.CustNum = termMobSub.custnum.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhIMSI).
   END. /* ELSE DO: */

   FIND FIRST Msowner WHERE
              Msowner.MsSeq = termMobSub.MsSeq EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE Msowner THEN DO:
      fReqError("Msowner not found").
      UNDO TRANS_BLK, RETURN.
   END. /* IF NOT AVAILABLE Msowner THEN DO: */
   ELSE DO:
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSOWNER).
      MSOWner.TSEnd = 99999999.99999.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSOWNER).
   END. /* ELSE DO: */

   CREATE Mobsub.
   BUFFER-COPY TermMobsub TO Mobsub.
   DELETE TermMobsub.

   RELEASE MSISDN.

   IF NOT Mobsub.PayType THEN
      ASSIGN lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS")
             lcALLPostpaidBundles  = fCParamC("ALL_POSTPAID_CONTRACTS")
             lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").
   ELSE
      lcPrePaidDataBundles  = fCParamC("PREPAID_DATA_CONTRACTS").

   IF MobSub.MultiSIMId > 0 AND MobSub.MultiSimType > 0 THEN DO:
      liMultiSIMType = (IF MobSub.MultiSIMType EQ {&MULTISIMTYPE_PRIMARY}
                        THEN {&MULTISIMTYPE_SECONDARY}
                        ELSE {&MULTISIMTYPE_PRIMARY}).

      FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSimID WHERE
                 lbMobSub.Brand = gcBrand AND
                 lbMobSub.MultiSimID = MobSub.MultiSimID AND
                 lbMobSub.MultiSimType = liMultiSIMType AND
                 lbMobSub.Custnum = MobSub.Custnum NO-ERROR.
      IF AVAIL lbMobSub THEN llMultiSIMActive = TRUE.

      /* Always find primary subs. for DSS activation */
      IF llMultiSIMActive THEN
         FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSimID WHERE
                    lbMobSub.Brand = gcBrand AND
                    lbMobSub.MultiSimID = MobSub.MultiSimID AND
                    lbMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
                    lbMobSub.Custnum = MobSub.Custnum NO-ERROR.
   END. /* IF MobSub.MultiSIMId > 0 AND */

   /* Make requests for reactivation of terminated periodical contracts */
   EMPTY TEMP-TABLE ttContract.

   /* Reactivate the Barring package(if present) */
   IF MobSub.MsStatus NE 4 THEN DO:

       RUN barrengine.p(MobSub.MsSeq,
                        "#REFRESH",
                        {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION},
                        katun,               /* creator */
                        ldCurrTS,            /* activate */
                        "",                  /* sms */
                        OUTPUT lcResult).

       liRequest = 0.
       liRequest = INTEGER(lcResult) NO-ERROR.
       /* Write possible error to a memo */
       IF liRequest = 0 OR liRequest = ? THEN
          DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                     "MobSub",
                     STRING(Mobsub.MsSeq),
                     Mobsub.CustNum,
                     "Barring and suspension",
                     "Barring REFRESH request failed: "
                     + lcResult).
   END. /* FOR EACH bSubMsRequest WHERE */

   /* LTE service reactivation */
   FIND FIRST SubSer WHERE
              SubSer.MsSeq   = Mobsub.MsSeq AND
              SubSer.ServCom = "LTE" NO-LOCK NO-ERROR.
   IF AVAIL SubSer AND SubSer.SSStat = 1 THEN DO:
      liRequest = fServiceRequest(MobSub.MsSeq,
                                  SubSer.ServCom,
                                  1,     /* ON */
                                  "",
                                  ldCurrTS,
                                  "",
                                  TRUE, /* fees */
                                  FALSE, /* sms */
                                  "",
                                  {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION},
                                  iiMsRequest, /* Father Request */
                                  FALSE,
                                  OUTPUT lcError).
      /* Write possible error to a memo */
      IF liRequest = 0 THEN
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "MobSub",
                          STRING(Mobsub.MsSeq),
                          Mobsub.CustNum,
                          SubSer.ServCom + " Service Reactivation",
                          "Service " + SubSer.ServCom +
                          " request failed " + lcError).
   END. /* IF AVAIL SubSer AND SubSer.SSStat = 1 THEN DO: */

   /* Get all the sub requests processed with termination request */
   FOR LAST bTermMsRequest WHERE
            bTermMsRequest.MsSeq     = MsRequest.MsSeq AND
            bTermMsRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
            bTermMsRequest.ReqStatus = {&REQUEST_STATUS_DONE} NO-LOCK:

     ldeTermStamp = bTermMsRequest.ActStamp.

     FOR EACH bSubMsRequest WHERE
              bSubMsRequest.OrigRequest = bTermMsRequest.MsRequest NO-LOCK:

       /* BlackBerry Reactivation */
       IF bSubMsRequest.ReqCParam1 = "BB" AND
          bSubMsRequest.ReqIParam1 = 0    AND
          bSubMsRequest.ReqStatus  = {&REQUEST_STATUS_DONE} THEN DO:
          liRequest = fServiceRequest(MobSub.MsSeq,
                                      bSubMsRequest.ReqCParam1,
                                      1,     /* ON */
                                      "",
                                      ldCurrTS,
                                      "",
                                      TRUE, /* fees */
                                      FALSE, /* sms */
                                      "",
                                      {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION},
                                      iiMsRequest, /* Father Request */
                                      FALSE,
                                      OUTPUT lcError).
          /* Write possible error to a memo */
          IF liRequest = 0 THEN
             DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                        "MobSub",
                        STRING(Mobsub.MsSeq),
                        Mobsub.CustNum,
                        "BB Service Reactivation",
                        "Service " + bSubMsRequest.ReqCParam1 +
                        " request failed " + lcError).
       END. /* IF bSubMsRequest.ReqCParam1 = "BB" AND */

       /* Cancel ongoing CONTM termination request */
       IF bSubMsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
          bSubMsRequest.ReqStatus = {&REQUEST_STATUS_NEW} AND
          bSubMsRequest.ReqCparam3 = STRING({&SUBSCRIPTION_TERM_REASON_MULTISIM}) AND
          llMultiSIMActive AND MobSub.MultiSIMId > 0 AND
          MobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} THEN DO:

          /* Change the SubRequest buffer name because
             func has MsRequest buffere reference */
          FIND FIRST MsRequest WHERE
                     MsRequest.MsRequest = bSubMsRequest.MsRequest
               NO-LOCK NO-ERROR.
          fReqStatus(4,"Primary Subscription Reactivation").
          NEXT.
       END. /* IF bSubMsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} */

       /* Handle all the periodical contracts */
       IF bSubMsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} OR
          bSubMsRequest.ReqType = {&REQTYPE_DSS} THEN DO:

          /* If MultiSIM is not active then don't reactivate DSS */
          IF bSubMsRequest.ReqCParam3 BEGINS {&DSS} THEN DO:
             IF bSubMsRequest.ReqStatus <> {&REQUEST_STATUS_NEW} THEN NEXT.
             IF NOT (llMultiSIMActive OR
                     (bSubMsRequest.ReqCParam3 = "DSS2" AND
                      fIsDSS2Allowed(MobSub.CustNum,MobSub.MsSeq,ldCurrTS,
                                     OUTPUT liDSSMsSeq,OUTPUT lcError)))
             THEN NEXT.
          END. /* IF bSubMsRequest.ReqCParam3 = {&DSS} THEN DO: */
          
          /* Ongoing request with status=0 */
          IF bSubMsRequest.ReqStatus = {&REQUEST_STATUS_NEW} THEN DO:

             /* Activate the SHAPER and HSDPA */
             IF LOOKUP(bSubMsRequest.ReqCParam3,lcPostpaidDataBundles) > 0
             THEN DO:
                llReStoreDefaultShaper = FALSE.
                /* Service Packages that need to be activated */
                FOR EACH DCServicePackage NO-LOCK WHERE
                         DCServicePackage.Brand     = gcBrand AND
                         DCServicePackage.DCEvent   = bSubMsRequest.ReqCParam3 AND
                         DCServicePackage.ToDate   >= TODAY AND
                         DCServicePackage.FromDate <= TODAY:

                    RUN pCopyPackage(MobSub.CLIType,
                                     DCServicePackage.ServPac,
                                     bSubMsRequest.ReqCParam3,
                                     MobSub.MSSeq,
                                     TODAY,
                                     ?, 
                                     FALSE,  /* create fees */
                                     TRUE,   /* solog (provisioning) */
                                     iiMsRequest,
                                     FALSE,   /* mandatory subrequest */
                                     OUTPUT liRequest). 

                    /* Write possible error to a memo */
                    IF liRequest = 0 THEN
                       DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                        "MobSub",
                                        STRING(Mobsub.MsSeq),
                                        Mobsub.CustNum,
                                        DCServicePackage.ServPac +
                                        " Service Activation",
                                        "Service " + DCServicePackage.ServPac +
                                        " request failed.").
                END. /* FOR EACH DCServicePackage NO-LOCK WHERE */
             END. /* IF LOOKUP(bSubMsRequest.ReqCParam3, */

             /* Change the SubRequest buffer name because
                func has MsRequest buffere reference */
             FIND FIRST MsRequest WHERE
                        MsRequest.MsRequest = bSubMsRequest.MsRequest
                  NO-LOCK NO-ERROR.
             fReqStatus(4,"Subscription Reactivation").

          END. /* IF bSubMsRequest.ReqStatus = {&REQUEST_STATUS_NEW} THEN DO: */

          ELSE IF bSubMsRequest.ReqStatus = {&REQUEST_STATUS_SUB_REQUEST_DONE}
          THEN DO:
             CREATE ttContract.
             ASSIGN ttContract.DCEvent   = bSubMsRequest.ReqCParam3
                    ttContract.PerContID = IF ttContract.DCEvent BEGINS "PAYTERM" THEN 
                                              bSubMsRequest.ReqIParam3 
                                           ELSE 0.
             
             FIND FIRST MsRequest WHERE
                MsRequest.MsRequest = bSubMsRequest.MsRequest NO-LOCK NO-ERROR.
             fReqStatus(4,"Subscription Reactivation").
          END.
          
          ELSE IF bSubMsRequest.ReqStatus = {&REQUEST_STATUS_DONE}      
          THEN DO:
             CREATE ttContract.
             ASSIGN ttContract.DCEvent = bSubMsRequest.ReqCParam3
                    ttContract.PerContID = IF ttContract.DCEvent BEGINS "PAYTERM" THEN
                                              bSubMsRequest.ReqIParam3
                                           ELSE 0.
          END. /* ELSE IF bSubMsRequest.ReqStatus = {&REQUEST_STATUS_DONE} */
          
       END. /* IF bSubMsRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} */
     END. /* FOR EACH bSubMsRequest WHERE */
   END. /* FOR LAST bTermMsRequest WHERE */

   /* Now find the Main request buffer */
   FIND FIRST MsRequest WHERE
              MsRequest.MsRequest = iiMsRequest NO-LOCK NO-ERROR.

   /* Activate DSS service */
   IF llMultiSIMActive THEN DO:

      IF fIsDSSActive(MobSub.CustNum,MsRequest.ActStamp) THEN
         llDSSActive = TRUE.

      IF NOT llDSSActive AND
         NOT fOngoingDSSAct(MobSub.CustNum) AND
         NOT CAN-FIND(FIRST ttContract WHERE
                            ttContract.DCEvent = {&DSS}) THEN DO:
         CREATE ttContract.
         ASSIGN ttContract.DCEvent   = {&DSS}
                ttContract.PerContID = 0.

         fIsDSSAllowed(INPUT  MobSub.CustNum,
                       INPUT  MobSub.MsSeq,
                       INPUT  MsRequest.ActStamp,
                       INPUT  {&DSS},
                       INPUT  "",
                       OUTPUT ldeCurrMonthLimit,
                       OUTPUT ldeConsumedData,
                       OUTPUT ldeOtherMonthLimit,
                       OUTPUT lcDSSResult).
      END. /* IF NOT llDSSActive AND */
   END. /* IF llMultiSIMActive AND */
   /* Activate DSS2 service */
   ELSE IF LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0 AND
      NOT CAN-FIND(FIRST ttContract WHERE
                         ttContract.DCEvent BEGINS {&DSS}) AND
      NOT fIsDSSActive(MobSub.CustNum,ldCurrTS) AND
      NOT fOngoingDSSAct(MobSub.CustNum) AND
      fIsDSS2Allowed(MobSub.CustNum,MobSub.MsSeq,ldCurrTS,
                     OUTPUT liDSSMsSeq,OUTPUT lcError) THEN DO:
      FIND FIRST lbMobSub WHERE
                 lbMobSub.MsSeq = liDSSMsSeq NO-LOCK NO-ERROR.
      IF AVAIL lbMobSub THEN DO:
         liRequest = fDSSRequest(lbMobSub.MsSeq,
                                 lbMobSub.CustNum,
                                 "CREATE",
                                 "",
                                 "DSS2",
                                 fSecOffSet(ldCurrTS,180),
                                 {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION},
                                 "",
                                 TRUE, /* create fees */
                                 0,
                                 FALSE,
                                 OUTPUT lcError).
         IF liRequest = 0 THEN
            /* write possible error to a memo */
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                             "MobSub",
                             STRING(MobSub.MsSeq),
                             MobSub.Custnum,
                             "DSS2 activation failed",
                             lcError).

      END.
   END.

   FOR EACH ttContract:

      /* Don't reactivate TARJ7 and TARJ9 service periodical contract */
      IF ttContract.DCEvent = "TARJ7" OR
         ttContract.DCEvent = "TARJ9" THEN NEXT.

      FIND FIRST DayCampaign WHERE
                 DayCampaign.Brand   = gcBrand AND
                 DayCampaign.DCEvent = ttContract.DCEvent AND
                 DayCampaign.ValidTo >= Today NO-LOCK NO-ERROR.
      IF NOT AVAIL DayCampaign THEN DO:
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                    "MobSub",
                    STRING(Mobsub.MsSeq),
                    Mobsub.CustNum,
                    "Periodical Contract",
                    ttContract.DCEvent +
                    ": Periodical contract information is missing!"). 
         DELETE ttContract.
         NEXT.
      END. /* IF NOT AVAIL DayCampaign THEN DO: */

      /* Don't store default SHAPER profile if bundle is being activated */
      /* Note: CONTF20 - DEFAULT SHAPER is configured on Contract level  */
      IF LOOKUP(ttContract.DCEvent,lcALLPostpaidBundles + "," +
                lcPrePaidDataBundles) > 0
      THEN llReStoreDefaultShaper = FALSE.

      IF ttContract.DCEvent = {&DSS} THEN
         fDSSRequest(lbMobSub.MsSeq,
                     lbMobSub.CustNum,
                     "CREATE",
                     lcDSSResult,
                     ttContract.DCEvent,
                     fSecOffSet(MsRequest.ActStamp,180),
                     {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION},
                     "",
                     TRUE,
                     MSRequest.MSRequest,
                     FALSE,
                     OUTPUT lcError).

      ELSE
         fPCActionRequest(MobSub.MsSeq,
                       ttContract.DCEvent,
                       "reactivate",
                       (IF ttContract.DCEvent EQ "BONO_VOIP" THEN
                        fSecOffSet(ldCurrTS,180) ELSE ldCurrTS),
                       TRUE,             /* create fees */
                       {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION},
                       "",
                       MSRequest.MSRequest, /* Father Request */
                       FALSE,
                       "", 
                       0,
                       ttContract.PerContID,
                       OUTPUT lcError).

      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                 "MobSub",
                 STRING(Mobsub.MsSeq),
                 Mobsub.CustNum,
                 "Periodical Contract",
                 ttContract.DCEvent +
                 ": Reactivated along with the subscription" +
                 (IF lcError > ""
                  THEN ". Request failed: " + lcError
                  ELSE "")).

      DELETE ttContract.

   END. /* FOR EACH ttContract: */

   /* Re-store DEFAULT SHAPER, if there is no bundle being reactivated */
   IF llReStoreDefaultShaper THEN DO:
      RUN pCopyPackage(MobSub.CLIType,
                       "SHAPER",
                       "",
                       MobSub.MSSeq,
                       TODAY,
                       ?,
                       FALSE,  /* create fees */
                       TRUE,   /* solog (provisioning) */
                       MsRequest.MsRequest,
                       FALSE,   /* mandatory subrequest */
                       OUTPUT liRequest).
      IF liRequest = 0 THEN
         /* write possible error to a memo */
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.Custnum,
                          "DEFAULT SHAPER ACTIVATION FAILED",
                          "DEFAULT SHAPER ACTIVATION FAILED").
   END. /* IF llReStoreDefaultShaper THEN DO: */

   /* DSS related changes */
   IF NOT MobSub.PayType THEN DO:
      /* Add postpaid subs. to DSS group if DSS group is active */
      lcBundleId = fGetActiveDSSId(INPUT MobSub.CustNum,INPUT ldCurrTS).
      IF lcBundleId = {&DSS} OR (lcBundleId = "DSS2" AND
         LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0) THEN
         RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                               INPUT Mobsub.CLI,
                               INPUT Mobsub.CustNum,
                               INPUT "ADD",
                               INPUT "",        /* Optional param list */
                               INPUT MsRequest.MsRequest,
                               INPUT fSecOffSet(ldCurrTS,180),
                             INPUT {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION},
                               INPUT lcBundleId).
   END. /* IF NOT MobSub.PayType THEN DO: */

   /* Create Topup for prepaid subscriptions */
   IF MobSub.PayType THEN DO:
      FOR EACH Memo WHERE
               Memo.Brand     = gcBrand AND
               Memo.CustNum   = MobSub.CustNum AND
               Memo.HostTable = "MobSub" AND
               Memo.KeyValue  = STRING(MobSub.MsSeq) AND
               Memo.MemoTitle = "Prepaid Balance" NO-LOCK
          BY Memo.MemoSeq DESC:
         IF Memo.MemoText > "" AND
            NUM-ENTRIES(Memo.MemoText," ") >= 3 THEN
            ldeTopupAmount = DEC(ENTRY(3,Memo.MemoText," ")) NO-ERROR.
         LEAVE.
      END. /* FOR EACH Memo WHERE */

      IF ldeTopupAmount <= 0 THEN ldeTopupAmount = 0.01.

      FIND FIRST Customer WHERE
                 Customer.CustNum = MobSub.CustNum NO-LOCK NO-ERROR.
      IF AVAIL Customer THEN
         lcTaxZone = fRegionTaxZone(Customer.Region).

      fCreateTopUpRequest(MobSub.MsSeq,
                          MobSub.CLI,
                          "AdjustmentTRequest",
                          "COMP",
                          "AdjustmentTRequest",
                          "970",
                          "",    /* reference */
                          lcTaxZone,
                          0,
                          ldeTopupAmount * 100,
                          0).
   END. /* IF MobSub.PayType THEN DO: */

   /* Request handled succesfully */
   FIND FIRST MsRequest WHERE
              MsRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.

   FIND Customer NO-LOCK WHERE
        Customer.Custnum = MobSub.Custnum NO-ERROR.
   
   IF AVAIL Customer AND Customer.Language NE 1 THEN DO:

      liRequest = fServiceRequest(
                     MobSub.MsSeq,
                     "LANG",
                     Customer.Language,
                     "", /* param */
                     ldCurrTS,
                     "", /* salesman */
                     TRUE,      /* fees */
                     FALSE,      /* sms */
                     "", /* usercode */
                     {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION},
                     msrequest.msrequest, /* father request */
                     false, /* mandatory for father request */
                     OUTPUT lcerror).
      
      IF liRequest = 0 THEN                               
         /* write possible error to a memo */
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "MobSub",
                          STRING(MobSub.MsSeq),
                          MobSub.Custnum,
                          "Voicemail language change failed",
                          lcError).
   END.

   fReqStatus(2,"").

   /* Mark Order as delivered */
   IF MsRequest.ReqIparam1 > 0 THEN DO:
      FIND FIRST Order WHERE
                 Order.Brand   = gcBrand AND
                 Order.OrderId = MsRequest.ReqIparam1
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL Order THEN DO:

         /* Mark mnp process finished */
         FIND FIRST MNPProcess WHERE
                    MNPProcess.OrderId = Order.OrderId AND
                    MNPProcess.MNPType = {&MNP_TYPE_IN} AND
                    MNPProcess.StatusCode = {&MNP_ST_ACON}
              EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL MNPProcess THEN
            ASSIGN MNPProcess.UpdateTS = ldCurrTS
                   MNPProcess.StatusCode = {&MNP_ST_APOR}
                   Order.MNPStatus = {&MNP_ST_APOR} + 1.

         /* Mark order to delivery */
         fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_DELIVERED}).
         fMarkOrderStamp(Order.OrderID,
                         "Delivery",
                         MsRequest.ActStamp).
        /* YDR-2037
          Restore STC/BTC request by finding a 
          cancelled STC/BTC request
          * Subscription has been terminated due to MNP OUT reason
          * STC/BTC request has been cancelled due to subscription termination
         */
         ASSIGN
            lbolSTCRequestFound = FALSE
            lbolBTCRequestFound = FALSE.
         /* STC */
        IF CAN-FIND(FIRST MSRequest WHERE
                   MSRequest.MsSeq       EQ Mobsub.MsSeq AND
                   MSRequest.ReqType     EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                   MSRequest.ReqStatus   EQ {&REQUEST_STATUS_CANCELLED} AND
                   MSRequest.ActStamp    <  Msrequest.ActStamp) THEN
        lbolSTCRequestFound = TRUE.           
        /* BTC */
        IF NOT lbolSTCRequestFound THEN
        IF CAN-FIND(FIRST MSRequest WHERE
                   MSRequest.MsSeq       EQ Mobsub.MsSeq AND
                   MSRequest.ReqType     EQ {&REQTYPE_BUNDLE_CHANGE} AND
                   MSRequest.ReqStatus   EQ {&REQUEST_STATUS_CANCELLED} AND
                   MSRequest.ActStamp    <  Msrequest.ActStamp) THEN
        lbolBTCRequestFound = TRUE.           
        /* Termination request */
        FOR EACH bMSRequest NO-LOCK WHERE
                   bMSRequest.MsSeq       EQ Mobsub.MsSeq AND
                   bMSRequest.ReqType     EQ {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                   bMSRequest.ReqStatus   EQ {&REQUEST_STATUS_DONE} AND
                   bMSRequest.ReqCParam3  EQ STRING({&SUBSCRIPTION_TERM_REASON_MNP}) AND
                   MSRequest.ActStamp    <  Msrequest.ActStamp
                   BREAK BY bMSRequest.ActStamp DESC:
           IF FIRST(bMSRequest.ActStamp) THEN LEAVE.
        END. /* FOR EACH bMSRequest */
        /* renewal order exists with the same creation date
           as cancelled STC request creation date */
        FIND FIRST bRenewalOrder NO-LOCK WHERE
                   bRenewalOrder.MSSeq     EQ bMSRequest.MsSeq AND
                   LOOKUP(bRenewalOrder.StatusCode,{&ORDER_CLOSE_STATUSES}) EQ 0 AND
                   bRenewalOrder.OrderType EQ {&ORDER_TYPE_RENEWAL} AND
                   TRUNCATE(bRenewalOrder.CrStamp,0) EQ TRUNCATE(bMSRequest.CreStamp,0)
        NO-ERROR.
        /*  create a new request with the same input parameters */
        IF AVAILABLE bRenewalOrder AND
           AVAILABLE bMSRequest THEN
        DO:
           IF lbolSTCRequestFound THEN   
           liRequest = fCTChangeRequest(
             bMSRequest.MsSeq,        /* Subscription */
             bMSRequest.ReqCParam1,   /* CLIType */
             bMSRequest.ReqCparam5,   /* BundleType */
             bMSRequest.ReqCparam3,   /* BankNumber */
             bMSRequest.ReqDParam1,   /* ChgStamp */
             bMSRequest.ReqIParam1,   /* CreditCheck */
             bMSRequest.ReqIParam5,   /* RequestFlags */
             bMSRequest.Salesman,     /* Salesman */
             bMSRequest.CreateFees,   /* CreateFees */
             IF bMSRequest.SendSMS EQ 1 THEN TRUE ELSE FALSE, /* SendSMS */
             bMSRequest.UserCode,     /* Creator */
             bMSRequest.ReqDParam2,   /* Fee */
             bMSRequest.ReqSource,    /* RequestSource */
             bMSRequest.ReqIParam2,   /* OrderId */
             bMSRequest.OrigRequest,  /* Father request id */
             bMSRequest.ReqCparam6,   /* For DMS usage contract_id */
             OUTPUT lcInfo).
           lbolIsUpgradeUpsell = IF bMSRequest.ReqCParam5 EQ 
              (bMSRequest.ReqCParam1 + "TO" + bMSRequest.ReqCParam2)
              THEN TRUE ELSE FALSE.
           IF lbolBTCRequestFound THEN  
           liRequest = fBundleChangeRequest(
              bMSRequest.MsSeq,        /* Subscription */
              bMSRequest.ReqCParam1,   /* old (current) bundle */ 
              bMSRequest.ReqCParam2,   /* new bundle */
              fMakeTS(),               /* When request should be handled */
              bMSRequest.ReqSource,    /* RequestSource */
              bMSRequest.UserCode,     /* Creator */ 
              bMSRequest.CreateFees,   /* CreateFees */    
              bMSRequest.OrigRequest,  /* main request */
              (IF bMSRequest.Mandatory 
              EQ 1 THEN TRUE ELSE FALSE), /* is subrequest mandatory */
              lbolIsUpgradeUpsell,     /* is upgrade upsell */
              bMSRequest.ReqIParam5,   /* extend terminal contract */
              bMSRequest.ReqCparam6,   /* For DMS usage, contract_id */
              OUTPUT lcInfo).
           IF liRequest EQ 0 THEN                               
              /* write possible error to a memo */
              DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                               "MobSub",
                               STRING(MobSub.MsSeq),
                               MobSub.Custnum,
                               "STC/BTC creation failed",
                               lcInfo).
        END.
        /* YDR-2037 */
         /* Re-launch retention order if present */
         FIND FIRST OrderAction WHERE
                    OrderAction.Brand    = gcBrand AND
                    OrderAction.OrderId  = Order.OrderId AND
                    OrderAction.ItemType = "OrderId" NO-LOCK NO-ERROR.
         IF AVAIL OrderAction THEN DO:
            FIND FIRST bOrder WHERE
                       bOrder.Brand   = gcBrand AND
                       bOrder.StatusCode = {&ORDER_STATUS_CLOSED} AND
                       bOrder.OrderId = INT(OrderAction.ItemKey)
                 NO-LOCK NO-ERROR.
            IF AVAIL bOrder THEN DO:

               /* Iban conversion for BankAccount */
               FIND FIRST OrderCustomer NO-LOCK WHERE
                          OrderCustomer.Brand = gcBrand AND
                          OrderCustomer.OrderID = Order.OrderID AND
                          OrderCustomer.RowType = 1 NO-ERROR.
               IF AVAIL OrderCustomer AND LENGTH(OrderCustomer.BankCode) EQ 20 THEN DO:
                  FIND CURRENT OrderCustomer EXCLUSIVE-LOCK.
                  lcBankAccount = fCalcSepaBankAcc(OrderCustomer.BankCode).

                  ASSIGN
                     OrderCustomer.BankCode = lcBankAccount.

                  RELEASE OrderCustomer.
               END.

               /* Delete existing close order stamp */
               FIND FIRST OrderTimeStamp WHERE
                          OrderTimeStamp.Brand   = gcBrand   AND
                          OrderTimeStamp.OrderID = bOrder.OrderId AND
                          OrderTimeStamp.RowType = 3
                    EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
               IF AVAIL OrderTimeStamp THEN DELETE OrderTimeStamp.

               fSetOrderStatus(bOrder.OrderId,{&ORDER_STATUS_RENEWAL}).
               fMarkOrderStamp(bOrder.OrderId,
                               "Change",
                               MsRequest.ActStamp).
            END. /* IF AVAIL bOrder THEN DO: */
         END. /* IF AVAIL OrderAction THEN DO: */
      END. /* IF AVAIL Order THEN DO: */
   END. /* IF MsRequest.ReqIparam1 > 0 THEN DO: */

   /* YDR-819 - Secondary line CONTM can exists alone for certain period */
   IF AVAIL MobSub AND MobSub.MultiSIMId > 0 AND
      MobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY} THEN DO:
      FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
                 lbMobSub.Brand  = gcBrand AND
                 lbMobSub.MultiSimID = MobSub.MultiSimID AND
                 lbMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
                 lbMobSub.Custnum = MobSub.Custnum NO-ERROR.
      IF NOT AVAIL lbMobSub THEN DO:
         FIND FIRST TermMobSub NO-LOCK /*USE-INDEX MultiSIM*/ WHERE
                    TermMobSub.Brand  = gcBrand AND
                    TermMobSub.MultiSimID = MobSub.MultiSimID AND
                    TermMobSub.MultiSimType = {&MULTISIMTYPE_PRIMARY} AND
                    TermMobSub.Custnum = MobSub.Custnum NO-ERROR.
         IF NOT AVAIL TermMobSub THEN
            ldaSecSIMTermDate = TODAY.
         ELSE DO:
            FIND FIRST Msowner WHERE 
                       Msowner.MsSeq = TermMobsub.MsSeq NO-LOCK NO-ERROR.
            IF AVAIL Msowner THEN
               fSplitTS(Msowner.TSEnd,OUTPUT ldaSecSIMTermDate,
                        OUTPUT liSecSIMTermTime).
            ELSE ldaSecSIMTermDate = TODAY.
         END. /* ELSE DO: */

         /* Termination logic related to Multisim (Mainline, Additional line) */
         fTermAdditionalSim(MobSub.Msseq,
                            MobSub.CLI,
                            MobSub.CustNum,
                            {&SUBSCRIPTION_TERM_REASON_MULTISIM},
                            ldaSecSIMTermDate,
                            {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION},
                            iiMSRequest,
                            OUTPUT lcError).
                            
      END. /* IF NOT AVAIL lbMobSub THEN DO: */
   END. /* IF AVAIL MobSub AND MobSub.MultiSIMId > 0 AND */

   /* HPD - Trigger some extra events to Cassandra */
   RUN pTriggerEvents(INPUT MobSub.MsSeq,
                      INPUT MobSub.CLIType,
                      INPUT ldeTermStamp).

END. /* DO TRANSACTION: */

RETURN "".

END PROCEDURE.


PROCEDURE pTriggerEvents:

   DEF INPUT PARAMETER iiMsSeq        AS INT  NO-UNDO.
   DEF INPUT PARAMETER icCLIType      AS CHAR NO-UNDO.
   DEF INPUT PARAMETER ideTermStamp   AS DEC  NO-UNDO.

   DEF VAR ldStartDate                AS DATE NO-UNDO.
   DEF VAR ldTermDate                 AS DATE NO-UNDO.
   DEF VAR liTermTime                 AS INT  NO-UNDO.
   DEF VAR ldeStartStamp              AS DEC  NO-UNDO.
   DEF VAR ldtTimeStamp               AS DATETIME NO-UNDO.
   DEF VAR lcReqChar                  AS CHAR NO-UNDO.
   DEF VAR lcDel2                     AS CHAR NO-UNDO.
   DEF VAR lcServList                 AS CHAR NO-UNDO.
   DEF VAR lcServCom                  AS CHAR NO-UNDO.
   DEF VAR liCount                    AS INT  NO-UNDO.
   DEF VAR liNumEntries               AS INT  NO-UNDO.

   fSplitTS(ideTermStamp,OUTPUT ldTermDate,OUTPUT liTermTime).

   ASSIGN ldStartDate   = DATE(MONTH(ldTermDate),1,YEAR(ldTermDate))
          ldeStartStamp = fMake2Dt(ldStartDate,0)
          ldtTimeStamp  = DATETIME(TODAY,MTIME) + 15000
          lcServList    = {&HPD_SERVICES}
          liNumEntries  = NUM-ENTRIES(lcServList)
          lcDel2        = CHR(255).

   FOR EACH MServiceLimit WHERE
            MServiceLimit.MsSeq  = iiMsSeq AND
            MServiceLimit.EndTS >= ldeStartStamp NO-LOCK,
      FIRST ServiceLimit WHERE
            ServiceLimit.SlSeq = MServiceLimit.SlSeq NO-LOCK:

      IF fMatrixAnalyse(gcBrand,
                        "PERCONTR",
                        "PerContract;SubsTypeTo",
                        ServiceLimit.GroupCode + ";" + icCLIType,
                        OUTPUT lcReqChar) NE 1 THEN NEXT.

      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(MServiceLimit)
         Common.RepLog.TableName = "MServiceLimit"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = STRING(MServiceLimit.MSID)
         Common.RepLog.EventTS   = ldtTimeStamp.

      RELEASE Common.RepLog.
 
   END. /* FOR EACH MServiceLimit WHERE */

   FOR EACH MServiceLPool WHERE
            MServiceLPool.MsSeq  = iiMsSeq AND
            MServiceLPool.EndTS >= ldeStartStamp NO-LOCK,
      FIRST ServiceLimit WHERE
            ServiceLimit.SlSeq = MServiceLPool.SlSeq NO-LOCK:

      IF fMatrixAnalyse(gcBrand,
                        "PERCONTR",
                        "PerContract;SubsTypeTo",
                        ServiceLimit.GroupCode + ";" + icCLIType,
                        OUTPUT lcReqChar) NE 1 THEN NEXT.

      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(MServiceLPool)
         Common.RepLog.TableName = "MServiceLPool"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = STRING(MServiceLPool.MsSeq) + lcDel2 +
                                   STRING(MServiceLPool.SLSeq) + lcDel2 +
                                   STRING(MServiceLPool.EndTS)
         Common.RepLog.EventTS   = ldtTimeStamp.

      RELEASE Common.RepLog.
 
   END. /* FOR EACH MServiceLPool WHERE */

   FOR EACH DCCLI WHERE
            DCCLI.MsSeq    = iiMsSeq AND
            DCCLI.ValidTo >= ldStartDate NO-LOCK:

      CREATE Mobile.RepLog.
      ASSIGN
         Mobile.RepLog.RecordId  = RECID(DCCLI)
         Mobile.RepLog.TableName = "DCCLI"
         Mobile.RepLog.EventType = "MODIFY"
         Mobile.RepLog.KeyValue  = STRING(DCCLI.PerContractID)
         Mobile.RepLog.EventTS   = ldtTimeStamp.

      RELEASE Mobile.RepLog.

   END. /* FOR EACH DCCLI WHERE */

   DO liCount = 1 TO liNumEntries:

      lcServCom = ENTRY(liCount,lcServList).

      FOR FIRST SubSer WHERE
                SubSer.MsSeq   = MobSub.MsSeq AND
                SubSer.ServCom = lcServCom NO-LOCK:

         CREATE Mobile.RepLog.
         ASSIGN
            Mobile.RepLog.RecordId  = RECID(SubSer)
            Mobile.RepLog.TableName = "SubSer"
            Mobile.RepLog.EventType = "MODIFY"
            Mobile.RepLog.KeyValue  = STRING(SubSer.MsSeq) + lcDel2 +
                                      SubSer.ServCom       + lcDel2 +
                                      STRING(SubSer.SSDate)
            Mobile.RepLog.EventTS   = ldtTimeStamp.

         RELEASE Mobile.RepLog.

      END. /* FOR FIRST SubSer WHERE */
   END. /* DO liCount = 1 TO liNumEntries: */

END PROCEDURE.

