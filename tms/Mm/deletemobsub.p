/* deletemobsub.p       2006/jp

   changes:             17.01.07/aam terminate periodical contract (dccli)
                        16.08.07/aam clean eventlog
                        03.09.07/aam don't mark create fees for prepaid on
                                     per.contract termination
                        11.03.08/jp  removed hardcoded "begins tarj"        
      22.sep.2015 hugo.lujan - YPR-2521 - [Q25] - TMS - Subscription 
       termination/ MNP out porting, STC (postpaid to prepaid)
*/
   
{Syst/commali.i}
{Func/date.i}
{Syst/eventval.i}
{Func/fctserval.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
{Func/msreqfunc.i}
{Func/msisdn.i}
{Func/fcreditreq.i}
{Func/flimitreq.i}
{Func/fdss.i}
{Func/msisdn_prefix.i}
{Func/fsubstermreq.i}
{Mnp/mnpoutchk.i}
{Func/ordercancel.i}
{Func/dextra.i}
{Func/main_add_lines.i}
{Func/fixedlinefunc.i}
{Func/orderfunc.i}
{Mc/dpmember.i}
{Func/multitenantfunc.i}

DEFINE INPUT  PARAMETER iiMSrequest AS INT  NO-UNDO.

llCleanFLimitReqEventLog = FALSE.

DEFINE VARIABLE llOutPort   AS LOGICAL   NO-UNDO.       
DEFINE VARIABLE lcOutOper   AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldBilled    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lcError     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldCurrTS    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE llCreateFee AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE ldaKillDate AS DATE      NO-UNDO.
DEFINE VARIABLE liTime      AS INTEGER   NO-UNDO.
DEFINE VARIABLE liQuarTime  AS INTEGER   NO-UNDO.
DEFINE VARIABLE liMsisdnStat AS INTEGER NO-UNDO.
DEFINE VARIABLE liSimStat   AS INTEGER NO-UNDO.
DEFINE VARIABLE lcTermReason AS CHARACTER NO-UNDO.

DEF BUFFER bMNPSub FOR MNPSub.
DEF BUFFER bMobsub FOR MobSub.

DEF TEMP-TABLE ttContract NO-UNDO
   FIELD DCEvent   AS CHAR
   FIELD PerContID AS INT
   FIELD CreateFee AS LOG
   FIELD ActTS     AS DEC.

FUNCTION fLocalMemo RETURNS LOGIC
   (icHostTable AS CHAR,
    icKey       AS CHAR,
    icTitle     AS CHAR,
    icText      AS CHAR):

   CREATE Memo.
   ASSIGN
      Memo.Brand     = gcBrand
      Memo.CreStamp  = ldCurrTS
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.Custnum   = (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0)
      Memo.HostTable = icHostTable
      Memo.KeyValue  = icKey
      Memo.CreUser   = katun
      Memo.MemoTitle = icTitle
      Memo.Memotext  = icText.
      
END FUNCTION.


ldCurrTS = fMakeTS().

FIND FIRST MSRequest WHERE 
           MSRequest.MSRequest = iiMSRequest
NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR 
   MsRequest.ReqType NE {&REQTYPE_SUBSCRIPTION_TERMINATION} 
   THEN RETURN "ERROR".

IF llDoEvent THEN DO:

   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMobSub    AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhMSISDN    AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhMsOwner   AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhSIM       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhIMSI      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhSingleFee AS HANDLE    NO-UNDO.
   
   lhMobSub  = BUFFER MobSub:HANDLE.
   lhMSISDN  = BUFFER MSISDN:HANDLE.
   lhMsOwner = BUFFER MsOwner:HANDLE.
   lhSIM     = BUFFER SIM:HANDLE.
   lhIMSI    = BUFFER IMSI:HANDLE.
   lhSingleFee = BUFFER SingleFee:HANDLE.

   RUN StarEventInitialize(lhMobSub).
   RUN StarEventInitialize(lhMSISDN).
   RUN StarEventInitialize(lhMsOwner).
   RUN StarEventInitialize(lhSIM).
   RUN StarEventInitialize(lhIMSI).
   RUN StarEventInitialize(lhSingleFee).
END.

RUN pTerminate.

fCleanEventObjects().

RETURN RETURN-VALUE.
   

PROCEDURE pTerminate:
 
   DEF VAR liArrivalStatus    AS INT  NO-UNDO.
   DEF VAR liMsSeq            AS INT  NO-UNDO.
   DEF VAR liDSSMsSeq         AS INT  NO-UNDO.
   DEF VAR llDSSActive        AS LOG  NO-UNDO.
   DEF VAR llDSSTransferred   AS LOG  NO-UNDO.
   DEF VAR lcBundleId         AS CHAR NO-UNDO.
   DEF VAR ldMonthEndDate     AS DATE NO-UNDO.
   DEF VAR ldeMonthEndTS      AS DEC  NO-UNDO.
   DEF VAR liRequest          AS INT  NO-UNDO.
   DEF VAR ldeDSSLimit        AS DEC  NO-UNDO.
   DEF VAR liTermRequest      AS INT  NO-UNDO. 
   DEF VAR lcSMSText          AS CHAR NO-UNDO. 
   DEF VAR ldeSMSStamp        AS DEC  NO-UNDO.
   DEF VAR liPeriod           AS INT  NO-UNDO.
   DEF VAR llCloseRVTermFee   AS LOG  NO-UNDO INIT TRUE.
   DEF VAR lcResult           AS CHAR NO-UNDO. 
   DEF VAR ldeActStamp        AS DEC  NO-UNDO.  

   DEF VAR lcPostpaidDataBundles AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.

   DEF VAR liPeriodPostpone    AS INT  NO-UNDO.
   DEF VAR ldaKillDatePostpone AS DATE NO-UNDO.

   DEF VAR llHardBook          AS LOG  NO-UNDO INIT FALSE.
   DEF VAR llCallProc          AS LOG  NO-UNDO.   
   
   DEF VAR lcTerminationType AS CHAR NO-UNDO INIT {&TERMINATION_TYPE_FULL}.

   ASSIGN liArrivalStatus = MsRequest.ReqStatus
          liMsSeq = MsRequest.MsSeq.
   
   IF LOOKUP(STRING(liArrivalStatus),"6,8") = 0 THEN RETURN.
 
    /* request is under work */
   IF NOT fReqStatus(1,"") THEN DO:
      RETURN "ERROR".
   END.

   fSplitTS(MsRequest.ActStamp, OUTPUT ldaKillDate, OUTPUT liTime).

   ASSIGN
      llOutport      = (MsRequest.ReqCParam2 NE "")
      lcOutoper      = MsRequest.ReqCParam2
      liMsisdnStat   = MsRequest.ReqIParam1
      liSimStat      = MsRequest.ReqIParam2
      liQuarTime     = MsRequest.ReqIParam3
      lcTermReason   = MsRequest.ReqCParam3
      lcTerminationType = MsRequest.ReqCParam6 WHEN MsRequest.ReqCParam6 NE ""
      lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS").

   ASSIGN ldMonthEndDate = fLastDayOfMonth(ldaKillDate)
          ldeMonthEndTS  = fMake2DT(ldMonthEndDate,86399)
          liPeriod       = YEAR(ldaKillDate) * 100 + MONTH(ldaKillDate).

   FIND FIRST MobSub WHERE
              MobSub.MsSeq = MSRequest.MSSeq
   EXCLUSIVE-LOCK NO-ERROR.

   IF NOT Avail Mobsub THEN DO:
      fReqError("Mobile subscription already deactivated").
      RETURN.
   ENd.

   /* COFF if partial termination cli = fixednumber (no actions needed) */
   IF NOT(MobSub.cli BEGINS "8" OR MobSub.cli BEGINS "9") THEN DO:
      FIND FIRST MSISDN WHERE
                 MSISDN.Brand = gcBrand AND
                 MSISDN.CLI   = MobSub.CLI
      EXCLUSIVE-LOCK NO-ERROR.

      FIND FIRST IMSI WHERE
                 IMSI.IMSI = MobSub.IMSI
      EXCLUSIVE-LOCK NO-ERROR.

      FIND FIRST SIM WHERE
                 SIM.ICC = MobSub.ICC
      EXCLUSIVE-LOCK NO-ERROR.

      fMakeMsidnHistory(INPUT RECID(MSISDN)).

      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSISDN).
      
      MSISDN.StatusCode = liMsisdnStat.
      MSISDN.CustNum    = 0.
      ASSIGN
         MSISDN.ValidTo    = fDate2TS(TODAY + liQuarTime) + (time / 100000)
                             WHEN liQuarTime >= 0
         MSISDN.ValidTo    = 99999999.99999 WHEN liQuarTime = -1.

      IF llOutPort THEN ASSIGN
         MSISDN.PortingDate = ldaKillDate  /* Date ported out  */
         MSISDN.OutOperator = lcOutOper.   /*Name of receiving op.*/

      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSISDN).
      
      /* SIM card is (TO be) destroyed */
      IF AVAIL SIM THEN DO:
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSIM).
         ASSIGN
            SIM.SimStat = liSimStat
            SIM.CustNum = 0.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSIM).
      END.
      
      IF AVAIL IMSI THEN DO:
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhIMSI).
         ASSIGN
            IMSI.CustNum = 0
            IMSI.UserSeq = 0.
      
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhIMSI).
      END.
   END.
/* COFF check */
   FIND FIRST MSOwner WHERE 
              MSOwner.CLI    = MobSub.CLI AND
              MSOwner.TsEnd >= fHMS2TS(TODAY,STRING(time,"hh:mm:ss"))
   EXCLUSIVE-LOCK NO-ERROR.

   /* TimeStamp  */
   IF AVAIL MSOwner AND lcTerminationType EQ {&TERMINATION_TYPE_FULL} THEN DO:
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsOwner).
      MSOwner.TsEnd = ldCurrTS.   
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsOwner).
   END.
   ELSE IF AVAIL MSOwner AND lcTerminationType EQ {&TERMINATION_TYPE_PARTIAL} 
      THEN 
      fUpdatePartialMSOwner(MobSub.msseq, Mobsub.fixedNumber).

   IF llOutport THEN DO:

      fLocalMemo("Customer",
                 STRING(Mobsub.CustNum),
                 "OUTPORTED to " + lcOutoper,
                 "Number:" + MobSub.CLI).
   END.

   IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMobSub).

   /* Reset flag if it is prepaid */
   IF MobSub.PayType = TRUE THEN llCloseRVTermFee = FALSE.

   /* cancel existing periodical contract termination requests */
   FOR EACH MSRequest EXCLUSIVE-LOCK WHERE
            MSRequest.MsSeq EQ liMsSeq AND
            MSRequest.ReqType EQ {&REQTYPE_CONTRACT_TERMINATION} AND
            LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9,99") = 0 :
                  
            CASE MSRequest.ReqStatus:
                 WHEN 0 THEN fReqStatus(4,"Cancelled by subs. termination").
                 WHEN 3 THEN fReqStatus(9,"Handled by subs. termination").
            END.
   END.

   /* Cancel existing periodical contract activation requests */
   FOR EACH MSRequest EXCLUSIVE-LOCK WHERE
            MSRequest.MsSeq EQ liMsSeq AND
            MSRequest.ReqType EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
            LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9,99") = 0 :
                  
            CASE MSRequest.ReqStatus:
                 WHEN 0 THEN fReqStatus(4,"Cancelled by subs. termination").
                 WHEN 3 THEN fReqStatus(9,"Handled by subs. termination").
            END. /* CASE MSRequest.ReqStatus: */
   END. /* FOR EACH MSRequest EXCLUSIVE-LOCK WHERE */

   /* Update Request Change status */
   FOR EACH MSRequest EXCLUSIVE-LOCK WHERE
      MSRequest.MsSeq EQ Mobsub.MsSeq AND
      MSRequest.ReqType EQ {&REQTYPE_SERVICE_CHANGE}:

      CASE MSRequest.ReqStatus:
      WHEN 0 THEN fReqStatus(4,"Cancelled by subs. termination").
      WHEN 3 THEN fReqStatus(9,"Handled by subs. termination").
      END.

   END.

   /* Cancel Pending DSS request */
   FOR EACH MSRequest EXCLUSIVE-LOCK WHERE
            MSRequest.MsSeq     EQ Mobsub.MsSeq   AND
            MSRequest.ReqType   EQ {&REQTYPE_DSS} AND
            MsRequest.ReqStatus EQ {&REQUEST_STATUS_CONFIRMATION_PENDING}:
      fReqStatus(4,"Subscription is terminated").
   END. /* FOR EACH MSRequest EXCLUSIVE-LOCK WHERE */

   IF MobSub.MultiSIMID > 0 THEN RUN pMultiSIMTermination(iiMSRequest).
   
   ASSIGN ldeActStamp = 0
          ldeActStamp = fHMS2TS(TODAY + 1,"").

   fAdditionalLineSTC(iiMSRequest,
                     ldeActStamp,
                     "DELETE").
   
   /* Find Original request */
   FIND FIRST MSRequest WHERE
              MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.

   /* Check whether DSS active or not */
   IF NOT MobSub.PayType AND
      fGetDSSMsSeqLimit(INPUT MobSub.CustNum,
                        INPUT MsRequest.ActStamp,
                        OUTPUT liDSSMsSeq,
                        OUTPUT ldeDSSLimit,
                        OUTPUT lcBundleId) THEN
      llDSSActive = TRUE.

   IF llDSSActive THEN DO:

      IF lcBundleId = "DSS2" THEN
         lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").

      /* Nothing if subs. type is not part of DSS2 */
      IF lcBundleId = "DSS2" AND
         LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) = 0 THEN .

      /* If directly linked to DSS */
      ELSE IF MobSub.MsSeq = liDSSMsSeq THEN DO:
         IF fIsDSSTransferAllowed(INPUT MobSub.CLI,
                                  INPUT MobSub.CustNum,
                                  INPUT MsRequest.ActStamp,
                                  INPUT lcBundleId,
                                  OUTPUT liDSSMsSeq,
                                  OUTPUT lcError) THEN DO:
            /* Transfer DSS/UPSELL */
            IF fTransferDSS(INPUT MobSub.MsSeq,
                            INPUT liDSSMsSeq,
                            INPUT ldaKillDate,
                            INPUT MsRequest.UserCode,
                            INPUT "SubscriptionTermination",
                            OUTPUT lcError) THEN DO:
               llDSSTransferred = TRUE.

               fLocalMemo("Customer",
                          STRING(Mobsub.CustNum),
                          "DSS Bundle/UPSELL",
                          "DSS Bundle/UPSELL is transferred from Subs.Id " +
                          STRING(MobSub.MsSeq) + " to Subs. Id " +
                          STRING(liDSSMsSeq)).
            END. /* IF fTransferDSS(INPUT MobSub.MsSeq,INPUT liDSSMsSeq, */
            ELSE
               fLocalMemo("Customer",
                          STRING(Mobsub.CustNum),
                          "DSS Bundle/UPSELL Transfer Failed",
                          "DSS Bundle/UPSELL was not transferred from Subs.Id " +
                          STRING(MobSub.MsSeq) + " to Subs. Id " +
                          STRING(liDSSMsSeq) + ". " + lcError).
         END. /* IF fIsDSSTransferAllowed(INPUT MobSub.CLI */

         /* DSS is not transferred - delete DSS group now */
         IF NOT llDSSTransferred THEN
            RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                                  INPUT Mobsub.CLI,
                                  INPUT Mobsub.CustNum,
                                  INPUT "DELETE",
                                  INPUT "",      /* Optional param list */
                                  INPUT MsRequest.MsRequest,
                                  INPUT ldeMonthEndTS,
                                  INPUT {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                                  INPUT lcBundleId).
         /* If DSS is transferred then remove subs. from DSS group */
         ELSE
            RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                                  INPUT Mobsub.CLI,
                                  INPUT Mobsub.CustNum,
                                  INPUT "REMOVE",
                                  INPUT "",        /* Optional param list */
                                  INPUT MsRequest.MsRequest,
                                  INPUT MsRequest.ActStamp,
                                  INPUT {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                                  INPUT lcBundleId).

      END. /* IF MobSub.MsSeq = liDSSMsSeq THEN DO: */
      /* DSS is not linked directly */
      ELSE DO:
         /* If 2nd last postpaid subs. is being changed then Delete DSS */
         IF NOT fCanDSSKeepActive(INPUT  Mobsub.CustNum,
                                  INPUT  Mobsub.MsSeq,
                                  INPUT  MsRequest.ActStamp,
                                  INPUT  lcBundleId,
                                  OUTPUT lcError) THEN
            RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                                  INPUT Mobsub.CLI,
                                  INPUT Mobsub.CustNum,
                                  INPUT "DELETE",
                                  INPUT "",     /* Optional param list */
                                  INPUT MsRequest.MsRequest,
                                  INPUT ldeMonthEndTS,
                                  INPUT {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                                  INPUT lcBundleId).
         /* Otherwise just remove subs. from DSS group */
         ELSE
            RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                                  INPUT Mobsub.CLI,
                                  INPUT Mobsub.CustNum,
                                  INPUT "REMOVE",
                                  INPUT "",     /* Optional param list */
                                  INPUT MsRequest.MsRequest,
                                  INPUT MsRequest.ActStamp,
                                  INPUT {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                                  INPUT lcBundleId).
      END. /* ELSE DO: */
   END. /* IF llDSSActive THEN DO: */

   /* make requests for terminating periodical contracts */
   EMPTY TEMP-TABLE ttContract.

   FOR EACH DCCLI EXCLUSIVE-LOCK WHERE
            DCCLI.MsSeq   = MobSub.MsSeq  AND
            DCCLI.ValidTo >= TODAY:
         
      DCCLI.TermDate = ?.

      /* COFF Partial termination */
      IF (lcTerminationType EQ {&TERMINATION_TYPE_PARTIAL} AND
         fIsConvergentFixedContract(DCCLI.DCEvent)) THEN NEXT.

      FIND FIRST DayCampaign NO-LOCK WHERE
                 DayCampaign.Brand = gcBrand AND
                 DayCampaign.DcEvent = DCCLI.DcEvent NO-ERROR.
   
      CREATE ttContract.
      ASSIGN
         ttContract.DCEvent   = DCCLI.DCEvent
         ttContract.CreateFee = DCCLI.CreateFee
         ttContract.PerContID = (IF AVAIL DayCampaign AND
                                   DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT}
                                THEN DCCLI.PerContractID 
                                ELSE 0).

      /* Reset the flag if leasing installment contract is being terminated */
      IF DCCLI.DCEvent BEGINS "PAYTERM" AND
         DCCLI.Amount > 0 THEN llCloseRVTermFee = FALSE.
   END.

   FOR EACH MServiceLimit EXCLUSIVE-LOCK WHERE
            MServiceLimit.MsSeq = MobSub.MsSeq AND
            MServiceLimit.EndTS > ldCurrTS, 
      FIRST ServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
            ServiceLimit.SLSeq = MServiceLimit.SLSeq:

      /* COFF Partial termination */
      IF (lcTerminationType EQ {&TERMINATION_TYPE_PARTIAL} AND 
         fIsConvergentFixedContract(ServiceLimit.groupcode)) THEN NEXT.
         
      /* DSS bundle has been handled before */
      
      IF ServiceLimit.GroupCode BEGINS {&DSS} THEN NEXT.

      FIND FIRST DayCampaign WHERE 
                 DayCampaign.Brand      = gcBrand AND 
                 DayCampaign.DCEvent    = ServiceLimit.GroupCode AND 
                 DayCampaign.ValidFrom <= Today AND 
                 DayCampaign.ValidTo   >= Today NO-LOCK NO-ERROR.
              
      IF AVAILABLE DayCampaign THEN DO:
         IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 AND
            STRING(DayCampaign.DCType) <> {&DCTYPE_CUMULATIVE_RATING}
         THEN DO:
            FIND FIRST ttContract WHERE
                       ttContract.DCEvent = ServiceLimit.GroupCode NO-ERROR.
            IF NOT AVAILABLE ttContract THEN DO:
               CREATE ttContract.
               ASSIGN
                  ttContract.DCEvent   = ServiceLimit.GroupCode
                  ttContract.CreateFee = (DayCampaign.TermFeeModel > "").

               /* If bundle is linked with DSS and last month fee is full
                  then set bundle deactivation date End of month because
                  customer can use bundle till EOM */
               IF llDSSActive AND 
                  LOOKUP(ttContract.DCEvent,lcPostpaidDataBundles) > 0 THEN DO:

                  IF lcBundleId = {&DSS} OR (lcBundleId = "DSS2" AND
                     LOOKUP(MobSub.CLIType,lcAllowedDSS2SubsType) > 0) THEN
                  FOR FIRST FeeModel NO-LOCK WHERE
                            FeeModel.Brand = gcBrand AND
                            FeeModel.FeeModel = DayCampaign.FeeModel,
                      FIRST FMItem NO-LOCK WHERE
                            FMItem.Brand = gcBrand AND
                            FMItem.FeeModel = FeeModel.FeeModel AND
                            FMItem.Todate >= TODAY AND
                            FMItem.BrokenRental = 1: /* full month */
                     ttContract.ActTS = ldeMonthEndTS.
                  END.
               END.
            END.
               
         END. /* IF LOOKUP(DayCampaign.DCType,"1,4") > 0 THEN DO: */
         
         /* No need to close UPSELL contracts, it will closed end of month */
         ELSE IF LOOKUP(DayCampaign.DCType,{&DCTYPE_POOL_RATING}) > 0 THEN NEXT.

      END. /* IF AVAILABLE DayCampaign THEN DO: */

      /* not related to a periodical contract */
      ELSE DO:
         MServiceLimit.EndTS = ldCurrTS.
      END.
   END.

   FOR EACH ttContract:

      FIND FIRST DayCampaign WHERE 
                 DayCampaign.Brand      = gcBrand AND 
                 DayCampaign.DCEvent    = ttContract.DCEvent AND 
                 DayCampaign.ValidTo   >= Today NO-LOCK NO-ERROR.
              
      IF NOT AVAIL DayCampaign THEN DO:
         fLocalMemo("Customer",
                    STRING(Mobsub.CustNum),
                    "Periodical Contract",
                    ttContract.DCEvent +
                    ": Periodical contract information is missing!"). 
         NEXT.
      END.

      IF MobSub.PayType = FALSE AND ttContract.CreateFee
      THEN llCreateFee = TRUE.
      ELSE llCreateFee = FALSE.

      IF LOOKUP(lcTermReason,"3,5,9,10,11,12") > 0 THEN
         llCreateFee = FALSE.
      
      liTermRequest = 
         fPCActionRequest(MobSub.MsSeq,  
                          ttContract.DCEvent,
                          "term",
                          IF ttContract.ActTS > 0
                          THEN ttContract.ActTS
                          ELSE fSecOffSet(ldCurrTS,60),
                          llCreateFee,             /* create fees */
                          {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                          "",
                          iiMsRequest,
                          FALSE,
                          "",
                          0,
                          ttContract.PerContID,
                          OUTPUT lcError).

      fLocalMemo("Customer",
                 STRING(Mobsub.CustNum),
                 "Periodical Contract",
                 ttContract.DCEvent +
                 ": Terminated along with the subscription" +
                 (IF lcError > ""
                  THEN ". Request failed: " + lcError
                  ELSE "")).

      DELETE ttContract.
   END.

   /* Update Subscription type change status */
   FOR EACH MSRequest EXCLUSIVE-LOCK WHERE
      MSRequest.MsSeq EQ Mobsub.MsSeq AND
      MSRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}:

      CASE MSRequest.ReqStatus:
      WHEN 0 THEN fReqStatus(4,"Cancelled by subs. termination").
      WHEN 3 THEN fReqStatus(9,"Handled by subs. termination").
      WHEN 7 THEN fReqStatus(4,"Cancelled by subs. termination").
      WHEN 8 THEN fReqStatus(4,"Cancelled by subs. termination").
      END.

   END.

   /* Update ICC Change status */
   FOR EACH MSRequest EXCLUSIVE-LOCK WHERE
      MSRequest.MsSeq EQ Mobsub.MsSeq AND
      MSRequest.ReqType EQ {&REQTYPE_ICC_CHANGE}:
   
      CASE MSRequest.ReqStatus:
      WHEN 0  THEN fReqStatus(4,"Cancelled by subs. termination").
      WHEN 3  THEN fReqStatus(9,"Handled by subs. termination").
      WHEN 15 THEN fReqStatus(4,"Cancelled by subs. termination").
      WHEN 19 THEN fReqStatus(4,"Cancelled by subs. termination").
      END.

   END.
 
   /* Update MSISDN Change status */
   FOR EACH MSRequest EXCLUSIVE-LOCK WHERE 
      MSRequest.MsSeq EQ Mobsub.MsSeq AND
      MSRequest.ReqType EQ {&REQTYPE_MSISDN_CHANGE}: 
   
      CASE MSRequest.ReqStatus:
      WHEN 0 THEN fReqStatus(4,"Cancelled by subs. termination").
      WHEN 3 THEN fReqStatus(9,"Handled by subs. termination").
      END.

   END.

   /* Update Bundle Change status */
   FOR EACH MSRequest EXCLUSIVE-LOCK WHERE 
      MSRequest.MsSeq EQ Mobsub.MsSeq AND
      MSRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}: 
   
      CASE MSRequest.ReqStatus:
      WHEN 0 THEN fReqStatus(4,"Cancelled by subs. termination").
      WHEN 3 THEN fReqStatus(9,"Handled by subs. termination").
      END.

   END.

   /* Update ACC status */
   FOR EACH MSRequest EXCLUSIVE-LOCK WHERE
      MSRequest.MsSeq EQ Mobsub.MsSeq AND
      MSRequest.ReqType EQ {&REQTYPE_AGREEMENT_CUSTOMER_CHANGE}:

      CASE MSRequest.ReqStatus:
      WHEN 0 THEN fReqStatus(4,"Cancelled by subs. termination").
      WHEN 8 THEN fReqStatus(4,"Cancelled by subs. termination").
      WHEN 19 THEN fReqStatus(4,"Cancelled by subs. termination").
      END.
   END.

   /* Quota 25 q25 - YPR-2521 */
   FOR EACH MSRequest NO-LOCK WHERE  
            MSRequest.MsSeq      EQ Mobsub.MsSeq AND
            MSRequest.ReqType    EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
            MsRequest.ReqStatus  EQ 0 AND
            MSREquest.REqcparam3 EQ "RVTERM12":   
      fReqStatus(4,"Cancelled by subs. termination").
   END.
         
   /* Find Original request */
   FIND FIRST MSRequest WHERE
              MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.

   /* commission termination */
   RUN Ar/commission_term.p(MobSub.MsSeq,
                       "termination",
                       OUTPUT liReqCnt).

   /* mark mnp process finished */
   IF lcTermReason = "2" THEN DO:

      MNP_LOOP:
      FOR EACH MNPSub WHERE
         MNPSub.MsSeq = MobSub.MsSeq NO-LOCK,
         FIRST MNPProcess WHERE
               MNPProcess.MNPSeq = MNPSub.MNPSeq AND 
               MNPProcess.StatusCode = ({&MNP_ST_ACON}) AND
               MNPProcess.MNPType = ({&MNP_TYPE_OUT}) EXCLUSIVE-LOCK:

         /* check that all subscriptions are terminated 
            before marking mnp out process closed */
         FOR EACH bMNPSub WHERE
                  bMNPSub.MNPSeq = MNPSub.MNPSeq AND
                  bMNPSub.MsSeq NE MNPSub.MsSeq NO-LOCK:
            IF CAN-FIND(FIRST bMobsub NO-LOCK WHERE
               bMobsub.MsSeq = bMNPSub.MsSeq) THEN LEAVE MNP_LOOP.
         END.

         ASSIGN
            MNPProcess.UpdateTS = fMakeTS()
            MNPProcess.StatusCode = ({&MNP_ST_APOR}).
      END.
         
      FOR EACH Order NO-LOCK WHERE
               Order.MsSeq = MobSub.MsSeq AND
               Order.StatusCode = {&ORDER_STATUS_MNP_RETENTION}:

         RUN Mc/closeorder.p(Order.OrderID, TRUE).
         
         FIND FIRST MNPRetPlatform WHERE
                    MNPRetPlatform.Brand = gcBrand AND
                    MNPRetPlatform.RetentionPlatform = MNPSub.RetentionPlatform
         NO-LOCK NO-ERROR.

         IF AVAIL MNPRetPlatform AND 
            NOT fIsConvergenceTariff(Order.CliType) THEN DO:

            FIND FIRST Customer NO-LOCK WHERE
                       Customer.Custnum = MobSub.Custnum NO-ERROR.

            lcSMSText = fGetSMSTxt("MNPCloseRetention",
                                   TODAY,
                                   (IF AVAIL Customer
                                   THEN Customer.Language ELSE 1),
                                   OUTPUT ldeSMSStamp).

            IF lcSMSText > "" THEN DO:

               lcSMSText = REPLACE(lcSMSText,"#SMSSENDER",MNPRetPlatform.SMSSender).

               fMakeSchedSMS2(MobSub.CustNum,
                             MobSub.CLI,
                             12,
                             lcSMSText,
                             ldeSMSStamp,
                             MNPRetPlatform.SMSSender,
                             "").
            END.
         END.
      END.
   END.

   /* YNC-61 + YDR-193 */
   IF LOOKUP(lcTermReason,"1,4,5,6,9,10") > 0 THEN 
   DO:
      IF fIsYoigoCLI(MobSub.CLI) EQ FALSE AND fIsMasmovilCLI(MobSub.CLI) EQ FALSE THEN
      DO:
          RUN Mnp/mnpnumbertermrequest.p(MobSub.CLI,MobSub.MsSeq).
          
          IF RETURN-VALUE BEGINS "ERROR" THEN
             fLocalMemo("TermMobsub",
                        STRING(MobSub.MsSeq),
                        "BAJA",
                        RETURN-VALUE). 
      END.
      ELSE IF BUFFER-TENANT-NAME(MobSub) = {&TENANT_YOIGO} AND fIsMasmovilCLI(MobSub.CLI) THEN
      DO:
          fsetEffectiveTenantForAllDB({&TENANT_MASMOVIL}).

          RUN pReturnMSISDN(MobSub.CLI).

          fsetEffectiveTenantForAllDB({&TENANT_YOIGO}).    

          IF RETURN-VALUE BEGINS "ERROR" THEN
             fLocalMemo("TermMobsub",
                        STRING(MobSub.MsSeq),
                        "BAJA",
                        RETURN-VALUE).
      END.   
      ELSE IF BUFFER-TENANT-NAME(MobSub) = {&TENANT_MASMOVIL} AND fIsYoigoCLI(MobSub.CLI) THEN
      DO:
          fsetEffectiveTenantForAllDB({&TENANT_YOIGO}).

          RUN pReturnMSISDN(MobSub.CLI).

          fsetEffectiveTenantForAllDB({&TENANT_MASMOVIL}).   

          IF RETURN-VALUE BEGINS "ERROR" THEN
             fLocalMemo("TermMobsub",
                        STRING(MobSub.MsSeq),
                        "BAJA",
                        RETURN-VALUE).
      END.
   END.

   /* subscription is terminated by customer or by ported out request
      or order cancellation */
   IF LOOKUP(lcTermReason,"1,2,3") > 0 THEN
      RUN pOrderCancellation.

   IF LOOKUP(lcTermReason,SUBST("&1,&2,&3",
      {&SUBSCRIPTION_TERM_REASON_ORDER_CANCELLATION},
      {&SUBSCRIPTION_TERM_REASON_DIRECT_ORDER_CANCELATION},
      {&SUBSCRIPTION_TERM_REASON_POS_ORDER_CANCELATION})) > 0 THEN DO:

      FIND Order NO-LOCK WHERE
           Order.MsSeq = MobSub.MsSeq AND
           Order.OrderType < 2 NO-ERROR.
      IF AVAIL Order THEN DO:
            
         IF Order.Custnum NE MobSub.Custnum THEN DO:

            CREATE ErrorLog.
            ASSIGN ErrorLog.Brand     = gcBrand
                   ErrorLog.ActionID  = "ORDERCANCEL"
                   ErrorLog.TableName = "Order"
                   ErrorLog.KeyValue  = STRING(Order.OrderId) 
                   ErrorLog.ErrorMsg  = "Credit note not created due to ACC"
                   ErrorLog.UserCode  = katun
                   ErrorLog.ActionTS  = fMakeTS().
         END.

         RUN pCreatePaytermCreditNote(Order.OrderId).

         fReleaseImei(Order.OrderId).
   
         IF Order.InvNum > 0 THEN DO:
            lcResult = fCashInvoiceCreditnote(Order.Invnum, "1010").
            IF lcResult > "" THEN
                DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                               "MobSub",
                               STRING(MobSub.MsSeq),
                               MobSub.Custnum,
                               "CREDIT NOTE CREATION FAILED",
                               lcResult). 
         END.

      END.

      IF MobSub.PayType EQ FALSE THEN
      fCreateLimitHistory(MobSub.InvCust,
                          MobSub.MsSeq,
                          {&LIMIT_TYPE_BILLPERM},
                          {&LIMIT_BILLPERM_PROHIBITED},
                          0,
                          0,
                          FALSE,
                          TODAY,
                          12/31/2049).
   END.

   /* Inform Dextra for Subscription terminations with 
      reason code Order cancellation */
   IF LOOKUP(lcTermReason,SUBST("&1,&2",
      {&SUBSCRIPTION_TERM_REASON_ORDER_CANCELLATION},
      {&SUBSCRIPTION_TERM_REASON_DIRECT_ORDER_CANCELATION})) > 0 THEN DO:

      FIND Order NO-LOCK WHERE
           Order.MsSeq = MobSub.MsSeq NO-ERROR.
      IF AVAIL Order THEN DO:

         FIND FIRST OrderAccessory OF Order WHERE
                    OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE})
                    NO-LOCK NO-ERROR.
         IF AVAILABLE OrderAccessory AND
            LOOKUP(STRING(OrderAccessory.HardBook),"1,2") > 0 THEN
            llHardBook = TRUE.

         IF NOT MsRequest.UserCode BEGINS "Dextra" AND
            llHardBook = TRUE THEN DO:
            fLogisticsRequest(
               Order.MsSeq,
               Order.OrderId,
               "CANCEL",
               fMakeTS(),
               {&REQUEST_SOURCE_ORDER_CANCELLATION},
               OUTPUT lcResult).

            IF lcResult > "" THEN
               DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                "Order",
                                STRING(Order.OrderID),
                                0,
                                "Logistics cancel failed",
                                lcResult).
         END.
      END.
   END.

   /* Close Residual Amount Single Fee */
   IF llCloseRVTermFee THEN DO:
      /* YDR-1584 */
      ASSIGN liPeriodPostpone     = fPer2PerAdd(liPeriod,1)
             ldaKillDatePostpone  = ADD-INTERVAL(ldaKillDate,1,"months").

      FOR EACH SingleFee USE-INDEX Custnum WHERE
               SingleFee.Brand       = gcBrand AND
               SingleFee.Custnum     = MobSub.CustNum AND
               SingleFee.HostTable   = "Mobsub" AND
               SingleFee.KeyValue    = STRING(MobSub.MsSeq) AND
               SingleFee.BillPeriod  > liPeriod AND
               SingleFee.CalcObj     = "RVTERM" EXCLUSIVE-LOCK:

         IF SingleFee.Invnum > 0 AND
            CAN-FIND(FIRST Invoice NO-LOCK WHERE
                           Invoice.Invnum = SingleFee.Invnum AND
                           Invoice.InvType NE 99) THEN NEXT.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSingleFee).

         ASSIGN SingleFee.BillPeriod  = liPeriodPostpone
                SingleFee.Concerns[1] = YEAR(ldaKillDatePostpone) * 10000 +
                                        MONTH(ldaKillDatePostpone) * 100  +
                                        DAY(ldaKillDatePostpone).
         
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSingleFee).
      END.      
   END. /* IF llCloseRVTermFee THEN DO: */

   /* COFF fixed line stays in partial termination */
   IF lcTerminationType EQ {&TERMINATION_TYPE_FULL} THEN DO: 
      FOR EACH Order NO-LOCK WHERE
               Order.MsSeq = MobSub.MsSeq AND
               Order.OrderType = {&ORDER_TYPE_STC} AND
               Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE},
         FIRST OrderFusion NO-LOCK WHERE
               OrderFusion.Brand = gcBrand AND
               OrderFusion.OrderID = Order.OrderID:

         IF OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_ONGOING} THEN DO:

            fSetOrderStatus(Order.Orderid, {&ORDER_STATUS_IN_CONTROL}).
            
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                 "Order",
                 STRING(Order.OrderID),
                 Order.CustNum,
                 "Order handling stopped",
                 "Subscription is terminated, Convergent order cannot proceed").
         
         END.
         ELSE DO:
            RUN Mc/fusion_order_cancel.p(Order.OrderID).
            IF NOT RETURN-VALUE BEGINS "OK" THEN
               DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                    "Order",
                    STRING(Order.OrderID),
                    Order.CustNum,
                    "Convergent order closing failed",
                    STRING(RETURN-VALUE)).
         END.
      END.
   END.
   /* YDR-2052, Change the delivery type to paper only if the customer 
      don't have any other active subscription with delivery type EMAIL or SMS*/
   IF NOT MobSub.PayType AND 
      CAN-FIND(FIRST Customer NO-LOCK WHERE
                     Customer.CustNum = MobSub.CustNum        AND
                    (Customer.DelType = {&INV_DEL_TYPE_EMAIL} OR
                     Customer.DelTYpe = {&INV_DEL_TYPE_SMS})) THEN
   DO:
      llCallProc = TRUE.
      
      FOR EACH bMobSub NO-LOCK WHERE
               bMobsub.Brand    = gcBrand        AND
               bMobSub.CustNum  = MobSub.CustNum AND
               bMobSub.MsSeq   <> liMsSeq        AND 
               bMobSub.PayType  = NO:
         llCallProc = NO.
         LEAVE.
      END.
      
      IF llCallProc THEN   
         RUN pChangeDelType(MobSub.CustNum).
   END. 

   /* ADDLINE-20 Additional Line */
   IF LOOKUP(MobSub.CliType, {&ADDLINE_CLITYPES}) > 0 THEN DO:
      fCloseDiscount(ENTRY(LOOKUP(MobSub.CLIType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS}),
                     MobSub.MsSeq,
                     fLastDayOfMonth(TODAY),
                     FALSE).
   END.

   /* COFF Partial termination */
   IF lcTerminationType EQ {&TERMINATION_TYPE_PARTIAL} THEN DO:
      CREATE TermMobsub.
      BUFFER-COPY Mobsub TO TermMobsub.
      ASSIGN
         TermMobsub.fixednumber = "" /* Fixed line stays active */
         Mobsub.cli = Mobsub.fixednumber
         Mobsub.icc = ""
         Mobsub.imsi = ""
         MobSub.msStatus = {&MSSTATUS_MOBILE_NOT_ACTIVE}.
   END.      
   ELSE DO: 
      FIND FIRST TermMobsub WHERE 
                 TermMobsub.msseq EQ liMsSeq NO-ERROR.
      IF AVAIL TermMobsub THEN  /* Partial terminated case */
         BUFFER-COPY Mobsub EXCEPT Cli icc imsi msstatus TO TermMobsub.
      ELSE DO:
         CREATE TermMobsub.
         BUFFER-COPY Mobsub TO TermMobsub.
      END.
      DELETE MobSub.

   END.   
   IF AVAIL MSISDN THEN RELEASE MSISDN.

   /* ADDLine-20 Additional Line */
   IF fIsConvergentORFixedOnly(TermMobSub.CLIType) THEN DO:
      FOR EACH bMobSub NO-LOCK WHERE
               bMobSub.Brand   = gcBrand        AND
               bMobSub.AgrCust = TermMobSub.CustNum AND
               bMobSub.MsSeq  <> TermMobSub.MsSeq   AND
               LOOKUP(bMobSub.CliType, {&ADDLINE_CLITYPES}) > 0:
         fCloseAddLineDiscount(bMobSub.CustNum,
                               bMobSub.MsSeq,
                               bMobSub.CLIType,
                               fLastDayOfMonth(TODAY)).
      END.
   END.

   /* Find Original request */
   FIND FIRST MSRequest WHERE
              MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.

   /* request handled succesfully */
   fReqStatus(2,""). 

   RETURN "".
   
END PROCEDURE.

PROCEDURE pReturnMSISDN:
   DEF INPUT PARAMETER icMSISDN AS CHAR NO-UNDO.

   FIND FIRST MSISDN WHERE MSISDN.Brand = gcBrand AND MSISDN.CLI = icMSISDN NO-LOCK USE-INDEX CLI NO-ERROR.
   IF NOT AVAILABLE MSISDN THEN 
      RETURN "ERROR:MSISDN not available".

   FIND MobSub WHERE MobSub.Brand = gcBrand AND MobSub.Cli = MSISDN.Cli NO-LOCK NO-ERROR.
   IF AVAIL MobSub THEN 
      RETURN "ERROR:MSISDN is in use".
   
   FIND Order WHERE Order.Brand = gcBrand AND Order.cli = msisdn.cli AND LOOKUP(Order.statuscode,{&ORDER_INACTIVE_STATUSES}) = 0 NO-LOCK NO-ERROR.
   IF AVAIL Order THEN 
      RETURN "ERROR:MSISDN is in ongoing order".

   fMakeMsidnHistory(RECID(MSISDN)).
   
   ASSIGN
      MSISDN.StatusCode = ({&MSISDN_ST_RETURNED_TO_YOIGO})
      MSISDN.Actiondate = TODAY.

   RETURN "".

END PROCEDURE.

/* YOT-1013 */
PROCEDURE pOrderCancellation:
   
   DEF VAR lcResult AS CHAR NO-UNDO. 
   
   IF NOT AVAIL MobSub THEN RETURN.

   FIND Order WHERE
        Order.MsSeq = MobSub.MsSeq AND
        Order.OrderType < 2 NO-LOCK NO-ERROR.
   IF NOT AVAIL Order THEN RETURN.

   IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) = 0 THEN RETURN.

   /* Logistic Operator Status in TMS: Not Delivered or NULL. */
   FIND FIRST OrderDelivery OF Order NO-LOCK NO-ERROR.

   IF AVAIL OrderDelivery THEN DO:
      IF OrderDelivery.LOStatusId NE 10 THEN RETURN.
      IF lcTermReason = "3" THEN RETURN.
   END.
   /* check that order is not yet sent to dextra */
   ELSE IF Order.Logistics > "" THEN RETURN.
   
   /* Subscription  has  no  traffic. The  following  traffic must be 
      considered  as  no  traffic  ("Call Forwarding to Yoigo, 
      "Voice mail calls"). */
   
   /* only max. 1 year old calls are available */
   IF MobSub.Activationdate < TODAY - 330 THEN RETURN.

   IF NOT MobSub.PayType THEN DO:
      FIND FIRST MobCDR WHERE
                 MobCDR.CLI = MobSub.CLI AND
                 MobCDR.datest >= MobSub.ActivationDate AND
                 MobCDR.RateCCN NE 30 AND
                 MobCDR.RateCCN NE 98 NO-LOCK USE-INDEX CLI NO-ERROR.
      IF AVAIL MobCDR THEN RETURN.
   END.
   ELSE DO:
      FIND FIRST PrepCDR WHERE
                 PrepCDR.CLI = MobSub.CLI AND
                 PrepCDR.datest >= MobSub.ActivationDate AND
                 PrepCDR.RateCCN NE 30 AND
                 PrepCDR.RateCCN NE 98 NO-LOCK USE-INDEX CLI NO-ERROR.
      IF AVAIL PrepCdr THEN RETURN.
   END.
   
   /* Create a credit note of the initial order */
   IF Order.InvNum > 0 THEN DO:
      lcResult = fCashInvoiceCreditnote(Order.Invnum, "1010").
      IF lcResult > "" THEN
          DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                         "MobSub",
                         STRING(MobSub.MsSeq),
                         MobSub.Custnum,
                         "CREDIT NOTE CREATION FAILED",
                         lcResult). 
   END. /* IF Order.InvNum > 0 THEN DO: */

   /* YDR-1034 */
   ELSE IF lcTermReason = "3" THEN DO:
      FOR FIRST OrderPayment NO-LOCK WHERE
                OrderPayment.Brand = gcBrand AND
                OrderPayment.OrderId = Order.OrderId AND
               (OrderPayment.Method = {&ORDERPAYMENT_M_CREDIT_CARD} OR
                OrderPayment.Method = {&ORDERPAYMENT_M_PAYPAL}): 
         CREATE ActionLog.
         ASSIGN ActionLog.Brand     = gcBrand
                ActionLog.ActionID  = "OrderCancel"
                ActionLog.ActionTS  = fMakeTS()
                ActionLog.TableName = "Order"
                ActionLog.KeyValue  = STRING(Order.OrderId)
                ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}.
      END. /* FOR FIRST OrderPayment NO-LOCK WHERE */
   END. /* ELSE DO: */

  /* Set billing status to prohibited. */
   llCleanFLimitReqEventLog = FALSE.
   fCreateLimitHistory(MobSub.InvCust,
                       MobSub.MsSeq,
                       {&LIMIT_TYPE_BILLPERM},
                       {&LIMIT_BILLPERM_PROHIBITED},
                       0,
                       0,
                       FALSE,
                       TODAY,
                       12/31/2049).

   FIND FIRST SIM WHERE
              SIM.ICC = Order.ICC
   EXCLUSIVE-LOCK NO-ERROR.
   
   /* Release ICC. */
   IF AVAIL SIM THEN DO:

      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSIM).
      ASSIGN
         SIM.SimStat = {&SIM_SIMSTAT_AVAILABLE}
         SIM.CustNum = 0.
                                
      CREATE ActionLog.
      ASSIGN
         ActionLog.ActionTS     = fMakeTS()
         ActionLog.Brand        = gcBrand  
         ActionLog.TableName    = "Order"  
         ActionLog.KeyValue     = STRING(Order.Orderid)
         ActionLog.UserCode     = katun
         ActionLog.ActionID     = "SIMRELEASE"
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
         ActionLog.ActionStatus = 2
         ActionLog.ActionChar   = SIM.ICC.

      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSIM).
   END.

END PROCEDURE. 


PROCEDURE pMultiSIMTermination:

   DEF INPUT PARAM piOrigRequest AS INT NO-UNDO.

   DEF BUFFER lbMobSub FOR Mobsub. 
   DEF VAR liMsisdnStat AS INT NO-UNDO.
   DEF VAR liSimStat AS INT NO-UNDO. 
   DEF VAR liQuarTime AS INT NO-UNDO.
   DEF VAR liRequest AS INT NO-UNDO. 
   DEF VAR ldeSMSStamp AS DEC NO-UNDO. 
   DEF VAR ldaSecSIMTermDate AS DATE NO-UNDO.
   DEF VAR ldeSecSIMTermStamp AS DEC NO-UNDO.

   DEF BUFFER Customer FOR Customer.
     
   IF NOT AVAIL Mobsub THEN RETURN.
   IF NOT MobSub.MultiSimID > 0 THEN RETURN.

   FIND FIRST lbMobSub NO-LOCK USE-INDEX MultiSIM WHERE
              lbMobSub.Brand  = gcBrand AND
              lbMobSub.MultiSimID = MobSub.MultiSimID AND
              lbMobSub.MultiSimType NE MobSub.MultiSimType AND
              lbMobSub.Custnum = MobSub.Custnum NO-ERROR.

   IF NOT AVAIL lbMobSub THEN RETURN.

   CREATE ActionLog.
   ASSIGN
      ActionLog.ActionTS     = fMakeTS()
      ActionLog.Brand        = gcBrand  
      ActionLog.TableName    = "Customer"  
      ActionLog.KeyValue     = STRING(MobSub.Custnum)
      ActionLog.UserCode     = katun
      ActionLog.ActionID     = "MultiSIMTermination"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_LOGGED}
      ActionLog.ActionChar   = SUBST("Terminated subscription: &1, " + "
                                      MultiSIMID: &2, MultiSimType &3",
                               MobSub.MsSeq, MobSub.MultiSimID,
                               MobSub.MultiSIMType).
   
   IF Mobsub.MultiSimType EQ {&MULTISIMTYPE_PRIMARY} AND
      NOT CAN-FIND (FIRST MsRequest WHERE
          MsRequest.MsSeq   = lbMobSub.Msseq AND
          MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
          LOOKUP(STRING(MsRequest.ReqStatus),
                 {&REQ_INACTIVE_STATUSES}) = 0) AND
      NOT CAN-FIND (FIRST MsRequest WHERE
          MsRequest.MsSeq   = lbMobSub.Msseq AND
          MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
          LOOKUP(STRING(MsRequest.ReqStatus),
          {&REQ_INACTIVE_STATUSES}) = 0) AND
      NOT fIsMNPOutOngoing(INPUT lbMobSub.CLI) THEN DO:

      ASSIGN ldaSecSIMTermDate  = ADD-INTERVAL(TODAY, 1,"months")
             ldaSecSIMTermDate  = fLastDayOfMonth(ldaSecSIMTermDate)
             ldeSecSIMTermStamp = fMake2Dt(ldaSecSIMTermDate,86399).

      fInitialiseValues(
         {&SUBSCRIPTION_TERM_REASON_MULTISIM},
         (fIsYoigoCLI(lbMobSub.CLI) OR fIsMasmovilCLI(lbMobSub.CLI)),
         OUTPUT liMsisdnStat,
         OUTPUT liSimStat,
         OUTPUT liQuarTime).

      liRequest = fTerminationRequest(
                          lbMobSub.Msseq,
                          ldeSecSIMTermStamp,
                          liMsisdnStat,
                          liSimStat,
                          liQuarTime,
                          1, /* create fees */
                          "", /* out oper. */
                          STRING({&SUBSCRIPTION_TERM_REASON_MULTISIM}),
                          {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                          katun,
                          piOrigRequest, /* orig. request */
                          {&TERMINATION_TYPE_FULL},
                          OUTPUT lcError). 
               
      IF lcError > "" THEN 
         fLocalMemo("TermMobsub",
                    STRING(lbMobSub.MsSeq),
                    "Multi SIM termination failed",
                    lcError).
      /* MNP Outporting */
      ELSE IF lcTermReason = "2" THEN DO:
         FIND Customer NO-LOCK WHERE
              Customer.Custnum = MobSub.Custnum NO-ERROR.

         lcSMSText = fGetSMSTxt("MultiSIMSecondaryTermRem1",
                                TODAY,
                               (IF AVAIL Customer
                                THEN Customer.Language ELSE 1),
                                OUTPUT ldeSMSStamp).
         lcSMSText = REPLACE(lcSMSText,"#DATE",STRING(ldaSecSIMTermDate)).

         IF lcSMSText > "" THEN
            fMakeSchedSMS2(lbMobSub.CustNum,
                           lbMobSub.CLI,
                           11, /* service change */
                           lcSMSText,
                           ldeSMSStamp,
                           "22622",
                           "").
      END. 
   END.
   ELSE IF MobSub.MultiSimType EQ {&MULTISIMTYPE_SECONDARY} THEN DO:

      FIND Customer NO-LOCK WHERE
           Customer.Custnum = MobSub.Custnum NO-ERROR.

      lcSMSText = fGetSMSTxt("MultiSIMSecondaryTerm",
                             TODAY,
                             (IF AVAIL Customer
                             THEN Customer.Language ELSE 1),
                             OUTPUT ldeSMSStamp).

      IF lcSMSText > "" THEN
         fMakeSchedSMS2(lbMobSub.CustNum,
                       lbMobSub.CLI,
                       11, /* service change */
                       lcSMSText,
                       ldeSMSStamp,
                       "22622",
                       "").
   END.
   
END PROCEDURE.

PROCEDURE pChangeDelType:
   DEFINE INPUT  PARAMETER liCustNum AS INTEGER NO-UNDO.   

   DEF VAR lhCustomer   AS HANDLE NO-UNDO. 
   DEF VAR ldeStartTime AS DEC    NO-UNDO. 
   DEF VAR llgStarted   AS LOG    NO-UNDO.
   DEF VAR llgInvDate   AS LOG    NO-UNDO.
   DEF VAR llgInvType   AS LOG    NO-UNDO. 

   lhCustomer = BUFFER Customer:HANDLE.

   RUN StarEventInitialize(lhCustomer).

   FIND FIRST Customer EXCLUSIVE-LOCK WHERE 
              Customer.CustNum = liCustNum NO-ERROR.

   IF AVAILABLE Customer THEN      
   DO:

      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer).       

      /* If subscription termination is on 1st day of month then change the 
         delivery type and delivery status for invoices generated AND but not delivered */
      IF DAY(TODAY) = 1 THEN
      DO:
         ASSIGN ldeStartTime = fMake2Dt(TODAY,0)
                llgStarted   = FALSE
                llgInvDate   = FALSE
                llgInvType   = FALSE. 

         FOR EACH FuncRunQSchedule NO-LOCK WHERE 
                  FuncRunQSchedule.StartTS  >= ldeStartTime AND
                  FuncRunQSchedule.RunMode   = "Production", 
             EACH FuncRunExec NO-LOCK WHERE
                  FuncRunExec.FRQScheduleID EQ FuncRunQSchedule.FRQScheduleID AND
                  FuncRunExec.FRConfigID    EQ {&INVPRINTSPLIT_CONFIG}: 
                    
             FOR EACH FuncRunQSParam NO-LOCK WHERE 
                      FuncRunQSParam.FRQScheduleID EQ FuncRunQSchedule.FRQScheduleID AND 
                      FuncRunQSParam.FRConfigID    EQ FuncRunExec.FRConfigID:      
             
                IF FuncRunQSParam.ParamSeq  EQ 1     AND 
                   FuncRunQSParam.DateParam EQ TODAY THEN 
                   llgInvDate = TRUE.
                ELSE IF FuncRunQSParam.ParamSeq EQ 2                  AND 
                        FuncRunQSParam.IntParam EQ {&INV_TYPE_NORMAL} THEN 
                   llgInvType = TRUE.
                  
             END.   
             
             IF FuncRunExec.StartTS > 0 THEN 
                llgStarted = TRUE.
         END.

         /* Update invoice deliverytype to paper only when funcrun
            execution process "InvPrintSplit" is not started for current month */
         IF NOT llgStarted AND 
                llgInvDate AND 
                llgInvType THEN DO: 
            FOR EACH Invoice EXCLUSIVE-LOCK WHERE
                     Invoice.Brand      = gcBrand            AND
                     Invoice.CustNum    = Customer.CustNum   AND
                     Invoice.InvDate    = TODAY              AND
                     Invoice.InvType    = {&INV_TYPE_NORMAL} AND 
                     Invoice.PrintState = 0:
               Invoice.DelType  = {&INV_DEL_TYPE_PAPER}.
            END.          
         END.

      END.

      Customer.DelType = {&INV_DEL_TYPE_PAPER}.

      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustomer).

   END.

END PROCEDURE.
