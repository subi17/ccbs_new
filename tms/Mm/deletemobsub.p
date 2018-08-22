/* deletemobsub.p       2006/jp

   changes:             17.01.07/aam terminate periodical contract (dccli)
                        16.08.07/aam clean eventlog
                        03.09.07/aam don't mark create fees for prepaid on
                                     per.contract termination
                        11.03.08/jp  removed hardcoded "begins tarj"        
      22.sep.2015 hugo.lujan - YPR-2521 - [Q25] - TMS - Subscription 
       termination/ MNP out porting, STC (postpaid to prepaid)
      14/4/18  - DIAMANTEE PHASE 3 - Added the ability to atttach current extra lines to other main line 
                                      for same customer if the main line got terminated
*/
   
{Syst/commali.i}
{Syst/eventval.i}
{Func/fctserval.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
{Func/msreqfunc.i}
{Func/fcreditreq.i}
{Func/flimitreq.i}
{Func/dss_request.i}
{Func/dss_matrix.i}
{Func/msisdn_prefix.i}
{Func/fsubstermreq.i}
{Func/ordercancel.i}
{Func/dextra.i}
{Func/add_lines_request.i}
{Func/addline_discount.i}
{Func/orderfunc.i}
{Func/multitenantfunc.i}
{Func/profunc_request.i}
{Func/digital_signature.i}

DEFINE INPUT  PARAMETER iiMSrequest AS INT  NO-UNDO. 

llCleanFLimitReqEventLog = FALSE.

DEFINE VARIABLE llOutPort            AS LOG  NO-UNDO.       
DEFINE VARIABLE lcOutOper            AS CHAR NO-UNDO.
DEFINE VARIABLE ldBilled             AS DEC  NO-UNDO.
DEFINE VARIABLE lcError              AS CHAR NO-UNDO. 
DEFINE VARIABLE ldCurrTS             AS DEC  NO-UNDO.
DEFINE VARIABLE llCreateFee          AS LOG  NO-UNDO. 
DEFINE VARIABLE ldaKillDate          AS DATE NO-UNDO.
DEFINE VARIABLE liTime               AS INT  NO-UNDO.
DEFINE VARIABLE liQuarTime           AS INT  NO-UNDO.
DEFINE VARIABLE liMsisdnStat         AS INT  NO-UNDO.
DEFINE VARIABLE liSimStat            AS INT  NO-UNDO.
DEFINE VARIABLE lcTermReason         AS CHAR NO-UNDO.
DEFINE VARIABLE liMLMsSeq            AS INT  NO-UNDO.
DEFINE VARIABLE liELCount            AS INT  NO-UNDO.
DEFINE VARIABLE lcExtraLineCLITypes  AS CHAR NO-UNDO.
DEFINE VARIABLE lcMandatoryELines    AS CHAR NO-UNDO.
DEFINE VARIABLE liSTCELMSSeq         AS INT  NO-UNDO.
DEFINE VARIABLE lcSTCELCLIType       AS CHAR NO-UNDO.
DEFINE VARIABLE lisMandatoryELExists AS LOG  NO-UNDO.
DEFINE VARIABLE liSTCRequest         AS INT  NO-UNDO.

/* YCO-250 Variables created to control discount closing */
DEFINE VARIABLE lClose AS LOGICAL NO-UNDO.

DEF BUFFER bMNPSub FOR MNPSub.
DEF BUFFER bMobsub FOR MobSub.
DEF BUFFER bCLIType FOR CLIType.

/* Additional line Mobile only ALFMO */
DEF BUFFER bCustomer FOR Customer.
DEF BUFFER bOrder    FOR Order.
DEF BUFFER bOrdTemp  FOR Order.

/* Extra line */
DEFINE VARIABLE liExtraLineMsSeq        AS INT  NO-UNDO. 

DEF BUFFER lELOrder       FOR Order.        
DEF BUFFER lMLMobSub      FOR MobSub.
DEF BUFFER lELMobSub      FOR MobSub.
DEF BUFFER lELMobSub2     FOR MobSub.
DEF BUFFER lELOrderAction FOR OrderAction.

/* YCO-250 */
DEF BUFFER bCustomerAdd   FOR Customer.

DEF TEMP-TABLE ttContract NO-UNDO
   FIELD DCEvent   AS CHAR
   FIELD PerContID AS INT
   FIELD CreateFee AS LOG
   FIELD ActTS     AS DEC.

FUNCTION fUpdateDSSNewtorkForExtraLine RETURNS LOGICAL
   (INPUT iiMsSeq        AS INT,
    INPUT icCLIType      AS CHAR,
    INPUT iiMultiSimId   AS INT,
    INPUT iiMsRequest    AS INT,
    INPUT ideActStamp    AS DEC,
    INPUT lcBundleId     AS CHAR):

   DEF VAR lcDSSBundleId AS CHAR NO-UNDO. 

   DEFINE BUFFER lbMLMobSub FOR MobSub.
   DEFINE BUFFER lbELMobSub FOR MobSub.

   IF NOT fCheckActiveExtraLinePair(iiMsSeq,
                                    icCLIType,
                                    OUTPUT lcDSSBundleId) THEN
      RETURN FALSE.

   IF fCLITypeIsExtraLine(icCLIType) THEN DO:

      FIND FIRST lbMLMobSub NO-LOCK WHERE
                 lbMLMobSub.MsSeq      = iiMultiSimId        AND
                (lbMLMobSub.MsStatus   = {&MSSTATUS_ACTIVE}  OR
                 lbMLMobSub.MsStatus   = {&MSSTATUS_BARRED}) NO-ERROR.
      IF AVAIL lbMLMobSub THEN DO:
         IF fExtraLineCountForMainLine(lbMLMobsub.MsSeq,
                                       lbMLMobsub.CustNum) EQ 1 THEN
            RUN pUpdateDSSNetwork(INPUT lbMLMobsub.MsSeq,
                                  INPUT lbMLMobsub.CLI,
                                  INPUT lbMLMobsub.CustNum,
                                  INPUT "REMOVE",
                                  INPUT "",        /* Optional param list */
                                  INPUT iiMsRequest,
                                  INPUT ideActStamp,
                                  INPUT {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                                  INPUT lcBundleId).
      END.
   END.
   ELSE IF fCLITypeIsMainLine(icCLIType) THEN DO:

      FOR EACH lbELMobSub NO-LOCK WHERE
               lbELMobSub.Brand        EQ Syst.Var:gcBrand      AND
               lbELMobSub.MultiSimId   EQ iiMsSeq               AND
               lbELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}:

         RUN pUpdateDSSNetwork(INPUT lbELMobsub.MsSeq,
                               INPUT lbELMobsub.CLI,
                               INPUT lbELMobsub.CustNum,
                               INPUT "REMOVE",
                               INPUT "",        /* Optional param list */
                               INPUT iiMsRequest,
                               INPUT ideActStamp,
                               INPUT {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                               INPUT lcBundleId).

      END.

   END.

   RETURN TRUE.

END FUNCTION.

ldCurrTS = Func.Common:mMakeTS().

FIND FIRST MSRequest WHERE 
           MSRequest.MSRequest = iiMSRequest
NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR 
   MsRequest.ReqType NE {&REQTYPE_SUBSCRIPTION_TERMINATION} 
   THEN RETURN "ERROR".

IF llDoEvent THEN DO:

   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   
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
   DEF VAR ldtCloseDate       AS DATE NO-UNDO. /* Mobile only additional line ALFMO-5 */

   DEF VAR lcPostpaidDataBundles AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS4SubsType AS CHAR NO-UNDO. 
   DEF VAR lcDSSRelatedSubsType  AS CHAR NO-UNDO. 

   DEF VAR liPeriodPostpone    AS INT  NO-UNDO.
   DEF VAR ldaKillDatePostpone AS DATE NO-UNDO.

   DEF VAR llHardBook          AS LOG  NO-UNDO INIT FALSE.
   DEF VAR llCallProc          AS LOG  NO-UNDO.   
   
   DEF VAR lcTerminationType AS CHAR NO-UNDO INIT {&TERMINATION_TYPE_FULL}.
   /* Additional line mobile only ALFMO */
   DEF VAR lcAddlineClitypes AS CHARACTER   NO-UNDO.
   DEF VAR llDelete          AS LOGICAL     NO-UNDO.
   
   ASSIGN liArrivalStatus = MsRequest.ReqStatus
          liMsSeq = MsRequest.MsSeq.
   
   IF LOOKUP(STRING(liArrivalStatus),"6,8") = 0 THEN RETURN.
 
    /* request is under work */
   IF NOT fReqStatus(1,"") THEN DO:
      RETURN "ERROR".
   END. 

   Func.Common:mSplitTS(MsRequest.ActStamp, OUTPUT ldaKillDate, OUTPUT liTime).

   ASSIGN
      llOutport      = (MsRequest.ReqCParam2 NE "")
      lcOutoper      = MsRequest.ReqCParam2
      liMsisdnStat   = MsRequest.ReqIParam1
      liSimStat      = MsRequest.ReqIParam2
      liQuarTime     = MsRequest.ReqIParam3
      lcTermReason   = MsRequest.ReqCParam3
      lcTerminationType = MsRequest.ReqCParam6 WHEN MsRequest.ReqCParam6 NE ""
      lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS").

   ASSIGN ldMonthEndDate = Func.Common:mLastDayOfMonth(ldaKillDate)
          ldeMonthEndTS  = Func.Common:mMake2DT(ldMonthEndDate,86399)
          liPeriod       = YEAR(ldaKillDate) * 100 + MONTH(ldaKillDate).

   FIND FIRST MobSub WHERE
              MobSub.MsSeq = MSRequest.MSSeq
   EXCLUSIVE-LOCK NO-ERROR.

   IF NOT Avail Mobsub THEN DO:
      fReqError("Mobile subscription already deactivated").
      RETURN.
   END.

   /* YOT-5580 If in mobile tariff change termination to full */
   IF NOT fIsConvergentORFixedOnly(Mobsub.CliType) THEN 
      lcTerminationType = {&TERMINATION_TYPE_FULL}.

   /* COFF if partial termination cli = fixednumber (no actions needed) */
   IF NOT(MobSub.cli BEGINS "8" OR MobSub.cli BEGINS "9") THEN DO:
      
      FIND FIRST MSISDN WHERE
                 MSISDN.Brand = Syst.Var:gcBrand AND
                 MSISDN.CLI   = MobSub.CLI
      EXCLUSIVE-LOCK NO-ERROR.

      FIND FIRST IMSI WHERE
                 IMSI.IMSI = MobSub.IMSI
      EXCLUSIVE-LOCK NO-ERROR.

      FIND FIRST SIM WHERE
                 SIM.ICC = MobSub.ICC
      EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL MSISDN THEN DO:
         /* Mobile not been activated. No need to return MSISDN. */
         IF liMsisdnStat     EQ {&MSISDN_ST_WAITING_RETURN} AND
            (MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} OR
             MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) THEN DO:
            IF fIsYoigoCLI(MobSub.CLI) EQ FALSE THEN
                 liMsisdnStat = {&MSISDN_ST_MNP_OUT_YOIGO}.
            ELSE liMsisdnStat = {&MSISDN_ST_ASSIGNED_TO_ORDER}.
            liQuarTime = 0.
         END.

         fMakeMsidnHistory(INPUT RECID(MSISDN)).

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMSISDN).
         
         MSISDN.StatusCode = liMsisdnStat.
         MSISDN.CustNum    = 0.
         ASSIGN
            MSISDN.ValidTo    = Func.Common:mDate2TS(TODAY + liQuarTime) + (time / 100000)
                                WHEN liQuarTime >= 0
            MSISDN.ValidTo    = 99999999.99999 WHEN liQuarTime = -1.

         IF llOutPort THEN ASSIGN
            MSISDN.PortingDate = ldaKillDate  /* Date ported out  */
            MSISDN.OutOperator = lcOutOper.   /*Name of receiving op.*/

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMSISDN).
      END.
   
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
              MSOwner.MsSeq = MobSub.MsSeq AND
              MSOwner.TsEnd >= Func.Common:mHMS2TS(TODAY,STRING(time,"hh:mm:ss"))
   EXCLUSIVE-LOCK NO-ERROR.

   /* TimeStamp  */
   IF AVAIL MSOwner AND lcTerminationType EQ {&TERMINATION_TYPE_FULL} THEN DO:
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsOwner).
      MSOwner.TsEnd = ldCurrTS.   
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsOwner).
   END.
   ELSE IF AVAIL MSOwner AND lcTerminationType EQ {&TERMINATION_TYPE_PARTIAL} THEN 
      fUpdatePartialMSOwner(MobSub.msseq, Mobsub.fixedNumber).

   IF llOutport THEN DO:

      Func.Common:mWriteMemo("Customer",
                 STRING(Mobsub.CustNum),
                 (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0),
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
   
   /* if pro and fixedline terminated, terminate SVAs also */
   IF (lcTerminationType EQ {&TERMINATION_TYPE_FULL} AND
       fIsConvergenceTariff(Mobsub.CliType)) THEN DO:
      fTerminateSVAs(liMsSeq, FALSE).
      /* Find Original request */
      FIND FIRST MSRequest WHERE
                 MSRequest.MSRequest = iiMSRequest NO-LOCK NO-ERROR.
      fDeactivateTVService(liMsSeq, MsRequest.UserCode).
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
          ldeActStamp = Func.Common:mHMS2TS(TODAY + 1,"").

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

      ASSIGN lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
             lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE").

      IF lcBundleId EQ {&DSS4} THEN 
         lcDSSRelatedSubsType = lcAllowedDSS4SubsType.
      ELSE IF lcBundleId EQ {&DSS2} THEN 
         lcDSSRelatedSubsType = lcAllowedDSS2SubsType.

      /* Nothing if subs. type is not part of DSS2 */
      IF (lcBundleId = {&DSS2} OR lcBundleId = {&DSS4}) AND
         LOOKUP(MobSub.CLIType,lcDSSRelatedSubsType) = 0 THEN .

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

               Func.Common:mWriteMemo("Customer",
                          STRING(Mobsub.CustNum),
                          (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0),
                          "DSS Bundle/UPSELL",
                          "DSS Bundle/UPSELL is transferred from Subs.Id " +
                          STRING(MobSub.MsSeq) + " to Subs. Id " +
                          STRING(liDSSMsSeq)).
            END. /* IF fTransferDSS(INPUT MobSub.MsSeq,INPUT liDSSMsSeq, */
            ELSE
               Func.Common:mWriteMemo("Customer",
                          STRING(Mobsub.CustNum),
                          (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0),
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
                                  INPUT ldCurrTS,
                                  INPUT {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                                  INPUT lcBundleId).
         /* If DSS is transferred then remove subs. from DSS group */
         ELSE DO: 
            RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                                  INPUT Mobsub.CLI,
                                  INPUT Mobsub.CustNum,
                                  INPUT "REMOVE",
                                  INPUT "",        /* Optional param list */
                                  INPUT MsRequest.MsRequest,
                                  INPUT MsRequest.ActStamp,
                                  INPUT {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                                  INPUT lcBundleId).
            /* If it is Extraline associated subscription */
            IF (lcBundleId EQ {&DSS2} OR lcBundleId EQ {&DSS4}) AND
               (fCLITypeIsMainLine(MobSub.CLIType) OR
                fCLITypeIsExtraLine(MobSub.CLIType))            THEN
                fUpdateDSSNewtorkForExtraLine(MobSub.MsSeq,
                                              MobSub.CLIType,   
                                              MobSub.MultiSimId,
                                              MsRequest.MsRequest,
                                              MsRequest.ActStamp,
                                              lcBundleId).

         END.

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
                                  INPUT ldCurrTS,
                                  INPUT {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                                  INPUT lcBundleId).
         /* Otherwise just remove subs. from DSS group */
         ELSE DO:
            RUN pUpdateDSSNetwork(INPUT Mobsub.MsSeq,
                                  INPUT Mobsub.CLI,
                                  INPUT Mobsub.CustNum,
                                  INPUT "REMOVE",
                                  INPUT "",     /* Optional param list */
                                  INPUT MsRequest.MsRequest,
                                  INPUT MsRequest.ActStamp,
                                  INPUT {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                                  INPUT lcBundleId).
            /* If it is Extraline associated subscription */
            IF (lcBundleID EQ {&DSS2} OR lcBundleID EQ {&DSS4}) AND 
               (fCLITypeIsMainLine(MobSub.CLIType) OR
                fCLITypeIsExtraLine(MobSub.CLIType))            THEN
                fUpdateDSSNewtorkForExtraLine(MobSub.MsSeq,
                                              MobSub.CLIType,
                                              MobSub.MultiSimId,
                                              MsRequest.MsRequest,
                                              MsRequest.ActStamp,
                                              lcBundleId).
         END.
      END. /* ELSE DO: */
   END. /* IF llDSSActive THEN DO: */

   /* make requests for terminating periodical contracts */
   EMPTY TEMP-TABLE ttContract.

   FOR EACH DCCLI EXCLUSIVE-LOCK WHERE
            DCCLI.MsSeq   = MobSub.MsSeq  AND
            DCCLI.ValidTo >= TODAY:
         
      /* COFF Partial termination */
      IF (lcTerminationType EQ {&TERMINATION_TYPE_PARTIAL} AND
         fIsConvergentFixedContract(DCCLI.DCEvent)) THEN NEXT.

      FIND FIRST DayCampaign NO-LOCK WHERE
                 DayCampaign.Brand = Syst.Var:gcBrand AND
                 DayCampaign.DcEvent = DCCLI.DcEvent NO-ERROR.
      
      IF AVAIL DayCampaign AND (DayCampaign.BundleTarget = {&TELEVISION_BUNDLE} OR DayCampaign.DCEvent BEGINS "TVTERM") THEN 
         NEXT.

      DCCLI.TermDate = ?.

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
                 DayCampaign.Brand      = Syst.Var:gcBrand AND 
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

                  IF lcBundleId EQ {&DSS}  OR 
                    ((lcBundleId EQ {&DSS2} OR lcBundleId EQ {&DSS4}) AND
                     LOOKUP(MobSub.CLIType,lcDSSRelatedSubsType) > 0) THEN
                  FOR FIRST FeeModel NO-LOCK WHERE
                            FeeModel.Brand    EQ Syst.Var:gcBrand AND
                            FeeModel.FeeModel EQ DayCampaign.FeeModel,
                      FIRST FMItem NO-LOCK WHERE
                            FMItem.Brand        EQ Syst.Var:gcBrand  AND
                            FMItem.FeeModel     EQ FeeModel.FeeModel AND
                            FMItem.Todate       >= TODAY             AND
                            FMItem.BrokenRental EQ 1: /* full month */
                     ttContract.ActTS = ldCurrTS.
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
                 DayCampaign.Brand      = Syst.Var:gcBrand AND 
                 DayCampaign.DCEvent    = ttContract.DCEvent AND 
                 DayCampaign.ValidTo   >= Today NO-LOCK NO-ERROR.
              
      IF NOT AVAIL DayCampaign THEN DO:
         Func.Common:mWriteMemo("Customer",
                    STRING(Mobsub.CustNum),
                    (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0),
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
                          ELSE Func.Common:mSecOffSet(ldCurrTS,60),
                          llCreateFee,             /* create fees */
                          {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                          "",
                          iiMsRequest,
                          FALSE,
                          "",
                          0,
                          ttContract.PerContID,
                          "",
                          OUTPUT lcError).

      Func.Common:mWriteMemo("Customer",
                 STRING(Mobsub.CustNum),
                 (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0),
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

      IF LOOKUP(STRING(MSRequest.ReqStatus),
                SUBSTITUTE("&1,&2,&3",
                           {&REQUEST_STATUS_NEW},
                           {&REQUEST_STATUS_SUB_REQUEST_DONE},
                           {&REQUEST_STATUS_CONFIRMATION_PENDING})) > 0
      THEN DO:
         fReqStatus(4,"Cancelled by subs. termination").
         fChangeOrderStatus(MsRequest.ReqIParam4, {&ORDER_STATUS_CLOSED}).
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

         /* check that all mobile subscriptions are terminated 
            before marking mnp out process closed */
         FOR EACH bMNPSub WHERE
                  bMNPSub.MNPSeq = MNPSub.MNPSeq AND
                  bMNPSub.MsSeq NE MNPSub.MsSeq NO-LOCK:
            IF CAN-FIND(FIRST bMobsub NO-LOCK WHERE
               bMobsub.MsSeq    EQ bMNPSub.MsSeq AND
               bMobsub.MsStatus NE {&MSSTATUS_MOBILE_NOT_ACTIVE})
               THEN LEAVE MNP_LOOP.
         END.

         ASSIGN
            MNPProcess.UpdateTS = Func.Common:mMakeTS()
            MNPProcess.StatusCode = ({&MNP_ST_APOR}).
      END.
         
      FOR EACH Order NO-LOCK WHERE
               Order.MsSeq = MobSub.MsSeq AND
               Order.StatusCode = {&ORDER_STATUS_MNP_RETENTION}:

         RUN Mc/closeorder.p(Order.OrderID, TRUE).
         
         FIND FIRST MNPRetPlatform WHERE
                    MNPRetPlatform.Brand = Syst.Var:gcBrand AND
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
      IF (BUFFER-TENANT-NAME(MobSub) = {&TENANT_YOIGO}    AND fIsYoigoCLI(MobSub.CLI)    EQ FALSE) OR 
         (BUFFER-TENANT-NAME(MobSub) = {&TENANT_MASMOVIL} AND fIsMasmovilCLI(MobSub.CLI) EQ FALSE) THEN
      DO:
         IF NOT (MobSub.MsStatus EQ {&MSSTATUS_MOBILE_PROV_ONG} OR
                 MobSub.MsStatus EQ {&MSSTATUS_MOBILE_NOT_ACTIVE}) THEN
            RUN Mnp/mnpnumbertermrequest.p(MobSub.CLI,MobSub.MsSeq).
          
         IF RETURN-VALUE BEGINS "ERROR" THEN
             Func.Common:mWriteMemo("TermMobsub",
                        STRING(MobSub.MsSeq),
                        (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0),
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
            ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
                   ErrorLog.ActionID  = "ORDERCANCEL"
                   ErrorLog.TableName = "Order"
                   ErrorLog.KeyValue  = STRING(Order.OrderId) 
                   ErrorLog.ErrorMsg  = "Credit note not created due to ACC"
                   ErrorLog.UserCode  = Syst.Var:katun
                   ErrorLog.ActionTS  = Func.Common:mMakeTS().
         END.
         /* RES-538 Digital Signature */
         fHandleSignature(Order.OrderId,{&ORDER_STATUS_CLOSED}).

         RUN pCreatePaytermCreditNote(Order.OrderId).

         fReleaseImei(Order.OrderId).
   
         IF Order.InvNum > 0 THEN DO:
            lcResult = fCashInvoiceCreditnote(Order.Invnum, "1010").
            IF lcResult > "" THEN
                Func.Common:mWriteMemo("MobSub",
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
               Func.Common:mMakeTS(),
               {&REQUEST_SOURCE_ORDER_CANCELLATION},
               OUTPUT lcResult).

            IF lcResult > "" THEN
               Func.Common:mWriteMemo("Order",
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
               SingleFee.Brand       = Syst.Var:gcBrand AND
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
               OrderFusion.Brand = Syst.Var:gcBrand AND
               OrderFusion.OrderID = Order.OrderID:

         IF OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_ONGOING} THEN DO:

            fSetOrderStatus(Order.Orderid, {&ORDER_STATUS_IN_CONTROL}).
            
            Func.Common:mWriteMemo("Order",
                 STRING(Order.OrderID),
                 Order.CustNum,
                 "Order handling stopped",
                 "Subscription is terminated, Convergent order cannot proceed").
         
         END.
         ELSE DO:
            RUN Mc/fusion_order_cancel.p(Order.OrderID).
            IF NOT RETURN-VALUE BEGINS "OK" THEN
               Func.Common:mWriteMemo("Order",
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
                     Customer.DelType = {&INV_DEL_TYPE_ESI} OR
                     Customer.DelTYpe = {&INV_DEL_TYPE_SMS})) THEN
   DO:
      llCallProc = TRUE.
      
      FOR EACH bMobSub NO-LOCK WHERE
               bMobsub.Brand    = Syst.Var:gcBrand        AND
               bMobSub.CustNum  = MobSub.CustNum AND
               bMobSub.MsSeq   <> liMsSeq        AND 
               bMobSub.PayType  = NO AND
               bMobSub.MsStatus NE {&MSSTATUS_MOBILE_NOT_ACTIVE}:
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
                     Func.Common:mLastDayOfMonth(TODAY),
                     NO).
      fCloseDiscount(ENTRY(LOOKUP(MobSub.CLIType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS_20}),
                     MobSub.MsSeq,
                     Func.Common:mLastDayOfMonth(TODAY),
                     NO).
      
      /* Additional Line with mobile only ALFMO-5 */
      IF MONTH(MobSub.ActivationDate) = MONTH(TODAY) AND 
         YEAR(MobSub.ActivationDate) = YEAR(TODAY) THEN
         ASSIGN ldtCloseDate = Func.Common:mLastDayOfMonth(TODAY).
      ELSE IF MONTH(MobSub.ActivationDate) < MONTH(TODAY) OR
           YEAR(MobSub.ActivationDate) < YEAR(TODAY) THEN
         ASSIGN ldtCloseDate = TODAY.

      fCloseDiscount(ENTRY(LOOKUP(MobSub.CLIType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS_HM}),
                     MobSub.MsSeq,
                     ldtCloseDate,
                     NO).
   END.

   /* COFF Partial termination */
   IF lcTerminationType EQ {&TERMINATION_TYPE_PARTIAL} THEN DO:

      CREATE TermMobsub.
      BUFFER-COPY Mobsub TO TermMobsub.
      ASSIGN 
         TermMobsub.fixednumber = "" /* Fixed line stays active */
         MobSub.CLI = MobSub.FixedNumber
         Mobsub.icc = ""
         Mobsub.imsi = ""
         MobSub.msStatus = {&MSSTATUS_MOBILE_NOT_ACTIVE}.
                       
      /* YDR-2495 Auto STC for Convergent After Mobile Line Termination to Fixed Line  */
      liRequest = fConvFixedSTCReq(MobSub.CLIType,
                                   MobSub.MsSeq,
                                   IF llOutport THEN Func.Common:mMake2DT(TODAY + 5,0)
                                   ELSE Func.Common:mMake2DT(TODAY + 1,0),
                                   {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                                   MsRequest.MsRequest).
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

   /* If Extraline associated mainline is terminated, then check for any existing
      mainline with available count. If available then link to new mainline,
      else close the discount
      (OR)
      IF Extraline is terminated then close the discount */ 
   FIND bCLIType NO-LOCK WHERE
        bCLIType.Brand   EQ Syst.Var:gcBrand   AND
        bCLIType.CLIType EQ TermMobSub.CLIType NO-ERROR.

   IF AVAILABLE bCLIType AND
      ((bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT} AND
        fCLITypeIsMainLine(bCLIType.CLIType)) OR
       (bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY} AND
        fCLITypeIsExtraLine(bCLIType.CLIType)))
   THEN DO:

      IF fCLITypeIsExtraLine(TermMobSub.CLIType) THEN 
      DO:
        
          FIND FIRST lMLMobSub NO-LOCK WHERE 
                     lMLMobsub.brand    EQ Syst.Var:gcBrand       AND
                     lMLMobSub.msseq    EQ TermMobSub.MultiSimId  AND
                    (lMLMobSub.MsStatus EQ {&MSSTATUS_ACTIVE}  OR
                     lMLMobSub.MsStatus EQ {&MSSTATUS_BARRED})    NO-ERROR.
            
          IF AVAILABLE lMLMobSub THEN DO:
         
          lcMandatoryELines = fGetMandatoryExtraLineForMainLine(lMLMobSub.CLIType) .
         
          IF LOOKUP (TermMobSub.CLIType , lcMandatoryELines ) > 0 
          AND fExtraLineCountForMainLine(lMLMobSub.msseq , lMLMobSub.CustNum )  > 0 
          THEN DO:
              
              IF CAN-FIND(FIRST lELMobSub NO-LOCK WHERE
                                lELMobSub.Brand        EQ Syst.Var:gcBrand          AND
                                lELMobSub.MultiSimId   EQ lMLMobSub.msseq           AND
                                lELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} AND 
                                (lELMobSub.MsStatus    EQ {&MSSTATUS_ACTIVE} OR
                                 lELMobSub.MsStatus    EQ {&MSSTATUS_BARRED})       AND 
                                (LOOKUP(lELMobSub.CLIType , lcMandatoryELines) > 0 )  )
                                
              THEN lisMandatoryELExists = YES.            
              
              
              IF lisMandatoryELExists = NO  
              THEN DO:
                  
                  FOR EACH lELMobSub NO-LOCK WHERE
                       lELMobSub.Brand        EQ Syst.Var:gcBrand          AND
                       lELMobSub.MultiSimId   EQ lMLMobSub.msseq           AND
                       lELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} AND 
                       (lELMobSub.MsStatus    EQ {&MSSTATUS_ACTIVE} OR
                        lELMobSub.MsStatus    EQ {&MSSTATUS_BARRED})   BY lELMobSub.ActivationTS:
                            
                  
                       ASSIGN liSTCELMSSeq    =  lELMobSub.MSSeq
                              lcSTCELCLIType  =  lELMobSub.CLIType. /* Oldest Extraline That requires STC */
                       LEAVE.              
                            
                  END. 
                  
                  lcSTCELCLIType = fGetExtraLineMandatoryCLIType(INPUT lMLMobSub.CLIType , INPUT lcSTCELCLIType).
                  
                  IF fCLITypeAllowedForExtraLine(lMLMobSub.CLIType , lcSTCELCLIType , OUTPUT liELCount) 
                  AND lcSTCELCLIType > ""
                  THEN DO:
                      
                      liSTCRequest = fCTChangeRequest(liSTCELMSSeq,                          /* The MSSeq of the subscription to where the STC is made */
                                                      lcSTCELCLIType,                      /* NEW CLITYPE */
                                                      "",                                     
                                                      "",                                      
                                                      TRUNC(Func.Common:mMakeTS(),0),
                                                      0,                                        
                                                      0,                                        
                                                      "",                                        
                                                      FALSE,                                   
                                                      TRUE,                                     
                                                      "",
                                                      0,
                                                      {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION},
                                                      0,
                                                      MSRequest.MSRequest,  /*Parent Request*/
                                                      "",                                      
                                                      OUTPUT lcError).

                      IF liSTCRequest = 0 THEN
                      Func.Common:mWriteMemo("MobSub",
                                             STRING(TermMobSub.MsSeq),
                                             TermMobSub.Custnum,
                                             "Extraline STC request creation failed",
                                             lcError).                                    
                      
                  END. /*IF fCLITypeAllowedForExtraLine*/                 
              END. /* IF lisMandatoryELExists */            
          END. /* IF LOOKUP */                             
          
          ASSIGN TermMobSub.MultiSimId   = 0
                 TermMobSub.MultiSimType = 0.
         
          fCloseExtraLineDiscount(TermMobSub.MsSeq,
                                  TermMobSub.CLIType + "DISC",
                                  TODAY).                        
                           
      END.
      END.
      ELSE IF fCLITypeIsMainLine(TermMobSub.CLIType) THEN
      DO:
         FIND FIRST bCustomer NO-LOCK WHERE
                    bCustomer.custnum EQ Termmobsub.custnum NO-ERROR.

         IF NOT AVAIL bCustomer THEN LEAVE.

         lcExtraLineCLITypes = fExtraLineCLITypes().

         DO liELCount = 1 TO NUM-ENTRIES(lcExtraLineCLITypes):

            FOR EACH lELMobSub EXCLUSIVE-LOCK WHERE
                     lELMobSub.Brand        EQ Syst.Var:gcBrand          AND
                     lELMobSub.MultiSimId   EQ TermMobSub.MsSeq          AND
                     lELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} AND
                     lELMobSub.CLIType      EQ ENTRY(liELCount,lcExtraLineCLITypes):

               fCheckExistingMainLineAvailForExtraLine(INPUT lELMobsub.CLIType ,
                                                       INPUT bCustomer.CustIdType ,
                                                       INPUT bCustomer.OrgID,
                                                       OUTPUT liMLMsSeq).
               IF liMLMsSeq > 0 THEN
                  lELMobsub.MultiSimId = liMLMsSeq.
               ELSE DO:
                  ASSIGN lELMobsub.MultiSimId   = 0
                         lELMobsub.MultiSimType = 0.

                  fCloseExtraLineDiscount(lELMobsub.MsSeq,
                                          lELMobsub.CLIType + "DISC",
                                          TODAY).
               END.
            END.

         END.

      END. /* IF fCLITypeIsMainLine */
   END.

   /* ADDLine-20 Additional Line 
      ADDLINE-323 fixed bug 
      Additional Line with mobile only ALFMO-5 */
   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = Syst.Var:gcBrand AND
              DiscountPlan.DPRuleID = ENTRY(LOOKUP(TermMobSub.CLIType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS_HM}) NO-LOCK NO-ERROR.
   IF CAN-FIND(FIRST bCLIType NO-LOCK WHERE
               bCLIType.Brand      = Syst.Var:gcBrand           AND
               bCLIType.CLIType    = TermMobSub.CLIType                AND
              (bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT}  OR 
               bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_FIXEDONLY})) THEN
   DO:
      FOR EACH bMobSub NO-LOCK WHERE
               bMobSub.Brand   = Syst.Var:gcBrand            AND
               bMobSub.AgrCust = TermMobSub.CustNum AND
               bMobSub.MsSeq  <> TermMobSub.MsSeq   AND
               LOOKUP(bMobSub.CliType, {&ADDLINE_CLITYPES}) > 0:

         fCloseAddLineDiscount(bMobSub.CustNum,
                               bMobSub.MsSeq,
                               bMobSub.CLIType,
                               Func.Common:mLastDayOfMonth(TODAY)).
      END.
   END.

   /* Additional Line with mobile only ALFMO-5 */
   ELSE IF AVAIL DiscountPlan AND 
        CAN-FIND(FIRST bCLIType NO-LOCK WHERE
                 bCLIType.Brand      = Syst.Var:gcBrand   AND
                 bCLIType.CLIType    = TermMobSub.CLIType AND
                 bCLIType.TariffType = {&CLITYPE_TARIFFTYPE_MOBILEONLY}) AND 
        NOT CAN-FIND(FIRST DPMember WHERE
                           DPMember.DPId = DiscountPlan.DPId AND
                           DPMember.HostTable = "MobSub" AND
                           DPMember.KeyValue  = STRING(TermMobSub.MsSeq) AND
                           DPMember.ValidTo   <> 12/31/49) THEN 
   DO:            
      FIND FIRST bCustomer WHERE bCustomer.CustNum = TermMobSub.CustNum NO-LOCK NO-ERROR. 
        /* Just to create the list of clitypes of 
           additional line in ongoing status 
           Additional line mobile only ALFMO */
      IF AVAILABLE bCustomer THEN 
      DO:
         FOR EACH OrderCustomer NO-LOCK WHERE
             OrderCustomer.Brand      = Syst.Var:gcBrand    AND
             OrderCustomer.CustIDType = bCustomer.CustIDType AND
             OrderCustomer.CustID     = bCustomer.OrgID     AND
             OrderCustomer.RowType    = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
         FIRST bOrdTemp NO-LOCK WHERE
               bOrdTemp.Brand      = Syst.Var:gcBrand           AND
               bOrdTemp.OrderID    = OrderCustomer.OrderID             AND
               LOOKUP(bOrdTemp.StatusCode, {&ORDER_INACTIVE_STATUSES}) = 0 AND
               LOOKUP(bOrdTemp.CLIType, {&ADDLINE_CLITYPES}) > 0:
    
            IF CAN-FIND(FIRST OrderAction NO-LOCK WHERE
                        OrderAction.Brand    = Syst.Var:gcBrand AND
                        OrderAction.OrderID  = bOrdTemp.OrderID   AND
                        OrderAction.ItemType = "AddLineDiscount" AND
                        LOOKUP(OrderAction.ItemKey, {&ADDLINE_DISCOUNTS_HM}) > 0 ) THEN
            DO:      
               IF lcAddlineClitypes = "" THEN
                  ASSIGN lcAddlineClitypes = bOrdTemp.CLIType.
               ELSE
                  ASSIGN lcAddlineClitypes = lcAddlineClitypes + "," + bOrdTemp.CLIType.
            END.
         END.

         /* If Main Line is terminated and customer has no other mobile only 
            main line then removing the orderaction from ongoing
            additional line */
         FOR EACH OrderCustomer NO-LOCK WHERE
                  OrderCustomer.Brand      = Syst.Var:gcBrand    AND
                  OrderCustomer.CustIDType = bCustomer.CustIDType AND
                  OrderCustomer.CustID     = bCustomer.OrgID     AND
                  OrderCustomer.RowType    = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
            FIRST bOrder NO-LOCK WHERE
                  bOrder.Brand      = Syst.Var:gcBrand           AND
                  bOrder.OrderID    = OrderCustomer.OrderID             AND
                  LOOKUP(bOrder.StatusCode, {&ORDER_INACTIVE_STATUSES}) = 0 AND
                  LOOKUP(bOrder.CLIType, {&ADDLINE_CLITYPES}) > 0:         
            FIND FIRST OrderAction EXCLUSIVE-LOCK WHERE
                       OrderAction.Brand    = Syst.Var:gcBrand    AND
                       OrderAction.OrderID  = bOrder.OrderID   AND
                       OrderAction.ItemType = "AddLineDiscount" AND
                LOOKUP(OrderAction.ItemKey, {&ADDLINE_DISCOUNTS_HM}) > 0 NO-ERROR.
            IF AVAILABLE OrderAction THEN 
            DO:
               IF NOT fCheckOngoingMobileOnly(OrderCustomer.CustIdType,
                                              OrderCustomer.CustID,
                                              bOrder.CliType) AND
                  NOT fCheckExistingMobileOnly(OrderCustomer.CustIdType,
                                               OrderCustomer.CustID,
                                               bOrder.CliType) THEN 
               DO:
                  llDelete = TRUE.            
                  IF NUM-ENTRIES(lcAddlineClitypes) > 1 THEN
                  DO:
                     IF LOOKUP(ENTRY(3,{&ADDLINE_CLITYPES}),lcAddlineClitypes) > 0 THEN
                     DO:
                        IF bOrder.CLIType <> ENTRY(3,{&ADDLINE_CLITYPES}) THEN
                           llDelete = FALSE.
                     END.
                     ELSE IF LOOKUP(ENTRY(4,{&ADDLINE_CLITYPES}),lcAddlineClitypes) > 0 THEN
                     DO:
                        IF bOrder.CLIType <> ENTRY(4,{&ADDLINE_CLITYPES}) THEN
                           llDelete = FALSE.
                     END.
                  END.
        
                  IF llDelete THEN
                  DO:            
                     DELETE OrderAction.
                     Func.Common:mWriteMemo("Order",
                                      STRING(bOrder.OrderID),
                                      0,
                                      "ADDLINE DISCOUNT ORDERACTION REMOVED",
                                      "Removed AddLineDiscount Item from OrderAction").            
                  END.      
               END.
            END.
         END.
      END.
      FOR EACH bMobSub NO-LOCK WHERE
               bMobSub.Brand   = Syst.Var:gcBrand            AND
               bMobSub.AgrCust = TermMobSub.CustNum AND
               bMobSub.MsSeq  <> TermMobSub.MsSeq   AND
               LOOKUP(bMobSub.CliType, {&ADDLINE_CLITYPES}) > 0:
    
          /* Additional Line with mobile only ALFMO-5 */
         IF MONTH(bMobSub.ActivationDate) = MONTH(TODAY) AND 
            YEAR(bMobSub.ActivationDate) = YEAR(TODAY) THEN
            ASSIGN ldtCloseDate = Func.Common:mLastDayOfMonth(TODAY).
         ELSE IF MONTH(bMobSub.ActivationDate) < MONTH(TODAY) OR
            YEAR(bMobSub.ActivationDate) < YEAR(TODAY) THEN
            ASSIGN ldtCloseDate = TODAY.
    
         fCloseAddLineDiscount(bMobSub.CustNum,
                               bMobSub.MsSeq,
                               bMobSub.CLIType,
                               ldtCloseDate).
                               
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

   FIND FIRST MSISDN WHERE MSISDN.Brand = Syst.Var:gcBrand AND MSISDN.CLI = icMSISDN NO-LOCK USE-INDEX CLI NO-ERROR.
   IF NOT AVAILABLE MSISDN THEN 
      RETURN "ERROR:MSISDN not available".

   FIND MobSub WHERE MobSub.Brand = Syst.Var:gcBrand AND MobSub.Cli = MSISDN.Cli NO-LOCK NO-ERROR.
   IF AVAIL MobSub THEN 
      RETURN "ERROR:MSISDN is in use".
   
   FIND Order WHERE Order.Brand = Syst.Var:gcBrand AND Order.cli = msisdn.cli AND LOOKUP(Order.statuscode,{&ORDER_INACTIVE_STATUSES}) = 0 NO-LOCK NO-ERROR.
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
          Func.Common:mWriteMemo("MobSub",
                         STRING(MobSub.MsSeq),
                         MobSub.Custnum,
                         "CREDIT NOTE CREATION FAILED",
                         lcResult). 
   END. /* IF Order.InvNum > 0 THEN DO: */

   /* YDR-1034 */
   ELSE IF lcTermReason = "3" THEN DO:
      FOR FIRST OrderPayment NO-LOCK WHERE
                OrderPayment.Brand = Syst.Var:gcBrand AND
                OrderPayment.OrderId = Order.OrderId AND
               (OrderPayment.Method = {&ORDERPAYMENT_M_CREDIT_CARD} OR
                OrderPayment.Method = {&ORDERPAYMENT_M_PAYPAL}): 
         CREATE ActionLog.
         ASSIGN ActionLog.Brand     = Syst.Var:gcBrand
                ActionLog.ActionID  = "OrderCancel"
                ActionLog.ActionTS  = Func.Common:mMakeTS()
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
         ActionLog.ActionTS     = Func.Common:mMakeTS()
         ActionLog.Brand        = Syst.Var:gcBrand  
         ActionLog.TableName    = "Order"  
         ActionLog.KeyValue     = STRING(Order.Orderid)
         ActionLog.UserCode     = Syst.Var:katun
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
              lbMobSub.Brand  = Syst.Var:gcBrand AND
              lbMobSub.MultiSimID = MobSub.MultiSimID AND
              lbMobSub.MultiSimType NE MobSub.MultiSimType AND
              lbMobSub.Custnum = MobSub.Custnum NO-ERROR.

   IF NOT AVAIL lbMobSub THEN RETURN.

   CREATE ActionLog.
   ASSIGN
      ActionLog.ActionTS     = Func.Common:mMakeTS()
      ActionLog.Brand        = Syst.Var:gcBrand  
      ActionLog.TableName    = "Customer"  
      ActionLog.KeyValue     = STRING(MobSub.Custnum)
      ActionLog.UserCode     = Syst.Var:katun
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
      NOT Mnp.MNPOutGoing:mIsMNPOutOngoing(INPUT lbMobSub.CLI) THEN DO:

      ASSIGN ldaSecSIMTermDate  = ADD-INTERVAL(TODAY, 1,"months")
             ldaSecSIMTermDate  = Func.Common:mLastDayOfMonth(ldaSecSIMTermDate)
             ldeSecSIMTermStamp = Func.Common:mMake2DT(ldaSecSIMTermDate,86399).

      fInitialiseValues(
         {&SUBSCRIPTION_TERM_REASON_MULTISIM},
         fIsYoigoCLI(lbMobSub.CLI), 
         fIsMasmovilCLI(lbMobSub.CLI),
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
                          Syst.Var:katun,
                          piOrigRequest, /* orig. request */
                          {&TERMINATION_TYPE_FULL},
                          OUTPUT lcError). 
               
      IF lcError > "" THEN 
         Func.Common:mWriteMemo("TermMobsub",
                    STRING(lbMobSub.MsSeq),
                    (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0),
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
         ASSIGN ldeStartTime = Func.Common:mMake2DT(TODAY,0)
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
                     Invoice.Brand      = Syst.Var:gcBrand            AND
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
