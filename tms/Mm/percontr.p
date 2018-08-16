/*-----------------------------------------------------------------------------
  MODULE .......: percontr.p
  FUNCTION .....: handle requests for periodical contracts
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 06.02.06
  CHANGED.. ....: 13.03.06/aam separated from msrequest.i
                  03.04.06/aam different contract types in activation
                  24.05.06/aam pin "aspa" is accepted
                  21.09.06/aam new parameter fo creafat.p (%)
                  17.01.07/aam contract termination may be made as part of the 
                               subscription termination 
                               -> use msowner instead of mobsub 
                                  in pPerContractAct,
                               don't check 14 day limit in termination,
                               create penalty fee for termination
                  10.09.07 vk  Added a coefficient to penalty fee              
                  18.03.08/aam pMaintainContract 
                  05.-2.18 Ashok YDR-2750 Date need not be checked 
                                          for last month fee and service pkgs.
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Rate/daycampaign.i}
{Func/penaltyfee.i}
{Func/fmakeservlimit.i}
{Func/service.i}
{Mm/fbundle.i}
{Func/customerextralinefunc.i}
{Func/nncoit2.i}
{Func/contract_end_date.i}
{Func/fcpfat.i}
{Rate/rerate_request.i}
{Rate/tmqueue_analysis.i}
{Mc/dpmember.i}
{Func/terminal_financing.i}
{Func/ordercancel.i}
{Func/fprepaidfee.i}
{Func/fcreditreq.i}
{Func/fsendsms.i}
{Func/profunc_request.i}
{Func/fixedfee.i}

DEF VAR lcEmailErr AS CHAR NO-UNDO.
DEFINE VARIABLE lcEmailAddr AS CHARACTER NO-UNDO.

FUNCTION fBundleWithSTCCustomer RETURNS LOG
   (iiCustnum    AS INT,
    ideActStamp  AS DEC):

   DEF BUFFER MsRequest FOR MsRequest.

   DEF VAR ldaReqDate    AS DATE NO-UNDO.
   DEF VAR liReqTime     AS INT  NO-UNDO.

   DEF VAR lcPostpaidDataBundles  AS CHAR NO-UNDO.
   DEF VAR lcDataBundleCLITypes   AS CHAR NO-UNDO.

   Func.Common:mSplitTS(ideActStamp,OUTPUT ldaReqDate,OUTPUT liReqTime).

   IF liReqTime > 0 THEN
      ideActStamp = Func.Common:mMake2DT(ldaReqDate + 1,0).

   ASSIGN lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS")
          lcDataBundleCLITypes  = fCParamC("DATA_BUNDLE_BASED_CLITYPES").

   /* Check STC Request with data bundle */
   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.Brand = Syst.Var:gcBrand AND
              MsRequest.Custnum = iiCustnum AND
              MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
              LOOKUP(STRING(MsRequest.ReqStat),"4,9,99,3") = 0 AND
              MsRequest.ActStamp = ideActStamp USE-INDEX Custnum NO-ERROR.
   IF AVAIL MsRequest AND
      (LOOKUP(MsRequest.ReqCparam2,lcDataBundleCLITypes) > 0 OR
       LOOKUP(MsRequest.ReqCparam5,lcPostpaidDataBundles) > 0)
   THEN RETURN TRUE.

   /* Check BTC Request with data bundle */
   FIND FIRST MsRequest NO-LOCK WHERE
              MsRequest.Brand = Syst.Var:gcBrand AND
              MsRequest.Custnum = iiCustnum AND
              MsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
              LOOKUP(STRING(MsRequest.ReqStat),"4,9,99,3") = 0 AND
              MsRequest.ActStamp = ideActStamp USE-INDEX Custnum NO-ERROR.
   IF AVAIL MsRequest AND
      LOOKUP(MsRequest.ReqCparam2,lcPostpaidDataBundles) > 0
   THEN RETURN TRUE.

   RETURN FALSE. 

END FUNCTION.

FUNCTION fUpdateServicelCounterMSID RETURNS LOGICAL
   ( iiCustNum AS INTEGER,
     iiMsSeq   AS INTEGER,
     iiSlSeq   AS INTEGER,
     iiPeriod  AS INTEGER,
     iiOldMSID AS INTEGER,
     iiNewMSID AS INTEGER):

   IF iiOldMSID = 0 OR iiOldMSID = ?
   THEN RETURN TRUE.

   DEFINE VARIABLE liQty AS INTEGER NO-UNDO.

   DEFINE BUFFER ServiceLCounter FOR ServiceLCounter.

   DO WHILE TRUE:

      IF iiCustNum > 0 THEN
         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.Custnum = iiCustNum AND
                    ServiceLCounter.Period  = iiPeriod  AND
                    ServiceLCounter.SLseq   = iiSlSeq   AND
                    ServiceLCounter.MSID    = iiOldMSID
              EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      ELSE
         FIND FIRST ServiceLCounter WHERE
                    ServiceLCounter.Msseq   = iiMsSeq   AND
                    ServiceLCounter.Period  = iiPeriod  AND
                    serviceLCounter.SLseq   = iiSlSeq   AND
                    ServiceLCounter.MSID    = iiOldMSID
              EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

      liQty = liQty + 1.

      IF liQty > 30
      THEN RETURN FALSE.

      IF LOCKED(ServiceLCounter)
      THEN DO:
         PAUSE 1 NO-MESSAGE.
         NEXT.
      END.

      IF NOT AVAILABLE ServiceLCounter
      THEN LEAVE.

      ServiceLCounter.MSID = iiNewMSID.

      RELEASE ServiceLCounter.

      LEAVE.

  END.

  RETURN TRUE.

END FUNCTION.

FUNCTION fExtractWebContractId RETURNS CHARACTER(INPUT icMemo AS CHARACTER ) :
    DEFINE VARIABLE ii     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcChar AS CHARACTER NO-UNDO.
    DO ii = 1 TO NUM-ENTRIES(icMemo):
        lcChar = ENTRY(ii,icMemo).
        IF TRIM(lcChar) BEGINS 'WebContractID=' THEN 
            RETURN ENTRY(2,lcChar,"="). 
    END.
    RETURN ''.
END FUNCTION. 

FUNCTION fTerminationProvisionNeeded RETURNS LOGICAL
   ( iiOrigMsRequest AS INTEGER,
     iiTermMsRequest AS INTEGER ) :

   DEFINE BUFFER MsRequest FOR MsRequest.
   DEFINE BUFFER DayCampaign FOR DayCampaign.

   /* If no parent request (origrequest) then this is
      independent dataplan deletion and provision is needed */
   IF iiOrigMsRequest EQ 0
   THEN RETURN TRUE.

   FIND MsRequest NO-LOCK WHERE MsRequest.MsRequest = iiOrigMsRequest NO-ERROR.

   /* Provision is not needed for the termination request if the parent request
      (origrequest) is STC or bundle change request
      => Then the activation request having the same parent will make
         dataplan modify to the provision system (SAPC) containing
         the information about the termination request */
   IF AVAILABLE MsRequest AND
      ( MsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR
        MsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE} )
   THEN DO:

      /* We need to verify that the activation request for STC or
         bundle change will be provisioned (i.e. the EMAcode > "")
         => If it is not then normal termination provision command
            is sent */
      FOR EACH MsRequest NO-LOCK USE-INDEX OrigRequest WHERE
               MsRequest.OrigRequest EQ iiOrigMsRequest AND
               MsRequest.ReqType     EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
               MsRequest.ReqIParam2  EQ iiTermMsRequest,
          FIRST DayCampaign NO-LOCK WHERE
                DayCampaign.Brand   EQ "1" AND
                DayCampaign.DCEvent EQ MsRequest.ReqCParam3 AND
                DayCampaign.EMACode > "":
         RETURN FALSE.
      END.
   END.

   RETURN TRUE.

END FUNCTION.

DEF BUFFER bPendRequest FOR MsRequest.
DEF BUFFER bOrigRequest FOR MsRequest.

IF llDoEvent THEN DO:

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhFatime AS HANDLE NO-UNDO.
   lhFatime = BUFFER Fatime:HANDLE.
   RUN StarEventInitialize(lhFatime).

   DEFINE VARIABLE lhRequest AS HANDLE NO-UNDO.
   lhRequest = BUFFER bPendRequest:HANDLE.
   RUN StarEventInitialize(lhRequest).

   DEFINE VARIABLE lhDCCLI AS HANDLE NO-UNDO.
   lhDCCLI = BUFFER DCCLI:HANDLE.
   RUN StarEventInitialize(lhDCCLI).

   DEFINE VARIABLE lhMServiceLimit AS HANDLE NO-UNDO.
   lhMServiceLimit = BUFFER MServiceLimit:HANDLE.
   RUN StarEventInitialize(lhMServiceLimit).

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
   RUN StarEventInitialize(lhSingleFee).
   
   DEFINE VARIABLE lhMsOwner AS HANDLE NO-UNDO.
   lhMsOwner = BUFFER MsOwner:HANDLE.
   RUN StarEventInitialize(lhMsOwner).

END.

/********* Main start ********/

llCleanServLimitEventLog = FALSE.

DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

DEF VAR lcReturnValue              AS CHAR NO-UNDO.
DEF VAR lcALLPostpaidBundles       AS CHAR NO-UNDO.
DEF VAR lcALLPostpaidUPSELLBundles AS CHAR NO-UNDO.
DEF VAR liOrigStatus               AS INT  NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest THEN 
   RETURN "ERROR:Unknown request".

/* Remove duplicate Ext API requests where same content. YTS-7583 */
DEF BUFFER bMsRequest    FOR MsRequest.

/* Duplicate check concerns only Ext Api requests */
IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_EXTERNAL_API} THEN DO:

   /* Content for both requests must be the same and have come within 15 seconds */
   IF CAN-FIND(FIRST bMsRequest NO-LOCK WHERE
            bMsRequest.Brand     EQ MsRequest.Brand AND
            bMsRequest.ReqType   EQ MsRequest.ReqType AND
            bMsRequest.ReqStatus EQ MsRequest.ReqStatus AND
            bMsRequest.ActStamp  <= MsRequest.ActStamp + 0.00015 AND
            bMsRequest.ActStamp  >= MsRequest.ActStamp - 0.00015 AND
            bmsrequest.CreStamp  <= MsRequest.CreStamp + 0.00001 AND
            bmsrequest.CreStamp  >= MsRequest.CreStamp - 0.00001 AND
            bMsRequest.MsSeq     EQ MsRequest.MsSeq AND
            bMsRequest.CustNum   EQ MsRequest.CustNum AND
            bMsRequest.ReqCParam3 EQ MsRequest.ReqCParam3 AND
            bMsRequest.ReqSource EQ MsRequest.ReqSource AND
            ROWID(bMsRequest)    NE ROWID(MsRequest)) THEN DO:

      fReqStatus({&REQUEST_STATUS_HANDLED},"Duplicate request.").
      RETURN.
   END.
END.

/* is there another request that should be completed first */
IF MsRequest.ReqStat = 0 THEN 
DO:
   /*YPRO SVA special handling for PRO customers*/
   IF MsRequest.ReqCParam6 BEGINS "SVA_NO_WAIT" THEN 
   DO:
      lcEmailErr = fSendEmailByRequest(MsRequest.MsRequest, "SVA_" + MsRequest.ReqCparam3).
      IF lcEmailErr NE "" THEN 
          RETURN "ERROR: " + lcEmailErr.      
   END.
   ELSE IF MsRequest.ReqCParam6 BEGINS "SVA" THEN 
   DO:
      /*YPRO-84: go to waitinf for bob tool in state 19.*/
      fReqStatus({&REQUEST_STATUS_CONFIRMATION_PENDING},""). /*19*/
      lcEmailErr = fSendEmailByRequest(MsRequest.MsRequest, "SVA_" + MsRequest.ReqCparam3).

      IF lcEmailErr NE "" THEN 
          RETURN "ERROR: " + lcEmailErr.

      RETURN.
   END.
   
   IF MsRequest.ReqIParam2 > 0 THEN DO:
      FIND FIRST bPendRequest WHERE
                 bPendRequest.MsRequest = MsRequest.ReqIParam2
      NO-LOCK NO-ERROR.
      IF AVAILABLE bPendRequest AND 
         LOOKUP(STRING(bPendRequest.ReqStatus),
                {&REQ_INACTIVE_STATUSES} + ",3") = 0 THEN
         RETURN "ERROR:Another request that this depends on has not been " +
                "completed".
   END. /* IF MsRequest.ReqIParam2 > 0 THEN DO: */

   IF LOOKUP(MsRequest.ReqCParam3,{&DSS_BUNDLES}) > 0 AND
      MsRequest.ReqType = 8 THEN DO:

      ASSIGN lcALLPostpaidBundles       = fCParamC("ALL_POSTPAID_CONTRACTS")
             lcALLPostpaidUPSELLBundles = fCParamC("POSTPAID_DATA_UPSELLS").

      IF CAN-FIND(FIRST bPendRequest NO-LOCK WHERE
         bPendRequest.Brand   = Syst.Var:gcBrand AND
         bPendRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} AND
         bPendRequest.Custnum = MsRequest.CustNum AND
         (LOOKUP(bPendRequest.ReqCParam3,lcALLPostpaidBundles) > 0 OR
          LOOKUP(bPendRequest.ReqCParam3,lcALLPostpaidUPSELLBundles) > 0) AND
         LOOKUP(STRING(bPendRequest.ReqStatus),
                {&REQ_INACTIVE_STATUSES} + ",3") = 0
         USE-INDEX CustNum) THEN
      RETURN "ERROR:Another request that this depends on has not been " +
             "completed".
   END. /* IF LOOKUP(MsRequest.ReqCParam3,{&DSS_BUNDLES}) > 0 AND */

END. /* IF MsRequest.ReqStat = 0 THEN DO: */

liOrigStatus = MsRequest.ReqStatus.

CASE MsRequest.ReqType:
/* PIN for periodical contract */
WHEN {&REQTYPE_CONTRACT_PIN} THEN DO:
   RUN pPerContractPIN.
END.

/* activate or continue periodical contract */
WHEN {&REQTYPE_CONTRACT_ACTIVATION} THEN DO:
   /*SVA activation / YPRO*/
   IF MsRequest.ReqCParam6 BEGINS "SVA" AND 
      MsRequest.ReqStatus EQ {&REQUEST_STATUS_HLR_DONE} THEN
      RUN pContractActivation.
   
   IF MsRequest.ReqCParam2 = "update" 
   /* maintenance of a contract */
   THEN RUN pMaintainContract.

   /* re-create contract */ 
   ELSE IF MsRequest.ReqCParam2 = "recreate" THEN DO:
      CASE MsRequest.ReqStatus:
      WHEN {&REQUEST_STATUS_NEW} THEN DO:
         RUN pContractTermination.
         IF NOT RETURN-VALUE BEGINS "ERROR" THEN 
            RUN pContractActivation.
      END.
      WHEN {&REQUEST_STATUS_SUB_REQUEST_DONE} THEN DO:
         RUN pFinalize(INPUT 9). /* Trigger only Deactivation SMS */
         IF NOT RETURN-VALUE BEGINS "ERROR" THEN 
            RUN pFinalize(INPUT 8). /* Trigger only Activation SMS */
      END.
      END CASE.
   END.
   
   /* Reactivate the Terminated Contracts */
   ELSE IF MsRequest.ReqCParam2 = "Reactivate" THEN DO:
      CASE MsRequest.ReqStatus:
      WHEN {&REQUEST_STATUS_NEW} THEN RUN pContractReactivation.
      WHEN {&REQUEST_STATUS_SUB_REQUEST_DONE} OR WHEN {&REQUEST_STATUS_HLR_DONE}
      THEN RUN pFinalize(INPUT 8). /* Trigger only Activation SMS */
      END CASE. /* CASE MsRequest.ReqStatus: */
   END. /* ELSE IF MsRequest.ReqCParam2 = "Reactivate" THEN DO: */

   /* creation, renewal */
   ELSE DO:
      CASE MsRequest.ReqStatus:
      WHEN {&REQUEST_STATUS_NEW} THEN RUN pContractActivation.
      WHEN {&REQUEST_STATUS_SUB_REQUEST_DONE} OR WHEN {&REQUEST_STATUS_HLR_DONE}
      THEN RUN pFinalize(INPUT 8). /* Trigger only Activation SMS */
      END CASE.
   END.
END.   

/* terminate a periodical contract */
WHEN {&REQTYPE_CONTRACT_TERMINATION} THEN DO:

   IF MsRequest.ReqCParam6 BEGINS "SVA" AND 
      MsRequest.ReqStatus EQ {&REQUEST_STATUS_HLR_DONE}
   THEN RUN pContractTermination. /*SVA, YPRO*/

   ELSE CASE MsRequest.ReqStatus:
      WHEN {&REQUEST_STATUS_NEW} THEN RUN pContractTermination.
      WHEN {&REQUEST_STATUS_SUB_REQUEST_DONE} OR WHEN {&REQUEST_STATUS_HLR_DONE}
      THEN RUN pFinalize(INPUT 9). /* Trigger only Deactivation SMS */
   END CASE.
END.

OTHERWISE lcReturnValue = "ERROR:Unknown request type". 
END CASE.

IF lcReturnValue = "" THEN lcReturnValue = RETURN-VALUE.

RETURN lcReturnValue.

FINALLY:
   /* clean eventlog */
   fCleanEventObjects().
END.

/********* Main end ********/
PROCEDURE pIsBundleActivationAllowed:
   DEF INPUT PARAMETER iiMsSeq   AS INTE NO-UNDO.
   DEF INPUT PARAMETER icDCEvent AS CHAR NO-UNDO.

   DEF VAR lcBONOContracts          AS CHAR NO-UNDO.
   DEF VAR lcVoiceBundles           AS CHAR NO-UNDO.
   DEF VAR lcSupplementDataBundles  AS CHAR NO-UNDO.
   DEF VAR lcSupplementVoiceBundles AS CHAR NO-UNDO.

   ASSIGN 
       lcBONOContracts          = fCParamC("BONO_CONTRACTS")
       lcVoiceBundles           = fCParamC("VOICE_BONO_CONTRACTS")
       lcSupplementDataBundles  = fCParamC("SUPPLEMENT_DATA_BONO_CONTRACTS")
       lcSupplementVoiceBundles = fCParamC("SUPPLEMENT_VOICE_BONO_CONTRACTS").
       
    /* Make sure subscription should not have active multiple bundles at the same time */
   IF LOOKUP(icDCEvent,lcBONOContracts) > 0 THEN
   DO:
      FOR EACH ServiceLimit WHERE LOOKUP(ServiceLimit.GroupCode,lcBONOContracts) > 0 NO-LOCK,
          FIRST MServiceLimit WHERE 
                MServiceLimit.MsSeq    = iiMsSeq               AND
                MServiceLimit.DialType = ServiceLimit.DialType AND
                MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
                MServiceLimit.EndTS   >= MsRequest.ActStamp    NO-LOCK:

         IF LOOKUP(ServiceLimit.GroupCode, lcSupplementDataBundles) > 0 THEN 
            NEXT.

         RETURN ERROR "Subscription already has active Data bundle".
      END. /* FOR EACH ServiceLimit NO-LOCK WHERE */
   END.
   ELSE IF LOOKUP(icDCEvent,lcVoiceBundles) > 0 THEN 
   DO:
      FOR EACH ServiceLimit NO-LOCK WHERE LOOKUP(ServiceLimit.GroupCode,lcVoiceBundles) > 0,
          FIRST MServiceLimit WHERE
                MServiceLimit.MsSeq    = iiMsSeq               AND
                MServiceLimit.DialType = ServiceLimit.DialType AND
                MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
                MServiceLimit.EndTS   >= MsRequest.ActStamp    NO-LOCK:

         IF LOOKUP(ServiceLimit.GroupCode, lcSupplementVoiceBundles) > 0 THEN 
            NEXT.

         RETURN ERROR "Subscription already has active Voice bundle".
      END. /* FOR EACH ServiceLimit NO-LOCK WHERE */
   END. 
   ELSE IF LOOKUP(icDCEvent,lcSupplementDataBundles) > 0 THEN 
   DO:
      FOR EACH ServiceLimit NO-LOCK WHERE LOOKUP(ServiceLimit.GroupCode,lcSupplementDataBundles) > 0,
          FIRST MServiceLimit WHERE
                MServiceLimit.MsSeq    = iiMsSeq               AND
                MServiceLimit.DialType = ServiceLimit.DialType AND
                MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
                MServiceLimit.EndTS   >= MsRequest.ActStamp    NO-LOCK:

         RETURN ERROR "Subscription already has active supplementary data bundle".
      END. /* FOR EACH ServiceLimit NO-LOCK WHERE */
   END. 
   ELSE IF LOOKUP(icDCEvent,lcSupplementVoiceBundles) > 0 THEN 
   DO:
      FOR EACH ServiceLimit NO-LOCK WHERE LOOKUP(ServiceLimit.GroupCode,lcSupplementVoiceBundles) > 0,
          FIRST MServiceLimit WHERE
                MServiceLimit.MsSeq    = iiMSSeq               AND
                MServiceLimit.DialType = ServiceLimit.DialType AND
                MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND
                MServiceLimit.EndTS   >= MsRequest.ActStamp    NO-LOCK:
         RETURN ERROR "Subscription already has active supplementary voice bundle".
      END. /* FOR EACH ServiceLimit NO-LOCK WHERE */
   END.   

   RETURN "".

END PROCEDURE.

/* activate a periodical contract */
PROCEDURE pContractActivation:

   DEF VAR lcActType     AS CHAR NO-UNDO.
   DEF VAR lcDCEvent     AS CHAR NO-UNDO.
   DEF VAR ldtFromDate   AS DATE NO-UNDO.
   DEF VAR ldtEndDate    AS DATE NO-UNDO.
   DEF VAR liEndTime     AS INT  NO-UNDO.
   DEF VAR ldtContrDate  AS DATE NO-UNDO.
   DEF VAR liFatPeriod   AS INT  NO-UNDO.
   DEF VAR ldtFatDate    AS DATE NO-UNDO.
   DEF VAR ldBegStamp    AS DEC  NO-UNDO.
   DEF VAR ldEndStamp    AS DEC  NO-UNDO.
   DEF VAR liNewReqStatus AS INT  INITIAL {&REQUEST_STATUS_SUB_REQUEST_DONE} NO-UNDO.
   DEF VAR lcError       AS CHAR NO-UNDO.
   DEF VAR lcUseCLIType  AS CHAR NO-UNDO.
   DEF VAR lcSMSName     AS CHAR NO-UNDO.
   DEF VAR lcSender      AS CHAR NO-UNDO.
   DEF VAR llResult      AS LOG  NO-UNDO.
   DEF VAR liCurrentServiceClass AS INT NO-UNDO. 
   DEF VAR llRerate      AS LOG  NO-UNDO.
   DEF VAR i             AS INT NO-UNDO. 
   def buffer bOrigMsRequest FOR MsRequest.
   DEF VAR ldeBundleFee      AS DEC  NO-UNDO.
   DEF VAR ldaPromoStartDate AS DATE NO-UNDO.
   DEF VAR ldaPromoActStamp  AS DEC  NO-UNDO.
   DEF VAR lcFeeSourceTable  AS CHAR NO-UNDO. 
   DEF VAR lcFeeSourceKey    AS CHAR NO-UNDO. 
   DEF VAR liOrderId         AS INT  NO-UNDO.
   DEF VAR lcReqSource       AS CHAR NO-UNDO.
   DEF VAR lcBundleId        AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.
   DEF VAR lcALLPostpaidUPSELLBundles AS CHAR NO-UNDO.
   DEF VAR ldtPrepActDate             AS DATE NO-UNDO.
   DEF VAR liPrepActTime              AS INT  NO-UNDO.
   DEF VAR liRequest                  AS INT  NO-UNDO.
   DEF VAR liConCount                 AS INT  NO-UNDO.
   DEF VAR ldeFeeAmount               AS DEC  NO-UNDO INIT ?.
   DEF VAR ldeResidualFeeDisc         AS DEC  NO-UNDO. 
   DEF VAR ldaResidualFee             AS DATE NO-UNDO.
   DEF VAR llQ25CreditNote            AS LOG  NO-UNDO. 
   DEF VAR lcSubInvNums               AS CHAR NO-UNDO.
   DEF VAR lcInvRowDetails            AS CHAR NO-UNDO.
   DEF VAR ldeTotalRowAmt             AS DEC  NO-UNDO.
   DEF VAR liCount                    AS CHAR NO-UNDO.
   DEF VAR lcExtraOfferId             AS CHAR NO-UNDO.
   DEF VAR liDiscReq                  AS INTE NO-UNDO.
   DEF VAR lcErrMsg                   AS CHAR NO-UNDO.
   DEF VAR lcDelayedPerContList       AS CHAR NO-UNDO.
   DEF VAR liDelayedDays              AS INT  NO-UNDO.
   DEFINE VARIABLE llSAPC AS LOGICAL INITIAL FALSE NO-UNDO.
   DEFINE VARIABLE loProCommand AS CLASS Gwy.SAPC.ProCommandNBCH NO-UNDO.

   /* DSS related variables */
   DEF VAR lcResult         AS CHAR NO-UNDO.
   DEF VAR lcActDSSBundleId AS CHAR NO-UNDO. 

   DEF BUFFER bOrigReq      FOR MsRequest.
   DEF BUFFER bQ25SingleFee FOR SingleFee.
   DEF BUFFER bMobSub       FOR Mobsub.
   DEF BUFFER bf_Offer      FOR Offer.
   DEF BUFFER bf_OfferItem  FOR OfferItem.

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".
    
   FIND FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE
              MsOwner.MsSeq  = MsRequest.MsSeq AND
              MsOwner.TsEnd >= 99999999 NO-ERROR.
   IF NOT AVAILABLE MsOwner THEN 
   FIND FIRST MsOwner NO-LOCK WHERE
              MsOwner.MsSeq  = MsRequest.MsSeq NO-ERROR.
      
   IF NOT AVAILABLE MsOwner THEN DO:
      fReqError("Subscription not found").
      RETURN.
   END.

   FIND Customer WHERE Customer.CustNum = MsOwner.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   Func.Common:mSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).

   /* day campaign id */
   ASSIGN
      llSAPC       = Customer.AccGrp EQ 2 
      lcDCEvent    = MsRequest.ReqCParam3
      llRerate     = FALSE.
   
   FIND FIRST DayCampaign WHERE 
              DayCampaign.Brand   = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = lcDCEvent NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DayCampaign OR 
      DayCampaign.ValidFrom > ldtActDate OR
      DayCampaign.ValidTo   < ldtActDate
   THEN DO:
      fReqError("Periodical contract is not valid").
      RETURN.
   END.

   /* predetermined length */  
   IF DayCampaign.DurType = 2 OR DayCampaign.DurType = 3 THEN DO:

      IF DayCampaign.DurMonths = 0 AND
         DayCampaign.DurUnit NE 1 THEN DO:
         fReqError("Duration in months has not been defined").
         RETURN.
      END.
   END.

   /* which subscription type should be used for rules */
   lcUseCLIType = MsOwner.CLIType.
   IF MsRequest.OrigRequest > 0 THEN 
   FOR FIRST bOrigRequest NO-LOCK WHERE
             bOrigRequest.MsRequest = MsRequest.OrigRequest:
      IF bOrigRequest.ReqType = 0 THEN 
         lcUseCLIType = bOrigRequest.ReqCParam2. 
   END.
   
   IF NOT CAN-FIND(FIRST TMSRelation WHERE 
                     TMSRelation.TableName   = {&PERIODICAL_CONTRACT_TABLE} AND 
                     TMSRelation.keyType     = {&KEY_SKIP_MATRIX}           AND 
                     TMSRelation.ParentValue = lcDCEvent) 
   THEN  
       IF fMatrixAnalyse(Syst.Var:gcBrand,
                         "PERCONTR",
                         "PerContract;SubsTypeTo",
                         lcDCEvent + ";" + lcUseCLIType,
                         OUTPUT lcReqChar) NE 1
       THEN DO: 
           fReqError("Contract is not allowed for this subscription type").
           RETURN.
       END.

   RUN pIsBundleActivationAllowed(MsOwner.MsSeq,lcDCEvent) NO-ERROR.
   IF ERROR-STATUS:ERROR AND RETURN-VALUE <> "" THEN
   DO: 
       fReqStatus(3, RETURN-VALUE).
       RETURN. 
   END.

   /* Fetch TARJ7 and TARJ9 contract start date */
   IF lcDCEvent = "TARJ7_UPSELL" THEN
      FOR FIRST ServiceLimit WHERE
                ServiceLimit.GroupCode = MsOwner.CliType NO-LOCK:
         FIND FIRST MServiceLimit WHERE
                    MServiceLimit.MsSeq    = MsOwner.MsSeq AND
                    MServiceLimit.DialType = ServiceLimit.DialType AND
                    MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
                    MServiceLimit.EndTS   >= Func.Common:mMakeTS() NO-LOCK NO-ERROR.
         IF NOT AVAIL MServiceLimit THEN DO:
            RUN pSendSMS(INPUT MsOwner.MsSeq,INPUT 0,INPUT "UpsellTARJ7Failed",
                         INPUT 10,INPUT {&UPSELL_SMS_SENDER},INPUT "").
            fReqStatus(3,"Base data contract is not active").
            RETURN.
         END. /* IF NOT AVAIL MServiceLimit THEN DO: */
         Func.Common:mSplitTS(MServiceLimit.FromTS,
                  OUTPUT ldtPrepActDate,
                  OUTPUT liPrepActTime).
      END. /* FOR EACH ServiceLimit NO-LOCK WHERE */
   
   /* Validate Prepaid Balance before making PMDUB activation request */
   IF lcDCEvent BEGINS {&PMDUB} THEN DO:
      ldeBundleFee = fgetPrepaidFeeAmount(lcDCEvent, TODAY).
      IF lcDCEvent = {&PMDUB} THEN DO:
         IF MsRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_CREATION}
         THEN DO:
      
            ASSIGN ldaPromoStartDate = fCParamDa("PMDUB_PROMO_START_DATE")
                   ldaPromoActStamp  = Func.Common:mMake2DT(ldaPromoStartDate,0).

            FIND FIRST Order WHERE
                       Order.MsSeq = MsRequest.MsSeq AND
                       Order.OrderType < 2 NO-LOCK NO-ERROR.
            IF AVAIL Order AND Order.CrStamp < ldaPromoActStamp THEN
               ldeBundleFee = fCParamDe("PMDUBFee_NEW_SUB").
            ELSE
               ldeBundleFee = fCParamDe("PMDUBFee_NEW_SUB_PROMO").
         END.
      END. /* IF lcDCEvent = {&PMDUB} THEN DO: */

      RUN pEnoughBalance(INPUT MsOwner.CLI,INPUT ldeBundleFee,OUTPUT llResult).
      IF NOT llResult THEN DO:
         lcSMSName = (IF lcDCEvent = {&PMDUB} THEN {&PMDUB}
                      ELSE "PMDUBU") + "BalChk".
         IF lcDCEvent = {&PMDUB} THEN
            lcSender = {&BONO8_SMS_SENDER}.
         ELSE IF lcDCEvent = {&PMDUB} + "_UPSELL" THEN
            lcSender = {&UPSELL_SMS_SENDER}.
         RUN pSendSMS(INPUT MsOwner.MsSeq, INPUT 0, INPUT lcSMSName,
                      INPUT 10, INPUT lcSender, INPUT "").
         fReqStatus(9, "Not enough balance").
         RETURN.
      END. /* IF NOT llResult THEN DO: */
   END. /* IF lcDCEvent BEGINS {&PMDUB} AND */

   /* Validate Prepaid Balance before making Prepaid UPSell activation request */
   IF LOOKUP(lcDCEvent,"TARJ_UPSELL,TARJ7_UPSELL") > 0 THEN DO:
      ldeBundleFee = fgetPrepaidFeeAmount(lcDCEvent, TODAY).
      IF lcDCEvent = "TARJ_UPSELL" THEN
         lcSMSName = "UpsellTARJ6NoBal".
      ELSE
         lcSMSName = "UpsellTARJ7NoBal".

      RUN pEnoughBalance(INPUT MsOwner.CLI,INPUT ldeBundleFee,OUTPUT llResult).

      IF NOT llResult THEN DO:
         RUN pSendSMS(INPUT MsOwner.MsSeq, INPUT 0, INPUT lcSMSName,
                      INPUT 10, INPUT {&UPSELL_SMS_SENDER}, INPUT "").
         fReqStatus(9, "Not enough balance").
         RETURN.
      END. /* IF NOT enough_balance(INPUT lcDCEvent, INPUT MsOwner.CLI) */
   END. /* IF lcDCEvent = {&TARJ_UPSELL} THEN DO: */

   IF lcDCEvent EQ {&PMDUB} AND MsOwner.CLIType EQ "TARJ5" THEN DO:
      
      RUN Gwy/air_get_account_details.p(MsOwner.CLI, 
                                    OUTPUT liCurrentServiceClass,
                                    OUTPUT lcError).
   
      IF lcError BEGINS "ERROR" THEN DO:
         fReqStatus(3, lcError).
         RETURN.
      END.

      IF liCurrentServiceClass EQ {&SC_TARJ5_PROMOTIONAL} THEN DO:

         RUN Gwy/air_update_serviceclass.p(MsOwner.CLI,
                                       {&SC_TARJ5_NORMAL_BONO},
                                       {&SC_TARJ5_PROMOTIONAL_BONO},
                                       ?,
                                       OUTPUT lcError).
         IF lcError BEGINS "ERROR" THEN DO:
            fReqError(lcError).
            RETURN.
         END.
      END.
   END.

   IF DayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN
      lcALLPostpaidUPSELLBundles = fCParamC("POSTPAID_DATA_UPSELLS").
      
   /* Check whether DSS is already active or not */
   IF LOOKUP(lcDCEvent,{&DSS_BUNDLES}) > 0 AND
      fIsDSSActive(INPUT MsOwner.CustNum,
                   INPUT MsRequest.ActStamp) THEN DO:
      fReqStatus(3,"DSS bundle is already active").
      RETURN.
   END. /* IF lcDCEvent = {&DSS} AND */
   ELSE IF (lcDCEvent BEGINS {&DSS} + "_UPSELL" OR
      lcDCEvent EQ  "DSS200_UPSELL" OR
      lcDCEvent BEGINS "DSS2_UPSELL" OR 
      lcDCEvent MATCHES "DSS*FLEX*UPSELL") AND
      NOT fIsDSSActive(INPUT MsOwner.CustNum,
                       INPUT MsRequest.ActStamp) THEN DO:
      fReqStatus(3,"DSS bundle is not active").
      RETURN.
   END. /* IF lcDCEvent BEGINS {&DSS} + "_UPSELL" AND */
   ELSE IF LOOKUP(lcDCEvent,lcALLPostpaidUPSELLBundles) > 0 THEN DO:
      lcBundleId = fGetActiveDSSId(INPUT MsOwner.CustNum,
                                   INPUT MsRequest.ActStamp).
      
      IF lcBundleId = {&DSS} THEN DO:
         fReqStatus(3,"Bundle Upsell can not be activated because " +
                    "DSS bundle is already active").
         RETURN.
      END.
      ELSE IF lcBundleId EQ "DSS2" THEN DO:
         ASSIGN
         lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").
         FIND FIRST bMobsub WHERE bMobsub.msseq EQ MsRequest.msseq 
         NO-LOCK NO-ERROR.
         IF AVAIL bMobSub AND LOOKUP(bMobSub.CLIType,lcAllowedDSS2SubsType) > 0
         THEN DO:
            IF (fCLITypeIsMainLine(bMobSub.CLIType) OR
                fCLITypeIsExtraLine(bMobSub.CLIType)) THEN DO:
               IF fCheckActiveExtraLinePair(bMobSub.MsSeq,
                                            bMobSub.CLIType,
                                            OUTPUT lcActDSSBundleId) 
               THEN DO:
                  fReqStatus(3,"Bundle Upsell can not be activated because " +
                             "DSS2 extra line analyse").
                  RETURN.
               END.
            END.    
            ELSE DO:
               fReqStatus(3,"Bundle Upsell can not be activated because " +
                          "DSS2 already active").
               RETURN.
            END.
         END.    

      END.
   END. /* ELSE IF LOOKUP(lcDCEvent,lcALLPostpaidUPSELLBundles) > 0 */

   /* activate contract */
   IF LOOKUP(MsRequest.ReqCParam2,"act,recreate") > 0 THEN DO:
      /* is there already an active contract */
      IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_DCCLI_DCTYPE}) > 0 THEN DO:
         IF lcDCEvent BEGINS "PAYTERM" THEN DO:
            FOR EACH DCCLI NO-LOCK WHERE
                     DCCLI.Brand      = Syst.Var:gcBrand         AND
                     DCCLI.MsSeq      = MsRequest.MsSeq AND
                     DCCLI.ValidTo   >= ldtActDate      AND
                     DCCLI.ValidFrom <= ldtActDate      AND 
                     DCCLI.DCEvent   BEGINS "PAYTERM":
                liConCount = liConCount + 1.             
            END.             
            
            IF liConCount >= 2 THEN DO:
                fReqError("Already two PayTerm contracts are actived on subscription").
                RETURN.
            END.
         END.
         ELSE IF CAN-FIND(FIRST DCCLI WHERE
                                DCCLI.Brand      = Syst.Var:gcBrand         AND
                                DCCLI.DCEvent    = lcDCEvent       AND
                                DCCLI.MsSeq      = MsRequest.MsSeq AND
                                DCCLI.ValidTo   >= ldtActDate      AND
                                DCCLI.ValidFrom <= ldtActDate)
         THEN DO:
            fReqError("Per.contract is already active on subscription").
            RETURN.
         END.         

         /* Q25 creation validation */
         IF MsRequest.ReqCParam3 EQ "RVTERM12" THEN DO:
            
            FIND FIRST FMItem NO-LOCK WHERE
                       FMITem.Brand = Syst.Var:gcBrand AND
                       FMItem.Feemodel = DayCampaign.FeeModel AND
                       FMItem.FromDate <= ldtActDate AND
                       FMITem.ToDate >= ldtActDate NO-ERROR.
               
            IF NOT AVAIL FMITem THEN DO:
               fReqError("RVTERM12 fee model not found").
               RETURN.
            END.
            
            IF MSRequest.ReqSource EQ {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE} OR
               ( MSRequest.ReqSource EQ {&REQUEST_SOURCE_NEWTON} AND
                 MSRequest.ReqDParam2 > 0 )
            THEN ASSIGN
                    ldeResidualFeeDisc = 0
                    ldeFeeAmount = TRUNC(MsRequest.ReqDParam2 / FMItem.FFItemQty,
                                         2).
            ELSE DO:

               FIND bQ25SingleFee NO-LOCK USE-INDEX Custnum WHERE
                    bQ25SingleFee.Brand       = Syst.Var:gcBrand AND
                    bQ25SingleFee.Custnum     = MsOwner.CustNum AND
                    bQ25SingleFee.HostTable   = "Mobsub" AND
                    bQ25SingleFee.KeyValue    = STRING(MsOwner.MsSeq) AND
                    bQ25SingleFee.SourceTable = "DCCLI" AND
                    bQ25SingleFee.SourceKey   = STRING(MsRequest.ReqIParam3) AND
                    bQ25SingleFee.CalcObj     = "RVTERM" NO-ERROR.

               IF NOT AVAIL bQ25SingleFee THEN DO:
                  fReqError("Residual fee not found").
                  RETURN.
               END.

               /* If Quota 25 is already billed then create a 
               "credit note" with equivalent amount. */
               IF bQ25SingleFee.Billed EQ TRUE AND
                  NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                                     Invoice.Invnum = bQ25SingleFee.Invnum AND
                                     Invoice.InvType = 99) THEN DO:
                  IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_NEWTON} THEN
                     llQ25CreditNote = TRUE.
                  ELSE DO:
                     fReqError("Residual fee already billed").
                     RETURN.
                  END.
               END.
               ldaResidualFee = fInt2Date(bQ25SingleFee.Concerns[1],0).
               ldeFeeAmount = bQ25SingleFee.Amt.

               /* Find original installment contract */   
               FIND FIRST DCCLI NO-LOCK WHERE
                          DCCLI.Brand   = Syst.Var:gcBrand AND
                          DCCLI.DCEvent BEGINS "PAYTERM" AND
                          DCCLI.MsSeq   = MsRequest.MsSeq AND 
                          DCCLI.PerContractId = MsRequest.ReqIParam3 NO-ERROR.

               IF NOT AVAIL DCCLI THEN DO:
                  fReqError("Installment contract not found").
                  RETURN.
               END.

               IF DCCLI.TermDate NE ? THEN DO:
                  fReqError("Installment contract terminated").
                  RETURN.
               END.

               RELEASE DCCLI.
               
               FOR EACH DiscountPlan NO-LOCK WHERE
                        DiscountPlan.Brand = Syst.Var:gcBrand AND
                       (DiscountPlan.DPRuleID = "RVTERMDT1DISC" OR
                        DiscountPlan.DPRuleID = "RVTERMDT4DISC"),
                   EACH DPMember NO-LOCK WHERE
                        DPMember.DPId      = DiscountPlan.DPId AND
                        DPMember.HostTable = "MobSub" AND
                        DPMember.KeyValue  = STRING(MsRequest.MsSeq) AND
                        DPMember.ValidTo >= ldaResidualFee AND
                        DPMember.ValidTo <= Func.Common:mLastDayOfMonth(ldaResidualFee) AND
                        DPMember.ValidTo >= DPMember.ValidFrom:
                   ldeFeeAmount = ldeFeeAmount - DPMember.DiscValue.
               END.
               
               ASSIGN
                  ldeResidualFeeDisc = ldeFeeAmount
                  ldeFeeAmount = TRUNC(ldeFeeAmount / FMItem.FFItemQty,2).
            END.

            IF ldeFeeAmount <= 0 THEN DO:
               fReqError("Zero or negative quota 25 extension amount").
               RETURN.
            END.
      
         END.
      END.

      ASSIGN 
         ldtContrDate = ldtActDate
         lcActType    = "Act".
         
      /* if STC triggered this then override 'effective' definition */
      IF MsRequest.ReqSource = "2" AND DAY(ldtActDate) = 1 THEN 
         ldtFromDate = ldtActDate.
         
      ELSE    
      /* when contract becomes effective */
      CASE DayCampaign.Effective:
      
      /* beginning of next month */
      WHEN 2 THEN DO:
         IF MONTH(ldtActDate) = 12 
         THEN ldtFromDate = DATE(1,1,YEAR(ldtActDate) + 1).
         ELSE ldtFromDate = DATE(MONTH(ldtActDate) + 1,1,YEAR(ldtActDate)).
      END.
      
      /* beginning of next billing period = currently same as next month */
      WHEN 3 THEN DO:
         IF MONTH(ldtActDate) = 12 
         THEN ldtFromDate = DATE(1,1,YEAR(ldtActDate) + 1).
         ELSE ldtFromDate = DATE(MONTH(ldtActDate) + 1,1,YEAR(ldtActDate)).
      END.

      /* immediately */
      OTHERWISE ldtFromDate = ldtActDate.

      END CASE.
      
   END.
   
   /* continue contract with 1 year */
   ELSE IF MsRequest.ReqCParam2 = "cont" THEN DO:
      
      /* current contract */
      FIND FIRST DCCLI WHERE
                 DCCLI.Brand   = Syst.Var:gcBrand      AND
                 DCCLI.DCEvent = lcDCEvent    AND
                 DCCLI.MsSeq   = MsRequest.MsSeq
      NO-LOCK NO-ERROR.
                 
      IF NOT AVAILABLE DCCLI OR DCCLI.ValidTo < ldtActDate - 10 THEN DO:
         fReqError("No active contract was found").
         RETURN.
      END.

      /* already in termination process */
      IF DCCLI.TermDate NE ? THEN DO:
         fReqError("Current contract is marked to be terminated").
         RETURN.
      END.
      
      /* mark current contract terminated */
      FIND CURRENT DCCLI EXCLUSIVE-LOCK.
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDCCLI).
      DCCLI.TermDate = DCCLI.ValidTo.
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDCCLI).

      ASSIGN ldtContrDate = DCCLI.ContractDate
             ldtFromDate  = DCCLI.ValidTo + 1
             lcActType    = "Cont".
   END.
 
   ELSE DO:
      fReqError("Nothing to do").
      RETURN.
   END.

   IF LOOKUP(lcDCEvent,"TARJ_UPSELL,TARJ7_UPSELL") > 0 THEN DO:
      RUN pAdjustBal(INPUT lcDCEvent,INPUT MsOwner.MsSeq,
                     INPUT ldeBundleFee,OUTPUT lcError).
      IF lcError > "" THEN DO:
         IF lcDCEvent = "TARJ7_UPSELL" THEN lcSMSName = "UpsellTARJ7Failed".
         RUN pSendSMS(INPUT MsOwner.MsSeq, INPUT 0, INPUT lcSMSName,
                      INPUT 10, INPUT {&UPSELL_SMS_SENDER}, INPUT "").
         fReqStatus(9, UPPER(lcDCEvent) +
                       " can not be activated due to error: " + lcError).
         RETURN.
      END. /* IF lcError > "" THEN DO: */
   END. /* IF LOOKUP(lcDCEvent,"TARJ_UPSELL,TARJ7_UPSELL") > 0 THEN DO: */

   /* Call the function to calculate the contract end date */
   IF lcDCEvent = "TARJ7_UPSELL" THEN DO:
      
      IF ldtPrepActDate NE ldtActDate AND
         DAY(ldtActDate) <= DAY(ldtPrepActDate) THEN DO:

         IF (DAY(ldtActDate) EQ DAY(ldtPrepActDate) OR
            Func.Common:mLastDayOfMonth(ldtActDate) EQ ldtActDate) AND 
            liActTime > 36000 THEN
            ldtEndDate = Func.Common:mLastDayOfMonth(ldtActDate) + 1.
         ELSE ldtEndDate = ldtActDate.
      END.
      ELSE ldtEndDate = Func.Common:mLastDayOfMonth(ldtActDate) + 1.
         
      IF DAY(Func.Common:mLastDayOfMonth(ldtEndDate)) >= DAY(ldtPrepActDate) THEN
         ldtEndDate = DATE(MONTH(ldtEndDate),
                           DAY(ldtPrepActDate),
                           YEAR(ldtEndDate)).
      ELSE ldtEndDate = Func.Common:mLastDayOfMonth(ldtEndDate). 

      liEndTime = 36000.
   END.
   ELSE ASSIGN
      ldtEndDate = fcontract_end_date(INPUT lcDCEvent,INPUT ldtFromDate)
      liEndTime = 86399.

   IF ldtEndDate >= 12/31/2049 THEN ldEndStamp = 99999999.99999.
   ELSE ldEndStamp = Func.Common:mMake2DT(ldtEndDate,liEndTime).
        
   /* YCO-757 Periodical Contracts with aplication date delayed.                */
   /* For this Periodical Contracts, delay FromDate, but it must keep EndDate.  */ 
   lcDelayedPerContList = Syst.Parameters:getc("DelayedPermanencies", "Discount").
   IF LOOKUP(lcDCEvent, lcDelayedPerContList) > 0 THEN DO:
      liDelayedDays = Syst.Parameters:geti("DelayPermanencyValue", "Discount").
      IF ldtFromDate < (ldtActDate + liDelayedDays) THEN
         ASSIGN 
            ldtFromDate  = (ldtActDate + liDelayedDays)
            ldtContrDate = (ldtActDate + liDelayedDays).          
   END.   

   ldBegStamp = Func.Common:mMake2DT(ldtFromDate,
                         IF ldtFromDate = ldtActDate
                         THEN liActTime
                         ELSE 0).
   
   IF LOOKUP(DayCampaign.DCType,
             {&PERCONTRACT_RATING_PACKAGE},{&DCTYPE_POOL_RATING}) > 0 AND
      ldBegStamp < Func.Common:mSecOffSet(Func.Common:mMakeTS(),-300) THEN DO:
         IF DAY(TODAY) > 1 OR MONTH(ldtFromDate) NE MONTH(TODAY) THEN 
            llReRate = TRUE.  
   END.   

   /* create rating package */
   IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 THEN DO:
  
      /* Adjust the balance from prepaid account to activate the PMDUB */
      IF lcDCEvent = {&PMDUB} THEN DO:
         RUN pAdjustBal(INPUT lcDCEvent,INPUT MsOwner.MsSeq,
                        INPUT ldeBundleFee,OUTPUT lcError).
         IF lcError > "" THEN DO:
            lcSMSName = lcDCEvent + "BalFail".
            RUN pSendSMS(INPUT MsOwner.MsSeq, INPUT 0, INPUT lcSMSName,
                         INPUT 10, INPUT {&BONO8_SMS_SENDER}, INPUT "").
            fReqStatus(9, UPPER(lcDCEvent) +
                          " can not be activated due to error: " + lcError).
            RETURN.
         END. /* IF lcError > "" THEN DO: */
      END. /* IF lcDCEvent = {&PMDUB} THEN DO: */
        
      IF NOT fMakeServLimit(lcDCEvent,       /* ServiceLimitGroup */
                            MsRequest.MsSeq,
                            MsOwner.CustNum,
                            ldBegStamp,
                            ldEndStamp,
                     OUTPUT lcResult) THEN DO:
         fReqError("Rating package " + lcDCEvent + " creation failed. " +
                   (IF lcResult > "" THEN lcResult ELSE "")).
         RETURN.
      END.
   END. 

   ELSE IF DayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN DO:

      /* Adjust the balance from prepaid account to activate the PMDUB */
      IF lcDCEvent = ({&PMDUB} + "_UPSELL") THEN DO:
         RUN pAdjustBal(INPUT lcDCEvent,INPUT MsOwner.MsSeq,
                        INPUT ldeBundleFee,OUTPUT lcError).
         IF lcError > "" THEN DO:
            lcSMSName = "PMDUBU" + "BalFail".
            RUN pSendSMS(INPUT MsOwner.MsSeq, INPUT 0, INPUT lcSMSName,
                         INPUT 10, INPUT {&UPSELL_SMS_SENDER}, INPUT "").
            fReqStatus(9, UPPER(lcDCEvent) +
                          " can not be activated due to error: " + lcError).
            RETURN.
         END. /* IF lcError > "" THEN DO: */
      END. /* IF pcBundleId EQ "PMDUB" THEN DO: */

      fMakeServLPool(lcDCEvent,       /* ServiceLimitGroup */
                     MsRequest.MsSeq,
                     MsOwner.CustNum,
                     ldBegStamp,
                     (IF lcDCEvent = "TARJ7_UPSELL" THEN ldEndStamp
                      ELSE 0), /* end stamp, default end of the month */
                     OUTPUT lcError).
      IF lcError > "" THEN DO:
         fReqError(lcError).
         RETURN.
      END.
   END. 

   /* create a new contract */
   ELSE DO:
     
      CREATE DCCLI.
      ASSIGN DCCLI.Brand         = Syst.Var:gcBrand
             DCCLI.PerContractID = NEXT-VALUE(PerContractID)
             DCCLI.DCEvent       = lcDCEvent
             DCCLI.MsSeq         = MsRequest.MsSeq
             DCCLI.CLI           = MsRequest.CLI
             DCCLI.ContractDate  = ldtContrDate
             DCCLI.ValidFrom     = ldtFromDate
             DCCLI.ValidTo       = ldtEndDate
             DCCLI.CreateFees    = LOOKUP(DayCampaign.DCType,"3,5") > 0.
      
      IF DayCampaign.DCEvent = "YOICARD" THEN 
          DCCli.ServiceStatus = MSRequest.ReqIParam1.
          
      IF DayCampaign.BundleTarget = {&DC_BUNDLE_TARGET_SVA} THEN DO:
          DCCLi.WebContractID = fExtractWebContractId(MsRequest.Memo). 
          IF Mm.MManMessage:mGetMessage("EMAIL", "SVA_ActEmail", 1) EQ TRUE THEN DO:
            FIND FIRST DiscountPlan WHERE DiscountPlan.Brand    = Syst.Var:gcBrand AND 
                                          DiscountPlan.DPRuleID = DayCampaign.DcEvent + "DISC" NO-LOCK NO-ERROR.
            FIND FIRST DPRate WHERE 
                       DPRate.DPId = DiscountPlan.DPId AND 
                       DPRate.ValidFrom <= TODAY AND
                       DPRate.ValidTo   >= TODAY NO-LOCK NO-ERROR.
            IF AVAILABLE DPrate THEN 
               Mm.MManMessage:ParamKeyValue = REPLACE(Mm.MManMessage:ParamKeyValue,"#SVADISC", STRING(DPRate.DiscValue) + " %" ).
            ELSE 
               Mm.MManMessage:ParamKeyValue = REPLACE(Mm.MManMessage:ParamKeyValue,"#SVADISC", '').
            FIND FIRST FMItem WHERE FMItem.Brand     EQ Syst.Var:gcBrand              AND 
                       FMItem.FeeModel  EQ DayCampaign.FeeModel AND 
                       FMItem.BillCode  <> ""                   AND 
                       FMItem.PriceList <> ""                   AND
                       FMItem.FromDate  <= TODAY                AND 
                       FMItem.ToDate    >= TODAY                NO-LOCK NO-ERROR.
              IF AVAILABLE FMITem THEN 
                  Mm.MManMessage:ParamKeyValue = REPLACE(Mm.MManMessage:ParamKeyValue,"#SVAMF" , STRING(FMItem.Amount)).
              Mm.MManMessage:ParamKeyValue = REPLACE(Mm.MManMessage:ParamKeyValue,"#SVANAME",DayCampaign.DCName).
              Mm.MManMessage:ParamKeyValue = fGetEmailKeyValuePairs( MSRequest.MsRequest ,Mm.MManMessage:ParamKeyValue ).
              IF NUM-ENTRIES(MSRequest.ReqCparam6,"|") GT 2 AND ENTRY(3,MSRequest.ReqCparam6, "|") <> "" THEN
                 lcEmailAddr = ENTRY(3,MSRequest.ReqCparam6, "|").
              ELSE IF NUM-ENTRIES(MSRequest.ReqCparam6,"|") EQ 2 AND ENTRY(2,MSRequest.ReqCparam6, "|") <> "" THEN
                 lcEmailAddr = ENTRY(2,MSRequest.ReqCparam6, "|").
              ELSE DO:
                   FIND FIRST Customer NO-LOCK WHERE
                        Customer.CustNum EQ MsRequest.CustNum NO-ERROR.  
                   IF AVAILABLE Customer THEN 
                        lcEmailAddr  = Customer.email.
              END. 
              IF lcEmailAddr NE "" THEN 
                 Mm.MManMessage:mCreateMMLogEmail(lcEmailAddr, TRUE).
              Mm.MManMessage:mClearData().
          END.
      END. 

      IF llDoEvent THEN RUN StarEventMakeCreateEvent (lhDCCLI).

      FIND CURRENT MsRequest EXCLUSIVE-LOCK.
      MsRequest.ReqIParam5 = DCCLI.PerContractId.

      /* create counters */
      IF DayCampaign.DurType NE 3 THEN DO:       
         fGenerateCounters(MsRequest.MsSeq,
                           lcDCEvent).
      END.
      
      /* create fatimes */
      IF DayCampaign.BillCode = "" AND 
         CAN-FIND(FIRST FatGroup WHERE
                        FatGroup.Brand = Syst.Var:gcBrand AND
                        FatGroup.FtGrp = DCCLI.DCEvent)
      THEN DO:
      
         /* first discount for the first full period */
         ldtFatDate = DCCLI.ValidFrom.
         IF DAY(ldtFatDate) > 1 THEN DO:
            IF MONTH(ldtFatDate) = 12 
            THEN ldtFatDate = DATE(1,1,YEAR(ldtFatDate) + 1).
            ELSE ldtFatDate = DATE(MONTH(ldtFatDate) + 1,1,YEAR(ldtFatDate)).
         END.

         liFatPeriod = YEAR(ldtFatDate) * 100 + MONTH(ldtFatDate).
         
         RUN Mc/creafat.p (MsOwner.CustNum,
                      DCCLI.MsSeq,
                      DCCLI.DCEvent,
                      0,   /* amount */
                      0,   /* %  */
                      ?,  
                      liFatPeriod,
                      999999,
                      OUTPUT lcReqChar).
      
         IF lcReqChar > "" THEN DO:
            fReqLog("Per.contract fatime creation failed: " + lcReqChar).
         END. 
      END.

      /* link to terminal */
      IF DayCampaign.DCType = "3" AND
         NOT DayCampaign.DCEvent BEGINS "FTERM" AND
         LOOKUP(MsRequest.ReqSource,"1,7") > 0 THEN DO:
         
         /* new subscription */
         IF MsRequest.ReqSource = "1" THEN 
            FIND FIRST Order WHERE 
                       Order.MsSeq = MsRequest.MsSeq AND
                       Order.OrderType < 2 NO-LOCK NO-ERROR.
         /* aftersales (renove) */
         ELSE DO:
            FIND FIRST bOrigReq WHERE 
                       bOrigReq.MsRequest = MsRequest.OrigReq NO-LOCK NO-ERROR.
            IF AVAILABLE bOrigReq THEN 
               FIND FIRST Order WHERE
                          Order.Brand   = Syst.Var:gcBrand AND
                          Order.OrderID = bOrigReq.ReqIParam1 NO-LOCK NO-ERROR.
         END.
         
         IF AVAILABLE Order THEN DO:
           FOR EACH SubsTerminal WHERE   
                    SubsTerminal.Brand   = Syst.Var:gcBrand AND
                    SubsTerminal.OrderID = Order.OrderID AND
                    SubsTerminal.MsSeq   = MsRequest.MsSeq AND
                    SubsTerminal.PerContractID = 0 EXCLUSIVE-LOCK:
               SubsTerminal.PerContractID = DCCLI.PerContractID.
            END.
         END.
         
      END.
         
      /* link to possible fee */
      IF DCCLI.PerContractID > 0 THEN ASSIGN
         lcFeeSourceTable = "DCCLI"
         lcFeeSourceKey = STRING(DCCLI.PerContractID).

   END.  /* dctype ne 1/4 */

   IF llSAPC
   THEN DO:
      /* Subscription creation request is handling the
         provision when ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_CREATION} */
      IF DayCampaign.EMACode > "" AND
         MsRequest.ReqSource NE {&REQUEST_SOURCE_SUBSCRIPTION_CREATION}
      THEN DO ON ERROR UNDO, THROW:
         loProCommand = NEW Gwy.SAPC.ProCommandNBCH(MsRequest.MsRequest). 
      
         loProCommand:mStoreProCommand().

         liNewReqStatus = {&REQUEST_STATUS_HLR_PENDING}.

         CATCH loError AS Progress.Lang.Error:
            fReqErrorObject(loError).
            RETURN.
         END CATCH.

         FINALLY:
            IF VALID-OBJECT(loProCommand)
            THEN DELETE OBJECT loProCommand.
         END FINALLY.
      END.
   END.

   /* Activate package related services (only when not SAPC) */
   ELSE RUN pActivateServicePackage(lcDCEvent,
                                    MsOwner.MsSeq,
                                    lcUseCLIType,
                                    (IF ldtFromDate < TODAY THEN TODAY
                                     ELSE ldtFromDate) /* Renewal order can be past date */,
                                    OUTPUT liNewReqStatus).

   /* create (monthly) fees for new contract */
   IF MsRequest.CreateFees AND
      (DayCampaign.FeeModel > "" OR MsRequest.ReqCparam5 > "") THEN DO:

      IF DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT} THEN ASSIGN
         liOrderid = fGetInstallmentOrderId(msrequest.msrequest)
         lcReqSource = ";" + MsRequest.Reqsource. 
               
      /* map q25 fee to original residual fee */
      IF AVAIL bQ25SingleFee THEN
         liOrderId = bQ25SingleFee.OrderId.

      RUN Mc/creasfee.p (MsOwner.CustNum,
                    (IF (lcDCEvent = {&DSS} + "_UPSELL" OR
                         lcDCEvent EQ  "DSS200_UPSELL" OR
                         lcDCEvent MATCHES "DSS*FLEX*UPSELL" OR
                         lcDCEvent = "DSS2_UPSELL") THEN liDSSMsSeq
                     ELSE MsRequest.MsSeq),
                    ldtFromDate,
                    "FeeModel",
                    (IF MsRequest.ReqCparam5 > "" THEN MsRequest.ReqCparam5
                     ELSE DayCampaign.FeeModel),
                    9,
                    ldeFeeAmount,
                    lcDCEvent + " created " + 
                       STRING(ldtActDate,"99.99.9999") +  /* memo */
                    "¤" + lcDCEvent,    /* calcobject */
                    FALSE,              /* no messages to screen */
                    MsRequest.UserCode,
                    "ContractActivation" + lcReqSource,
                    liOrderId,
                    lcFeeSourceTable,
                    lcFeeSourceKey,
                    OUTPUT lcReqChar).

      IF lcReqChar BEGINS "ERROR:" OR lcReqChar BEGINS "0" THEN DO:
         fReqLog("Fee creation (" + DayCampaign.FeeModel + 
                 ") failed for new contract: " + lcReqChar).
      END.
      /* PAYTERM residual fee */
      ELSE IF DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT} AND
         DayCampaign.DCEvent BEGINS "PAYTERM" AND
         AVAIL DCCLI AND
         MsRequest.ReqDParam2 > 0 THEN DO:

         RUN Mc/creasfee.p(MsOwner.CustNum,
                       MsOwner.MsSeq,
                       DCCLI.ValidTo + 1,
                       "FeeModel",
                       "RVTERM",
                       9,
                       MsRequest.ReqDParam2,
                       lcDCEvent + " created " + 
                          STRING(ldtActDate,"99.99.9999") +  /* memo */
                       "¤" +  "RVTERM" ,  /* calcobject */
                       FALSE,              /* no messages to screen */
                       MsRequest.UserCode,
                       "ContractActivation",
                       liOrderId, /* order id */
                       lcFeeSourceTable,
                       lcFeeSourceKey,
                       OUTPUT lcReqChar).

         IF lcReqChar BEGINS "ERROR:" OR lcReqChar BEGINS "0" THEN DO:
            fReqLog("Residual fee creation (RVTERM) failed for " + 
                    "new contract: " + lcReqChar).
         END.
         ELSE DCCLI.Amount = MsRequest.ReqDParam2.

      END.
      ELSE IF lcDCEvent EQ "RVTERM12" AND
         AVAIL bQ25SingleFee AND
         llQ25CreditNote EQ TRUE THEN DO:

         ASSIGN
           lcError = ""
           liRequest = 0.

         FOR FIRST Invoice NO-LOCK WHERE
                   Invoice.InvNum = bQ25SingleFee.InvNum AND
                   Invoice.InvType = 1,
             FIRST SubInvoice NO-LOCK WHERE
                   Subinvoice.InvNum = Invoice.InvNum AND
                   Subinvoice.MsSeq = MsOwner.MsSeq,
              EACH InvRow NO-LOCK WHERE
                   InvRow.InvNum = Invoice.InvNum AND
                   InvRow.SubInvNum = SubInvoice.SubInvNum AND
                  (InvRow.BillCode = bQ25SingleFee.BillCode OR
                   InvRow.BillCode = "RVTERMDTRW") AND
                   InvRow.CreditInvNum = 0:

            IF InvRow.BillCode = bQ25SingleFee.BillCode AND
               InvRow.Amt < bQ25SingleFee.Amt THEN NEXT.
            
            IF InvRow.OrderId > 0 AND
               bQ25SingleFee.OrderID > 0 AND
               InvRow.OrderId NE bQ25SingleFee.OrderId THEN NEXT.
            
            ASSIGN lcSubInvNums    = lcSubInvNums + "," + STRING(SubInvoice.SubInvNum) WHEN
                                     LOOKUP(STRING(SubInvoice.SubInvNum), lcSubInvNums) = 0    
                   lcInvRowDetails = lcInvRowDetails + "," +
                                     "InvRow="       + STRING(InvRow.InvRowNum) + "|" +
                                     "InvRowAmt="    + STRING(InvRow.Amt)
                   ldeTotalRowAmt  = ldeTotalRowAmt + InvRow.Amt.

         END.

         ASSIGN lcSubInvNums    = TRIM(lcSubInvNums,",")
                lcInvRowDetails = TRIM(lcInvRowDetails,",").

         IF lcSubInvNums = "" OR ldeTotalRowAmt < 0 THEN
            Func.Common:mWriteMemo("MobSub",
                             STRING(MsRequest.MsSeq),
                             MsRequest.Custnum,
                             "CREDIT NOTE CREATION FAILED",
                             "ERROR:Invoice is already credited").
         ELSE DO:
            liRequest = fFullCreditNote(Invoice.InvNum,
                                        lcSubInvNums,
                                        lcInvRowDetails,
                                        "Correct",
                                        "2013",
                                        "",
                                        OUTPUT lcError).
               
            IF liRequest = 0 THEN
               Func.Common:mWriteMemo("MobSub",
                                STRING(MsRequest.MsSeq),
                                MsRequest.Custnum,
                                "CREDIT NOTE CREATION FAILED",
                                "ERROR:" + lcError). 
         END.
      END. 
      ELSE IF lcDCEvent EQ "RVTERM12" AND
         ldeResidualFeeDisc > 0 THEN DO:

         lcError = fAddDiscountPlanMember(MsOwner.MsSeq,
                                          "RVTERMDT2DISC",
                                          ldeResidualFeeDisc,
                                          ldaResidualFee,
                                          ?,
                                          1,
                                          bQ25SingleFee.OrderId). /* Q25 OrderId */
         /* write possible error to an order memo */
         IF lcError BEGINS "ERROR" THEN
            Func.Common:mWriteMemo("MobSub",
                             STRING(MsOwner.MsSeq),
                             MsOwner.CustNum,
                             "RVTERMDT2 discount creation failed",
                             lcError).
      END.
      
      IF DayCampaign.BundleTarget EQ {&DC_BUNDLE_TARGET_SVA} THEN 
      DO:
          ASSIGN lcExtraOfferId = ENTRY(4, MsRequest.ReqCParam6, "|") NO-ERROR.

          IF lcExtraOfferId > "" THEN
          DO:
              FIND FIRST bf_Offer WHERE bf_Offer.Brand = Syst.Var:gcBrand AND
                                        bf_Offer.Offer = lcExtraOfferId   NO-LOCK NO-ERROR.
              IF AVAIL bf_Offer THEN 
              DO:
                  FIND FIRST bf_OfferItem WHERE bf_OfferItem.Brand       = bf_Offer.Brand        AND 
                                                bf_OfferItem.Offer       = bf_Offer.Offer        AND 
                                                bf_OfferItem.ItemType    = "DiscountPlan"     AND  
                                                bf_OfferItem.ItemKey     > ""                 AND 
                                                bf_OfferItem.BeginStamp <= MsRequest.ActStamp AND 
                                                bf_OfferItem.EndStamp   >= MsRequest.ActStamp NO-LOCK NO-ERROR.
                  IF AVAIL bf_OfferItem AND bf_OfferItem.Amount > 0 THEN
                  DO:
                      lcErrMsg = fAddDiscountPlanMember(MsRequest.MsSeq,
                                                        bf_OfferItem.ItemKey,
                                                        bf_OfferItem.Amount,
                                                        TODAY,
                                                        ?,
                                                        bf_OfferItem.Periods,
                                                        0).
                      IF lcErrMsg BEGINS "ERROR" THEN
                          fReqLog("Failed to add discount for (" + DayCampaign.DCEvent + "). Error: '" + lcErrMsg + "'").
                  END.  /* IF AVAIL OfferItem THEN */
              END.                       
          END.                    
      END. /* IF DayCampaign.BundleTarget EQ {&DC_BUNDLE_TARGET_SVA} THEN  */
      
   END.
   
   /* Temporary FAT creation for BONO_VOIP. YDA-173 */
   IF lcDCEvent EQ "BONO_VOIP" THEN DO:
      RUN Mc/creafat.p (MsOwner.CustNum,
                     MsOwner.MsSeq,
                     "BONOVOIPCPACT",
                     ?, /* amount */
                     0, /* percent */
                     ?, /* vat incl. */
                     YEAR(ldtFromDate) * 100 + MONTH(ldtFromdate),
                     999999,
                     OUTPUT lcError).

      /* write possible error to an order memo */
      IF lcError > "" THEN
         Func.Common:mWriteMemo("MobSub",
                          STRING(MsOwner.MsSeq),
                          MsOwner.CustNum,
                          "BONOVOIPCPACT FATIME CREATION FAILED",
                          lcError).
   END.

   IF AVAILABLE DCCLI THEN RELEASE DCCLI.
    
   FIND CURRENT MsRequest EXCLUSIVE-LOCK.
   MsRequest.ReqDtParam1 = ldtFromDate.
   IF llRerate THEN MsRequest.ReqIParam4 = 1.
   
   fReqStatus(liNewReqStatus, ""). 
 
   /* If DSS Upsell is being added then update DSS Quota */
   IF lcDCEvent BEGINS {&DSS} + "_UPSELL" OR
      lcDCEvent EQ  "DSS200_UPSELL" OR
      lcDCEvent BEGINS "DSS2_UPSELL" OR 
      lcDCEvent MATCHES "DSS*FLEX*UPSELL" THEN
      RUN pUpdateDSSNetworkLimit(INPUT MsOwner.MsSeq,
                                 INPUT MsOwner.CustNum,
                                 INPUT ldeDSSUpsellLimit,
                                 INPUT "QUOTA",
                                 INPUT FALSE,
                                 INPUT MsRequest.MsRequest,
                                 INPUT MsRequest.ActStamp,
                                 INPUT MsRequest.ReqSource,
                                 INPUT lcDCEvent).

   /* Update DSS Limit if other bundle is being added to DSS group */
   /* Again check latest and existing DSS Limit in MsRequest       */
   /* parameter if new DSS bundle is being added                   */
   ELSE IF ldeDSSTotalLimit > 0 THEN
      RUN pUpdateDSSNetworkLimit(INPUT MsOwner.MsSeq,
                                 INPUT MsOwner.CustNum,
                                 INPUT ldeDSSTotalLimit,
                                 INPUT "LIMIT",
                                 INPUT (IF LOOKUP(lcDCEvent,{&DSS_BUNDLES}) > 0
                                        THEN TRUE ELSE FALSE),
                                 INPUT MsRequest.MsRequest,
                                 INPUT MsRequest.ActStamp,
                                 INPUT MsRequest.ReqSource,
                                 INPUT lcDSSBundleId).

   /* If DSS bundle is being added then update consumption */
   /* to the network by HSDPA MSISDN List                  */
   IF LOOKUP(lcDCEvent,{&DSS_BUNDLES}) > 0 AND lcResult > "" THEN
      RUN pUpdateDSSNetwork(INPUT MsOwner.MsSeq,
                            INPUT MsOwner.CLI,
                            INPUT MsOwner.CustNum,
                            INPUT "MODIFY",
                            INPUT lcResult,    /* HSDPA MSISDN list */
                            INPUT MsRequest.MsRequest,
                            INPUT MsRequest.ActStamp,
                            INPUT MsRequest.ReqSource,
                            INPUT lcDCEvent).

   RETURN "".
    
END PROCEDURE.

PROCEDURE pFinalize:

   DEF INPUT PARAMETER iiRequestType AS INT  NO-UNDO.
  
   DEF VAR lcSender                  AS CHAR NO-UNDO.
   DEF VAR lcDCEvent                 AS CHAR NO-UNDO.
   DEF VAR liPeriod                  AS INT  NO-UNDO.
   DEF VAR ldeFirstMonthLimit        AS DEC  NO-UNDO.
   DEF VAR ldeOtherMonthLimit        AS DEC  NO-UNDO.
   DEF VAR ldeFirstMonthFee          AS DEC  NO-UNDO.
   DEF VAR ldeOtherMonthFee          AS DEC  NO-UNDO.
   DEF VAR ldeRerateActStamp         AS DEC  NO-UNDO.
   DEF VAR ldtDoneDate               AS DATE NO-UNDO.
   DEF VAR liDoneTime                AS INT  NO-UNDO.
   DEF VAR lcIPLContracts            AS CHAR NO-UNDO.
   DEF VAR lcCONTDContracts          AS CHAR NO-UNDO.
   DEF VAR lcFLATContracts           AS CHAR NO-UNDO.
   DEF VAR lcCONTSContracts          AS CHAR NO-UNDO.
   DEF VAR lcCONTSFContracts         AS CHAR NO-UNDO.
   DEF VAR lcBundleCLITypes          AS CHAR NO-UNDO.
   DEF VAR lcDSS2PrimarySubsType     AS CHAR NO-UNDO.
   DEF VAR lcDSS4PrimarySubsType     AS CHAR NO-UNDO. 
   DEF VAR lcAllowedDSS2SubsType     AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS4SubsType     AS CHAR NO-UNDO.
   DEF VAR lcDSSRelatedSubsType      AS CHAR NO-UNDO. 
   DEF VAR liDSSMsSeq                AS INT  NO-UNDO.
   DEF VAR liRequest                 AS INT  NO-UNDO.
   DEF VAR lcError                   AS CHAR NO-UNDO.
   DEF VAR ldCurrTS                  AS DEC  NO-UNDO.
   DEF VAR ldeLastDayofMonthStamp    AS DEC  NO-UNDO.
   DEF VAR lcPostpaidDataBundles     AS CHAR NO-UNDO.
   DEF VAR lcALLPostpaidUPSELLBundles AS CHAR NO-UNDO.
   DEF VAR ldaPrepResetDate          AS DATE NO-UNDO.
   DEF VAR liCurrDSSMsSeq            AS INT  NO-UNDO.
   DEF VAR ldeDSSLimit               AS DEC  NO-UNDO.
   DEF VAR lcDSSBundleId             AS CHAR NO-UNDO.
   DEF VAR lcDSSId                   AS CHAR NO-UNDO. 
   DEF VAR ldeNextMonthStamp         AS DEC  NO-UNDO.
   DEF VAR ldaCont15PromoFrom        AS DATE NO-UNDO. 
   DEF VAR ldaCont15PromoEnd         AS DATE NO-UNDO. 
   DEF VAR ldaOrderDate              AS DATE NO-UNDO. 

   DEF BUFFER bMsRequest FOR MsRequest.
   DEF BUFFER bCLIType   FOR CLIType.

   /* check that subrequests really are ok */
   IF fGetSubRequestState(MsRequest.MsRequest) NE 2 THEN DO:
      IF MsRequest.ReqStat NE 7 THEN DO:
         /* back to waiting mode */
         fReqStatus(7,"").
      END.   
      RETURN "ERROR:Subrequests not ok".
   END.
   
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".
      
   Func.Common:mSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).
    
   ASSIGN liPeriod = YEAR(ldtActDate) * 100 + MONTH(ldtActDate)
          ldeLastDayofMonthStamp = Func.Common:mMake2DT(Func.Common:mLastDayOfMonth(ldtActDate),86399)
          ldeNextMonthStamp      = Func.Common:mMake2DT((Func.Common:mLastDayOfMonth(ldtActDate) + 1),0).

   FIND FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE
              MsOwner.MsSeq  = MsRequest.MsSeq AND
              MsOwner.TsEnd >= 99999999 NO-ERROR.
   IF NOT AVAILABLE MsOwner THEN 
   FIND FIRST MsOwner NO-LOCK WHERE
              MsOwner.MsSeq  = MsRequest.MsSeq NO-ERROR.
      
   IF NOT AVAILABLE MsOwner THEN DO:
      fReqError("Subscription not found").
      RETURN.
   END.

   FIND Customer WHERE Customer.CustNum = MsOwner.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   FIND FIRST DayCampaign WHERE 
              DayCampaign.Brand   = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = MsRequest.ReqCParam3 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DayCampaign THEN DO:
      fReqError("Periodical contract is not valid").
      RETURN.
   END.

   /* day campaign id */
   ASSIGN lcDCEvent             = MsRequest.ReqCParam3
          lcSMSText             = MsRequest.SMSText
          lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
          lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE")
          lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE")
          lcDSS4PrimarySubsType = fCParamC("DSS4_PRIMARY_SUBS_TYPE").

   IF DayCampaign.DCType = {&DCTYPE_SERVICE_PACKAGE} OR
      DayCampaign.DCType = {&DCTYPE_BUNDLE} THEN
      ASSIGN lcIPLContracts        = fCParamC("IPL_CONTRACTS")
             lcCONTDContracts      = fCParamC("CONTD_CONTRACTS")
             lcFLATContracts       = fCParamC("FLAT_CONTRACTS")
             lcCONTSContracts      = fCParamC("CONTS_CONTRACTS")
             lcCONTSFContracts     = fCParamC("CONTSF_CONTRACTS")
             lcBundleCLITypes      = fCParamC("BUNDLE_BASED_CLITYPES")
             lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS").
   ELSE IF DayCampaign.DCType = {&DCTYPE_POOL_RATING} THEN
      lcALLPostpaidUPSELLBundles = fCParamC("POSTPAID_DATA_UPSELLS").

   /* Welcome SMS for FLAT Tariffs */
   IF LOOKUP(lcDCEvent,lcFLATContracts) > 0 AND
      MsRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_CREATION}
   THEN DO:
      lcSMSText = "WelcomeCONTF".

      FOR EACH  ServiceLimit NO-LOCK WHERE
                ServiceLimit.GroupCode = lcDCEvent AND
                ServiceLimit.DialType  = {&DIAL_TYPE_VOICE},
          FIRST MServiceLimit NO-LOCK WHERE
                MServiceLimit.MsSeq = MsRequest.MsSeq AND
                MServiceLimit.DialType = ServiceLimit.DialType AND
                MServiceLimit.SlSeq   = ServiceLimit.SlSeq AND
                MServiceLimit.FromTS <= MsRequest.ActStamp AND
                MServiceLimit.EndTS  >= MsRequest.ActStamp:
         ASSIGN ldeFirstMonthLimit = MServiceLimit.InclAmt
                ldeOtherMonthLimit = ServiceLimit.InclAmt.
      END. /* FOR EACH ServiceLimit NO-LOCK WHERE */

      FOR EACH FixedFee NO-LOCK USE-INDEX HostTable WHERE
               FixedFee.Brand     = Syst.Var:gcBrand AND
               FixedFee.HostTable = "MobSub" AND
               FixedFee.KeyValue  = STRING(MsRequest.MsSeq) AND
               FixedFee.FeeModel  = DayCampaign.FeeModel AND
               FixedFee.CalcObj   = DayCampaign.DCEvent AND
               FixedFee.InUse     = TRUE AND
               FixedFee.BegDate  <= ldtActDate,
          FIRST FFItem NO-LOCK WHERE
                FFItem.FFNum = FixedFee.FFNum AND
                FFItem.BillPeriod = liPeriod:
          ASSIGN ldeFirstMonthFee = FFItem.Amt
                 ldeOtherMonthFee = FixedFee.Amt.
      END. /* FOR EACH FixedFee NO-LOCK USE-INDEX HostTable WHERE */
   END. /* IF LOOKUP(lcDCEvent,lcFLATContracts) > 0 AND */

   IF LOOKUP(lcDCEvent,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 THEN DO:
      CASE iiRequestType:
         WHEN 8 THEN ASSIGN lcSMSText = lcDCEvent + "RenewalOK"
                            ldaPrepResetDate = ADD-INTERVAL(ldtActDate,1,"months").
         WHEN 9 THEN ASSIGN lcSMSText = lcDCEvent + "RenewalNOK" 
                            WHEN MsRequest.ReqSource = {&REQUEST_SOURCE_SCRIPT}.
      END CASE.
   END.

   IF lcSMSText > "" THEN DO:

      lcSMSText = fGetSMSTxt(lcSMSText,
                             TODAY,
                             Customer.Language,
                             OUTPUT ldReqStamp).
      
      CASE lcDCEvent:
           WHEN "MDUB" OR WHEN "PMDUB" THEN ASSIGN
              lcSMSText = REPLACE(lcSMSText,"#BUNDLE", "8")
              lcSender = {&BONO8_SMS_SENDER}.
           WHEN "MDUB2" THEN ASSIGN
              lcSMSText = REPLACE(lcSMSText,"#BUNDLE", "15")
              lcSender = "22644".
           WHEN "MDUB3" THEN ASSIGN
              lcSMSText = REPLACE(lcSMSText,"#BUNDLE", "25")
              lcSender = "22644".
           WHEN "MDUB4" THEN ASSIGN
              lcSMSText = REPLACE(lcSMSText,"#BUNDLE", "35")
              lcSender = "22644".
           WHEN "MDUB5" THEN ASSIGN
              lcSMSText = REPLACE(lcSMSText,"#BUNDLE", "12")
              lcSender = "22644".
           WHEN "TARJ7" OR WHEN "TARJ9" 
                        OR WHEN "TARJ10" 
                        OR WHEN "TARJ11" 
                        OR WHEN "TARJ12" 
                        OR WHEN "TARJ13" THEN
              ASSIGN lcSMSText = REPLACE(lcSMSText,"#DATE",
                                         STRING(DAY(ldaPrepResetDate)) + "/" +
                                         STRING(MONTH(ldaPrepResetDate)))
                                         WHEN iiRequestType = 8
                     lcSender  = "22622".
      END. /* CASE lcDCEvent: */

      IF LOOKUP(lcDCEvent,lcFLATContracts) > 0 THEN
         ASSIGN
            lcSMSText = REPLACE(lcSMSText,"#XXX",STRING(ldeFirstMonthLimit))
            lcSMSText = REPLACE(lcSMSText,"#YYY",STRING(ldeFirstMonthFee))
            lcSMSText = REPLACE(lcSMSText,"#ZZZ",STRING(ldeOtherMonthLimit))
            lcSMSText = REPLACE(lcSMSText,"#WWW",STRING(ldeOtherMonthFee))
            lcSender = "622".
   END. /* IF lcSMSText > "" THEN */

   /* send SMS */
   IF lcSMSText > "" THEN DO:

      /* YBM-117 */
      IF INDEX(MsRequest.SMSText,"upsell") > 0 THEN
         lcSender = {&UPSELL_SMS_SENDER}.
        
      IF lcSMSText > "" THEN DO:                    
         ASSIGN lcSMSText = REPLACE(lcSMSText,"#MSISDN",MsRequest.CLI)
                lcSMSText = REPLACE(lcSMSText,"#PCTYPE",MsRequest.ReqCParam3).

         /* both to agreement cust and user */
         fMakeSchedSMS2(MsOwner.CustNum,
                       MsRequest.CLI,
                       10,
                       lcSMSText,
                       ldReqStamp,
                       lcSender,
                       "").
      END. 
   END.

   /* Create Re-rate request for DSS activation */
   IF LOOKUP(MsRequest.ReqCparam3,{&DSS_BUNDLES}) > 0 AND
      iiRequestType = 8 THEN DO:
   
      IF MsRequest.ReqIParam4 NE 1 THEN DO:
         FIND CURRENT MsRequest EXCLUSIVE-LOCK.
         MsRequest.ReqIParam4 = 1.
      END.
      
      Func.Common:mSplitTS(MsRequest.UpdateStamp,
               OUTPUT ldtDoneDate,
               OUTPUT liDoneTime).

      /* If call is made on last day before 1 hours then create
         re-reate request for last second of current day */
      IF DAY(ldtDoneDate) = DAY(Func.Common:mLastDayOfMonth(ldtDoneDate)) AND
         liDoneTime <= (86399 - 3600) THEN DO:
         IF 86399 < (liDoneTime + 14400) THEN
            ldeRerateActStamp = Func.Common:mMake2DT(ldtDoneDate,86399).
         ELSE ldeRerateActStamp = Func.Common:mSecOffSet(MsRequest.UpdateStamp,14400).
      END. /* IF DAY(ldtDoneDate) = DAY(Func.Common:mLastDayOfMonth(ldtDoneDate)) */
      /* If call is made before last day then create re-reate
         request with 4 hours delay */
      ELSE IF DAY(ldtDoneDate) < DAY(Func.Common:mLastDayOfMonth(ldtDoneDate)) THEN
         ldeRerateActStamp = Func.Common:mSecOffSet(MsRequest.UpdateStamp,14400).
   END. /* IF MsRequest.ReqCparam3 = {&DSS} THEN DO: */

   ELSE IF MsRequest.ReqIParam4 = 1 THEN DO:
      Func.Common:mSplitTS(MsRequest.ActStamp,
               OUTPUT ldtDoneDate,
               OUTPUT liDoneTime).
      ldeRerateActStamp = Func.Common:mMakeTS().
   END.
      
   /* Prevent creating extra re-rate requests if contract */
   /* is activating or terminating from STC/BTC request   */
   IF MsRequest.ReqIParam4 = 1 AND ldeRerateActStamp > 0 AND
      MsRequest.ReqSource <> {&REQUEST_SOURCE_BTC} AND
      MsRequest.ReqSource <> {&REQUEST_SOURCE_STC} THEN
      RUN pReRate(MsRequest.MsSeq,
                  MsRequest.CustNum,
                  ldtDoneDate,
                  ldeRerateActStamp).

   /* Update TariffBundle to MobSub table */
   IF LOOKUP(MsRequest.ReqCparam3,lcIPLContracts)   > 0 OR
      LOOKUP(MsRequest.ReqCparam3,lcCONTDContracts) > 0 OR
      LOOKUP(MsRequest.ReqCparam3,lcFLATContracts)  > 0 OR
      LOOKUP(MsRequest.ReqCparam3,lcCONTSContracts) > 0 OR
      LOOKUP(MsRequest.ReqCparam3,lcCONTSFContracts) > 0 THEN DO:

      IF (iiRequestType = 8 AND LOOKUP(MsOwner.CLIType,lcBundleCLITypes) > 0) OR
         (iiRequestType = 9 AND LOOKUP(MsOwner.CLIType,lcBundleCLITypes) = 0)
      THEN DO:

         /* Mark current tariff bundle to MobSub */
         FIND FIRST MobSub WHERE
                    MobSub.MsSeq = MsOwner.MsSeq EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL MobSub THEN DO:
            IF iiRequestType = 8 THEN
               MobSub.TariffBundle = MsRequest.ReqCparam3.
            ELSE
               MobSub.TariffBundle = "".
            FIND CURRENT Mobsub NO-LOCK NO-ERROR.
         END.

         /* Mark current tariff bundle to MsOwner */
         IF iiRequestType = 8 AND
            MsRequest.ReqSource <> {&REQUEST_SOURCE_BTC} AND
            MsRequest.ReqSource <> {&REQUEST_SOURCE_STC} AND
            MsRequest.ReqSource <> {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION} 
            THEN DO:
            /* YTS-8096: prevent wrong tariffbundle saving in case
               there is STC for the same day than Reactivation */
            FIND CURRENT MsOwner EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL MsOwner THEN DO:
               IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMsOwner).
               MsOwner.TariffBundle = MsRequest.ReqCparam3.
               IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMsOwner).
            END.
            FIND CURRENT MsOwner NO-LOCK NO-ERROR.
         END.
      END.
   END.

   /* request handled succesfully */   
   fReqStatus(2,""). 

   /* Send the SMS using Request Action Rules */
   RUN Mm/requestaction_sms.p(INPUT MsRequest.MsRequest,
                           INPUT MsOwner.CliType,
                           INPUT MsRequest.ReqSource).

   /* terminate BB service package etc. */
   RUN Mm/requestaction_exec.p(MsRequest.MsRequest,
                            MsOwner.CLIType,        /* CLI Type */
                            0,                      /* order    */
                            MsRequest.ActStamp,
                            Func.Common:mSecOffSet(MsRequest.ActStamp,-1),
                            NO,                     /* create fees */
                            MsRequest.ReqSource,    /* req.source  */
                            {&REQUEST_ACTIONLIST_ALL}).

   IF iiRequestType = 8 THEN DO:

      FIND FIRST bCLIType NO-LOCK WHERE
                 bCLIType.Brand   EQ Syst.Var:gcBrand AND
                 bCLIType.CLIType EQ MsOwner.CLIType  NO-ERROR.

      IF AVAIL bCLIType                                       AND
         bCLIType.BaseBundle EQ MsRequest.ReqCparam3          AND
         (LOOKUP(MsOwner.CLIType,lcAllowedDSS2SubsType) > 0 OR
          LOOKUP(MsOwner.CLIType,lcAllowedDSS4SubsType) > 0)  THEN DO:

         ldCurrTS = (IF MsRequest.ActStamp > Func.Common:mMakeTS() THEN
                        MsRequest.ActStamp
                     ELSE Func.Common:mMakeTS()).

         fUpdateDSSAccount(MsOwner.MsSeq,
                           MsRequest.ReqSource,
                           MsRequest.MsRequest,
                           ldCurrTS,
                           "CREATE").
      END.

   END. /* IF iiRequestType = 8 THEN DO: */

   IF MsRequest.ReqType = 8 AND
      LOOKUP(DayCampaign.DCType,"1,4,6") > 0 AND
      MONTH(ldtActDate) <= MONTH(TODAY) THEN DO:
      
      /* YDR-2109 Creating or Updating Fraud Limit Counter for 
         New Bundles, upsells and roaming upsells */
      RUN pFraudCounterLimit (MsOwner.MsSeq,
                             DayCampaign.DCEvent,
                             MsOwner.CustNum).
      
      RUN pUpdateTMCounterLimit(
         MsOwner.MSSeq,
         (IF DayCampaign.DCEvent EQ "UPGRADE_UPSELL" THEN
            MsRequest.ReqCParam5 ELSE DayCampaign.DcEvent)).
   END.

   IF (LOOKUP(DayCampaign.DCEvent,lcPostpaidDataBundles) > 0 OR
       LOOKUP(DayCampaign.DCEvent,lcALLPostpaidUPSELLBundles) > 0 OR
       DayCampaign.DCEvent BEGINS "DSS") AND
      MONTH(ldtActDate) <= MONTH(TODAY) THEN
      RUN pUpdateDSSTMCounterLimit(MsOwner.CustNum).
   
   /* YOT-3647 */
   IF iiRequestType EQ 8 AND
       CAN-FIND(FIRST CLIType NO-LOCK WHERE
                      CLIType.Brand = Syst.Var:gcBrand AND
                      CLIType.CLIType = lcDCEvent AND
                      CLIType.LineType = {&CLITYPE_LINETYPE_MAIN}) THEN DO:

      FOR EACH bMSRequest NO-LOCK WHERE
               bMSRequest.Brand = Syst.Var:gcBrand AND
               bMSRequest.ReqType = 18 AND
               bMSRequest.Custnum = MsOwner.Custnum AND
               bMSRequest.ActStamp > MsRequest.ActStamp AND
               bMSRequest.ReqStatus = 0 AND
               bMSRequest.ReqCParam3 = 
                 STRING({&SUBSCRIPTION_TERM_REASON_ADDITIONALSIM}):
            
         FIND FIRST MSRequest NO-LOCK WHERE 
                    MSRequest.MSRequest = bMSRequest.MSRequest NO-ERROR.
         
         fReqStatus(4,"Cancelled due to main line activation").
         
         /* restore buffer reference */
         FIND FIRST MSRequest NO-LOCK WHERE 
                    MSRequest.MSrequest = iiRequest NO-ERROR.
      END.
            
   END.
   
   /* When STCed between CONT15 and Convergent with CONT15 base bundle */
   IF (lcDCEvent EQ "CONT15" OR LOOKUP(lcDCEvent,{&YOIGO_CONVERGENT_BASE_BUNDLES_LIST}) > 0) AND 
      MsRequest.ReqType    EQ 8     AND
      MsRequest.ReqCParam2 EQ "act" THEN 
   DO:
      IF NOT CAN-FIND(FIRST CliType WHERE CLIType.Brand      = Syst.Var:gcBrand         AND 
                                          CliType.CliType    = MsOwner.CliType AND 
                                          CliType.BaseBundle = "CONT15"        NO-LOCK) THEN
          LEAVE.
      ELSE IF NOT CAN-FIND(FIRST MsRequest WHERE MsRequest.MsSeq     = MsOwner.MsSeq                  AND 
                                                 MsRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_CREATE} AND 
                                                 MsRequest.ReqStatus > 0                              USE-INDEX MsSeq NO-LOCK) THEN
          LEAVE. 
      ELSE IF CAN-FIND(FIRST MsRequest WHERE MsRequest.MsSeq      = MsOwner.MsSeq                             AND
                                             MsRequest.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION}            AND
                                             LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0 AND 
                                             MsRequest.ReqCParam3 = "VOICE100" USE-INDEX MsSeq NO-LOCK)       THEN      
          LEAVE.
      ELSE IF fIsProSubscription(MsOwner.MsSeq) THEN 
          LEAVE.    
      ELSE 
      DO:
          FIND FIRST ServiceLimit WHERE ServiceLimit.GroupCode = "VOICE100" NO-LOCK NO-ERROR.
          IF AVAIL ServiceLimit AND CAN-FIND(FIRST MServiceLimit WHERE MServiceLimit.MsSeq    = MsOwner.MsSeq         AND 
                                                                       MServiceLimit.DialType = ServiceLimit.Dialtype AND 
                                                                       MServiceLimit.SLSeq    = ServiceLimit.SLSeq    AND 
                                                                       MServiceLimit.EndTS   >= MsRequest.ActStamp    NO-LOCK) THEN 
              LEAVE.
      END.

      ASSIGN
         ldaCont15PromoFrom = fCParamDa("CONT15PromoFromDate")
         ldaCont15PromoEnd  = fCParamDa("CONT15PromoEndDate")
         ldaOrderDate = ?.
      
      IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_CREATION} THEN
         FOR FIRST Order NO-LOCK WHERE
                   Order.MsSeq = MsRequest.MsSeq AND
                   Order.OrderType < 2:
            Func.Common:mTS2Date(Order.CrStamp, OUTPUT ldaOrderDate).
         END.
      ELSE IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_STC} AND
              MsRequest.OrigRequest > 0 THEN
         FOR FIRST bMSRequest NO-LOCK WHERE
                   bMSRequest.MsRequest = MsRequest.OrigRequest AND
                   bMSRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}:
         
            IF bMSRequest.ReqIParam2 > 0 THEN
               FIND FIRST Order NO-LOCK WHERE
                          Order.Brand = Syst.Var:gcBrand AND
                          Order.OrderId = bMSRequest.ReqIParam2 NO-ERROR.
            ELSE RELEASE Order.

            IF AVAIL Order THEN 
               Func.Common:mTS2Date(Order.CrStamp, OUTPUT ldaOrderDate).
            ELSE Func.Common:mTS2Date(bMsRequest.CreStamp, OUTPUT ldaOrderDate).
         END.

      IF ldaOrderDate NE ? AND
         ldaCont15PromoFrom NE ? AND
         ldaCont15PromoEnd NE ? AND
         ldaOrderDate >= ldaCont15PromoFrom AND
         ldaOrderDate <= ldaCont15PromoEnd AND
         /* Convergent+CONT15 has VOICE200 instead of VOICE100 after 5.6.2017 */
        (MsOwner.CLIType EQ "CONT15" OR ldaOrderDate < 6/5/2017) THEN DO:

         liRequest = fPCActionRequest(MsRequest.MsSeq,
                                      "VOICE100",
                                      "act",
                                      MsRequest.ActStamp,
                                      TRUE, /* fees */
                                      {&REQUEST_SOURCE_CONTRACT_ACTIVATION},
                                      "",
                                      MsRequest.MsRequest,
                                      FALSE,
                                      "",
                                      0,
                                      0,
                                      "",
                                      OUTPUT lcError).
         IF liRequest = 0 THEN
            /* write possible error to a memo */
            Func.Common:mWriteMemo("MobSub",
                             STRING(MsOwner.MsSeq),
                             MsOwner.Custnum,
                             "VOICE100 activation failed",
                             lcError).
      END.
   END.

   RETURN "".
   
END PROCEDURE.  /* Finalize */


/*  terminate a periodical contract */
PROCEDURE pContractTermination:

   DEF VAR lcTerminationType       AS CHAR NO-UNDO.
   DEF VAR lcDCEvent               AS CHAR NO-UNDO.
   DEF VAR ldtCreDate              AS DATE NO-UNDO.
   DEF VAR liEndPeriod             AS INT  NO-UNDO.
   DEF VAR ldCoefficient           AS DECI NO-UNDO.
   DEF VAR ldEndStamp              AS DEC  NO-UNDO.
   DEF VAR lcTermFeeCalc           AS CHAR NO-UNDO.
   DEF VAR ldtOrigValidFrom        AS DATE NO-UNDO.
   DEF VAR ldtOrigRenewalDate      AS DATE NO-UNDO.
   DEF VAR ldtOrigValidTo          AS DATE NO-UNDO.
   DEF VAR ldtOrigValidToOrig      AS DATE NO-UNDO INIT ?.
   DEF VAR ldPrice                 AS DEC  NO-UNDO INIT ?.
   DEF VAR llCreatePenaltyFee      AS LOG  NO-UNDO INIT FALSE.
   DEF VAR liNewReqStatus AS INT  INITIAL {&REQUEST_STATUS_SUB_REQUEST_DONE} NO-UNDO.
   DEF VAR ldNewEndStamp           AS DEC  NO-UNDO.
   DEF VAR liSLCount               AS INT  NO-UNDO.
   DEF VAR ldeInclAmt              AS DEC  NO-UNDO.
   DEF VAR liCheckMsSeq            AS INT  NO-UNDO.
   DEF VAR llRelativeLast          AS LOG  NO-UNDO.
   DEF VAR ldaFromDate             AS DATE NO-UNDO.
   DEF VAR ldaLastDay              AS DATE NO-UNDO. 
   DEF VAR llPostpaidBundleTerm    AS LOG  NO-UNDO.
   DEF VAR llRerate                AS LOG  NO-UNDO INIT FALSE. 
   DEF VAR lcHandled               AS CHAR NO-UNDO. 
   DEF VAR llKeepDSSActive         AS LOG  NO-UNDO.
   DEF VAR lcFatGroups             AS CHAR NO-UNDO. 
   DEF VAR i                       AS INT  NO-UNDO. 
   DEF VAR ldeNextMonthStamp       AS DEC  NO-UNDO.
   DEF VAR ldeLastDayofMonthStamp  AS DEC  NO-UNDO.
   DEF VAR liCurrentServiceClass   AS INT  NO-UNDO. 
   DEF VAR lcError                 AS CHAR NO-UNDO. 
   DEF VAR liRequest               AS INT  NO-UNDO.  
   DEF VAR ldContractEndDate       AS DATE NO-UNDO.
   DEF VAR lcIPhoneDiscountRuleIds AS CHAR NO-UNDO.
   DEF VAR lcIPhoneDiscountRuleId  AS CHAR NO-UNDO.
   DEF VAR liContractID            AS INT  NO-UNDO.
   DEF VAR liFFNum                 AS INT  NO-UNDO. 
   DEF VAR lcFeeSourceTable        AS CHAR NO-UNDO. 
   DEF VAR lcFeeSourceKey          AS CHAR NO-UNDO.
   DEF VAR lcBundleId              AS CHAR NO-UNDO.
   DEF VAR lcCLIType               AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType   AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS4SubsType   AS CHAR NO-UNDO.
   DEF VAR lcDSS4PrimarySubsType   AS CHAR NO-UNDO.
   DEF VAR lcDSS2PrimarySubsType   AS CHAR NO-UNDO. 
   DEF VAR lcPostpaidDataBundles   AS CHAR NO-UNDO.
   DEF VAR ldeNextDayStamp         AS DEC  NO-UNDO.
   DEF VAR llChargeUsageBased      AS LOG  NO-UNDO.
   DEF VAR liSlSeq                 AS INT  NO-UNDO.
   DEF VAR ldeConsumption          AS DEC  NO-UNDO.
   DEF VAR lcAdjustConsProfile     AS CHAR NO-UNDO.
   DEF VAR liCustNum               AS INT  NO-UNDO.
   DEF VAR llgResult               AS LOG  NO-UNDO.
   DEF VAR llCancelOrder           AS LOG  NO-UNDO. 
   DEF VAR llCancelInstallment     AS LOG  NO-UNDO. 
   DEF VAR liOrderId               AS INT  NO-UNDO. 
   DEF VAR liCnt                   AS INT  NO-UNDO.
   DEF VAR liEndPeriodPostpone     AS INT  NO-UNDO.
   DEF VAR ldtActDatePostpone      AS DATE NO-UNDO.
   DEF VAR llActiveInstallment     AS LOG  NO-UNDO.   
   DEF VAR lcDSSRelatedSubsType    AS CHAR NO-UNDO.
   DEF VAR llFMFee                 AS LOG  NO-UNDO. 
   DEF VAR liDSSMsSeq              AS INT  NO-UNDO. 
   DEF VAR ldaMonth22              AS DATE NO-UNDO.
   DEFINE VARIABLE llSAPC AS LOGICAL INITIAL FALSE NO-UNDO.
   DEFINE VARIABLE loProCommand AS CLASS Gwy.SAPC.ProCommandNBCH NO-UNDO.

   DEF BUFFER bLimit           FOR MServiceLimit.
   DEF BUFFER bMsRequest       FOR MsRequest.
   DEF BUFFER bMsOwner         FOR MsOwner.
   DEF BUFFER bServiceLCounter FOR ServiceLCounter.
   DEF BUFFER bServiceLimit    FOR ServiceLimit.
   DEF BUFFER bMServiceLimit   FOR MServiceLimit.
   DEF BUFFER bMServiceLPool   FOR MServiceLPool.
   DEF BUFFER bDCCLI           FOR DCCLI.

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   /* Note: Make sure always find latest msowner record  */
   /* If STC is happened then we need New MsOwner record */
   FIND FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE
              MsOwner.MsSeq  = MsRequest.MsSeq AND
              MsOwner.TsEnd >= 99999999 NO-ERROR.
   IF NOT AVAILABLE MsOwner THEN 
      FIND FIRST MsOwner NO-LOCK WHERE
                 MsOwner.MsSeq  = MsRequest.MsSeq NO-ERROR.

   IF NOT AVAILABLE MsOwner THEN DO:
      fReqError("Subscription not found").
      RETURN.
   END.

   FIND Customer WHERE Customer.CustNum = MsOwner.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   Func.Common:mSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).

   ASSIGN
      ldeLastDayofMonthStamp = Func.Common:mMake2DT(Func.Common:mLastDayOfMonth(ldtActDate),86399)
      ldeNextMonthStamp      = Func.Common:mMake2DT((Func.Common:mLastDayOfMonth(ldtActDate) + 1),0)
      ldeNextDayStamp        = Func.Common:mMake2DT((ldtActDate + 1),0)
      liEndPeriod            = YEAR(ldtActDate) * 100 + MONTH(ldtActDate).

   IF MsRequest.ReqCParam2 = "recreate" THEN ASSIGN
      ldtActDate = ldtActDate - 1
      liActTime  = 86399.
   
   ASSIGN
      lcDCEvent = MsRequest.ReqCParam3
      lcTerminationType = MsRequest.ReqCParam2
      liCustNum = MsRequest.CustNum.
   
   FIND FIRST DayCampaign WHERE 
              DayCampaign.Brand   = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = lcDCEvent NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DayCampaign THEN DO:
      fReqError("Unknown periodical contract").
      RETURN.
   END.

   IF DayCampaign.DCType = {&DCTYPE_SERVICE_PACKAGE} OR
      DayCampaign.DCType = {&DCTYPE_BUNDLE} THEN
      lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS").
      
   /* Note: MsSeq could be different for DSS because any */
   /* subs. can request to terminate DSS bundle          */
   IF LOOKUP(lcDCEvent,{&DSS_BUNDLES}) > 0 AND
      fGetDSSMsSeqLimit(INPUT  MsRequest.CustNum,
                        INPUT  MsRequest.ActStamp,
                        OUTPUT liDSSMsSeq,
                        OUTPUT ldeDSSTotalLimit,
                        OUTPUT lcBundleId) THEN
      liCheckMsSeq = liDSSMsSeq.
   ELSE
      liCheckMsSeq = MsRequest.MsSeq.

   /* Check postpaid bundle is being terminated except DSS */
   /* Don't set the flag if DSS is already terminated or   */
   /* scheduled to deactivate end of month                 */
   IF LOOKUP(lcDCEvent,lcPostpaidDataBundles) > 0 AND
      /* check if dss/dss2/dss4 was active before bundle termination */
      fGetDSSMsSeqLimitTerm(INPUT  MsRequest.CustNum,
                            INPUT  ldeNextDayStamp,
                            INPUT  MsRequest.ActStamp,
                            OUTPUT liDSSMsSeq,
                            OUTPUT ldeDSSTotalLimit,
                            OUTPUT lcBundleId) THEN DO:

      IF (lcBundleID EQ {&DSS4}  OR
          lcBundleId EQ {&DSS2}) THEN DO:        

         ASSIGN lcAllowedDSS4SubsType = fCParamC("DSS4_SUBS_TYPE")
                lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE")
                lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE")
                lcDSS4PrimarySubsType = fCParamC("DSS4_PRIMARY_SUBS_TYPE")
                llPostpaidBundleTerm  = TRUE.
 
         FIND FIRST bMsOwner NO-LOCK USE-INDEX MsSeq WHERE
                    bMsOwner.MsSeq EQ MsRequest.MsSeq    AND
                    bMsOwner.TsEnd EQ MsRequest.ActStamp NO-ERROR.

         IF AVAILABLE bMsOwner THEN
            lcCLIType = bMsOwner.CLIType.
         ELSE
            lcCLIType = MsOwner.CLIType.

         IF (LOOKUP(lcCLIType,lcAllowedDSS4SubsType) GT 0  OR
             LOOKUP(lcCLIType,lcAllowedDSS2SubsType) GT 0) THEN DO:

            IF lcBundleId EQ {&DSS4} THEN
               lcDSSRelatedSubsType = lcDSS4PrimarySubsType.
            ELSE IF lcBundleId EQ {&DSS2} THEN
               lcDSSRelatedSubsType = lcDSS2PrimarySubsType.

            /* Check STC Request with DSS2 data bundle */
            FIND FIRST bMsRequest NO-LOCK USE-INDEX Custnum WHERE
                       bMsRequest.Brand    EQ Syst.Var:gcBrand                    AND
                       bMsRequest.ReqType  EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                       bMsRequest.Custnum  EQ MsOwner.Custnum                     AND
                       bMsRequest.ActStamp EQ ldeNextDayStamp                     AND 
         LOOKUP(STRING(bMsRequest.ReqStat),"4,9,99,3") EQ 0                       AND
               (LOOKUP(bMsRequest.ReqCparam2,lcDSSRelatedSubsType) GT 0 OR
                LOOKUP(bMsRequest.ReqCparam5,lcDSSRelatedSubsType) GT 0)          NO-ERROR.

            IF AVAIL bMsRequest THEN llKeepDSSActive = TRUE.

            /* Check BTC Request with DSS2 data bundle */
            IF NOT llKeepDSSActive THEN DO:
               FIND FIRST bMsRequest NO-LOCK USE-INDEX Custnum WHERE
                          bMsRequest.Brand    EQ Syst.Var:gcBrand          AND
                          bMsRequest.ReqType  EQ {&REQTYPE_BUNDLE_CHANGE}  AND
                          bMsRequest.Custnum  EQ MsOwner.Custnum           AND
                          bMsRequest.ActStamp EQ ldeNextDayStamp           AND
            LOOKUP(STRING(bMsRequest.ReqStat),"4,9,99,3") EQ 0             AND
                   LOOKUP(bMsRequest.ReqCparam2,lcDSSRelatedSubsType) GT 0 NO-ERROR.

               IF AVAIL bMsRequest THEN llKeepDSSActive = TRUE.
            END.
         END.
      END.
      ELSE IF lcBundleId EQ {&DSS} THEN DO:

         llPostpaidBundleTerm = TRUE.

         IF fBundleWithSTCCustomer(INPUT MsRequest.Custnum,
                                   INPUT ldeNextDayStamp) THEN
            llKeepDSSActive = TRUE.
      END.
   END.
      
   IF lcDCEvent EQ {&PMDUB} AND
      MsOwner.CLIType EQ "TARJ5" AND
      MsRequest.ReqSource NE {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION}
      THEN DO:
      
      RUN Gwy/air_get_account_details.p(MsOwner.CLI, 
                                    OUTPUT liCurrentServiceClass,
                                    OUTPUT lcError).
   
      IF lcError BEGINS "ERROR" THEN DO:
         fReqError(lcError).
         RETURN.
      END.

      IF liCurrentServiceClass EQ {&SC_TARJ5_PROMOTIONAL_BONO} THEN DO:

         RUN Gwy/air_update_serviceclass.p(MsOwner.CLI,
                                       {&SC_TARJ5_NORMAL},
                                       {&SC_TARJ5_PROMOTIONAL},
                                       ?, 
                                       OUTPUT lcError).
         IF lcError BEGINS "ERROR" THEN DO:
            fReqError(lcError).
            RETURN.
         END.
      END.
   END.

   /* terminate contract (service package) */
   IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 THEN DO:

      ASSIGN
         ldEndStamp = MsRequest.ActStamp
         liReqCnt   = 0
         liSLCount = 0
         lcHandled = ""
         ldeInclAmt = 0
         liSlSeq    = 0.
      
      FOR EACH ServiceLimit NO-LOCK WHERE
               ServiceLimit.GroupCode = lcDCEvent,
          EACH MServiceLimit EXCLUSIVE-LOCK WHERE
               MServiceLimit.MsSeq    = liCheckMsSeq AND
               MServiceLimit.DialType = ServiceLimit.DialType AND
               MServiceLimit.SLSeq    = ServiceLimit.SLSeq AND
               MServiceLimit.EndTS   >= ldEndStamp:

         IF LOOKUP(STRING(RECID(MServiceLimit)),lcHandled) > 0 THEN NEXT. 
             
         ASSIGN 
           ldNewEndStamp = ldEndStamp
           llRelativeLast = FALSE.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMServiceLimit).
         
         /* last month should have a relative limit */
         IF ServiceLimit.LastMonthCalc = 1 AND 
            LOOKUP(MsRequest.ReqCParam2,"recreate,canc") = 0
         THEN DO:
            Func.Common:mSplitTS(MServiceLimit.FromTS,
                     OUTPUT ldaFromDate,
                     OUTPUT liReqCnt).
   
            /* end this to the end of previous month and
               create a new one for this last month */
            IF TRUNCATE(MServiceLimit.EndTS / 100,0) NE 
               TRUNCATE(MServiceLimit.FromTS / 100,0) AND
               TRUNCATE(MServiceLimit.FromTS / 100,0) NE
               TRUNCATE(ldNewEndStamp / 100,0) THEN DO:
                  IF MServiceLimit.FromTS < ldNewEndStamp THEN ASSIGN 
                     llRelativeLast = TRUE
                     ldNewEndStamp = Func.Common:mMake2DT(DATE(MONTH(ldtActDate),1,
                                                   YEAR(ldtActDate)) - 1,
                                              86399).
                  ELSE ldNewEndStamp = Func.Common:mMake2DT(DATE(MONTH(ldaFromDate),1,
                                                   YEAR(ldaFromDate)) - 1,
                                                86399).
             END.                 
             /* last month = first month, or service limit begins on last 
                -> new limit for this one */
             ELSE DO:
                IF MONTH(ldtActDate) = 12 THEN liReqCnt = 31.
                ELSE ASSIGN 
                   ldaLastDay = 
                      DATE(MONTH(ldtActDate) + 1,1,YEAR(ldtActDate)) - 1
                   liReqCnt = DAY(ldaLastDay).   
                
                MServiceLimit.InclAmt = INT(ServiceLimit.InclAmt * 
                               (ldtActDate - ldaFromDate + 1) / liReqCnt).
                llRerate = TRUE.               
             END.
         END.
         
         IF MServiceLimit.EndTS > ldNewEndStamp THEN DO WHILE TRUE:
            IF NOT CAN-FIND(FIRST bLimit WHERE
               bLimit.MsSeq    = MServiceLimit.MsSeq AND
               bLimit.DialType = MServiceLimit.DialType AND
               bLimit.SlSeq    = MServiceLimit.SlSeq AND
               bLimit.EndTS    = ldNewEndStamp)
            THEN LEAVE.
            ldNewEndStamp = Func.Common:mSecOffSet(ldNewEndStamp,-1). 
         END.

         IF LOOKUP(lcDCEvent,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 THEN DO:
            FOR FIRST bServiceLimit WHERE
                      bServiceLimit.GroupCode = "TARJ7_UPSELL",
                FIRST bMServiceLimit EXCLUSIVE-LOCK WHERE
                      bMServiceLimit.MsSeq    = liCheckMsSeq           AND
                      bMServiceLimit.DialType = bServiceLimit.DialType AND
                      bMServiceLimit.SLSeq    = bServiceLimit.SLSeq    AND
                      bMServiceLimit.EndTS   >= ldNewEndStamp:
               
               IF llDoEvent THEN DO:
                  RUN StarEventInitialize((BUFFER bMServiceLimit:HANDLE)).
                  RUN StarEventSetOldBuffer((BUFFER bMServiceLimit:HANDLE)).
               END.
               
               bMServiceLimit.EndTs = ldNewEndStamp.
               
               IF llDoEvent THEN DO:
                  RUN StarEventMakeModifyEvent((BUFFER bMServiceLimit:HANDLE)).
               END.

               FIND FIRST bMServiceLPool EXCLUSIVE-LOCK WHERE
                          bMserviceLPool.MsSeq   = bMServiceLimit.MsSeq AND
                          bMserviceLPool.SLSeq   = bMServiceLimit.SLSeq AND
                          bMserviceLPool.EndTS  >= ldNewEndStamp        AND
                          bMserviceLPool.FromTS <= ldNewEndStamp        NO-ERROR.

               IF NOT AVAILABLE bMServiceLPool THEN NEXT.

               IF llDoEvent THEN DO:
                  RUN StarEventInitialize((BUFFER bMServiceLPool:HANDLE)).
                  RUN StarEventSetOldBuffer((BUFFER bMServiceLPool:HANDLE)).
               END.

               bMserviceLPool.EndTs = ldNewEndStamp.

               IF llDoEvent THEN DO:
                  RUN StarEventMakeModifyEvent((BUFFER bMServiceLPool:HANDLE)).
               END. 
            END.
         END.
         
         ASSIGN 
            MServiceLimit.EndTS = ldNewEndStamp
            liSLCount           = liSLCount + 1
            lcHandled = lcHandled + STRING(RECID(MServiceLimit)) + ",".
  
         IF MServiceLimit.FromTS < ldNewEndStamp AND
            ServiceLimit.DialType = {&DIAL_TYPE_GPRS} THEN 
            ASSIGN ldeInclAmt = ldeInclAmt + MServiceLimit.InclAmt
                   liSlSeq = ServiceLimit.SLSeq.
             
         IF lldoevent THEN RUN StarEventMakeModifyEvent (lhMServiceLimit).

         /* a new limit to last month */
         IF llRelativeLast THEN DO:
            CREATE bLimit.
            BUFFER-COPY MServiceLimit EXCEPT EndTS MSID TO bLimit.
            ASSIGN
               bLimit.MSID   = NEXT-VALUE(mServiceLimit)
               bLimit.FromTS = Func.Common:mSecOffSet(ldNewEndStamp,1)
               ldNewEndStamp = ldEndStamp.

            DO WHILE TRUE:
               IF NOT CAN-FIND(FIRST bLimit WHERE
                  bLimit.MsSeq    = MServiceLimit.MsSeq AND
                  bLimit.DialType = MServiceLimit.DialType AND
                  bLimit.SlSeq    = MServiceLimit.SlSeq AND
                  bLimit.EndTS    = ldNewEndStamp)
               THEN LEAVE.
               ldNewEndStamp = Func.Common:mSecOffSet(ldNewEndStamp,1). 
            END.
   
            bLimit.EndTS  = ldNewEndStamp.
            IF MONTH(ldtActDate) = 12 THEN liReqCnt = 31.
            ELSE ASSIGN 
               ldaLastDay = DATE(MONTH(ldtActDate) + 1,1,YEAR(ldtActDate)) - 1
               liReqCnt = DAY(ldaLastDay).   
               
            ASSIGN 
               lcHandled = lcHandled + STRING(RECID(bLimit)) + ","
               bLimit.InclAmt = INT(ServiceLimit.InclAmt * 
                                         DAY(ldtActDate) / liReqCnt)
               llRerate = TRUE.                          

            fMakeCreateEvent((BUFFER bLimit:HANDLE),
                             "",
                             Syst.Var:katun,
                             "").

            fUpdateServicelCounterMSID(bLimit.CustNum,
                                       bLimit.MSSeq,
                                       bLimit.SlSeq,
                                       INTEGER(TRUNCATE(bLimit.FromTS / 100,0)),
                                       MServiceLimit.MSID,
                                       bLimit.MSID).
         END.
      END.

      /* service limits were changed */
      IF llRerate THEN DO:
         FIND CURRENT MsRequest EXCLUSIVE-LOCK.
         MsRequest.ReqIParam4 = 1.
      END.
      
      IF liSLCount = 0 THEN DO:
         FIND CURRENT MsRequest EXCLUSIVE-LOCK.
         MsRequest.Memo = MsRequest.Memo + 
                         (IF MsRequest.Memo > ""
                          THEN ", " 
                          ELSE "") +
                         "Note: No active contract was found".
         fReqStatus(2,"").
         RETURN.                
      END.
      ELSE IF llPostpaidBundleTerm AND ldeInclAmt > 0 THEN
         /* Update DSS Limit to reduce postpaid bundle(s) HSDPA limit */
         RUN pUpdateDSSLimit(INPUT MsRequest.CustNum,
                             INPUT "REMOVE",
                             INPUT ldeInclAmt,
                             INPUT 0,
                             INPUT MsRequest.ActStamp,
                             OUTPUT ldeDSSTotalLimit).

   END.
   
   /* terminate contract (others) */
   ELSE DO:

      /* current contract */
      FIND FIRST DCCLI WHERE
                 DCCLI.Brand         = Syst.Var:gcBrand              AND
                 DCCLI.DCEvent       = lcDCEvent            AND
                 DCCLI.MsSeq         = MsRequest.MsSeq      AND
                (IF DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT} THEN
                    DCCLI.PerContractID = MsRequest.ReqIParam3 
                 ELSE TRUE)                                 AND  
                 DCCLI.ValidTo      >= ldtActDate           AND
                 DCCLI.ValidFrom    <= DCCLI.ValidTo
      NO-LOCK NO-ERROR.
                 
      IF NOT AVAILABLE DCCLI THEN DO:
         
         IF MsRequest.ReqCParam2 = "recreate" THEN RETURN "".

         fReqError("No active contract was found").
         RETURN.
      END.
      
      /* Count the days the contract would have been active */
      ASSIGN 
         ldtOrigValidFrom   = DCCLI.ValidFrom
         ldtOrigRenewalDate = DCCLI.RenewalDate
         ldtOrigValidTo     = DCCLI.ValidTo
         ldtOrigValidToOrig = DCCLI.ValidToOrig
         liContractID       = DCCLI.PerContractID.
            
      IF DCCLI.TermDate NE ? THEN DO:
         fReqError("Contract has already been terminated").
         RETURN.
      END.
      
      Func.Common:mSplitTS(MsRequest.CreStamp,
               OUTPUT ldtCreDate,
               OUTPUT liActTime).


      /* cancellation must be done max. 14 days after activation */
      IF MsRequest.ReqCParam2 = "canc" AND 
         DayCampaign.DCType NE {&DCTYPE_INSTALLMENT} AND
         ldtCreDate > DCCLI.ContractDate + 14 
      THEN DO:
         FIND CURRENT DCCLI EXCLUSIVE-LOCK.
         IF lldoevent THEN RUN StarEventSetOldBuffer(lhDCCLI).
         DCCLI.TermDate = ?. 
         IF lldoevent THEN RUN StarEventMakeModifyEvent (lhDCCLI).
         fReqError("Over 14 days has already passed from activation").
         RETURN.
      END. 
      
      FIND CURRENT DCCLI EXCLUSIVE-LOCK.
      IF lldoevent THEN RUN StarEventSetOldBuffer(lhDCCLI).
      ASSIGN DCCLI.TermDate = ldtActDate
             DCCLI.ValidTo  = ldtActDate. /* in cancellation termination 
                                             date is not end of period */
      IF lldoevent THEN RUN StarEventMakeModifyEvent (lhDCCLI).

      FOR EACH DCCounter EXCLUSIVE-LOCK WHERE
               DCCounter.MsSeq   = MsRequest.MsSeq AND
               DCCounter.DCDate  > DCCLI.ValidTo   AND
               DCCounter.DCEvent = lcDCEvent: 
         DELETE DCCounter.
      END.

      /* in cancellation and immediate termin. all fatimes can be deleted */
      IF LOOKUP(MsRequest.ReqCParam2,"canc,iterm") > 0 
      THEN liEndPeriod = 0.
      ELSE liEndPeriod = YEAR(ldtActDate) * 100 + MONTH(ldtActDate).
      
      /* remove unused fatimes */
      FOR EACH FATime EXCLUSIVE-LOCK USE-INDEX MobSub WHERE
               FATime.Brand  = Syst.Var:gcBrand         AND
               FATime.MsSeq  = MsRequest.MsSeq AND
               FATime.Period > liEndPeriod     AND
               FATime.FtGrp  = DCCLI.DCEvent   AND
               FATime.InvNum = 0:
               
         IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFatime).

         DELETE FATime.      
      END.

      IF DayCampaign.BundleTarget EQ {&DC_BUNDLE_TARGET_SVA} THEN
      DO: 
          FIND FIRST DiscountPlan WHERE DiscountPlan.Brand    = Syst.Var:gcBrand AND 
                                        DiscountPlan.DPRuleID = DayCampaign.DcEvent + "DISC" NO-LOCK NO-ERROR.
          IF AVAIL DiscountPlan THEN                                    
              fCloseDiscount(DiscountPlan.DPRuleID,
                             MsRequest.MsSeq,
                             ldtActDate,
                             NO).
      END.

      /* Close iphone discounts */
      IF DCCLI.DCEvent BEGINS "PAYTERM" THEN DO:
         lcIPhoneDiscountRuleIds = fCParamC("IPHONE_DISCOUNT").
         IF DAY(ldtOrigValidFrom) <> 1 THEN
            ldContractEndDate = fcontract_end_date(DCCLI.DCEvent,
                                ldtOrigValidFrom - DAY(ldtOrigValidFrom) + 1).
         ELSE
            ldContractEndDate = fcontract_end_date(DCCLI.DCEvent,ldtOrigValidFrom).

         DO i = 1 to NUM-ENTRIES(lcIPhoneDiscountRuleIds):
            lcIPhoneDiscountRuleId = ENTRY(i,lcIPhoneDiscountRuleIds).
            FOR FIRST DiscountPlan WHERE
                      DiscountPlan.Brand = Syst.Var:gcBrand AND
                      DiscountPlan.DPRuleID = lcIPhoneDiscountRuleId NO-LOCK,
                EACH  DPMember WHERE
                      DPMember.DPId       = DiscountPlan.DPId AND
                      DPMember.HostTable  = "MobSub" AND
                      DPMember.KeyValue   = STRING(MsRequest.MsSeq) AND
                      DPMember.ValidTo   >= ldtActDate AND
                      DPMember.ValidTo   >= DPMember.ValidFrom NO-LOCK:

                IF DPMember.ValidTo >= ldContractEndDate AND
                   ldContractEndDate <= ldtActDate THEN NEXT.

                fCloseDPMember(DPMember.DPMemberID,
                               DPMember.ValidFrom - 1).

            END. /* FOR FIRST DiscountPlan WHERE */
         END. /* DO i = 1 to NUM-ENTRIES(lcIPhoneDiscountRuleIds): */
      END. /* IF lcDCEvent BEGINS "PAYTERM" THEN DO: */
   END.            
 
   /* fee for termination (penalty fee) */
   IF MsRequest.CreateFees AND DayCampaign.TermFeeModel > "" AND
      DayCampaign.TermFeeCalc > 0
   THEN DO:
      
      IF DayCampaign.DCType EQ "5" THEN DO: 
         ldPrice = fAmtUnBilledFFItem(MsRequest.MsSeq,
                                      MsOwner.CustNum,
                                      ldtActDate,
                                      lcDCEvent,
                                      DCCLI.PerContractID,    
                                      OUTPUT liFFNum,
                                      OUTPUT liOrderId).
         IF ldPrice > 0 THEN ASSIGN
            llCreatePenaltyFee = TRUE
            lcFeeSourceTable   = "FixedFee" WHEN liFFNum > 0
            lcFeeSourceKey     = STRING(liFFNum) WHEN liFFNum > 0
            lcTermFeeCalc      = "¤" + "DC" + STRING(DCCLI.PerContractID).
      END.
      ELSE IF DayCampaign.DCType = "3" THEN DO:

         ldCoefficient = fCalculateFactor(ldtOrigValidFrom,
                                          ldtOrigRenewalDate,
                                          ldtOrigValidTo,
                                          ldtOrigValidToOrig,
                                          ldtActDate,
                                          DayCampaign.TermFeeCalc).

         lcTermFeeCalc = "¤¤¤" + STRING(ldCoefficient).
         IF ldCoefficient > 0 THEN llCreatePenaltyFee = TRUE.
         ldPrice = DCCLI.Amount.

         /* YPR-2515 */
         IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_RENEWAL} THEN DO:

            llActiveInstallment = FALSE.

            FOR EACH bDCCLI NO-LOCK WHERE
                     bDCCLI.MsSeq = MsRequest.MsSeq AND
                     bDCCLI.DCEvent BEGINS "PAYTERM" AND
                     bDCCLI.ValidFrom < ldtActDate:

               IF bDCCLI.TermDate NE ? THEN NEXT.

               /* filter out expired installments */
               IF bDCCLI.ValidTo < DATE(MONTH(ldtActDate + 1), 1, 
                                        YEAR(ldtActDate + 1)) THEN NEXT.

               llActiveInstallment = TRUE.
                                        
               ldaMonth22 = ADD-INTERVAL(bDCCLI.ValidFrom, 22, "months").
               ldaMonth22 = DATE(MONTH(ldaMonth22),1,YEAR(ldaMonth22)).

               IF ldtActDate + 1 >= ldaMonth22 THEN DO:
                  llCreatePenaltyFee = FALSE.
                  LEAVE.
               END.
            END.

            IF llCreatePenaltyFee AND NOT llActiveInstallment THEN DO:

               ldaMonth22  = ADD-INTERVAL(ldtOrigValidFrom, 22, "months").
               ldaMonth22  = DATE(MONTH(ldaMonth22),1,YEAR(ldaMonth22)).

               IF ldtActDate >= ldaMonth22 THEN
                  llCreatePenaltyFee = FALSE.
            END.

         END. /* IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_RENEWAL} THEN DO: */

      END.
      ELSE ASSIGN 
         llCreatePenaltyFee = TRUE
         ldPrice = ?.

      IF llCreatePenaltyFee THEN DO:

         RUN Mc/creasfee.p (MsOwner.CustNum,
                         MsRequest.MsSeq,
                         ldtActDate,
                         "FeeModel",
                         DayCampaign.TermFeeModel,
                         9,
                         ldPrice,
                         DayCampaign.DCEvent + " terminated " + 
                              STRING(ldtActDate,"99.99.9999") +
                              lcTermFeeCalc,
                         FALSE,              /* no messages to screen */
                         MsRequest.UserCode,
                         IF MsRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION} AND
                            (DayCampaign.DCType = {&DCTYPE_DISCOUNT} OR
                             DayCampaign.DCType = {&DCTYPE_INSTALLMENT})
                         THEN "ContractTermination¤Postpone"
                         ELSE "ContractTermination",
                         liOrderId, /* order id */
                         lcFeeSourceTable,
                         lcFeeSourceKey,
                         OUTPUT lcReqChar).

         IF lcReqChar BEGINS "ERROR:" OR lcReqChar BEGINS "0" THEN DO:
            fReqLog("Term. fee creation (" + DayCampaign.TermFeeModel + 
                    ") failed for contract: " + lcReqChar).
         END.
      END.
   END.
      
   llCancelInstallment = (lcTerminationType EQ "canc" AND
                          DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT}).
   llCancelOrder =
      (MsRequest.ReqSource = {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER} AND
       DCCLI.ValidTo < DCCLI.ValidFrom)
       OR
      (MsRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION}
       AND
       MsRequest.OrigRequest > 0 AND
       CAN-FIND(FIRST bMsRequest NO-LOCK WHERE
                      bMsRequest.MsRequest = MsRequest.Origrequest AND
                      bMsRequest.ReqType = 18 AND
               LOOKUP(bMsRequest.ReqCParam3,SUBST("&1,&2,&3",
               {&SUBSCRIPTION_TERM_REASON_POS_ORDER_CANCELATION},
               {&SUBSCRIPTION_TERM_REASON_DIRECT_ORDER_CANCELATION},
               {&SUBSCRIPTION_TERM_REASON_ORDER_CANCELLATION})) > 0)).
          
   /* close fees associated with this contract */  
   FOR EACH FixedFee NO-LOCK USE-INDEX HostTable WHERE
            FixedFee.Brand     = Syst.Var:gcBrand   AND 
            FixedFee.HostTable = "MobSub"  AND
            FixedFee.KeyValue  = STRING(liCheckMsSeq) AND
            FixedFee.CalcObj   = lcDCEvent,
      FIRST FMItem NO-LOCK WHERE
            FMItem.Brand     = Syst.Var:gcBrand AND
            FMItem.FeeModel  = FixedFee.FeeModel AND
            FMItem.FromDate <= FixedFee.BegDate AND
            FMItem.ToDate   >= FixedFee.BegDate:
      
      IF DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT} AND
         FixedFee.SourceKey NE STRING(DCCLI.PerContractID) THEN NEXT.   

      ASSIGN
         llChargeUsageBased = FALSE
         llFMFee = FALSE.

      /* If last month is usage based and bundle termination is   */
      /* not full month then fee will be calculated based on usage */
      /* Subscription is not part of DSS */
      IF MsRequest.ActStamp < ldeLastDayofMonthStamp AND
         llPostpaidBundleTerm = FALSE AND
         LOOKUP(lcDCEvent,lcPostpaidDataBundles) > 0 THEN DO:
         IF FMItem.BrokenRental = 0 THEN llChargeUsageBased = FALSE.
         ELSE IF FMItem.BrokenRental = 2 THEN llChargeUsageBased = TRUE.
         ELSE IF MsRequest.OrigRequest > 0 THEN DO:
            FIND FIRST bMsRequest WHERE
                       bMsRequest.MsRequest = MsRequest.OrigRequest
                 NO-LOCK NO-ERROR.
            IF AVAIL bMsRequest AND               
                bMsRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} THEN
               llChargeUsageBased = TRUE.
         END.
      END.

      /* YDR-1818 */
      IF MsRequest.ReqSource  = {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION} AND
         ( DayCampaign.DCType = {&DCTYPE_SERVICE_PACKAGE} OR
           DayCampaign.DCType = {&DCTYPE_BUNDLE})                          AND
         ( DayCampaign.BundleTarget  NE {&DB_BUNDLE_TARGET_TARIFF} )           THEN DO:
      
         FIND FIRST MobSub NO-LOCK WHERE
                    MobSub.MsSeq = MsRequest.MsSeq NO-ERROR.
         IF AVAILABLE MobSub THEN
            llFMFee = (MobSub.ActivationDate >= 5/6/2015).
         ELSE DO:
            FIND FIRST TermMobSub NO-LOCK WHERE
                       TermMobSub.MsSeq = MsRequest.MsSeq NO-ERROR.
            IF AVAILABLE TermMobSub THEN
               llFMFee = (TermMobSub.ActivationDate >= 5/6/2015).
         END.
      END.

      RUN Mc/closefee.p(FixedFee.FFNum,
                     ldtActDate,
                     FALSE, /* credit billed fees */
                     TRUE,
                     liCheckMsSeq,
                     IF llChargeUsageBased
                        THEN lcDCEvent
                        ELSE "", /* Data bundle id */
                     MsRequest.UserCode, /* eventlog.usercode */
                     "ContractTermination", /* eventlog.memo */
                     MsRequest.MsRequest,
                     llFMFee,
                     OUTPUT ldReqAmt).

      IF RETURN-VALUE > "" THEN NEXT.
      
      /* Write memo */
      Func.Common:mWriteMemo("FixedFee",
                 STRING(FixedFee.FFNum),
                 FixedFee.CustNum,
                 "Closed",
                 "Periodical contract " + lcDCEvent + " closed").
   
      /* Delete commission fee if the installment contract is closed
         due to revert renewal order or order cancellation */
      IF llCancelOrder OR llCancelInstallment OR 
         MsRequest.ReqSource EQ {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE} THEN
      FOR FIRST SingleFee USE-INDEX Custnum WHERE
                SingleFee.Brand       = Syst.Var:gcBrand AND
                SingleFee.Custnum     = MsOwner.CustNum AND
                SingleFee.HostTable   = "Mobsub" AND
                SingleFee.KeyValue    = STRING(MsOwner.MsSeq) AND
                SingleFee.SourceTable = "FixedFee" AND
                SingleFee.SourceKey   = STRING(FixedFee.FFNum) AND
                SingleFee.BillCode BEGINS "PAYTERMCG" EXCLUSIVE-LOCK:

         IF SingleFee.Billed AND
            CAN-FIND(FIRST Invoice NO-LOCK WHERE
                           Invoice.Invnum = SingleFee.Invnum AND
                           Invoice.InvType = 1) THEN NEXT.

         IF llDoEvent THEN
            RUN StarEventMakeDeleteEventWithMemo(
                (lhSingleFee),
                 MsRequest.UserCode,
                 "OrderCancellation").
         DELETE SingleFee.
      END.

      IF llCancelInstallment AND
         Fixedfee.Custnum EQ MsOwner.Custnum THEN
         RUN pCreditInstallment(FixedFee.FFNum,
                                TRUE,
                                MsRequest.MsRequest).
   END.
      
   IF DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT} AND
      DayCampaign.DCEvent BEGINS "PAYTERM" THEN
      FOR FIRST SingleFee USE-INDEX Custnum WHERE
               SingleFee.Brand       = Syst.Var:gcBrand AND
               SingleFee.Custnum     = MsOwner.CustNum AND
               SingleFee.HostTable   = "Mobsub" AND
               SingleFee.KeyValue    = STRING(MsOwner.MsSeq) AND
               SingleFee.SourceTable = "DCCLI" AND
               SingleFee.SourceKey   = STRING(DCCLI.PerContractID) AND
               SingleFee.CalcObj     = "RVTERM" EXCLUSIVE-LOCK:

         IF SingleFee.Billed AND
            CAN-FIND(FIRST Invoice NO-LOCK WHERE
                           Invoice.Invnum = SingleFee.Invnum AND
                           Invoice.InvType = 1) THEN NEXT.

         /* If Installment contract is closed from revert renewal order or 
            from order cancellation then delete single fee
            otherwise update the billing period */
         IF llCancelOrder OR llCancelInstallment OR
            MsRequest.ReqSource EQ {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE}
            THEN DO:
            
            IF llDoEvent THEN
               RUN StarEventMakeDeleteEventWithMemo(
                   (lhSingleFee),
                    MsRequest.UserCode,
                    "ContractTermination").
            DELETE SingleFee.
         END.
         ELSE IF SingleFee.BillPeriod > liEndPeriod THEN DO:
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSingleFee).

            /* YDR-1584 */
            IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION} THEN
               ASSIGN liEndPeriodPostpone = fPer2PerAdd(liEndPeriod,1)
                      ldtActDatePostpone  = ADD-INTERVAL(ldtActDate,1,"months").
            ELSE 
               ASSIGN liEndPeriodPostpone = liEndPeriod
                      ldtActDatePostpone  = ldtActDate.

            ASSIGN SingleFee.BillPeriod  = liEndPeriodPostpone
                   SingleFee.Concerns[1] = YEAR(ldtActDatePostpone) * 10000 + 
                                           MONTH(ldtActDatePostpone) * 100  +
                                           DAY(ldtActDatePostpone).
            IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSingleFee).
         END.
      END.

   /* Close promotion fat */
   CASE lcDCEvent:
      WHEN "MDUB" THEN
         fCloseFat("BONO8CPFREE",
                   MsRequest.MsSeq,
                   (YEAR(ldtActDate) * 100 + MONTH(ldtActDate))).

      /* YDA-171 */
      WHEN "BONO_VOIP" THEN DO:
         lcFatGroups = "BONOVOIPCPACT,BONOVOCPIPL,BONOVOCPBONO".
         DO i = 1 TO NUM-ENTRIES(lcFatGroups):
            fCloseFat(ENTRY(i,lcFatGroups),
                      MsRequest.MsSeq,
                      (YEAR(ldtActDate) * 100 + MONTH(ldtActDate))).
         END. /* DO i = 1 TO NUM-ENTRIES(lcFatGroups): */
      END. /* WHEN "BONO_VOIP" THEN DO: */

      WHEN "CONTS30" THEN DO:
         fCloseDiscount("CONTS30DISC",
                        MsRequest.MsSeq,
                        ldtActDate,
                        NO).
      END.
   END CASE. /* CASE lcDCEvent: */

   IF AVAILABLE DCCLI THEN RELEASE DCCLI.

   /* No need to send termination command for Service Package if bundle   */
   /* is being terminated along with subscription because prodigy will    */
   /* send a command to EMA to de-provision the SHAPER automatically when */
   /* prodigy receives 'DELETE' HLR command from TMS.                     */
   IF MsRequest.ReqSource <> {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION} AND
      LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 AND
      NOT lcDCEvent BEGINS {&DSS} THEN DO:

      IF llSAPC
      THEN DO:
         /* Subscription termination request is handling the
            provision when ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION} */
         IF DayCampaign.EMACode > "" AND
            MsRequest.ReqSource NE {&REQUEST_SOURCE_SUBSCRIPTION_TERMINATION} AND
            fTerminationProvisionNeeded(MsRequest.OrigRequest, MsRequest.MsRequest)
         THEN DO ON ERROR UNDO, THROW:
            loProCommand = NEW Gwy.SAPC.ProCommandNBCH(MsRequest.MsRequest). 
         
            loProCommand:mStoreProCommand().
   
            liNewReqStatus = {&REQUEST_STATUS_HLR_PENDING}.
   
            CATCH loError AS Progress.Lang.Error:
               fReqErrorObject(loError).
               RETURN.
            END CATCH.
   
            FINALLY:
               IF VALID-OBJECT(loProCommand)
               THEN DELETE OBJECT loProCommand.
            END FINALLY.
         END.
      END.
      ELSE RUN pTerminateServicePackage(lcDCEvent,
                                        MsOwner.MsSeq,
                                        MsOwner.CLIType,
                                        ldtActDate,
                                        OUTPUT liNewReqStatus).

      /* Deactivate Bono6 */
      IF lcDCEvent EQ "DATA6" THEN DO: 
         DO liCnt = 1 TO NUM-ENTRIES({&BONO6DISCOUNTS}):
            FIND FIRST DiscountPlan WHERE 
                       DiscountPlan.Brand      = Syst.Var:gcBrand     AND 
                       DiscountPlan.DPRuleId   = ENTRY(liCnt, 
                                                       {&BONO6DISCOUNTS}) AND 
                       DiscountPlan.ValidTo   >= TODAY       NO-LOCK NO-ERROR.
                          
            IF AVAILABLE DiscountPlan THEN 
               llgResult = fCloseDiscount(DiscountPlan.DPRuleId,
                                          MsRequest.MsSeq,
                                          ldtActDate,
                                          NO).
         END.
      END.
          
      /* iSTC - Reduce bundle consumption to network for non-DSS */
      IF llSAPC = FALSE                              AND
         LOOKUP(lcDCEvent,lcPostpaidDataBundles) > 0 AND
         MsRequest.ActStamp < ldeLastDayofMonthStamp AND
         llPostpaidBundleTerm = FALSE AND
         MsRequest.OrigRequest > 0 THEN DO:
         FIND FIRST bMsRequest NO-LOCK WHERE
                    bMsRequest.MsRequest = MsRequest.OrigRequest NO-ERROR.
         IF AVAIL bMsRequest AND
            bMsRequest.ActStamp < ldeNextMonthStamp AND
            (bMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR
             bMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE}) THEN DO:

            FIND FIRST bServiceLCounter NO-LOCK WHERE
                       bServiceLCounter.MsSeq  = MsRequest.MsSeq AND
                       bServiceLCounter.Period = liEndPeriod AND
                       bServiceLCounter.SlSeq  = liSlSeq NO-ERROR.
            IF AVAIL bServiceLCounter AND bServiceLCounter.Amt > 0 THEN DO:
               ldeConsumption = (bServiceLCounter.Amt / 1024 / 1024).

               IF ldeConsumption > ldeInclAmt THEN
                  ldeConsumption = ldeInclAmt * 1024 * 1024.
               ELSE
                  ldeConsumption = bServiceLCounter.Amt.

               lcAdjustConsProfile = STRING(ldeConsumption) +
                                     ",GRACE=0" +
                                     ",TEMPLATE=HSPA".

               liRequest = fServiceRequest(
                             bMsRequest.MsSeq,
                             "SHAPER",
                             1, /* on */
                             lcAdjustConsProfile,
                             ldeNextDayStamp,
                             "", /* salesman */
                             FALSE, /* fees */
                             FALSE, /* sms */
                             "", /* creator */
                             (IF bMsRequest.ReqType EQ
                              {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} THEN
                              {&REQUEST_SOURCE_STC} ELSE
                              {&REQUEST_SOURCE_BTC}),
                             bMsRequest.MsRequest, /* father request */
                             FALSE, /* mandatory for father request */
                             OUTPUT lcError).
               IF liRequest = 0 THEN
                  Func.Common:mWriteMemo("MobSub",
                                   STRING(bMsRequest.MsSeq),
                                   bMsRequest.Custnum,
                                   "Contract consumption adjustment failed;",
                                   lcError).
            END. /* IF AVAIL bServiceLCounter AND bServiceLCounter.Amt > 0 */
         END. /* IF AVAIL bMsRequest AND */
      END. /* IF MsRequest.OrigRequest > 0 AND */
   END. /* IF MsRequest.ReqSource */
     
   FIND CURRENT MsRequest EXCLUSIVE-LOCK.
   MsRequest.ReqDtParam1 = ldtActDate.

   IF MsRequest.ReqCParam2 = "recreate" AND
      liContractID > 0 THEN MsRequest.ReqIParam1 = liContractID.
   
   fReqStatus(liNewReqStatus,""). 

   /* Update DSS limit if bundle is being deactivated from DSS group     */
   /* Delete DSS group if last bundle is being terminated from DSS group */
   IF llPostpaidBundleTerm THEN DO:
      IF ldeDSSTotalLimit > 0 THEN DO:
         IF NOT fOngoingDSSTerm(MsRequest.CustNum,MsRequest.ActStamp) THEN
            RUN pUpdateDSSNetworkLimit(INPUT MsRequest.MsSeq,
                                       INPUT MsRequest.CustNum,
                                       INPUT ldeDSSTotalLimit,
                                       INPUT "LIMIT",
                                       INPUT FALSE,
                                       INPUT MsRequest.MsRequest,
                                       INPUT MsRequest.ActStamp,
                                       INPUT MsRequest.ReqSource,
                                       INPUT lcBundleId).
      END.
      /* Delete DSS group if last bundle is being terminated from DSS group */
      /* in this case ldeDSSTotalLimit limit will be zero except BTC/STC    */
      ELSE DO:
         IF NOT llKeepDSSActive AND
            NOT fOngoingDSSTerm(MsRequest.CustNum,ldeLastDayofMonthStamp) THEN
            RUN pUpdateDSSNetwork(INPUT MsRequest.MsSeq,
                                  INPUT MsRequest.CLI,
                                  INPUT MsRequest.CustNum,
                                  INPUT "DELETE",
                                  INPUT "",        /* Optional param list */
                                  INPUT MsRequest.MsRequest,
                                  INPUT Func.Common:mMakeTS(),
                                  INPUT {&REQUEST_SOURCE_CONTRACT_TERMINATION},
                                  INPUT lcBundleId).
      END. /* ELSE DO: */
   END. /* IF llPostpaidBundleTerm THEN DO: */

   /* If DSS bundle is terminated then handle all ongoing DSS requests */
   IF lcDCEvent BEGINS {&DSS} THEN DO:
      FIND FIRST bMsRequest WHERE
                 bMsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.

      FOR EACH MsRequest EXCLUSIVE-LOCK WHERE
               MsRequest.Brand   = Syst.Var:gcBrand         AND
               MsRequest.ReqType = {&REQTYPE_DSS}  AND
               MsRequest.CustNum = liCustNum:

         IF MsRequest.ReqIParam2 > 0 THEN NEXT.

         IF AVAILABLE bMsRequest AND
            MsRequest.ReqCParam3 = lcDCEvent AND
            MsRequest.CreStamp > bMsRequest.CreStamp THEN NEXT.

         IF AVAILABLE bMsRequest                          AND
            MsRequest.ReqStatus  EQ {&REQUEST_STATUS_NEW} AND
            MsRequest.ReqCParam3 NE bMsRequest.ReqCParam3 THEN NEXT.

         CASE MsRequest.ReqStatus:
            WHEN 0 THEN fReqStatus(4,"DSS is terminated.").
            WHEN 3 THEN fReqStatus(9,"DSS is terminated.").
         END. /* CASE MsRequest.ReqStatus: */
      END. /* FOR EACH MsRequest EXCLUSIVE-LOCK WHERE */
   END. /* IF lcDCEvent BEGINS {&DSS} THEN DO: */

   FIND FIRST MsRequest WHERE
              MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.

   RETURN "".
   
END PROCEDURE.  /* terminate */


/* change a contract */
PROCEDURE pMaintainContract:
 
   DEF VAR lhContract  AS HANDLE NO-UNDO.
   DEF VAR lhField     AS HANDLE NO-UNDO.
   DEF VAR lcFieldList AS CHAR   NO-UNDO.
   DEF VAR lcField     AS CHAR   NO-UNDO.
   DEF VAR llCanDo     AS LOG    NO-UNDO.

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   lhContract = BUFFER DCCLI:HANDLE.

   FIND FIRST DCCLI WHERE 
              DCCLI.Brand      = Syst.Var:gcBrand              AND
              DCCLI.DCEvent    = MsRequest.ReqCParam3 AND     
              DCCLI.MsSeq      = MsRequest.MsSeq      AND
              DCCLI.ValidTo   >= TODAY                AND
              DCCLI.ValidFrom <= TODAY EXCLUSIVE-LOCK NO-ERROR.
   
   IF NOT AVAILABLE DCCLI THEN DO:
      fReqError("No active contract was found").
      RETURN.
   END.
    
   IF MsRequest.ReqCParam1 = "" THEN DO:
      fReqError("Nothing to do").
      RETURN.
   END.

   /* get possible field names */
   DO liReqCnt = 1 TO lhContract:NUM-FIELDS:
      lhField = lhContract:BUFFER-FIELD(liReqCnt).
      lcFieldList = lcFieldList + (IF liReqCnt > 1 THEN "," ELSE "") + 
                    lhField:NAME.
   END.
    
   /* fields to be changed are in CParam1 and new values in CParam2,
      don't check anymore if new value actually differs from old value or not,
      request creator must control given values and their sanity 
   */
   DO liReqCnt = 1 TO NUM-ENTRIES(MsRequest.ReqCParam1,";"):
   
      ASSIGN 
         lcField   = ENTRY(liReqCnt,MsRequest.ReqCParam1,";")
         lcReqChar = ENTRY(liReqCnt,MsRequest.ReqCParam4,";")
         NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN DO:
         fReqError("Invalid new values").
         RETURN.
      END.

      IF LOOKUP(lcField,lcFieldList) = 0 THEN DO:
         fReqError("Unknown field name " + lcField).
         RETURN.
      END.
      
      ASSIGN
         lhField = lhContract:BUFFER-FIELD(lcField)
         llCanDo = TRUE.
      
      CASE lhField:DATA-TYPE:

      WHEN "character" THEN lhField:BUFFER-VALUE = lcReqChar.

      WHEN "logical" THEN DO:
         CASE lcReqChar:
         WHEN "yes" OR WHEN "true"  THEN lhField:BUFFER-VALUE = TRUE.
         WHEN "no"  OR WHEN "false" THEN lhField:BUFFER-VALUE = FALSE.
         OTHERWISE llCanDo = FALSE.
         END CASE.
      END.
      
      WHEN "integer" THEN DO:
         lhField:BUFFER-VALUE = INTEGER(lcReqChar) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN llCanDo = FALSE.
      END.
      
      WHEN "decimal" THEN DO:
         lhField:BUFFER-VALUE = DECIMAL(lcReqChar) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN llCanDo = FALSE.
      END.
      
      WHEN "date" THEN DO:
        
         /* store original ValidTo, when it's changed for the first time */
         IF lcField = "ValidTo" THEN DO:
            /* reset in case of STC extention */
            IF (MsRequest.ReqSource EQ {&REQUEST_SOURCE_STC} OR
                MsRequest.ReqSource EQ {&REQUEST_SOURCE_BTC}) THEN 
               DCCLI.ValidToOrig = ?. 
            ELSE IF DCCLI.ValidToOrig = ? THEN
               DCCLI.ValidToOrig = DCCLI.ValidTo.
         END.
         
         lhField:BUFFER-VALUE = DATE(lcReqChar) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN llCanDo = FALSE.

      END.
       
      OTHERWISE DO:
         fReqError("Cannot handle field type " + lhField:DATA-TYPE).
         RETURN.
      END.
      
      END CASE.
      
      IF NOT llCanDo THEN DO:
         fReqError("Invalid new value " + lhField:NAME + "/" + lcReqChar).
         RETURN.
      END.
          
   END.
 
   FIND DayCampaign OF DCCLI NO-LOCK. 

   /* fee for changing contract */
   IF MsRequest.CreateFees AND DayCampaign.ModifyFeeModel > "" THEN DO:

      Func.Common:mSplitTS(MsRequest.ActStamp,
               OUTPUT ldtActDate,
               OUTPUT liReqCnt).
               
      RUN Mc/creasfee.p (MsRequest.CustNum,
                    MsRequest.MsSeq,
                    ldtActDate,
                    "FeeModel",
                    DayCampaign.ModifyFeeModel,
                    9,
                    ?,
                    DCCLI.DCEvent + " modified " + 
                       STRING(ldtActDate,"99.99.9999"),  /* memo */
                    FALSE,              /* no messages to screen */
                    MsRequest.UserCode,
                    "ContractChange",
                    0,
                    "", /* fee source */
                    "", /* source key */
                    OUTPUT lcReqChar).

      IF lcReqChar BEGINS "ERROR:" OR lcReqChar BEGINS "0" THEN DO:
         fReqLog("Fee creation (" + DayCampaign.ModifyFeeModel + 
                 ") failed for modifying contract: " + lcReqChar).
      END.
      
   END.
    
   /* update pending requests */
   IF LOOKUP("CreateFees",MsRequest.ReqCParam1,";") > 0 THEN 
   FOR EACH bPendRequest EXCLUSIVE-LOCK WHERE
            bPendRequest.MsSeq      = DCCLI.MsSeq   AND
            bPendRequest.ReqType    = 9             AND
            bPendRequest.ReqCParam3 = DCCLI.DCEvent AND
            LOOKUP(STRING(bPendRequest.ReqStat),"0,3") > 0:

      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRequest).
      bPendRequest.CreateFees = DCCLI.CreateFees.       
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRequest).
   END.

   RELEASE DCCLI.
    
   DELETE OBJECT lhField.
   DELETE OBJECT lhContract.
   
   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.


/* send a letter with PIN for activating periodical contract */
PROCEDURE pPerContractPIN:

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   /* must be an agreement customer */
   IF Customer.CustNum NE Customer.AgrCust THEN DO:
      fReqError("Customer is not an agreement customer").
      RETURN.
   END. 

   Func.Common:mSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).

   /* generate PIN if empty */
   IF Customer.PContractPIN = "" THEN DO:
      FIND CURRENT Customer EXCLUSIVE-LOCK.
      Customer.PContractPIN = STRING(RANDOM(1,9999),"9999").
   END. 
   
   /* print a letter */
   RUN Mm/prinpcpin.p(MsRequest.CustNum,
                 MsRequest.MsRequest,
                 OUTPUT lcReqChar).

   IF lcReqChar > "" THEN DO:
      fReqLog("Per.contract PIN letter print failed: " + lcReqChar).
   END. 
   
   /* send SMS */
   IF MsRequest.SendSMS = 1 THEN DO:

      lcSMSText = fGetTxt("SMS",
                          "PerContractPIN",
                          TODAY,
                          Customer.Language).

      IF lcSMSText > "" THEN DO:                    
         /* don't send messages before 8 am. */
         ldReqStamp = Func.Common:mMakeOfficeTS().
         IF ldReqStamp = ? THEN ldReqStamp = Func.Common:mMakeTS().

         fMakeSchedSMS(Customer.CustNum,
                       Customer.SMSNumber,
                       17,
                       lcSMSText,
                       ldReqStamp).
      END. 
   END.

   RELEASE Customer.
 
   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.

PROCEDURE pReRate:

   DEF INPUT PARAMETER iiMsSeq     AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiInvCust   AS INT  NO-UNDO.
   DEF INPUT PARAMETER idtActDate  AS DATE NO-UNDO.
   DEF INPUT PARAMETER idtActStamp AS DEC  NO-UNDO.

   DEF VAR ldtFrom                 AS DATE NO-UNDO.
   DEF VAR ldtTo                   AS DATE NO-UNDO.
   DEF VAR lcResult                AS CHAR NO-UNDO.
   
   /* from the beginning of change month */
   ldtFrom = DATE(MONTH(idtActDate),1,YEAR(idtActDate)).
      
   /* to the end of it */
   ldtTo = IF MONTH(idtActDate) = 12
           THEN DATE(12,31,YEAR(idtActDate))
           ELSE DATE(MONTH(idtActDate) + 1,1,YEAR(idtActDate)) - 1.

   IF ldtTo > ldtFrom THEN      
      fRerateRequest(iiInvCust,
                     iiMsSeq,
                     ldtFrom,
                     ldtTo,
                     FALSE,   /* wait for other possible subrequests */
                     FALSE,                /* double check */
                     idtActStamp,          /* activate now */
                     "",                   /* creator */
                     MsRequest.ReqSource,  /* source */
                     MsRequest.MsRequest,
                     0,       /* not mandatory */
                     OUTPUT lcResult).
END PROCEDURE.

PROCEDURE pActivateServicePackage:

   DEF INPUT  PARAMETER icDCEvent         AS CHAR    NO-UNDO.
   DEF INPUT  PARAMETER iiMsSeq           AS INT     NO-UNDO.
   DEF INPUT  PARAMETER icCLIType         AS CHAR    NO-UNDO.
   DEF INPUT  PARAMETER idaActivationDate AS DATE    NO-UNDO.
   DEF OUTPUT PARAMETER oiNewReqStatus    AS INT     NO-UNDO.

   oiNewReqStatus = {&REQUEST_STATUS_SUB_REQUEST_DONE}.

   DEF VAR liService     AS INT  NO-UNDO.
   
   DEF BUFFER bMemoRequest FOR MsRequest.
   
   /* service packages that need to be activated */
   FOR EACH DCServicePackage NO-LOCK WHERE
            DCServicePackage.Brand     = Syst.Var:gcBrand AND
            DCServicePackage.DCEvent   = icDCEvent AND
            DCServicePackage.ToDate   >= idaActivationDate AND
            DCServicePackage.FromDate <= idaActivationDate TRANS:

      RUN pCopyPackage(icCLIType,
                       DCServicePackage.ServPac,
                       icDCEvent,
                       iiMSSeq,
                       idaActivationDate,
                       ?,      /* copy all, forced */
                       FALSE,  /* create fees */
                       TRUE,   /* solog (provisioning) */
                       MsRequest.MsRequest,
                       TRUE,   /* mandatory subrequest */
                       OUTPUT liService). 

      IF liService > 0 THEN oiNewReqStatus = {&REQUEST_STATUS_SUB_REQUEST_PENDING}.
      ELSE DO:   
         FIND FIRST bMemoRequest WHERE RECID(bMemoRequest) = RECID(MsRequest)
            EXCLUSIVE-LOCK.
         bMemoRequest.Memo = bMemoRequest.Memo + 
                             (IF bMemoRequest.Memo > "" THEN ", " ELSE "") +    
                             "No components activated for package " +
                             DCServicePackage.ServPac.
      END.
   END.

   RETURN "".
   
END PROCEDURE.

PROCEDURE pTerminateServicePackage:

   DEF INPUT  PARAMETER icDCEvent          AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER iiMsSeq            AS INT  NO-UNDO.
   DEF INPUT  PARAMETER icCurrentCLIType   AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER idaTerminationDate AS DATE NO-UNDO.
   DEF OUTPUT PARAMETER oiNewReqStatus     AS INT  NO-UNDO.
 
   DEF VAR liService             AS INT  NO-UNDO.
   DEF VAR lcBundles             AS CHAR NO-UNDO.
   DEF VAR lcOldCLIType          AS CHAR NO-UNDO.

   DEF VAR lcOnlyVoiceContracts                AS CHAR NO-UNDO.

   DEF BUFFER bPerContract FOR DayCampaign.
   DEF BUFFER bOrigRequest FOR MsRequest.
    
   ASSIGN 
       oiNewReqStatus       = {&REQUEST_STATUS_SUB_REQUEST_DONE}
       lcOnlyVoiceContracts = fCParamC("ONLY_VOICE_CONTRACTS").

   IF MsRequest.OrigRequest > 0 THEN 
      FIND FIRST bOrigRequest NO-LOCK WHERE
                 bOrigRequest.MsRequest = MsRequest.OrigRequest NO-ERROR.

   IF AVAIL bOrigRequest AND bOrigRequest.ReqType = 0 THEN
      lcOldCLIType = bOrigRequest.ReqCParam1.       
   ELSE lcOldCLIType = icCurrentCLIType.

   /* Don't send Shaper command if it is terminated from STC/BTC */
   IF icDCEvent = "BONO_VOIP" AND
      AVAIL bOrigRequest AND
      (bOrigRequest.ReqSource = {&REQUEST_SOURCE_STC} OR
       bOrigRequest.ReqSource = {&REQUEST_SOURCE_BTC} OR
       bOrigRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR
       bOrigRequest.ReqType   = {&REQTYPE_BUNDLE_CHANGE}) THEN RETURN "".

   /* No need to find retain bundle list for VOIP */
   IF icDCEvent <> "BONO_VOIP" THEN 
   DO:
      FIND FIRST bPerContract WHERE bPerContract.Brand = Syst.Var:gcBrand AND bPerContract.DCEvent = icDCEvent NO-LOCK NO-ERROR.
      IF AVAILABLE bPerContract AND LOOKUP(bPerContract.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 THEN 
      DO: 
         lcBundles = fGetActiveBundle(iiMsSeq,Func.Common:mSecOffSet(MsRequest.ActStamp,1)).
         lcBundles = REPLACE(lcBundles,"BONO_VOIP","").
      END.

      /* No need to terminate SHAPER and HSDPA if
         ongoing STC/BTC with data bundle */
      IF (lcBundles = "" OR LOOKUP(lcBundles,lcOnlyVoiceContracts) > 0) AND 
         fBundleWithSTC(iiMsSeq,Func.Common:mSecOffSet(MsRequest.ActStamp,1),FALSE) THEN 
         RETURN "".
   END.

   /* service packages that need to be deactivated */
   FOR EACH DCServicePackage NO-LOCK WHERE
            DCServicePackage.Brand     = Syst.Var:gcBrand AND
            DCServicePackage.DCEvent   = icDCEvent AND
            /* if some package has been ended on contract, it stays active
               on subscription also after this */
            DCServicePackage.ToDate   >= idaTerminationDate AND
            DCServicePackage.FromDate <= idaTerminationDate:

      RUN pTerminatePackage(icCurrentCLIType,
                            lcOldCLIType,
                            DCServicePackage.ServPac,
                            icDCEvent, /* Original Bundle */
                            lcBundles, /* Retain Bundle List */
                            iiMSSeq,
                            MsRequest.ActStamp,
                            FALSE,   /*  create fees */
                            TRUE,   /* solog (provisioning) */
                            MsRequest.MsRequest,
                            TRUE,
                            OUTPUT liService).

      IF liService > 0 THEN oiNewReqStatus = {&REQUEST_STATUS_SUB_REQUEST_PENDING}.

   END.

   RETURN "".
 
END PROCEDURE.


/* Reactivate a periodical contract */
PROCEDURE pContractReactivation:

   DEF VAR lcDCEvent     AS CHAR NO-UNDO.
   DEF VAR ldtFromDate   AS DATE NO-UNDO.
   DEF VAR ldtEndDate    AS DATE NO-UNDO.
   DEF VAR liNewReqStatus AS INT  INITIAL {&REQUEST_STATUS_SUB_REQUEST_DONE} NO-UNDO.
   DEF VAR lcUseCLIType  AS CHAR NO-UNDO.
   DEF VAR ldtActDate    AS DATE NO-UNDO.
   DEF VAR liActTime     AS INT  NO-UNDO.
   DEF VAR ldtMSActDate  AS DATE NO-UNDO.
   DEF VAR liMSActTime   AS INT  NO-UNDO.
   DEF VAR ldEndDate     AS DATE NO-UNDO.
   DEF VAR ldEndStamp    AS DEC  NO-UNDO.
   DEF VAR ldeLimitAmt   AS DEC  NO-UNDO.
   DEF VAR liEndPeriod   AS INT  NO-UNDO.
   DEF VAR ldaChangedEndDate AS DATE  NO-UNDO.
   DEF VAR ldaPeriodEndDate  AS DATE  NO-UNDO.
   DEF VAR ddate             AS DATE  NO-UNDO.
   DEF VAR liBegDate         AS INT   NO-UNDO.
   DEF VAR ldInclAmt         AS DEC   NO-UNDO. 
   DEF VAR llRerate          AS LOG   NO-UNDO INIT FALSE.
   DEF VAR liItemEnd         AS INT  NO-UNDO. 
   DEF VAR liFFBegPeriod     AS INT  NO-UNDO.
   DEF VAR lcReqChar         AS CHAR NO-UNDO.
   DEF VAR lcBundleId        AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.
   DEF VAR lcPostpaidDataBundles AS CHAR NO-UNDO.
   DEF VAR liLookUp          AS INT NO-UNDO.
   DEF VAR liPeriod          AS INT NO-UNDO.
   DEF VAR liPeriods         AS INT NO-UNDO.
   DEF VAR liRequest         AS INT NO-UNDO.
   DEF VAR lcResult          AS CHAR NO-UNDO.
   DEF VAR liReacPeriod      AS INT NO-UNDO.
   DEF VAR ldateDccli        AS DATE  NO-UNDO.
   DEF VAR llUpdateResidualFeeCode AS LOG NO-UNDO. 
   DEFINE VARIABLE llSAPC AS LOGICAL INITIAL FALSE NO-UNDO.
   DEFINE VARIABLE loProCommand AS CLASS Gwy.SAPC.ProCommandNBCH NO-UNDO.

   DEF BUFFER bMsRequest FOR MsRequest.
   
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   Func.Common:mSplitTS(MsRequest.ActStamp,
            OUTPUT ldtActDate,
            OUTPUT liActTime).

   FIND FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE
              MsOwner.MsSeq  = MsRequest.MsSeq AND
              MsOwner.TsEnd >= 99999999 NO-ERROR.
   IF NOT AVAILABLE MsOwner THEN 
   FIND FIRST MsOwner NO-LOCK WHERE
              MsOwner.MsSeq  = MsRequest.MsSeq NO-ERROR.
      
   IF NOT AVAILABLE MsOwner THEN DO:
      fReqError("Subscription not found").
      RETURN.
   END.

   FIND Customer WHERE Customer.CustNum = MsOwner.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   /* day campaign id and subscription type */
   ASSIGN llSAPC       = Customer.AccGrp EQ 2
          lcDCEvent = MsRequest.ReqCParam3
          lcUseCLIType = MsOwner.CLIType
          liReacPeriod = YEAR(ldtActDate) * 100 + MONTH(ldtActDate).

   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand   = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = lcDCEvent NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DayCampaign OR
      DayCampaign.ValidFrom > ldtActDate OR
      DayCampaign.ValidTo   < ldtActDate
   THEN DO:
      fReqError("Periodical contract is not valid").
      RETURN.
   END. /* IF NOT AVAILABLE DayCampaign OR */

   IF NOT CAN-FIND(FIRST TMSRelation WHERE 
                     TMSRelation.TableName   = {&PERIODICAL_CONTRACT_TABLE} AND 
                     TMSRelation.keyType     = {&KEY_SKIP_MATRIX}           AND 
                     TMSRelation.ParentValue = lcDCEvent) 
   THEN  
       IF fMatrixAnalyse(Syst.Var:gcBrand,
                         "PERCONTR",
                         "PerContract;SubsTypeTo",
                         lcDCEvent + ";" + lcUseCLIType,
                         OUTPUT lcReqChar) NE 1
       THEN DO: 
           fReqError("Contract is not allowed for this subscription type").
           RETURN.
       END.

   /* predetermined length */  
   IF DayCampaign.DurType = 2 OR DayCampaign.DurType = 3 THEN DO:
      IF DayCampaign.DurMonths = 0 AND
         DayCampaign.DurUnit NE 1 THEN DO:
         fReqError("Duration in months has not been defined").
         RETURN.
      END.
   END. /* IF DayCampaign.DurType = 2 OR DayCampaign.DurType = 3 THEN DO: */

   /* is there already an active contract */
   IF LOOKUP(DayCampaign.DCType, {&PERCONTRACT_RATING_PACKAGE}) = 0 AND
      LOOKUP(DayCampaign.DCType, {&DCTYPE_POOL_RATING}) = 0 THEN DO:
      IF CAN-FIND(FIRST DCCLI WHERE
                     DCCLI.Brand         = Syst.Var:gcBrand              AND
                     DCCLI.DCEvent       = lcDCEvent            AND
                     DCCLI.MsSeq         = MsRequest.MsSeq      AND
                     (IF DCCLI.DCEvent BEGINS "PAYTERM" THEN
                         DCCLI.PerContractID = MsRequest.ReqIParam3 
                       ELSE TRUE)                                 AND  
                     DCCLI.ValidTo       > ldtActDate           AND
                     DCCLI.ValidFrom    <= ldtActDate)
      THEN DO:
         fReqError("Per.contract is already active on subscription").
         RETURN.
      END. /* IF CAN-FIND(FIRST DCCLI WHERE */

      FIND FIRST DCCLI WHERE DCCLI.Brand         = Syst.Var:gcBrand              AND
                             DCCLI.DCEvent       = lcDCEvent            AND
                             DCCLI.MsSeq         = MsRequest.MsSeq      AND
                             (IF DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT} THEN
                                 DCCLI.PerContractID = MsRequest.ReqIParam3 
                              ELSE TRUE)                                AND  
                             DCCLI.ValidTo      <= ldtActDate NO-LOCK NO-ERROR.
      IF NOT AVAILABLE DCCLI THEN DO:
         fReqError("There is no terminated Per.contract present " + 
                   "on subscription").
         RETURN.
      END. /* IF NOT AVAILABLE DCCLI THEN DO: */

      ldtFromDate = DCCLI.ValidFrom.
      liFFBegPeriod = YEAR(DCCLI.ValidFrom) * 100 + MONTH(DCCLI.ValidFrom).

      /* Find correct ValidTo date, if END date is changed from Newton or
         from STC contract extension */
      FOR EACH bMsRequest WHERE bMsRequest.MsSeq     = MsRequest.MsSeq   AND
                                bMsRequest.ReqType   = MsRequest.ReqType AND
                                bMsRequest.ReqStatus = 2 AND
                                bMsRequest.ReqCParam3 = MsRequest.ReqCParam3 AND
                                bMsRequest.ReqCParam2 = "update" AND
      bMsRequest.ReqSource <> {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER} NO-LOCK
         BY bMsRequest.ActStamp DESC:

         liLookUp = LOOKUP("ValidTo",bMsRequest.ReqCParam1,";").
         IF liLookUp > 0 THEN DO:
            ldaChangedEndDate =
               DATE(ENTRY(liLookUp,bMsRequest.ReqCParam4,";")) NO-ERROR.
            LEAVE.
         END. /* IF bMsRequest.ReqCParam3 = MsRequest.ReqCParam3 AND */
      END. /* FOR EACH bMsRequest WHERE bMsRequest.Brand     = Syst.Var:gcBrand */
   END. /* IF LOOKUP(DayCampaign.DCType, {&PERCONTRACT_RATING_PACKAGE}) = 0 */

   IF ldtFromDate = ? THEN ldtFromDate = ldtActDate.

   /* Call the function to calculate the contract end date */
   ldtEndDate = fcontract_end_date (INPUT lcDCEvent, INPUT ldtFromDate).
   
   /* Open the related fixed fee */
   FOR FIRST FixedFee USE-INDEX Custnum WHERE
             FixedFee.Brand     = Syst.Var:gcBrand   AND
             FixedFee.Custnum   = Customer.Custnum AND
             FixedFee.HostTable = "MobSub"  AND
             FixedFee.KeyValue  = STRING(MsRequest.MsSeq) AND
             FixedFee.CalcObj   = DayCampaign.DCEvent AND
             (IF DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT} THEN
              FixedFee.SourceTable = "DCCLI" AND
              FixedFee.SourceKey = STRING(DCCLI.PerContractID)
              ELSE TRUE) AND
             (IF liFFBegPeriod > 0 THEN 
              FixedFee.BegPeriod >= liFFBegPeriod ELSE TRUE) EXCLUSIVE-LOCK,
      FIRST FMItem WHERE
            FMItem.Brand     = Syst.Var:gcBrand AND
            FMItem.FeeModel  = FixedFee.FeeModel AND
            FMItem.FromDate <= FixedFee.BegDate  AND
            FMItem.ToDate   >= FixedFee.BegDate NO-LOCK:

      IF NOT liFFBegPeriod > 0 THEN liFFBegPeriod = FixedFee.BegPeriod.

      IF DayCampaign.durMonths = 0 THEN
         liEndPeriod = 999999.
      ELSE
         ASSIGN ldEndDate = fPer2Date(liFFBegPeriod, 
                                      DayCampaign.durMonths)
                liEndPeriod = IF MONTH(ldEndDate) = 1 THEN
                                 ((YEAR(ldEndDate) - 1) * 100 + 12)
                              ELSE (YEAR(ldEndDate) * 100 +
                                   (MONTH(ldEndDate) - 1)).

      /* if fee is closed */
      IF FixedFee.CustPP > 0 THEN DO:
         ASSIGN FixedFee.EndPeriod = liEndPeriod
                FixedFee.CustPP    = 0.

         IF FixedFee.FinancedResult EQ {&TF_STATUS_HOLD_SENDING} OR
            FixedFee.FinancedResult EQ {&TF_STATUS_WAITING_SENDING} OR
            FIxedFee.FinancedResult = {&TF_STATUS_YOIGO_REACTIVATION_FBB} THEN
            FIxedFee.FinancedResult = {&TF_STATUS_YOIGO_REACTIVATION}.
         ELSE IF (LOOKUP(FixedFee.FinancedResult, {&TF_STATUSES_BANK}) > 0 OR
                  FixedFee.FinancedResult EQ {&TF_STATUS_SENT_TO_BANK}) THEN DO:
            FOR FIRST FixedFeeTF EXCLUSIVE-LOCK WHERE
                      FixedFeeTF.FFNum = FixedFee.FFNum:
               IF FixedFeeTF.CancelStatus EQ "NEW" THEN ASSIGN
                  FixedFeeTF.CancelStatus = ""
                  FixedFeeTF.CancelReason = "".
               ELSE IF FixedFeeTF.CancelStatus NE "" THEN ASSIGN
                  FixedFee.FinancedResult = {&TF_STATUS_YOIGO_REACTIVATION_FBB}
                  llUpdateResidualFeeCode = TRUE.
            END.
         END.

         /* At least 1 year unbilled fixed fee items */
         FIND LAST FFItem OF FixedFee NO-LOCK NO-ERROR.
         IF AVAILABLE FFItem THEN DO:
            IF FFItem.BillPeriod <= (YEAR(Today) * 100 + MONTH(Today) + 100) 
            THEN DO:
               ASSIGN 
                  ddate = fInt2Date(FFItem.Concerns[2],2)
                  ddate = Func.Common:mLastDayOfMonth(ddate)
                  liItemEnd = YEAR(ddate) * 10000 +
                              MONTH(ddate) * 100 +
                              DAY(ddate).

               /* last may have been broken rental (if last=first then it
                  will be reupdated in the 'FMItem.FirstMonthBR=0' section) */
               /* do not restore MF if reactivation doesn't happen on the
                  same month. YDR-1491 */
               IF FFItem.BillPeriod EQ liReacPeriod THEN DO:

                  IF FFItem.Billed EQ FALSE OR
                     (FFItem.InvNum > 0 AND
                      CAN-FIND(FIRST Invoice NO-LOCK WHERE
                                     Invoice.InvNum = FFItem.InvNum AND
                                     Invoice.InvType EQ 99)) THEN DO:
                     FIND CURRENT FFItem EXCLUSIVE-LOCK.
                     ASSIGN
                        FFItem.Amt = FixedFee.Amt
                        FFItem.Concerns[2] = liItemEnd.
                  END.
               END.
                
               /* do not create fees for inactive months */
               IF ddate < DATE(MONTH(ldtActDate),1,YEAR(ldtActDate)) - 1 AND
                  DayCampaign.DCType NE {&DCTYPE_INSTALLMENT}
               THEN ASSIGN
                  ddate = DATE(MONTH(ldtActDate),1,YEAR(ldtActDate)) - 1
                  liItemEnd = YEAR(ddate) * 10000 + 
                              MONTH(ddate) * 100 +
                              DAY(ddate).

               fMakeContractMore(INPUT Fixedfee.FFNum, 
                                  INPUT liItemEnd).
            END.                     
         END. /* IF AVAILABLE FFItem THEN DO: */
          
         ELSE DO:

            /* do not create fees for inactive months */
            IF FixedFee.BegPeriod < liReacPeriod AND
               DayCampaign.DCType NE {&DCTYPE_INSTALLMENT} THEN ASSIGN 
               ddate = DATE(MONTH(ldtActDate),1,YEAR(ldtActDate)) - 1
               liBegDate = YEAR(ddate) * 10000 + 
                           MONTH(ddate) * 100 +
                           DAY(ddate).
            ELSE ASSIGN
               ldaPeriodEndDate = fInt2Date(FixedFee.BegPeriod,1)
               liBegDate = YEAR(ldaPeriodEndDate - 1) * 10000 +
                           MONTH(ldaPeriodEndDate - 1) * 100  +
                           DAY(ldaPeriodEndDate - 1).

            fMakeContractMore(INPUT Fixedfee.FFNum, 
                               INPUT liBegDate).
         END.

         /* Update First month fee if not billed and marked as broken rental */
         IF DAY(FixedFee.BegDate) > 1 AND FMItem.FirstMonthBR = 0 AND
            FixedFee.BegPeriod EQ liReacPeriod THEN DO:
            FIND FIRST FFItem OF FixedFee EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE FFItem AND
                         FFItem.Billed EQ FALSE OR
                        (FFItem.InvNum > 0 AND
               CAN-FIND(FIRST Invoice NO-LOCK WHERE
                              Invoice.InvNum = FFItem.InvNum AND
                              Invoice.InvType EQ 99)) THEN DO:
               ddate = fInt2Date(FixedFee.BegPeriod,2).
               FFItem.Amt = FixedFee.Amt * (ddate - fixedfee.begdate + 1) /
                             DAY(ddate).
            END. /* IF AVAILABLE FFItem AND NOT FFItem.BILLED THEN DO: */
         END. /* IF DAY(FixedFee.BegPeriod) > 1 AND FMItem.FirstMonthBR = 0 */
         
         /* Remove singlefees created in termination */
         IF (MsRequest.ReqSource EQ {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER} OR
             MsRequest.ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION}) AND
             FixedFee.BillCode EQ "PAYTERM" THEN DO:

            FIND FIRST SingleFee USE-INDEX Custnum WHERE
                       SingleFee.Brand = Syst.Var:gcBrand AND
                       SingleFee.Custnum = MsRequest.CustNum AND
                       SingleFee.HostTable = "Mobsub" AND
                       SingleFee.KeyValue = STRING(MsRequest.MsSeq) AND
                       SingleFee.BillCode BEGINS "PAYTERMEND" AND
                       SingleFee.SourceTable = "FixedFee" AND
                       SingleFee.SourceKey = STRING(FixedFee.FFNum)
            EXCLUSIVE-LOCK NO-ERROR.
         
            IF AVAIL SingleFee AND
               (NOT SingleFee.Billed OR
               CAN-FIND(FIRST Invoice NO-LOCK WHERE
                              Invoice.InvNum = SingleFee.InvNum AND
                              Invoice.InvType = 99)) THEN DO:
               IF llDoEvent THEN
                  RUN StarEventMakeDeleteEventWithMemo(
                     (BUFFER SingleFee:HANDLE),
                      MsRequest.UserCode,
                     (IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER} 
                      THEN "RevertRenewalOrder"
                      ELSE "SubscriptionReactivation")).
               DELETE SingleFee.
            END.
          END.
         
          IF Fixedfee.BillCode EQ "PAYTERM" THEN DO:
          /* Open Residual SingleFee (if not billed) */
          FIND FIRST SingleFee USE-INDEX Custnum WHERE
                     SingleFee.Brand       = Syst.Var:gcBrand AND
                     SingleFee.Custnum     = FixedFee.CustNum AND
                     SingleFee.HostTable   = FixedFee.HostTable AND
                     SingleFee.KeyValue    = FixedFee.KeyValue AND
                     SingleFee.SourceTable = FixedFee.SourceTable AND
                     SingleFee.SourceKey   = FixedFee.SourceKey AND
                     SingleFee.CalcObj     = "RVTERM" EXCLUSIVE-LOCK NO-ERROR.
            
            IF AVAILABLE SingleFee THEN DO:

               IF NOT (SingleFee.Billed = TRUE AND
                  CAN-FIND (FIRST Invoice NO-LOCK WHERE
                                  Invoice.InvNum  = SingleFee.InvNum AND
                                  Invoice.InvType = 1)) THEN DO:

                  IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSingleFee).
               
                  ASSIGN SingleFee.BillCode    = "RVTERMF" WHEN llUpdateResidualFeeCode
                         SingleFee.BillPeriod  = YEAR(ldtEndDate + 1) * 100 +
                                                 MONTH(ldtEndDate + 1)
                         SingleFee.Concerns[1] = YEAR(ldtEndDate + 1) * 10000 + 
                                                 MONTH(ldtEndDate + 1) * 100  +
                                                 DAY(ldtEndDate + 1).

                  IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSingleFee).
               END.
            END.
            /* in case of order cancellation the fee might have been removed */
            ELSE IF NOT AVAIL SingleFee AND
               AVAIL DCCLI AND
                     STRING(DCCLI.PerContractID) = FixedFee.SourceKey AND
                     DCCLI.Amount NE ? AND DCCLI.Amount > 0 THEN DO:

               RUN Mc/creasfee.p(MsOwner.CustNum,
                             MsOwner.MsSeq,
                             DCCLI.ValidTo + 1,
                             "FeeModel",
                             "RVTERM",
                             9,
                             DCCLI.Amount,
                             lcDCEvent + " created " + 
                                STRING(ldtActDate,"99.99.9999") +  /* memo */
                             "¤" +  "RVTERM" ,  /* calcobject */
                             FALSE,              /* no messages to screen */
                             MsRequest.UserCode,
                             "ContractActivation",
                             FixedFee.OrderId, /* order id */
                             FixedFee.SourceTable,
                             FixedFee.SourceKey,
                             OUTPUT lcReqChar).

               IF lcReqChar BEGINS "ERROR:" OR lcReqChar BEGINS "0" THEN
                  fReqLog("Residual fee creation (RVTERM) failed for " + 
                          "new contract: " + lcReqChar).
            END.
         END. /*IF FixedFee.BillCode EQ "PAYTERM" THEN DO: */
      END. /* IF FixedFee.CustPP > 0 THEN DO: */
   END. /* FOR LAST FixedFee WHERE */

   /* create rating package */
   IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 THEN DO:
      
      /*YDR-2284 added validfrom AND validto conditions for getting valid record*/
      FOR EACH ServiceLimit NO-LOCK WHERE
               ServiceLimit.GroupCode = lcDCEvent AND
               ServiceLimit.ValidFrom <= ldtActDate AND
               ServiceLimit.ValidTo   >= ldtActDate,
         EACH MServiceLimit EXCLUSIVE-LOCK USE-INDEX MsSeq WHERE
              MServiceLimit.MsSeq    = MsRequest.MsSeq       AND
              MServiceLimit.DialType = ServiceLimit.DialType AND
              MServiceLimit.SLSeq    = ServiceLimit.SLSeq:

         ASSIGN
            ldEndStamp = 99999999.99999
            ldInclAmt  = MServiceLimit.InclAmt.

         Func.Common:mSplitTS(MServiceLimit.FromTS,
                  OUTPUT ldtMSActDate,
                  OUTPUT liMSActTime).

         /* first/last month is relative -> multiple MServiceLimit records */
         IF ServiceLimit.FirstMonthCalc = 1 OR 
            ServiceLimit.LastMonthCalc = 1
         THEN DO:
            IF DAY(ldtMSActDate) > 1 THEN DO: 
               ASSIGN 
                  ldaPeriodEndDate = Func.Common:mLastDayOfMonth(ldtMSActDate)
                  ldEndStamp = Func.Common:mMake2DT(ldaPeriodEndDate,86399).
               IF MServiceLimit.EndTS >= ldEndStamp THEN NEXT. 
             
               /* limit may have been changed on termination */
               ldInclAmt = INT(ServiceLimit.InclAmt * 
                              (ldaPeriodEndDate - ldtMSActDate + 1) /
                               DAY(ldaPeriodEndDate)).
            END.
            ELSE ldInclAmt = ServiceLimit.InclAmt.                     
         END. 
          
         IF MServiceLimit.EndTS = ldEndStamp THEN NEXT. 
            
         /* several activations for this instance -> handle only newest */
         IF CAN-FIND(FIRST MServiceLimit WHERE 
            MServiceLimit.MsSeq    = MsRequest.MsSeq       AND
            MServiceLimit.DialType = ServiceLimit.DialType AND
            MServiceLimit.SLSeq    = ServiceLimit.SLSeq    AND
            MServiceLimit.EndTS    = ldEndStamp) THEN NEXT. 
           
         IF ldInclAmt NE MServiceLimit.InclAmt OR
            ldEndStamp NE MServiceLimit.EndTS 
         THEN llRerate = TRUE.
          
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMServiceLimit).
         ASSIGN 
            MServiceLimit.EndTS = ldEndStamp
            MServiceLimit.InclAmt = ldInclAmt.

         IF MServiceLimit.MSID = 0 THEN
            MServiceLimit.MSID = NEXT-VALUE(mServiceLimit).

         IF MServiceLimit.DialType = {&DIAL_TYPE_GPRS} THEN
            ldeLimitAmt = MServiceLimit.InclAmt.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMServiceLimit).

         FOR EACH MServiceLPool WHERE
                  MServiceLPool.MsSeq  = MServiceLimit.MsSeq AND
                  MServiceLPool.SlSeq  = MServiceLimit.SlSeq AND
                  MServiceLPool.EndTS >= YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + 1 NO-LOCK:

            CREATE Common.RepLog.
            ASSIGN
               Common.RepLog.RowID     = STRING(ROWID(MServiceLPool))
               Common.RepLog.TableName = "MServiceLPool"
               Common.RepLog.EventType = "MODIFY"
               Common.RepLog.EventTime = NOW.

            RELEASE Common.RepLog.
       
         END. /* FOR EACH MServiceLPool WHERE */         

      END. /* FOR EACH MServiceLimit EXCLUSIVE-LOCK WHERE */
   END. /* IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 */

   ELSE DO:
      /* current contract */
      FIND FIRST DCCLI WHERE
                 DCCLI.Brand         = Syst.Var:gcBrand              AND
                 DCCLI.DCEvent       = lcDCEvent            AND
                 DCCLI.MsSeq         = MsRequest.MsSeq      AND
                 (IF DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT} THEN
                     DCCLI.PerContractID = MsRequest.ReqIParam3 
                  ELSE TRUE)                                AND  
                 DCCLI.ValidTo      <= ldtActDate EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE DCCLI THEN DO:
         fReqError("There is no terminated Per.contract present " +
                   "on subscription").
         RETURN.
      END. /* IF NOT AVAILABLE DCCLI THEN DO: */

      /* Remove singlefees created in termination */
      IF (MsRequest.ReqSource EQ {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER} OR
          MsRequest.ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION}) AND
         DCCLI.DCEvent BEGINS "TERM" THEN DO:

         IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION} THEN 
            ldateDccli = ADD-INTERVAL(DCCLI.ValidTo, 1, "months").
         ELSE ldateDccli = DCCLI.ValidTo.
            FIND FIRST SingleFee USE-INDEX Custnum WHERE
                       SingleFee.Brand = Syst.Var:gcBrand AND
                       SingleFee.Custnum = MsRequest.CustNum AND
                       SingleFee.HostTable = "Mobsub" AND
                       SingleFee.KeyValue = STRING(MsRequest.MsSeq) AND
                       SingleFee.BillCode = "TERMPERIOD" AND
                       SingleFee.BillPeriod = YEAR(ldateDccli) * 100 + MONTH(ldateDccli)
            EXCLUSIVE-LOCK NO-ERROR.
      
         IF AVAIL SingleFee AND
            (NOT SingleFee.Billed OR
            CAN-FIND(FIRST Invoice NO-LOCK WHERE
                           Invoice.InvNum = SingleFee.InvNum AND
                           Invoice.InvType = 99)) THEN DO:
            IF llDoEvent THEN
               RUN StarEventMakeDeleteEventWithMemo(
                  (BUFFER SingleFee:HANDLE),
                  MsRequest.UserCode,
                 (IF MsRequest.ReqSource EQ {&REQUEST_SOURCE_REVERT_RENEWAL_ORDER} 
                  THEN "RevertRenewalOrder"
                  ELSE "SubscriptionReactivation")).
            DELETE SingleFee.
         END.

      END.

      IF lldoevent THEN RUN StarEventSetOldBuffer(lhDCCLI).
      ASSIGN DCCLI.TermDate = ?
             DCCLI.ValidTo  = (IF ldaChangedEndDate <> ? 
                               THEN ldaChangedEndDate ELSE ldtEndDate).
      IF lldoevent THEN RUN StarEventMakeModifyEvent (lhDCCLI).

      /* Create counters */
      IF DayCampaign.DurType NE 3 THEN
         fGenerateCounters(MsRequest.MsSeq,
                           lcDCEvent).
                   
   END. /* ELSE DO: */

   IF llRerate THEN DO:
      FIND CURRENT MsRequest EXCLUSIVE-LOCK.
      MsRequest.ReqIParam4 = 1.
   END.

   IF llSAPC
   THEN DO:
      /* Subscription reactivation request is handling the
         provision when ReqSource EQ {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION} */
      IF DayCampaign.EMACode > "" AND
         MsRequest.ReqSource NE {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION}
      THEN DO ON ERROR UNDO, THROW:
         loProCommand = NEW Gwy.SAPC.ProCommandNBCH(MsRequest.MsRequest). 
      
         loProCommand:mStoreProCommand().

         liNewReqStatus = {&REQUEST_STATUS_HLR_PENDING}.

         CATCH loError AS Progress.Lang.Error:
            fReqErrorObject(loError).
            RETURN.
         END CATCH.

         FINALLY:
            IF VALID-OBJECT(loProCommand)
            THEN DELETE OBJECT loProCommand.
         END FINALLY.
      END.
   END.
   
   ELSE RUN pActivateServicePackage(lcDCEvent,
                                    MsOwner.MsSeq,
                                    lcUseCLIType,
                                    ldtActDate,
                                    OUTPUT liNewReqStatus).

   IF AVAILABLE DCCLI THEN RELEASE DCCLI.

   FIND CURRENT MsRequest EXCLUSIVE-LOCK NO-ERROR.
   MsRequest.ReqDtParam1 = ldtFromDate.

   fReqStatus(liNewReqStatus,""). 

   IF DayCampaign.DCType = {&DCTYPE_SERVICE_PACKAGE} OR
      DayCampaign.DCType = {&DCTYPE_BUNDLE} THEN
      lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS").

   /* Update DSS Limit if postpaid data bundle is being added to DSS group */ 
   IF LOOKUP(lcDCEvent,lcPostpaidDataBundles) > 0 THEN DO:
      lcBundleId = fGetActiveDSSId(MsOwner.CustNum,MsRequest.ActStamp).
      lcAllowedDSS2SubsType = fCParamC("DSS2_SUBS_TYPE").
      IF lcBundleId = {&DSS} OR
         (lcBundleId = "DSS2" AND
          LOOKUP(MsOwner.CLIType,lcAllowedDSS2SubsType) > 0) THEN DO:

         RUN pUpdateDSSLimit(INPUT MsOwner.CustNum,
                             INPUT "UPDATE",
                             INPUT ldeLimitAmt,
                             INPUT 0,
                             INPUT MsRequest.ActStamp,
                             OUTPUT ldeDSSTotalLimit).
         IF ldeDSSTotalLimit > 0 THEN
            RUN pUpdateDSSNetworkLimit(INPUT MsOwner.MsSeq,
                                       INPUT MsOwner.CustNum,
                                       INPUT ldeDSSTotalLimit,
                                       INPUT "Limit",
                                       INPUT FALSE,
                                       INPUT MsRequest.MsRequest,
                                       INPUT MsRequest.ActStamp,
                                       INPUT MsRequest.ReqSource,
                                       INPUT lcBundleId).
      END. /* IF fIsDSSActive(INPUT MsOwner.CustNum */
   END. /* IF LOOKUP(lcDCEvent,lcPostpaidDataBundles) > 0 */

   RETURN "".

END PROCEDURE. /* procedure pContractReactivation: */



