/* ----------------------------------------------------------------------------
  MODULE .......: requestaction_exec.p
  FUNCTION .....: Execute request actions after request is executed
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 10.09.08
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/fpcmaintreq.i}
{Func/service.i}
{Func/fmakemsreq.i}
{Mm/requestaction_exec.i}
{Func/penaltyfee.i}
{Func/fcustpl.i}
{Func/dss_matrix.i}
{Mm/fbundle.i}

DEF INPUT PARAMETER iiMsRequest  AS INT  NO-UNDO.
DEF INPUT PARAMETER icCLIType    AS CHAR NO-UNDO.
DEF INPUT PARAMETER iiOrderID    AS INT  NO-UNDO. 
DEF INPUT PARAMETER idActStamp   AS DEC  NO-UNDO.
DEF INPUT PARAMETER idTermStamp  AS DEC  NO-UNDO.
DEF INPUT PARAMETER ilCreateFees AS LOG  NO-UNDO. 
DEF INPUT PARAMETER icSource     AS CHAR NO-UNDO.
DEF INPUT PARAMETER icActionList AS CHAR NO-UNDO.

DEF VAR liMsSeq      AS INT  NO-UNDO.
DEF VAR liCustNum    AS INT  NO-UNDO.
DEF VAR liPayType    AS INT  NO-UNDO.
DEF VAR liReqType    AS INT  NO-UNDO.
DEF VAR ldaReqDate   AS DATE NO-UNDO.
DEF VAR liReqTime    AS INT  NO-UNDO.
DEF VAR ldaTermDate  AS DATE NO-UNDO.
DEF VAR liTermTime   AS INT  NO-UNDO.
DEF VAR lcMemoTable  AS CHAR NO-UNDO.
DEF VAR lcMemoKey    AS CHAR NO-UNDO.
DEF VAR ldContrStamp AS DEC  NO-UNDO.
DEF VAR liCopied     AS INT  NO-UNDO.
DEF VAR liRequest    AS INT  NO-UNDO.
DEF VAR lcResult     AS CHAR NO-UNDO.

DEF VAR lhRequest    AS HANDLE NO-UNDO.

DEF BUFFER bOrigRequest FOR MsRequest.


/****** Main start **********/

FIND FIRST bOrigRequest WHERE bOrigRequest.MsRequest = iiMsRequest
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE bOrigRequest THEN RETURN "ERROR:Unknown request".

lhRequest = BUFFER bOrigRequest:HANDLE.

FIND FIRST CLIType WHERE
           CLIType.Brand   = Syst.Var:gcBrand AND
           CLIType.CLIType = icCLIType NO-LOCK NO-ERROR.
IF AVAILABLE CLIType THEN liPayType = CLIType.PayType.

ASSIGN
   liMsSeq   = bOrigRequest.MsSeq
   liCustNum = bOrigRequest.CustNum
   liReqType = bOrigRequest.ReqType.
   
IF liMsSeq = 0 AND liCustNum = 0 THEN
   RETURN "ERROR:Action target missing".
   
IF liMsSeq > 0 THEN ASSIGN
   lcMemoTable = "MobSub"
   lcMemoKey   = STRING(liMsSeq).
ELSE ASSIGN
   lcMemoTable = "Customer"
   lcMemoKey   = STRING(liCustNum).

IF idActStamp  = 0 THEN idActStamp  = Func.Common:mMakeTS().
IF idTermStamp = 0 THEN idTermStamp = idActStamp.

Func.Common:mSplitTS(idActStamp,
         OUTPUT ldaReqDate,
         OUTPUT liReqTime).

IF iiOrderID > 0 THEN DO:
   FIND FIRST Order WHERE
              Order.Brand   = Syst.Var:gcBrand AND
              Order.OrderID = iiOrderID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Order THEN RETURN "ERROR:Order not found".
END.

/* actions that are handled here */
IF icActionList = "" THEN icActionList = {&REQUEST_ACTIONLIST_ALL}.

RUN pCollectRequestActions(liMsSeq,
                           lhRequest,
                           icCLIType,
                           liPayType,
                           liReqType,
                           ldaReqDate,
                           icActionList).
RUN pRequestActions(icCLIType).

/****** Main end **********/


PROCEDURE pRequestActions:

   DEF INPUT PARAMETER icCLIType AS CHAR NO-UNDO.

   DEF VAR llAllowed AS LOG  NO-UNDO.
   
   DEF BUFFER bAction FOR ttAction.
   
   /* execute */
   FOR EACH ttAction,
      FIRST RequestAction NO-LOCK WHERE
            RequestAction.RequestActionID = ttAction.ActionID
   BY RequestAction.Action DESC    /* terminations before activations */
   BY ttAction.ActionType
   BY ttAction.ActionKey:

      /* additional rules defined */
      RUN pDoRulesAllow(liMsSeq,
                        icCLIType,
                        lhRequest,
                        ldaReqDate,
                        RequestAction.RequestActionID,
                        ttAction.ActionType,
                        ttAction.ActionKey,
                        OUTPUT llAllowed).

      IF NOT llAllowed THEN NEXT.
   
      /* Renewal Order request special handling
         (do not terminate current contract if order.offer 
         includes the same contract)  YBU-317 */
      IF bOrigRequest.ReqType = 46 AND
         ttAction.ActionType = "DayCampaign" AND
         NOT ttAction.ActionKey BEGINS "PAYTERM" THEN DO:

         IF CAN-FIND(FIRST OfferItem NO-LOCK WHERE
                     OfferItem.Brand = Syst.Var:gcBrand AND
                     OfferItem.Offer = Order.Offer AND
                     OfferItem.ItemType = "PerContract" AND
                     OfferItem.ItemKey = ttAction.ActionKey AND
                     OfferItem.EndStamp >= Order.CrStamp AND
                     OfferItem.BeginStamp <= Order.CrStamp
                     USE-INDEX ItemType) THEN NEXT.
      END.
                 
      CASE ttAction.ActionType:

      /* periodical contracts to be created/terminated
        (defined for the new clitype on stc) */
      WHEN "DayCampaign" THEN DO:
         IF liMsSeq > 0 THEN RUN pPeriodicalContract.
      END.
      
      /* barrings */
      WHEN "Barring" THEN RUN pBarring.

      /* service packages */
      WHEN "CTServPac" THEN RUN pServicePackage.
      
      /* service packages */
      WHEN "Service" THEN RUN pServiceRequest.
      
      END CASE.
         
   END.  /* CLITypeAction */
      
END PROCEDURE.

PROCEDURE pPeriodicalContract:

   DEF VAR llCreateFees AS LOG  NO-UNDO.
   DEF VAR llDo         AS LOG  NO-UNDO.
   DEF VAR ldeContrCreStamp AS DEC NO-UNDO. 
   DEF VAR lcWaitFor    AS CHAR NO-UNDO.
   DEF VAR ldaNewEndDate AS DATE NO-UNDO. 
   DEF VAR lcActionName AS CHAR NO-UNDO. 
   DEF VAR ldeCoefficient AS DEC NO-UNDO. 
   DEF VAR ldeSTCPenalty AS DEC NO-UNDO. 
   DEF VAR lcPriceList AS CHAR NO-UNDO. 
   DEF VAR ldePenalty AS DEC NO-UNDO. 
   DEF VAR lcPerContractIDs AS CHAR NO-UNDO. 
   DEF VAR llFound AS LOG NO-UNDO. 
   DEF VAR lbolSTCExemptPenalty AS LOGICAL NO-UNDO.
   DEF VAR liFFCount AS INT NO-UNDO. 
   DEF VAR ldaMonth22 AS DATE NO-UNDO. 
   DEF VAR lcBundleId AS CHAR NO-UNDO. 
   DEF VAR lcParamValue AS CHAR NO-UNDO.

   DEF BUFFER bBundleRequest  FOR MsRequest.
   DEF BUFFER bBundleContract FOR DayCampaign.
   DEF BUFFER bServiceLimit   FOR ServiceLimit.
   DEF BUFFER bMServiceLimit  FOR MServiceLimit.
   DEF BUFFER bDCCLI          FOR DCCLI.
   DEF BUFFER bOrder       FOR Order.

   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand   = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = ttAction.ActionKey
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DayCampaign THEN RETURN "ERROR:Unknown contract".
   

   CASE RequestAction.Action:
          
   /* creation */
   WHEN 1 THEN DO:
        
      IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) = 0 AND
         CAN-FIND(FIRST DCCLI WHERE 
                        DCCLI.Brand   = Syst.Var:gcBrand AND
                        DCCLI.DCEvent = ttAction.ActionKey AND
                        DCCLI.MsSeq   = liMsSeq AND
                        DCCLI.ValidTo > ldaReqDate)
      THEN RETURN. 
      ELSE IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 
      THEN DO:      
               
         llDo = FALSE.
              
         FOR EACH bServiceLimit NO-LOCK WHERE
                  bServiceLimit.GroupCode = ttAction.ActionKey,
            FIRST bMServiceLimit NO-LOCK WHERE
                  bMServiceLimit.MsSeq    = liMsSeq AND
                  bMServiceLimit.DialType = bServiceLimit.DialType AND
                  bMServiceLimit.SLSeq    = bServiceLimit.SLSeq AND
                  bMServiceLimit.EndTS   >= idActStamp:
            llDo = TRUE.
            LEAVE.
         END.
              
         /* contract is already active*/
         IF llDo THEN RETURN. 
      END.

      IF iiOrderID > 0 AND Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN DO:
         IF Order.OrderChannel BEGINS "Retention" THEN ldContrStamp = Func.Common:mMakeTS().
         ELSE ldContrStamp = Order.CrStamp.
      END.
      ELSE ldContrStamp = idActStamp.
    
      ASSIGN 
         llCreateFees = (ilCreateFees AND DayCampaign.FeeModel > "")
         ldeContrCreStamp = ldContrStamp.
      
      /* request should wait until another bundle request is completed */
      lcWaitFor = "".
      IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 AND
         (icSource = {&REQUEST_SOURCE_SUBSCRIPTION_CREATION} OR
          icSource =  {&REQUEST_SOURCE_STC} ) THEN DO:

         FOR EACH bBundleRequest NO-LOCK USE-INDEX OrigRequest WHERE
                  bBundleRequest.OrigRequest = iiMsRequest AND
                  bBundleRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} AND
               LOOKUP(STRING(bBundleRequest.ReqStatus),
                  {&REQ_INACTIVE_STATUSES}) = 0,
            FIRST bBundleContract NO-LOCK USE-INDEX DCEvent WHERE
                  bBundleContract.Brand = Syst.Var:gcBrand AND
                  bBundleContract.DCEvent = bBundleRequest.ReqCParam3 AND
                  LOOKUP(bBundleContract.DCType,
                         {&PERCONTRACT_RATING_PACKAGE}) > 0:
            IF fIsConvergentFixedContract(bBundleRequest.ReqCParam3) THEN NEXT. 
            lcWaitFor = ":wait" + STRING(bBundleRequest.MsRequest).
         END.     
      END.

      lcBundleId = ttAction.ActionKey.
      FIND MobSub WHERE MobSub.MsSeq = liMsSeq NO-LOCK NO-ERROR.
      IF AVAIL Mobsub AND lcBundleId MATCHES "FLEX*UPSELL" AND
         fGetDSSId(mobsub.custnum, Func.Common:mMakeTS()) > "" THEN
         lcBundleId = fgetFlexUpsellBundle(Mobsub.custnum, Mobsub.msseq,
                                           fGetDSSId(mobsub.custnum,
                                           Func.Common:mMakeTS()),
                                           lcBundleId,
                                           Func.Common:mMakeTS()). 

      /*Back To School FLP project, temporary change YBU-6042, YPR-6085*/
      /*TODO remove after FTERM8 campaign period.*/
      DEF BUFFER bFTERMOrder FOR Order.
      FIND FIRST bFTERMOrder WHERE 
                 bFTERMOrder.brand EQ "1" AND
                 bFTERMOrder.OrderID EQ iiOrderID AND
                 INDEX(bFTERMOrder.Orderchannel, "pro") EQ 0 NO-LOCK NO-ERROR.
      /*FTERM12 is coming only from allowed channels. So olnly ActionKey anddate is checked.*/          
       
      IF ttAction.ActionKey EQ "FTERM12-100" AND 
         ((AVAIL bFTERMOrder AND bFTERMOrder.Crstamp < 20171101) OR
          (NOT AVAIL bFTERMOrder AND idActStamp < 20171101)) AND
         CAN-FIND(FIRST bFTERMOrder NO-LOCK WHERE 
                        bFTERMOrder.brand EQ Syst.Var:gcBrand AND
                        bFTERMOrder.OrderID EQ iiOrderID AND
                  INDEX(bFTERMOrder.Orderchannel, "pro") EQ 0)
      THEN lcBundleId = "FTERM8-100".
      
      /* CCP-3 override the default fixed line permancy*/
      IF ttAction.ActionKey BEGINS "FTERM" AND iiOrderID > 0 AND
         CAN-FIND(FIRST OrderAction NO-LOCK WHERE
                        OrderAction.Brand = Syst.Var:gcBrand AND
                        OrderAction.OrderID = iiOrderID AND
                        OrderAction.ItemType = "FixedPermanency") THEN RETURN.

      /*End of FLP temporary change*/

      /* Skip permanency creation request in case of merge stc request */
      IF bOrigRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
         ttAction.ActionKey   BEGINS "FTERM"                         AND
         fCheckMsRequestParam(bOrigRequest.MsRequest,
                              {&MERGE2P3P},
                              OUTPUT lcParamValue)                   THEN
         RETURN.


      /* Temporary check due to ongoing orders created before 5.6.2017
         TODO: REMOVE THE "THEN BLOCK" AFTER THERE ARE NO PENDING VOICE200 RELATED ORDERS */
      IF ttAction.ActionKey EQ "VOICE200" AND
         ( (NOT AVAILABLE Order AND Func.Common:mTSToDate(bOrigRequest.CreStamp) < RequestAction.ValidFrom) OR /* normal STC */
           (AVAILABLE Order AND Func.Common:mTSToDate(Order.CrStamp) < RequestAction.ValidFrom) ) /* New or STC order */
      THEN liRequest = 1.
      ELSE liRequest = fPCActionRequest(liMsSeq,
                                   lcBundleId,
                                   "act" + lcWaitFor,
                                   ldeContrCreStamp,
                                   llCreateFees,
                                   icSource,
                                   "",
                                   iiMsRequest,
                                   FALSE,
                                   "",
                                   0,
                                   0,
                                   "",
                                   OUTPUT lcResult).
   END.
   
   /* termination */
   WHEN 2 OR WHEN 11 THEN DO:
      
      /* YTS-8673 */
      IF RequestAction.ReqType = 46 AND
         ttAction.ActionKey = "RVTERM12" THEN RETURN.

      Func.Common:mSplitTS(idTermStamp,OUTPUT ldaTermDate,OUTPUT liTermTime).
      
      IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) = 0 THEN DO:
         IF DayCampaign.DCType NE {&DCTYPE_INSTALLMENT} THEN DO:
            FIND FIRST bDCCLI WHERE 
                       bDCCLI.Brand   = Syst.Var:gcBrand AND
                       bDCCLI.DCEvent = ttAction.ActionKey AND
                       bDCCLI.MsSeq   = liMsSeq AND
                       bDCCLI.ValidTo > ldaTermDate NO-LOCK NO-ERROR.
            IF AVAIL bDCCLI THEN DO:
               IF bDCCLI.TermDate NE ? THEN RETURN.
            END. /* IF AVAIL bDCCLI THEN DO: */
            ELSE RETURN.
         END.
      END. /* IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) */

      ELSE IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 
      THEN DO:      
               
         llDo = FALSE.
               
         FOR EACH bServiceLimit NO-LOCK WHERE
                  bServiceLimit.GroupCode = ttAction.ActionKey,
            FIRST bMServiceLimit NO-LOCK WHERE
                  bMServiceLimit.MsSeq    = liMsSeq AND
                  bMServiceLimit.DialType = bServiceLimit.DialType AND
                  bMServiceLimit.SLSeq    = bServiceLimit.SLSeq AND
                  bMServiceLimit.EndTS    > idTermStamp:
            llDo = TRUE.
            LEAVE.
         END.
            
         IF NOT llDo THEN RETURN. 
      END.
      /* should penalty fee be created */
      llCreateFees = (ilCreateFees AND
                      DayCampaign.TermFeeCalc > 0 AND
                      DayCampaign.TermFeeModel > "").

      /* YOT-2233 - Exclude Term Penalty if possible */
      IF AVAIL Order AND DayCampaign.DCType = "3" AND
         Order.OrderType = {&ORDER_TYPE_RENEWAL} AND
         CAN-FIND (FIRST OrderAction WHERE
                         OrderAction.Brand    = Syst.Var:gcBrand AND
                         OrderAction.OrderId  = Order.OrderId AND
                         OrderAction.ItemType = "ExcludeTermPenalty" NO-LOCK)
      THEN llCreateFees = FALSE.

      lbolSTCExemptPenalty = FALSE.
      IF (bOrigRequest.Reqtype EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR 
          bOrigRequest.Reqtype EQ {&REQTYPE_BUNDLE_CHANGE}) AND
          DayCampaign.DCType EQ {&DCTYPE_DISCOUNT} THEN
      DO:         
         /* YDR-2038 
            (0=no extend_term_contract
             1=extend_term_contract
             2=exclude_term_penalty)
         */
         IF bOrigRequest.ReqIParam5 EQ 2 THEN
            llCreateFees = FALSE.

         /* YDR-2035
           Don't charge penalty when:
           STC is requested on the same day of the renewal order AND
           New type is POSTPAID */
         IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                           CLIType.CLIType EQ bOrigRequest.reqcparam2 AND
                           CLIType.PayType = {&CLITYPE_PAYTYPE_POSTPAID}) 
            THEN DO:
            ORDER_LOOP:
            FOR EACH bOrder NO-LOCK WHERE
               bOrder.MSSeq EQ bOrigRequest.MsSeq AND
               LOOKUP(bOrder.StatusCode,{&ORDER_CLOSE_STATUSES}) EQ 0 AND
               TRUNC(bOrder.CrStamp,0) <= TRUNC(bOrigRequest.CreStamp,0) AND
               bOrder.OrderType EQ {&ORDER_TYPE_RENEWAL} BY bOrder.CrStamp DESC:

               IF NOT CAN-FIND
               (FIRST MsRequest NO-LOCK WHERE
                      MsRequest.MsSeq EQ bOrder.MsSeq AND
                      MsRequest.Reqtype EQ {&REQTYPE_REVERT_RENEWAL_ORDER} AND
                      MsRequest.Reqstatus EQ {&REQUEST_STATUS_DONE} AND
                      MsRequest.ReqIParam1 EQ bOrder.OrderId) THEN DO:

                  IF TRUNCATE(bOrder.CrStamp,0) EQ 
                     TRUNCATE(bOrigRequest.CreStamp,0) OR
                     /* YDR-2037 */
                     bOrigRequest.ReqSource EQ
                     {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION} THEN 
                     lbolSTCExemptPenalty = TRUE.
                  LEAVE ORDER_LOOP.

               END.

               IF TRUNC(bOrder.CrStamp,0) < TRUNC(bOrigRequest.CreStamp,0) THEN
                  LEAVE ORDER_LOOP.

            END. /* FOR EACH bOrder NO-LOCK WHERE */
         END. /* POSTPAID */
      END. /* IF (bOrigRequest.Reqtype EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}... */
      
      /* YPR-1763 - Exclude PayTerm termination */
      IF AVAIL Order AND DayCampaign.DCType = "5" AND
         Order.OrderType = {&ORDER_TYPE_RENEWAL} AND
         CAN-FIND (FIRST OrderAction WHERE
                         OrderAction.Brand    = Syst.Var:gcBrand AND
                         OrderAction.OrderId  = Order.OrderId AND
                         OrderAction.ItemType = "KeepInstallment" NO-LOCK)
      THEN RETURN.
         
      IF DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT} THEN DO:

         llFound = FALSE.
         FOR EACH bDCCLI WHERE 
                  bDCCLI.Brand   = Syst.Var:gcBrand AND
                  bDCCLI.DCEvent = ttAction.ActionKey AND
                  bDCCLI.MsSeq   = liMsSeq AND
                  bDCCLI.ValidTo > ldaTermDate NO-LOCK:

            IF bDCCLI.TermDate NE ? THEN NEXT.

            /* YPR-2515 */
            ldaMonth22  = ADD-INTERVAL(bDCCLI.ValidFrom, 22, "months").
            ldaMonth22  = DATE(MONTH(ldaMonth22),1,YEAR(ldaMonth22)).

            IF AVAIL Order AND
                     Order.OrderType = {&ORDER_TYPE_RENEWAL} AND
               ldaReqDate >= ldaMonth22 THEN DO:

               liFFCount = 0.
               
               /* assumes that possible installment activation request
                  has not been yet created from the same order */
               FOR EACH DCCLI NO-LOCK WHERE
                        DCCLI.Brand      = Syst.Var:gcBrand         AND
                        DCCLI.MsSeq      = MsRequest.MsSeq AND
                        DCCLI.ValidTo   >= ldaReqDate     AND
                        DCCLI.ValidFrom <= ldaReqDate     AND 
                        DCCLI.DCEvent   BEGINS "PAYTERM":
                   liFFCount = liFFCount + 1.             
               END.             

               IF CAN-FIND(FIRST OfferItem NO-LOCK WHERE
                                 OfferItem.Brand = Syst.Var:gcBrand AND
                                 OfferItem.Offer = Order.Offer AND
                                 OfferItem.ItemType = "PerContract" AND
                                 OfferItem.EndStamp >= Order.CrStamp AND
                                 OfferItem.BeginStamp <= Order.CrSTamp AND
                                 OfferItem.ItemKey BEGINS "PAYTERM") THEN
                  liFFCount = liFFCount + 1.

               IF liFFCount <= 2 THEN NEXT.
            END.

            llFound = TRUE.
         
            liRequest = fPCActionRequest(liMsSeq,
                                        ttAction.ActionKey,
                                        "term",
                                        idTermStamp,
                                        llCreateFees,
                                        icSource,
                                        "",
                                        iiMsRequest,
                                        FALSE,
                                        "",
                                        0,
                                        bDCCLI.PerContractId,
                                        "",
                                        OUTPUT lcResult).
         END. /* FOR EACH bDCCLI */
         IF NOT llFound THEN RETURN.
      END.
      ELSE IF DayCampaign.DCType EQ {&DCTYPE_DISCOUNT} AND
              lbolSTCExemptPenalty THEN RETURN.
      ELSE 
         liRequest = fPCActionRequest(liMsSeq,
                                        ttAction.ActionKey,
                                        "term",
                                        idTermStamp,
                                        llCreateFees,
                                        icSource,
                                        "",
                                        iiMsRequest,
                                        FALSE,
                                        "",
                                        0,
                                        0,
                                        "",
                                        OUTPUT lcResult).
   END.
         
   /* recreation (possible termination + creation) */
   WHEN 3 THEN DO:
            
      liRequest = fPCActionRequest(liMsSeq,
                                   ttAction.ActionKey,
                                   "recreate",
                                   idActStamp,
                                   TRUE,
                                   icSource,
                                   "",
                                   iiMsRequest,
                                   FALSE,
                                   "",
                                   0,
                                   0,
                                   "",
                                   OUTPUT lcResult).
   END.
   
   /* Don't do anything, leave as is. Used for exceptions, e.g. dctype level
      rules terminates contracts, but for one dcevent this rule skips 
      termination */
   WHEN 5 THEN RETURN.
   
   /* terminal contract extension */
   WHEN 6 THEN DO:
      
      IF DayCampaign.DCType NE {&DCTYPE_DISCOUNT} THEN RETURN.

      FIND FIRST MobSub NO-LOCK WHERE
                 MobSub.MsSeq = liMsSeq NO-ERROR.
      IF NOT AVAIL MobSub THEN RETURN.
      
      Func.Common:mSplitTS(idTermStamp,OUTPUT ldaTermDate,OUTPUT liTermTime).
      
      FIND FIRST bDCCLI WHERE 
                 bDCCLI.Brand   = Syst.Var:gcBrand AND
                 bDCCLI.DCEvent = ttAction.ActionKey AND
                 bDCCLI.MsSeq   = liMsSeq AND
                 bDCCLI.ValidTo > ldaTermDate NO-LOCK NO-ERROR.
      
      IF NOT AVAIL bDCCLI THEN RETURN.

      IF bDCCLI.TermDate NE ? THEN RETURN.
      IF bDCCLI.ValidTo < ldaReqDate THEN RETURN.

      IF bDCCLI.Amount NE ? THEN ldePenalty = bDCCLI.Amount.
      ELSE DO:
         
         lcPriceList = fFeeModelPriceList(MobSub.Custnum,
                                          MobSub.BillTarget,
                                          DayCampaign.TermFeeModel,
                                          TODAY).
         FIND FIRST FMItem NO-LOCK WHERE
                    FMItem.Brand     = Syst.Var:gcBrand       AND
                    FMItem.FeeModel  = DayCampaign.TermFeeModel AND
                    FMItem.PriceList = lcPriceList AND
                    FMItem.FromDate <= TODAY     AND
                    FMItem.ToDate   >= TODAY NO-ERROR.
         IF AVAIL FMItem THEN ldePenalty = fmitem.amount.
      END.

      ldeCoefficient = fCalculateFactor(bDCCLI.ValidFrom,
                                        bDCCLI.RenewalDate,
                                        bDCCLI.ValidTo,
                                        bDCCLI.ValidToOrig,
                                        ldaReqDate,
                                        DayCampaign.TermFeeCalc).

      ASSIGN
         ldePenalty = ldePenalty * ldeCoefficient.
         ldaNewEndDate = ADD-INTERVAL(bDCCLI.ValidTo,12,"months").
      
      IF NOT ldePenalty > 0 THEN RETURN.
   
      liRequest = fPCMaintenanceRequest(liMsSeq,
                                        ttAction.ActionKey,
                                        "ValidTo;RenewalDate;Amount",
                                        (STRING(ldaNewEndDate) + ";" +
                                        STRING(ldaReqDate) + ";" +
                                        STRING(ldePenalty)),
                                        idActStamp,
                                        FALSE,
                                        icSource,
                                        "",
                                        iiMsRequest,
                                        FALSE,
                                        OUTPUT lcResult).
   END.
   /* Refresh active contract without penalty */
   WHEN 7 THEN DO:

      IF NOT CAN-FIND(FIRST DCCLI WHERE
                            DCCLI.Brand   = Syst.Var:gcBrand AND
                            DCCLI.DCEvent = ttAction.ActionKey AND
                            DCCLI.MsSeq   = liMsSeq AND
                            DCCLI.ValidTo > ldaReqDate) THEN RETURN.

      liRequest = fPCActionRequest(liMsSeq,
                                   ttAction.ActionKey,
                                   "recreate",
                                   idActStamp,
                                   FALSE,
                                   icSource,
                                   "",
                                   iiMsRequest,
                                   FALSE,
                                   "",
                                   0,
                                   0,
                                   "",
                                   OUTPUT lcResult).
   END.
  
   OTHERWISE RETURN.
   END CASE.
   
   /* write possible error to a memo */
   IF liRequest = 0 THEN DO:
      
      lcActionName = Func.Common:mTMSCodeName("RequestAction",
                           "Action",
                           STRING(RequestAction.Action)).
      IF NOT lcActionName > "" THEN lcActionName = STRING(RequestAction.Action).
                    
      Func.Common:mWriteMemo(lcMemoTable,
                       lcMemoKey,
                       liCustNum,
                       "PERIODICAL CONTRACT " + 
                       UPPER(lcActionName) + " FAILED",   /* title */
                       ttAction.ActionKey + CHR(10) + lcResult).
   END.
  
END PROCEDURE.

PROCEDURE pServicePackage:
   
   DEF VAR lcActionKey AS CHAR NO-UNDO.
   DEF VAR lcDCEvent   AS CHAR NO-UNDO.
   DEF VAR lcParam     AS CHAR NO-UNDO.

   CASE RequestAction.Action:

   /* activation */
   WHEN 1 THEN DO:
      IF NUM-ENTRIES(ttAction.ActionKey,"|") EQ 2 THEN ASSIGN
         lcActionKey = ENTRY(1,ttAction.ActionKey,"|")
         lcDCEvent   = ENTRY(2,ttAction.ActionKey,"|").
      ELSE ASSIGN
         lcActionKey = ttAction.ActionKey
         lcDCEvent   = "".
        
      FIND FIRST CTServPac WHERE
                 CTServPac.Brand   = Syst.Var:gcBrand AND
                 CTServPac.CLIType = icCLIType AND
                 CTServPac.ServPac = lcActionKey AND  
                 CTServPac.ToDate >= ldaReqDate NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CTServPac THEN RETURN.
                    
      CASE CTServPac.ServType:
      /* barring */
      WHEN 8 THEN DO:
         RUN Mm/barrengine.p (liMsSeq,
                         lcActionKey,
                         icSource,            /* source  */
                         "ReqAct",            /* creator */
                         idActStamp,          /* activate */
                         "",                  /* sms */
                         OUTPUT lcResult).

         liRequest = 0.
         liRequest = INTEGER(lcResult) NO-ERROR. 
      END.
         
      /* normal packages */
      OTHERWISE DO:
         
         RUN pCopyPackage(icCLIType,
                          lcActionKey,
                          lcDCEvent,
                          liMsSeq,
                          ldaReqDate,
                          ?,      /* only new */
                          ilCreateFees,
                          TRUE,       /* make solog */
                          iiMsRequest,
                          FALSE,
                          OUTPUT liCopied).
         /* licopied is not the request nbr, but it is enough to know
            that its value is > 0 */
         liRequest = liCopied.                 
      END.
         
      END CASE.
         
      IF liRequest = 0 THEN DO:                              
         /* write possible error to a memo */
         Func.Common:mWriteMemo(lcMemoTable,
                          lcMemoKey,
                          liCustNum,
                          "SERVICE PACKAGE ACTIVATION FAILED",
                          ttAction.ActionKey).
      END.
   END.
   
   /* deactivation */
   WHEN 2 THEN DO:
 
      /* CTServPac should be used, but where can old clitype be retrieved
         in STC cases? */
      FOR FIRST ServPac NO-LOCK WHERE
                ServPac.Brand   = Syst.Var:gcBrand AND
                ServPac.ServPac = ttAction.ActionKey,
           EACH ServEl NO-LOCK WHERE
                ServEl.Brand   = Syst.Var:gcBrand AND
                ServEl.ServPac = ServPac.ServPac,
          FIRST SubSer NO-LOCK WHERE
                SubSer.MsSeq   = liMsSeq AND
                SubSer.ServCom = ServEl.ServCom:

         /* IF BB service is already suspended then don't send again */
         IF SubSer.ServCom = "BB" AND SubSer.SSStat = 2 THEN RETURN.

         IF SubSer.SSStat > 0 THEN DO:

            /* If subscription has ongoing STC or BTC with data bundle
               then no need to suspend BB service */
            IF SubSer.ServCom = "BB" AND
               LOOKUP(icSource,"4,6,11,15") > 0 AND
               fBundleWithSTC(liMsSeq,idActStamp) THEN RETURN.

            liRequest = fServiceRequest(liMsSeq,
                                        SubSer.ServCom,
                                        (IF SubSer.ServCom = "BB" THEN 2
                                         ELSE 0),
                                        (IF SubSer.ServCom = "BB" THEN ""
                                         ELSE SubSer.SSParam),
                                        (IF SubSer.ServCom = "BB" AND
                                         DAY(ldaReqDate) <> 1 THEN
                                         Func.Common:mSecOffSet(idActStamp,1)
                                         ELSE idActStamp),
                                        "",
                                        FALSE,      /* fees */
                                        FALSE,      /* sms */          
                                        "",
                                        icSource,
                                        iiMsRequest,
                                        FALSE,
                                        OUTPUT lcResult).
                 
            IF liRequest = 0 THEN DO:                              
               /* write possible error to a memo */
               Func.Common:mWriteMemo(lcMemoTable,
                                lcMemoKey,
                                liCustNum,
                                "SERVICE PACKAGE DEACTIVATION FAILED",
                                ttAction.ActionKey).
            END.

         END.       
      END.
   END.

   /* 3: Recreate/Modify */
   WHEN 3 THEN DO:
      FIND FIRST SubSer WHERE
                 SubSer.ServCom = ttAction.ActionKey AND
                 SubSer.MsSeq   = liMsSeq AND
                 SubSer.SsDate <= TODAY   NO-LOCK NO-ERROR.
      IF AVAILABLE SubSer THEN DO:
         IF SubSer.SSStat = 1 THEN DO:
            IF SubSer.ServCom = "BB" THEN lcParam = "4".
            ELSE lcParam = "".
         END.
         ELSE IF SubSer.SSStat  = 2 AND SubSer.ServCom = "BB" AND
                LOOKUP(icCLIType,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0
                THEN lcParam = "3".
         ELSE RETURN.

         liRequest = fServiceRequest(liMsSeq,
                                     SubSer.ServCom,
                                     1,     /* ON */
                                     lcParam,   /* Reset/Modify BB */
                                     idActStamp,
                                     "",
                                     FALSE, /* fees */
                                     FALSE, /* sms */
                                     "",
                                     icSource,
                                     iiMsRequest, /* father request */
                                     FALSE,
                                     OUTPUT lcResult).
         IF liRequest = 0 THEN
            /* write possible error to a memo */
            Func.Common:mWriteMemo(lcMemoTable,
                             lcMemoKey,
                             liCustNum,
                             "SERVICE PACKAGE UPDATE FAILED",
                             ttAction.ActionKey).
            
      END. /* IF AVAILABLE SubSer AND SubSer.SSStat = 1 THEN DO: */
   END. /* WHEN 3 THEN DO: */
   
   END CASE.
   
END PROCEDURE.

PROCEDURE pServiceRequest:

   DEF VAR lcParam          AS CHAR NO-UNDO. 
   DEF VAR lcService        AS CHAR NO-UNDO. 
   DEF VAR liAction         AS INT  NO-UNDO. 
   DEF VAR lcDSSBundleId    AS CHAR NO-UNDO.
   DEF VAR lcAllowedDSS2SubsType AS CHAR NO-UNDO.

   DEF BUFFER bMsOwner      FOR MsOwner.

   CASE RequestAction.Action:
      /* activation */
      WHEN 1 THEN liAction = 1.
      WHEN 2 THEN liAction = 0.
      OTHERWISE RETURN.
   END.
   
   IF NUM-ENTRIES(ttAction.ActionKey,"|") EQ 2 THEN ASSIGN
      lcService = ENTRY(1,ttAction.ActionKey,"|")
      lcParam   = ENTRY(2,ttAction.ActionKey,"|").
   ELSE ASSIGN
      lcService = ttAction.ActionKey
      lcParam   = "".

   liRequest = fServiceRequest(liMsSeq,
                               lcService,
                               liAction,
                               lcParam,
                               idActStamp,
                               "",
                               FALSE, /* fees */
                               FALSE, /* sms */
                               "",
                               icSource,
                               iiMsRequest, /* father request */
                               FALSE,
                               OUTPUT lcResult).
   IF liRequest = 0 THEN
      /* write possible error to a memo */
      Func.Common:mWriteMemo(lcMemoTable,
                       lcMemoKey,
                       liCustNum,
                       "SERVICE REQUEST CREATION FAILED",
                       ttAction.ActionKey).

END.

PROCEDURE pBarring:
   
   CASE RequestAction.Action:

   /* activation */
   WHEN 1 THEN DO:
         
      RUN Mm/barrengine.p (liMsSeq,
                      ttAction.ActionKey,
                      icSource,            /* source  */
                      "ReqAct",            /* creator */
                      idActStamp,          /* activate */
                      "",                  /* sms */
                      OUTPUT lcResult).

      liRequest = 0.
      liRequest = INTEGER(lcResult) NO-ERROR. 
         
      IF liRequest = 0 THEN DO:                              
         /* write possible error to a memo */
         Func.Common:mWriteMemo(lcMemoTable,
                          lcMemoKey,
                          liCustNum,
                          "BARRING FAILED",
                          ttAction.ActionKey + ":" + STRING(lcResult)).
      END.
   END.
   
   END CASE.
   
END PROCEDURE.
