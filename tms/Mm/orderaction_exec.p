/* ----------------------------------------------------------------------
  MODULE .......: orderaction_exec.p
  TASK .........: create periodical contract etc. according to orderaction definition
-------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/fmakemsreq.i}
{Func/service.i}
{Syst/tmsconst.i}
{Func/dss_request.i}
{Func/dss_matrix.i}
{Mc/dpmember.i}
{Func/q25functions.i}
{Func/orderfunc.i}
{Func/mdub.i}

DEF INPUT  PARAMETER iiMsSeq       AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiOrderId     AS INT  NO-UNDO.
DEF INPUT  PARAMETER ideActStamp   AS DEC  NO-UNDO.
DEF INPUT  PARAMETER iiOrigRequest AS INT  NO-UNDO. /* father request */
DEF INPUT  PARAMETER icSource      AS CHAR NO-UNDO. /* MsRequest.ReqSource */

DEF VAR ldeActStamp      AS DEC  NO-UNDO. 
DEF VAR lcIPLContracts   AS CHAR NO-UNDO.
DEF VAR lcCONTDContracts AS CHAR NO-UNDO.
DEF VAR lcFLATContracts  AS CHAR NO-UNDO.
DEF VAR lcCONTSContracts AS CHAR NO-UNDO.
DEF VAR lcCONTSFContracts AS CHAR NO-UNDO.
DEF VAR lcAllPostpaidContracts AS CHAR NO-UNDO.
DEF VAR lcFHParam AS CHAR NO-UNDO. 
DEF VAR lcSHParam AS CHAR NO-UNDO. 
DEF VAR lcBundleId AS CHAR NO-UNDO.
DEFINE VARIABLE lcContractID AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCounter     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcItemParams AS CHARACTER NO-UNDO.

FIND FIRST MSRequest WHERE
           MSRequest.brand EQ Syst.Var:gcBrand AND
           MsRequest.msRequest = iiOrigRequest NO-ERROR.

FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN RETURN "ERROR:Subscription not available".

FIND Customer WHERE Customer.Custnum = MobSub.Custnum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN RETURN "ERROR:Customer not available".

FIND Order WHERE 
     Order.Brand   = Syst.Var:gcBrand AND
     Order.OrderId = iiOrderId NO-LOCK NO-ERROR. 
IF NOT AVAILABLE Order OR Order.MsSeq NE iiMsSeq THEN 
   RETURN "ERROR:Unknown order".
    
/* YOB-390 */
IF ideActStamp EQ ? THEN
   ideActStamp = Order.CrStamp.

IF LOOKUP(Order.OrderChannel,"renewal_pos_stc,retention_stc") > 0 THEN DO:
   FIND FIRST msowner where
              msowner.msseq = MobSub.msseq USE-INDEX MsSeq NO-LOCK NO-ERROR.
   IF AVAIL msowner THEN ideActStamp = msowner.tsbegin.
   IF ideActStamp <= Order.Crstamp THEN ideActStamp = Func.Common:mMakeTS().
END.

ASSIGN lcIPLContracts   = fCParamC("IPL_CONTRACTS")
       lcCONTDContracts = fCParamC("CONTD_CONTRACTS")
       lcFLATContracts  = fCParamC("FLAT_CONTRACTS")
       lcCONTSContracts = fCParamC("CONTS_CONTRACTS")
       lcCONTSFContracts = fCParamC("CONTSF_CONTRACTS")
       lcAllPostpaidContracts = fCParamC("ALL_POSTPAID_CONTRACTS").

ORDERACTION_LOOP:
FOR EACH OrderAction NO-LOCK WHERE
         OrderAction.Brand     = Syst.Var:gcBrand AND
         OrderAction.OrderId   = iiOrderId:
   
   IF OrderAction.ItemType = "BundleItem" THEN     
   DO:
       FIND FIRST DayCampaign WHERE DayCampaign.Brand = Syst.Var:gcBrand AND DayCampaign.DCEvent = OrderAction.ItemKey NO-LOCK NO-ERROR.
       IF AVAIL DayCampaign THEN 
       DO:
           IF MsRequest.ReqType EQ {&REQTYPE_FIXED_LINE_CREATE} AND 
              LOOKUP(STRING(Daycampaign.BundleTarget), (STRING({&DC_BUNDLE_TARGET_FIXED}) + "," + 
                                                        STRING({&TELEVISION_BUNDLE})      + "," + 
                                                        STRING({&DC_BUNDLE_TARGET_SVA}))) = 0 THEN 
               NEXT ORDERACTION_LOOP.
           ELSE IF MsRequest.ReqType        EQ {&REQTYPE_SUBSCRIPTION_CREATE} AND 
                   Daycampaign.BundleTarget NE {&DC_BUNDLE_TARGET_MOBILE} THEN
               NEXT ORDERACTION_LOOP.
       END.
       ELSE 
           NEXT ORDERACTION_LOOP.
   END.    

   CASE OrderAction.ItemType:
      WHEN "BundleItem" THEN DO:
         /* DSS Order Action will be executed in separate block   */
         /* to ensure that data bundle must be handled before DSS */
         IF OrderAction.ItemKey EQ {&DSS} THEN NEXT.
         /* make flex_upsell / dss_flex_upsell at last */
         IF OrderAction.ItemKey MATCHES "FLEX*UPSELL" THEN NEXT.
                  
         /* Don't create bundle request if renewal order */
         /* with IPL/CONTF/GPRS bundles order actions    */
         IF (Order.OrderType EQ 2 OR Order.OrderType EQ 4) AND
            (LOOKUP(OrderAction.ItemKey,lcCONTDContracts) > 0 OR
             LOOKUP(OrderAction.ItemKey,lcCONTSContracts) > 0 OR
             LOOKUP(OrderAction.ItemKey,lcFLATContracts) > 0 OR
             LOOKUP(OrderAction.ItemKey,lcIPLContracts) > 0 OR
             LOOKUP(OrderAction.ItemKey,lcCONTSFContracts) > 0 OR
             OrderAction.ItemKey = "GPRS") THEN NEXT.

         IF OrderAction.ItemKey EQ "VOICE200B" THEN DO:
            /* should not exist any MDUB valid to the future */
            IF fGetActiveMDUB("VOICE_", INPUT Func.Common:mMakeTS()) > "" THEN NEXT.
         END.

         RUN pPeriodicalContract.
      END.
      WHEN "Service"           THEN RUN pService.
      WHEN "Discount" OR 
      WHEN "DiscountPlan"      THEN RUN pDiscountPlan. 
      WHEN "AddLineDiscount"   THEN RUN pAddLineDiscountPlan.
      WHEN "ExtraLineDiscount" THEN RUN pExtraLineDiscountPlan.
      WHEN "Q25Discount"       THEN RUN pQ25Discount.
      WHEN "Q25Extension"      THEN RUN pQ25Extension.
      WHEN "FixedPermanency"   THEN DO:
         IF MsRequest.ReqType EQ {&REQTYPE_FIXED_LINE_CREATE} OR
            MsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}
            THEN RUN pPeriodicalContract.
      END.
      OTHERWISE NEXT ORDERACTION_LOOP.
   END CASE.

   /* don't abort if an error occurred */
   IF RETURN-VALUE  BEGINS "ERROR:" THEN DO:
      Func.Common:mWriteMemo("MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.AgrCust,
                       "OrderAction " + OrderAction.ItemType,
                       "Creation failed. " + RETURN-VALUE).
   END.   
END.

IF MsRequest.ReqType EQ {&REQTYPE_FIXED_LINE_CREATE} THEN 
   RETURN "".

/* DSS Order Action will be executed now other */
/* data bundle request has been created        */
FOR EACH OrderAction NO-LOCK WHERE
         OrderAction.Brand     = Syst.Var:gcBrand AND
         OrderAction.OrderId   = iiOrderId AND
         OrderAction.ItemType = "BundleItem" AND
         OrderAction.ItemKey = {&DSS}:

   RUN pPeriodicalContract.

   /* don't abort if an error occurred */
   IF RETURN-VALUE  BEGINS "ERROR:" THEN DO:
      Func.Common:mWriteMemo("Customer",
                       STRING(MobSub.CustNum),
                       MobSub.AgrCust,
                       "OrderAction " + OrderAction.ItemType,
                       "Creation failed. " + RETURN-VALUE).
   END.   
END.

/* Create flex_upsell* or dss_flex_upsell* based on dss activity 
   YPPI-1 EXTRAL-108 */
FOR EACH OrderAction EXCLUSIVE-LOCK WHERE
         OrderAction.Brand     = Syst.Var:gcBrand AND
         OrderAction.OrderId   = iiOrderId AND
         OrderAction.ItemType = "BundleItem" AND
         OrderAction.ItemKey MATCHES "FLEX*UPSELL":

   IF fGetDSSId(mobsub.custnum, Func.Common:mMakeTS()) > "" THEN
      OrderAction.ItemKey = fgetFlexUpsellBundle(Mobsub.custnum, Mobsub.msseq,
                                                 fGetDSSId(mobsub.custnum, 
                                                 Func.Common:mMakeTS()),
                                                 OrderAction.ItemKey, 
                                                 Func.Common:mMakeTS()).
   RUN pPeriodicalContract.

   /* don't abort if an error occurred */
   IF RETURN-VALUE  BEGINS "ERROR:" THEN DO:
      Func.Common:mWriteMemo("Customer",
                       STRING(MobSub.CustNum),
                       MobSub.AgrCust,
                       "OrderAction " + OrderAction.ItemType,
                       "Creation failed. " + RETURN-VALUE).
   END.
END.

RETURN "".


PROCEDURE pPeriodicalContract:
   
   DEFINE VARIABLE liRequest AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcResult  AS CHARACTER NO-UNDO. 
 
   DEF VAR llCreateFees        AS LOG  NO-UNDO.
   DEF VAR ldeCurrMonthLimit   AS DEC  NO-UNDO.
   DEF VAR ldeOtherMonthLimit  AS DEC  NO-UNDO.
   DEF VAR ldeConsumedData     AS DEC  NO-UNDO.
   DEF VAR liPendingRequest    AS INT  NO-UNDO. 
   DEF VAR ldeContractActStamp AS DEC  NO-UNDO.

   DEF VAR ldaPMDUBPromoStartDate AS DATE NO-UNDO.
   DEF VAR ldePMDUBPromoActStamp  AS DEC  NO-UNDO.
   DEF VAR lcWaitFor              AS CHAR NO-UNDO. 
   DEF VAR liServSeq              AS INT  NO-UNDO.
   DEF VAR lcSVAParams            AS CHAR NO-UNDO.

   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER bBundleRequest  FOR MsRequest.
   DEF BUFFER bBundleContract FOR DayCampaign.
  
   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand   = Syst.Var:gcBrand AND
              DayCampaign.DCEvent = OrderAction.ItemKey
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DayCampaign THEN 
      RETURN "ERROR: Unknown periodical contract " + OrderAction.ItemKey.
   
   IF DayCampaign.BundleTarget = {&TELEVISION_BUNDLE} THEN 
   DO:
       ASSIGN liServSeq = fCreateNewTPService(iiMsSeq, 
                                              OrderAction.ItemKey, 
                                              "Huawei", 
                                              "Television", 
                                              {&TYPE_ACTIVATION}, 
                                              {&STATUS_NEW}, 
                                              OrderAction.ItemParam, 
                                              Order.Salesman).

       IF liServSeq > 0 THEN
           fCreateTPServiceMessage(iiMsSeq, liServSeq , {&SOURCE_TMS}, {&STATUS_NEW}).
       
       RETURN "".
   END.    
   /* override DayCampaign.Feemodel because of possible reactivation */
   IF Order.OrderType = 2 AND
     LOOKUP(DayCampaign.DCType,"3,5") > 0 THEN llCreateFees = TRUE.
   ELSE llCreateFees = (DayCampaign.FeeModel > "").

   ldeContractActStamp = 
                 (IF Order.OrderType EQ {&ORDER_TYPE_STC} THEN Func.Common:mMakeTS()
                  ELSE IF Order.OrderType NE 2 THEN ideActStamp
                  ELSE IF Order.OrderChannel BEGINS "Retention" THEN Func.Common:mMakeTS()
                  ELSE IF DayCampaign.DCEvent BEGINS "FTERM" THEN Func.Common:mMakeTS() 
                  ELSE IF DayCampaign.DCType = {&DCTYPE_DISCOUNT} THEN Order.CrStamp
                  ELSE ideActStamp).

   /* YDR-835 - Charge half price from Initial topup so have 5 min diff */
   IF OrderAction.ItemKey = {&PMDUB} AND
      icSource = {&REQUEST_SOURCE_SUBSCRIPTION_CREATION} THEN DO:
      ASSIGN ldaPMDUBPromoStartDate = fCParamDa("PMDUB_PROMO_START_DATE")
             ldePMDUBPromoActStamp  = Func.Common:mMake2DT(ldaPMDUBPromoStartDate,0).

      IF ldeContractActStamp >= ldePMDUBPromoActStamp THEN
         ldeContractActStamp = Func.Common:mSecOffSet(ldeContractActStamp,300).
   END. /* IF OrderAction.ItemKey = {&PMDUB} AND */
   
   IF OrderAction.ItemKey = {&DSS} THEN DO:
      IF fOngoingDSSAct(INPUT MobSub.CustNum) THEN
         RETURN "ERROR:DSS activation request is ongoing.".
      ELSE IF NOT fIsDSSAllowed(INPUT  MobSub.CustNum,
                                INPUT  MobSub.MsSeq,
                                INPUT  Func.Common:mMakeTS(),
                                INPUT  OrderAction.ItemKey,
                                INPUT  "",
                                OUTPUT ldeCurrMonthLimit,
                                OUTPUT ldeConsumedData,
                                OUTPUT ldeOtherMonthLimit,
                                OUTPUT lcResult) THEN DO:
         /* order might include data bundle, YBU-1312 */
         IF lcResult = "dss_no_data_bundle" THEN DO:
            FIND FIRST MsRequest NO-LOCK WHERE
                       MsRequest.msseq = MobSub.msseq AND
                       MsRequest.reqtype = {&REQTYPE_CONTRACT_ACTIVATION} AND
               LOOKUP(MsRequest.ReqCparam3,lcAllPostpaidContracts) > 0 AND
               LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0
               USE-INDEX MsSeq NO-ERROR.
            IF AVAIL MsRequest THEN
               ASSIGN
               liPendingRequest = MsRequest.MsREquest
               lcResult = "".
         END.
         ELSE IF lcResult <> "dss_no_postpaid_subscription" AND
                 lcResult <> "dss_no_data_bundle" THEN
            RETURN "ERROR:DSS activation is not allowed. " + lcResult.
      END.
      ELSE DO:      
         FIND FIRST MsRequest NO-LOCK WHERE
                    MsRequest.msseq = MobSub.msseq AND
                    MsRequest.reqtype = {&REQTYPE_CONTRACT_ACTIVATION} AND
              LOOKUP(MsRequest.ReqCparam3,lcAllPostpaidContracts) > 0 AND
              LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0
              USE-INDEX MsSeq NO-ERROR.
         IF AVAIL MsRequest THEN ASSIGN  
               liPendingRequest = MsRequest.MsREquest.
         ELSE DO:
            FIND FIRST MsRequest NO-LOCK WHERE
                       MsRequest.msseq = MobSub.msseq AND
                       MsRequest.reqtype = 1 AND
                       MsRequest.ReqCparam1 = "SHAPER" AND
                       MsRequest.ReqCparam2 = "DEFAULT" AND
                       LOOKUP(STRING(MsRequest.ReqStatus),
                              {&REQ_INACTIVE_STATUSES}) = 0
                   USE-INDEX MsSeq NO-ERROR.
            IF AVAIL MsRequest THEN
               liPendingRequest = MsRequest.MsREquest.
         END.
      END.

      liRequest = fDSSRequest(MobSub.MsSeq,
                              MobSub.CustNum,
                              "CREATE",
                              "",
                              OrderAction.ItemKey,
                              ldeContractActStamp,
                              icSource,
                              "",
                              llCreateFees,
                              iiOrigRequest,
                              FALSE,
                              OUTPUT lcResult).

      IF liRequest > 0 THEN DO:
         IF liPendingRequest > 0 THEN DO:
            find first msrequest where
                       msrequest.msrequest = lirequest
                 exclusive-lock no-error.
            if avail msrequest then
               msrequest.ReqIParam2 = liPendingRequest.
            RELEASE MsREquest.
         END.
      END.
   END. /* IF OrderAction.ItemKey = {&DSS} THEN DO: */
   ELSE DO:

      /* request should wait until another bundle request is completed */
      lcWaitFor = "".
      IF LOOKUP(DayCampaign.DCType,{&PERCONTRACT_RATING_PACKAGE}) > 0 AND
         (icSource = {&REQUEST_SOURCE_SUBSCRIPTION_CREATION} OR
          icSource =  {&REQUEST_SOURCE_STC} ) THEN DO:
         
         FOR EACH bBundleRequest NO-LOCK USE-INDEX OrigRequest WHERE
                  bBundleRequest.OrigRequest = iiOrigRequest AND
                  bBundleRequest.ReqType = {&REQTYPE_CONTRACT_ACTIVATION} AND
               LOOKUP(STRING(bBundleRequest.ReqStatus),
                      {&REQ_INACTIVE_STATUSES}) = 0,
            FIRST bBundleContract NO-LOCK WHERE
                  bBundleContract.Brand = Syst.Var:gcBrand AND
                  bBundleContract.DCEvent = bBundleRequest.ReqCParam3 AND
                  LOOKUP(bBundleContract.DCType,
                         {&PERCONTRACT_RATING_PACKAGE}) > 0:

            IF fIsConvergentFixedContract(bBundleRequest.ReqCParam3) THEN NEXT.
            lcWaitFor = ":wait" + STRING(bBundleRequest.MsRequest).
         END.     
      END.

      ASSIGN
          lcSVAParams  = (IF DayCampaign.BundleTarget = {&DC_BUNDLE_TARGET_SVA} THEN "SVA" ELSE "")
          lcContractID = "" .

      IF lcSVAParams <> "" THEN DO:
          lcItemParams = OrderAction.ItemParam.
          DO iCounter = 2 TO 5:
              IF NUM-ENTRIES(lcItemParams,"|") >= iCounter THEN  
                  IF ENTRY(iCounter,lcItemParams,"|") BEGINS "contract_id" THEN
                    ASSIGN  
                      lcContractID = ENTRY(2,ENTRY(iCounter,lcItemParams,"|"),"=")
                      ENTRY(iCounter,lcItemParams,"|") = CHR(10) + CHR(13)
                      lcItemParams = REPLACE(lcItemParams ,( "|" + CHR(10) + CHR(13) )  , "") .
          END.
          CASE DayCampaign.DCEvent :
              WHEN "OFFICE365" OR 
              WHEN "FAXTOEMAIL" THEN DO:
                  lcSVAParams = lcSVAParams + "|" + lcItemParams.
              END.
              OTHERWISE lcSVAParams = lcSVAParams + (IF lcItemParams <> "" THEN "|||" ELSE "") + lcItemParams. 
          END CASE.
      END.

      liRequest = fPCActionRequest(MobSub.MsSeq,
                                OrderAction.ItemKey,
                                (IF Order.OrderType = 2 AND
                                    DayCampaign.DCType EQ {&DCTYPE_DISCOUNT}
                                 THEN "recreate" 
                                 ELSE "act" + lcWaitFor),
                                ldeContractActStamp,
                                llCreateFees,
                                icSource,
                                "",
                                iiOrigRequest,
                                FALSE,
                                "",
                                0,
                                0,
                                lcSVAParams,
                                OUTPUT lcResult).
         IF liRequest NE 0 AND lcContractID NE "" THEN DO:
            FIND MsRequest WHERE MsRequest.MsRequest = liRequest EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
               IF AVAIL MsRequest THEN
                    ASSIGN MsRequest.Memo = MsRequest.Memo + (IF MsRequest.Memo > "" THEN ", "  ELSE "") + "WebContractID=" + lcContractID.
         END.
   END.
 
   IF liRequest = 0 THEN 
      RETURN "ERROR:Periodical contract not created; " + lcResult.
      
   RETURN "".
   
END PROCEDURE.  /* pPeriodicalContract */

PROCEDURE pService:

   DEFINE VARIABLE liReq    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE ocResult AS CHARACTER NO-UNDO.

   /* SERVICES */
   FIND FIRST SubSer NO-LOCK WHERE
              SubSer.MsSeq   = Order.MsSeq AND
              SubSer.ServCom = OrderAction.ItemKey NO-ERROR.
   IF AVAIL SubSer THEN DO:
      IF SubSer.SSStat = 1 THEN RETURN.
      liReq = fServiceRequest(Order.MsSeq,
                              SubSer.ServCom,
                              1, /* activate */
                              "", /* params */
                              Func.Common:mMakeTS(),
                              Syst.Var:katun,
                              FALSE, /* fees */
                              TRUE, /* sms */
                              "",
                              "",
                              0,
                              FALSE,
                              OUTPUT ocResult).
   END. /* IF AVAIL SubSer THEN DO: */
   ELSE
      liReq = fServiceRequest(Order.MsSeq,
                              OrderAction.ItemKey,
                              1, /* activate */
                              "", /* params */
                              Func.Common:mMakeTS(),
                              Syst.Var:katun,
                              FALSE, /* fees */
                              TRUE, /* sms */
                              "",
                              "",
                              0,
                              FALSE,
                              OUTPUT ocResult).

   IF liReq = 0 THEN
      RETURN "ERROR:Service request " + OrderAction.ItemKey +
             " creation failed; " + ocResult.

   RETURN "".

END PROCEDURE.

PROCEDURE pDiscountPlan:

   DEFINE VARIABLE ldate AS DATE NO-UNDO.
   DEF BUFFER bDiscountPlan FOR DiscountPlan.

   DEF VAR lcResult AS CHAR NO-UNDO. 
   DEF VAR ldaOrderDate AS DATE NO-UNDO. 
   DEFINE VARIABLE ldeDiscAmt AS DECIMAL NO-UNDO.
   DEFINE VARIABLE llError AS LOGICAL NO-UNDO.
   DEFINE VARIABLE liDiscPeriods AS INTEGER NO-UNDO.

   Func.Common:mts2Date(Order.CrStamp, OUTPUT ldaOrderDate).
   
   IF OrderAction.ItemType EQ "DiscountPlan" THEN
      FIND FIRST DiscountPlan NO-LOCK WHERE
                 DiscountPlan.Brand = Syst.Var:gcBrand AND
                 DiscountPlan.DPRuleID = OrderAction.ItemKey AND
                 DiscountPlan.ValidFrom <= ldaOrderDate AND
                 DiscountPlan.ValidTo >= ldaOrderDate NO-ERROR.
   ELSE
      FIND FIRST DiscountPlan NO-LOCK WHERE
                 DiscountPlan.DPId = INT(OrderAction.ItemKey) NO-ERROR.

   IF NOT AVAIL DiscountPlan THEN 
      RETURN "ERROR:DiscountPlan ID: " + OrderAction.ItemKey + " not found".
   
   FIND FIRST DPRate NO-LOCK WHERE
              DPRate.DPId = DiscountPlan.DPId AND 
              DPRate.ValidFrom <= TODAY AND
              DPRate.ValidTo >= TODAY NO-ERROR.

   IF NOT AVAIL DPRate THEN
      RETURN "ERROR:DPRate: " + OrderAction.ItemKey + " not found".
   
   FOR EACH DPMember NO-LOCK WHERE
            DPMember.Hosttable = "Mobsub" AND
            DPMember.KeyValue = STRING(Order.MsSeq) AND
            DPMember.ValidFrom <= TODAY AND
            DPMember.ValidTo >= TODAY,
      FIRST bDiscountPlan NO-LOCK WHERE   
            bDiscountPlan.DPID = DPMember.DPID:

      IF fMatrixAnalyse(Syst.Var:gcBrand,
                        "DISCOUNT-OVERRIDE",
                        "Discount;DiscountOld",
                        DiscountPlan.DPRuleID + ";" + bDiscountPlan.DPRuleID,
                        OUTPUT lcResult) EQ 0 THEN
         fCloseDiscount(bDiscountPlan.DPRuleID,
                        Order.MsSeq,
                        TODAY - 1,
                        NO).
   END.
   
   IF OrderAction.ItemParam > ""
   THEN DO:
      IF NUM-ENTRIES(ENTRY(1,OrderAction.ItemParam,"|"),"=") > 1
      THEN ldeDiscAmt = DECIMAL(ENTRY(2,ENTRY(1,OrderAction.ItemParam,"|"),"=")) NO-ERROR.
      IF ERROR-STATUS:ERROR = TRUE
      THEN llError = TRUE.
      IF NUM-ENTRIES(OrderAction.ItemParam,"|") > 1 AND 
         NUM-ENTRIES(ENTRY(2,OrderAction.ItemParam,"|"),"=") > 1
      THEN liDiscPeriods = INTEGER(ENTRY(2,ENTRY(2,OrderAction.ItemParam,"|"),"=")) NO-ERROR.
      IF ERROR-STATUS:ERROR = TRUE
      THEN llError = TRUE.
   END.

   IF NOT OrderAction.ItemParam > "" OR
      llError
   THEN ASSIGN
          ldeDiscAmt    = DPRate.DiscValue
          liDiscPeriods = DiscountPlan.ValidPeriods.
          
   /* YCO-1063. If not defined periods, then create until "validto"  */
   IF liDiscPeriods = 0 THEN 
      RETURN fAddDiscountPlanMember(Order.MsSeq,
                                    DiscountPlan.DPRuleID,
                                    ldeDiscAmt,
                                    TODAY,
                                    DiscountPlan.ValidTo,
                                    ?,
                                    0).      
   ELSE                          
      RETURN fAddDiscountPlanMember(Order.MsSeq,
                                    DiscountPlan.DPRuleID,
                                    ldeDiscAmt,
                                    TODAY,
                                    ?,
                                    liDiscPeriods,
                                    0).

END PROCEDURE.

PROCEDURE pQ25Extension:

   DEF VAR liPercontractId AS INT NO-UNDO. 
   DEF VAR liRequest AS INT NO-UNDO. 
   DEF VAR lcResult AS CHAR NO-UNDO. 
   DEF VAR ldeContractActStamp AS DEC NO-UNDO. 
   DEF VAR ldaDate AS DATE NO-UNDO. 
   DEF VAR lcSMSTxt AS CHAR NO-UNDO. 
   DEF VAR ldeSMSStamp AS DEC NO-UNDO. 

   DEF VAR liPeriod AS INT NO-UNDO. 
   DEF VAR ldaPerDate AS DATE NO-UNDO. 
   DEF VAR lcTFBank AS CHAR NO-UNDO.
   DEF VAR lcOrigKatun AS CHAR NO-UNDO.
   DEF VAR ldeDiscount AS DEC NO-UNDO. 
   DEF VAR ldeQ25ExtAmount AS DEC NO-UNDO. 

   DEF BUFFER SingleFee FOR SingleFee.
   DEF BUFFER MsRequest FOR MsRequest.
   DEF BUFFER bOrderAction FOR OrderAction.
   
   ASSIGN
      ldaPerDate = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
      liPeriod = YEAR(ldaPerDate) * 100 + MONTH(ldaPerDate)
      lcTFBank = ""
      liPercontractId = INT(OrderAction.ItemParam).
      lcOrigkatun = Syst.Var:katun.

   IF ERROR-STATUS:ERROR OR liPercontractId EQ 0 THEN
      RETURN "ERROR: incorrect contract id".

   FIND SingleFee USE-INDEX Custnum WHERE
        SingleFee.Brand       = Syst.Var:gcBrand AND
        SingleFee.Custnum     = MobSub.CustNum AND
        SingleFee.HostTable   = "Mobsub" AND
        SingleFee.KeyValue    = STRING(Mobsub.MsSeq) AND
        SingleFee.SourceTable = "DCCLI" AND
        SingleFee.SourceKey   = STRING(liPercontractId) AND
        SingleFee.CalcObj     = "RVTERM" NO-LOCK NO-ERROR.
   
   IF NOT AVAIL SingleFee THEN
      RETURN "ERROR: residual fee not found".
   
   IF SingleFee.OrderId > 0 THEN DO:

      FIND FIRST TermReturn NO-LOCK WHERE
                 TermReturn.OrderId = SingleFee.OrderId NO-ERROR.

      IF AVAIL TermReturn AND 
         ((TermReturn.DeviceScreen = TRUE AND TermReturn.DeviceStart  = TRUE) OR
          (TermReturn.DeviceScreen = ? AND TermReturn.DeviceStart  = ?))
         THEN RETURN "ERROR: already returned terminal".
   END.

   ldaDate = fPer2Date(SingleFee.BillPeriod,0).
   ldaDate = DATE(MONTH(ldaDate),21,YEAR(ldaDate)).

   IF TODAY < ldaDate THEN
      ldeContractActStamp = Func.Common:mMake2DT(ldaDate,0).
   ELSE ASSIGN
      ldeContractActStamp = Func.Common:mSecOffSet(Func.Common:mMakeTS(),5)
      ldaDate = TODAY.

   IF Order.OrderType = {&ORDER_TYPE_RENEWAL} THEN
      Syst.Var:katun = Order.OrderChannel + "_" + Order.Salesman.

   liRequest = fPCActionRequest(MobSub.MsSeq,
                             "RVTERM12",
                             "act",
                             ldeContractActStamp,
                             TRUE, /* create fees */
                             icSource,
                             "",
                             iiOrigRequest,
                             FALSE,
                             "",
                             0,
                             liPercontractId,
                             "",
                             OUTPUT lcResult).

   Syst.Var:katun = lcOrigKatun.
 
   IF liRequest = 0 THEN 
      RETURN "ERROR:Periodical contract not created; " + lcResult.
   ELSE DO:
      FIND FIRST MsRequest EXCLUSIVE-LOCK WHERE
                 MsRequest.MsRequest = liRequest NO-ERROR.
      IF AVAIL MsRequest THEN ASSIGN
         MsRequest.ReqIparam1 = Order.OrderId
         MsRequest.ReqCparam4 = OrderAction.ItemKey.
         MsRequest.ReqCparam6 = fBankByBillCode(SingleFee.BillCode).
      RELEASE MsRequest.

      CASE SingleFee.BillCode:
         WHEN "RVTERM1EF" THEN
            lcSMSTxt = fGetSMSTxt("Q25ExtensionUNOE",
                                  TODAY,
                                  Customer.Language,
                                  OUTPUT ldeSMSStamp).
         WHEN "RVTERMBSF" THEN
            lcSMSTxt = fGetSMSTxt("Q25ExtensionSabadell",
                                  TODAY,
                                  Customer.Language,
                                  OUTPUT ldeSMSStamp).
         WHEN "RVTERMBCF" THEN
            lcSMSTxt = fGetSMSTxt("Q25ExtensionCetelem",
                                  TODAY,
                                  Customer.Language,
                                  OUTPUT ldeSMSStamp). 
         OTHERWISE 
            lcSMSTxt = fGetSMSTxt("Q25ExtensionYoigo",
                                  TODAY,
                                  Customer.Language,
                                  OUTPUT ldeSMSStamp).
      END CASE.

      IF lcSMSTxt > "" THEN DO:

         ldeQ25ExtAmount = SingleFee.Amt.
         FIND FIRST bOrderAction NO-LOCK WHERE
                    bOrderAction.Brand = Order.Brand AND
                    bOrderAction.OrderId = Order.OrderId AND
                    bOrderAction.ItemType = "Q25Discount" NO-ERROR.

         IF AVAIL bOrderAction THEN DO:
            ldeDiscount = DEC(bOrderAction.ItemKey) NO-ERROR.
            IF ldeDiscount NE ? THEN
               ldeQ25ExtAmount = ldeQ25ExtAmount - ldeDiscount.
         END.

         IF ldeQ25ExtAmount > 0 THEN DO:

            ASSIGN
               /* Payment start at next month Q25 + 1 */
               ldaDate = DATE(MONTH(ldaDate),1,YEAR(ldaDate))
               ldaDate = ADD-INTERVAL(ldaDate, 1, 'months':U)
               lcSMSTxt = REPLACE(lcSMSTxt,"#MONTHNAME",
                                   lower(entry(month(ldaDate),{&MONTHS_ES})))
               lcSMSTxt = REPLACE(lcSMSTxt,"#YEAR", STRING(YEAR(ldaDate)))
               lcSMSTxt = REPLACE(lcSMSTxt,"#AMOUNT",
                     STRING(TRUNC(ldeQ25ExtAmount / 12, 2))).

            fMakeSchedSMS2(MobSub.CustNum,
                           MobSub.CLI,
                           {&SMSTYPE_CONTRACT_ACTIVATION},
                           lcSMSTxt,
                           ldeSMSStamp,
                           "Yoigo info",
                           "").
         END.
      END.

   END.

END PROCEDURE.

PROCEDURE pQ25Discount:

   DEF VAR liPercontractId AS INT NO-UNDO. 
   DEF VAR ldeDiscount AS DEC NO-UNDO. 
   DEF VAR lcDiscountPlan AS CHAR NO-UNDO. 

   liPercontractId = INT(OrderAction.ItemParam) NO-ERROR.
   IF ERROR-STATUS:ERROR OR liPercontractId EQ 0 THEN
      RETURN "ERROR:Q25 discount creation failed (incorrect contract id)".
                                            
   ldeDiscount = DEC(OrderAction.ItemKey) NO-ERROR.
   IF ERROR-STATUS:ERROR OR ldeDiscount EQ 0 THEN 
      RETURN "ERROR:Q25 discount creation failed (incorrect discount amount)".

   FIND SingleFee USE-INDEX Custnum WHERE
        SingleFee.Brand       = Syst.Var:gcBrand AND
        SingleFee.Custnum     = MobSub.Custnum AND
        SingleFee.HostTable   = "Mobsub" AND
        SingleFee.KeyValue    = STRING(Mobsub.MsSeq) AND
        SingleFee.SourceTable = "DCCLI" AND
        SingleFee.SourceKey   = STRING(liPerContractID) AND
        SingleFee.CalcObj     = "RVTERM" NO-LOCK NO-ERROR.

   IF NOT AVAIL SingleFee THEN
      RETURN "ERROR:Q25 discount creation failed (residual fee not found)".

   IF SingleFee.Billed AND 
      NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                         Invoice.InvNum = SingleFee.Invnum AND
                         Invoice.InvType = 99) THEN
      RETURN "ERROR:Q25 discount creation failed (residual fee is billed)".

   IF CAN-FIND(FIRST OrderAction NO-LOCK WHERE
                     OrderAction.Brand = Order.Brand AND
                     OrderAction.OrderId = Order.OrderId AND
                     OrderAction.ItemType = "Q25Extension") THEN
      lcDiscountPlan = "RVTERMDT4DISC".
   ELSE lcDiscountPlan = "RVTERMDT1DISC".

   RETURN fAddDiscountPlanMember(MobSub.MsSeq,
                                 lcDiscountPlan,
                                 ldeDiscount,
                                 fPer2Date(SingleFee.BillPeriod,0),
                                 ?,
                                 1,
                                 SingleFee.OrderId). /* Q25 OrderId */

END PROCEDURE.

PROCEDURE pAddLineDiscountPlan:

   FIND FIRST DiscountPlan NO-LOCK WHERE
              DiscountPlan.DPRuleID = OrderAction.ItemKey NO-ERROR.
   IF NOT AVAIL DiscountPlan THEN 
      RETURN "ERROR:Additional Line DiscountPlan ID: " + OrderAction.ItemKey + " not found".
    /* Additional Line with mobile only ALFMO-5 */
   fCreateAddLineDiscount(MobSub.MsSeq,
                          MobSub.CLIType,
                          TODAY,
                          OrderAction.ItemKey).
   IF RETURN-VALUE BEGINS "ERROR" THEN
      RETURN RETURN-VALUE.

END PROCEDURE.

PROCEDURE pExtraLineDiscountPlan:

   FIND FIRST DiscountPlan NO-LOCK WHERE 
              DiscountPlan.DPRuleID = OrderAction.ItemKey NO-ERROR.
   IF NOT AVAIL DiscountPlan THEN 
      RETURN "ERROR: Extra line DiscountPlan id: " + OrderAction.ItemKey + " not found".

   fCreateExtraLineDiscount(MobSub.MsSeq,
                            DiscountPlan.DPRuleID,
                            TODAY).
   IF RETURN-VALUE BEGINS "ERROR" THEN 
      RETURN RETURN-VALUE.

END PROCEDURE.
