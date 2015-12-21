/* ----------------------------------------------------------------------
  MODULE .......: orderaction_exec.p
  TASK .........: create periodical contract etc. according to orderaction definition
-------------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{cparam2.i}
{fmakemsreq.i}
{service.i}
{tmsconst.i}
{fdss.i}
{dpmember.i}

DEF INPUT  PARAMETER iiMsSeq       AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiOrderId     AS INT  NO-UNDO.
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

FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN RETURN "ERROR:Subscription not available".

FIND Customer WHERE Customer.Custnum = MobSub.Custnum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN RETURN "ERROR:Customer not available".

FIND Order WHERE 
     Order.Brand   = gcBrand AND
     Order.OrderId = iiOrderId NO-LOCK NO-ERROR. 
IF NOT AVAILABLE Order OR Order.MsSeq NE iiMsSeq THEN 
   RETURN "ERROR:Unknown order".
    
/* YOB-390 */
ldeActStamp = Order.CrStamp.

IF LOOKUP(Order.OrderChannel,"renewal_pos_stc,retention_stc") > 0 THEN DO:
   FIND FIRST msowner where
              msowner.msseq = MobSub.msseq USE-INDEX MsSeq NO-LOCK NO-ERROR.
   IF AVAIL msowner THEN ldeActStamp = msowner.tsbegin.
   IF ldeActStamp <= Order.Crstamp THEN ldeActStamp = fMakeTS().
END.

ASSIGN lcIPLContracts   = fCParamC("IPL_CONTRACTS")
       lcCONTDContracts = fCParamC("CONTD_CONTRACTS")
       lcFLATContracts  = fCParamC("FLAT_CONTRACTS")
       lcCONTSContracts = fCParamC("CONTS_CONTRACTS")
       lcCONTSFContracts = fCParamC("CONTSF_CONTRACTS")
       lcAllPostpaidContracts = fCParamC("ALL_POSTPAID_CONTRACTS").

ORDERACTION_LOOP:
FOR EACH OrderAction NO-LOCK WHERE
         OrderAction.Brand     = gcBrand AND
         OrderAction.OrderId   = iiOrderId:

   CASE OrderAction.ItemType:
      WHEN "BundleItem" THEN DO:
         /* DSS Order Action will be executed in separate block   */
         /* to ensure that data bundle must be handled before DSS */
         IF OrderAction.ItemKey EQ {&DSS} THEN NEXT.

         /* Don't create bundle request if renewal order */
         /* with IPL/CONTF/GPRS bundles order actions    */
         IF (Order.OrderType EQ 2 OR Order.OrderType EQ 4) AND
            (LOOKUP(OrderAction.ItemKey,lcCONTDContracts) > 0 OR
             LOOKUP(OrderAction.ItemKey,lcCONTSContracts) > 0 OR
             LOOKUP(OrderAction.ItemKey,lcFLATContracts) > 0 OR
             LOOKUP(OrderAction.ItemKey,lcIPLContracts) > 0 OR
             LOOKUP(OrderAction.ItemKey,lcCONTSFContracts) > 0 OR
             OrderAction.ItemKey = "GPRS") THEN NEXT.

         RUN pPeriodicalContract.
      END.
      WHEN "Service" THEN RUN pService.
      WHEN "Discount" THEN RUN pDiscountPlan.
      WHEN "Q25Discount" THEN RUN pQ25Discount.
      WHEN "Q25Extension" THEN RUN pQ25Extension.
      OTHERWISE NEXT ORDERACTION_LOOP.
   END CASE.

   /* don't abort if an error occurred */
   IF RETURN-VALUE  BEGINS "ERROR:" THEN DO:
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.AgrCust,
                       "OrderAction " + OrderAction.ItemType,
                       "Creation failed. " + RETURN-VALUE).
   END.   
END.

/* DSS Order Action will be executed now other */
/* data bundle request has been created        */
FOR EACH OrderAction NO-LOCK WHERE
         OrderAction.Brand     = gcBrand AND
         OrderAction.OrderId   = iiOrderId AND
         OrderAction.ItemType = "BundleItem" AND
         OrderAction.ItemKey = {&DSS}:

   RUN pPeriodicalContract.

   /* don't abort if an error occurred */
   IF RETURN-VALUE  BEGINS "ERROR:" THEN DO:
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "Customer",
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

   DEF BUFFER MsRequest FOR MsRequest.
  
   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand   = gcBrand AND
              DayCampaign.DCEvent = OrderAction.ItemKey
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DayCampaign THEN 
      RETURN "ERROR: Unknown periodical contract " + OrderAction.ItemKey.
      
   /* override DayCampaign.Feemodel because of possible reactivation */
   IF Order.OrderType = 2 AND
     LOOKUP(DayCampaign.DCType,"3,5") > 0 THEN llCreateFees = TRUE.
   ELSE llCreateFees = (DayCampaign.FeeModel > "").

   ldeContractActStamp = 
                 (IF Order.OrderType EQ {&ORDER_TYPE_STC} THEN fMakeTS()
                  ELSE IF Order.OrderType NE 2 THEN MobSub.ActivationTS
                  ELSE IF Order.OrderChannel BEGINS "Retention" THEN fMakeTS()
                  ELSE IF DayCampaign.DCType = "3" THEN Order.CrStamp
                  ELSE ldeActStamp).

   /* YDR-835 - Charge half price from Initial topup so have 5 min diff */
   IF OrderAction.ItemKey = {&PMDUB} AND
      icSource = {&REQUEST_SOURCE_SUBSCRIPTION_CREATION} THEN DO:
      ASSIGN ldaPMDUBPromoStartDate = fCParamDa("PMDUB_PROMO_START_DATE")
             ldePMDUBPromoActStamp  = fMake2Dt(ldaPMDUBPromoStartDate,0).

      IF ldeContractActStamp >= ldePMDUBPromoActStamp THEN
         ldeContractActStamp = fSecOffSet(ldeContractActStamp,300).
   END. /* IF OrderAction.ItemKey = {&PMDUB} AND */
   
   IF OrderAction.ItemKey = {&DSS} THEN DO:
      IF fOngoingDSSAct(INPUT MobSub.CustNum) THEN
         RETURN "ERROR:DSS activation request is ongoing.".
      ELSE IF NOT fIsDSSAllowed(INPUT  MobSub.CustNum,
                                INPUT  MobSub.MsSeq,
                                INPUT  fMakeTS(),
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
   ELSE 
      liRequest = fPCActionRequest(MobSub.MsSeq,
                                OrderAction.ItemKey,
                                (IF Order.OrderType = 2 AND
                                    DayCampaign.DCType EQ {&DCTYPE_DISCOUNT}
                                 THEN "recreate" 
                                 ELSE "act"),
                                ldeContractActStamp,
                                llCreateFees,
                                icSource,
                                "",
                                iiOrigRequest,
                                FALSE,
                                "",
                                0,
                                0,
                                OUTPUT lcResult).
 
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
                              fMakeTS(),
                              katun,
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
                              fMakeTS(),
                              katun,
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
   
   FIND FIRST DPMember NO-LOCK WHERE
              DPMember.DPId      = INT(OrderAction.ItemKey) AND
              DPMember.HostTable = "Mobsub"                 AND
              DPMember.KeyValue  = STRING(Order.MsSeq)      AND
              DPMember.ValidTo  >= TODAY                    NO-ERROR.
   IF AVAIL DPMember THEN 
      RETURN "ERROR:DPMember: " + OrderAction.ItemKey + " for " + 
         STRING(Order.MsSeq) + " already exist".

   CREATE DPMember.
   ASSIGN DPMember.DPId      = DiscountPlan.DPId
          DPMember.HostTable = "MobSub"
          DPMember.KeyValue  = STRING(Order.MsSeq)
          DPMember.ValidFrom = TODAY.

   IF OrderAction.ItemParam > "" THEN DO:
      ASSIGN
          lcFHParam = ENTRY(1,OrderAction.ItemParam,"|")
          lcSHParam = ENTRY(2,OrderAction.ItemParam,"|") WHEN
            NUM-ENTRIES(OrderAction.ItemParam,"|") > 1 
          DPMember.ValidTo = fCalcDPMemberValidTo(DPMember.ValidFrom,
                                   INT(ENTRY(2,lcSHParam,"=")))
          DPMember.DiscValue = DEC(ENTRY(2,lcFHParam,"=")).
   END.
   ELSE DO:
      ASSIGN
         DPMember.ValidTo = fCalcDPMemberValidTo(DPMember.ValidFrom,
                                       DiscountPlan.ValidPeriods)
         DPMember.DiscValue = DPRate.DiscValue.
   END.
   IF DiscountPlan.dprule EQ "BONO6WEBDISC" THEN /* YPR-3083 */
      DPMember.ValidTo = 12/31/16.

   RETURN "".

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

   DEF BUFFER SingleFee FOR SingleFee.
   DEF BUFFER MsRequest FOR MsRequest.
   
   ASSIGN
      ldaPerDate = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
      liPeriod = YEAR(ldaPerDate) * 100 + MONTH(ldaPerDate)
      lcTFBank = ""
      liPercontractId = INT(OrderAction.ItemParam).

   IF ERROR-STATUS:ERROR OR liPercontractId EQ 0 THEN
      RETURN "ERROR: incorrect contract id".

   FIND SingleFee USE-INDEX Custnum WHERE
        SingleFee.Brand       = gcBrand AND
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
      ldeContractActStamp = fMake2dt(ldaDate,0).
   ELSE ASSIGN
      ldeContractActStamp = fSecOffset(fMakeTS(),5)
      ldaDate = TODAY.

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
                             OUTPUT lcResult).
 
   IF liRequest = 0 THEN 
      RETURN "ERROR:Periodical contract not created; " + lcResult.
   ELSE DO:
      FIND FIRST MsRequest EXCLUSIVE-LOCK WHERE
                 MsRequest.MsRequest = liRequest NO-ERROR.
      IF AVAIL MsRequest THEN ASSIGN
         MsRequest.ReqIparam1 = Order.OrderId.
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
         OTHERWISE 
            lcSMSTxt = fGetSMSTxt("Q25ExtensionYoigo",
                                  TODAY,
                                  Customer.Language,
                                  OUTPUT ldeSMSStamp).
      END CASE.

      IF lcSMSTxt > "" THEN DO:

         ASSIGN
            lcSMSTxt = REPLACE(lcSMSTxt,"#MONTHNAME",
                                lower(entry(month(ldaDate),{&MONTHS_ES})))
            lcSMSTxt = REPLACE(lcSMSTxt,"#YEAR", STRING(YEAR(ldaDate)))
            lcSMSTxt = REPLACE(lcSMSTxt,"#AMOUNT",
                  STRING(ROUND(SingleFee.Amt / 12, 2))).

         fMakeSchedSMS2(MobSub.CustNum,
                        MobSub.CLI,
                        {&SMSTYPE_CONTRACT_ACTIVATION},
                        lcSMSTxt,
                        ldeSMSStamp,
                        "Yoigo info",
                        "").
      END.

   END.

END PROCEDURE.

PROCEDURE pQ25Discount:

   DEF VAR liPercontractId AS INT NO-UNDO. 
   DEF VAR ldeDiscount AS DEC NO-UNDO. 
   DEF VAR lcResult AS CHAR NO-UNDO. 
   DEF VAR lcDiscountPlan AS CHAR NO-UNDO. 

   liPercontractId = INT(OrderAction.ItemParam) NO-ERROR.
   IF ERROR-STATUS:ERROR OR liPercontractId EQ 0 THEN
      RETURN "ERROR:Q25 discount creation failed (incorrect contract id)".
                                            
   ldeDiscount = DEC(OrderAction.ItemKey) NO-ERROR.
   IF ERROR-STATUS:ERROR OR ldeDiscount EQ 0 THEN 
      RETURN "ERROR:Q25 discount creation failed (incorrect discount amount)".

   FIND SingleFee USE-INDEX Custnum WHERE
        SingleFee.Brand       = gcBrand AND
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

   fAddDiscountPlanMember(MobSub.MsSeq,
                         lcDiscountPlan, 
                         ldeDiscount,
                         fPer2Date(SingleFee.BillPeriod,0),
                         1,
                         OUTPUT lcResult).
   RETURN lcResult.

END PROCEDURE.
