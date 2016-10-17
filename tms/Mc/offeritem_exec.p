/* ----------------------------------------------------------------------
  MODULE .......: offeritem_exec.p
  TASK .........: create topup, fatime, periodical contract etc. according 
                  to offeritem definition
  CREATED ......: 23.01.09/aam 
  Version ......: yoigo
-------------------------------------------------------------------------- */

{commali.i}
{tmsconst.i}
{timestamp.i}
{ftaxdata.i}
{offer.i}
{ftopup.i}
{service.i}
{dpmember.i}
{contract_end_date.i}

DEF INPUT  PARAMETER iiMsSeq       AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiOrderID     AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiOrigRequest AS INT  NO-UNDO. /* father request */
DEF INPUT  PARAMETER icSource      AS CHAR NO-UNDO. /* MsRequest.ReqSource */

DEF VAR ldaOfferDate   AS DATE NO-UNDO.
DEF VAR liTime         AS INT  NO-UNDO.
DEF VAR liRequest      AS INT  NO-UNDO.
DEF VAR lcResult       AS CHAR NO-UNDO.
DEF VAR lcUseOffer     AS CHAR NO-UNDO.
DEF VAR ldOfferStamp   AS DEC  NO-UNDO.
DEF VAR ldActStamp     AS DEC  NO-UNDO.

DEF VAR lcPostpaidDataBundles AS CHAR NO-UNDO.
DEF VAR lcPrePaidDataBundles  AS CHAR NO-UNDO.
DEF VAR lcDataBundleCLITypes  AS CHAR NO-UNDO.
DEF VAR lcIPhoneDiscountRuleIds AS CHAR NO-UNDO.

DEF BUFFER bOfferItem FOR OfferItem.

/******** Main start *********/

FIND FIRST MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN RETURN "ERROR:Subscription not available".

FIND FIRST Order WHERE 
           Order.Brand   = gcBrand AND
           Order.OrderID = iiOrderID NO-LOCK NO-ERROR. 
IF NOT AVAILABLE Order OR Order.MsSeq NE iiMsSeq THEN 
   RETURN "ERROR:Unknown order".

IF Order.Offer = "" THEN RETURN "Nothing to do".

ASSIGN
   ldOfferStamp = Order.CrStamp
   ldActStamp   = Order.CrStamp.

/* YBU-1551 */
IF LOOKUP(Order.OrderChannel,"renewal_pos_stc,retention_stc") > 0 THEN DO:
   FIND FIRST msowner where
              msowner.msseq = MobSub.msseq USE-INDEX MsSeq NO-LOCK NO-ERROR.
   IF AVAIL msowner THEN ldActStamp = msowner.tsbegin.
   IF ldActStamp <= Order.Crstamp THEN ldActStamp = fMakeTS().
END.

/* YDR-123 */
FOR FIRST MsRequest NO-LOCK WHERE
          MsRequest.MsSeq = iiMsSeq AND
          MsRequest.ReqType = {&REQTYPE_IMEI_CHANGE} AND
          MsRequest.ReqStatus = {&REQUEST_STATUS_DONE} AND
          MsRequest.ReqIParam1 = Order.OrderID
   BY MsRequest.ActStamp DESC:
   ldOfferStamp = MsRequest.ActStamp.
END.

fSplitTS(ldOfferStamp,
         OUTPUT ldaOfferDate,
         OUTPUT liTime).
   
/* determine correct offer using 'permanencia' code */
IF LOOKUP(Order.Offer,"1,2") > 0 THEN 
   lcUseOffer = fGetOffer(Order.Offer,
                          ldaOfferDate).
ELSE lcUseOffer = Order.Offer.
   
IF lcUseOffer = "" THEN RETURN "ERROR:Unknown offer ID".

FIND FIRST Offer WHERE 
           Offer.Brand = gcBrand AND
           Offer.Offer = lcUseOffer NO-LOCK NO-ERROR.
IF NOT AVAILABLE Offer THEN RETURN "ERROR:Unknown offer".

ASSIGN lcPostpaidDataBundles = fCParamC("POSTPAID_DATA_CONTRACTS")
       lcPrePaidDataBundles  = fCParamC("PREPAID_DATA_CONTRACTS")
       lcDataBundleCLITypes  = fCParamC("DATA_BUNDLE_BASED_CLITYPES")
       lcIPhoneDiscountRuleIds = fCParamC("IPHONE_DISCOUNT").

FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK.
         
OFFERITEM_LOOP:
FOR EACH OfferItem NO-LOCK WHERE
         OfferItem.Brand     = gcBrand AND
         OfferItem.Offer     = Offer.Offer AND
         OfferItem.EndStamp   >= ldOfferStamp  AND
         OfferItem.BeginStamp <= ldOfferStamp:
         
   CASE OfferItem.ItemType:
   WHEN "Topup"          THEN RUN pTopup.
   WHEN "FATime"         THEN RUN pFatime.
   WHEN "PerContract" OR WHEN "PromotionalBundleItem"  
                         THEN RUN pPeriodicalContract.
   WHEN "ServicePackage" THEN RUN pServicePackage.
   WHEN "DiscountPlan"   THEN RUN pDiscountPlanMember.
   WHEN "BundleItem"     THEN DO:
   
      /* With renewal orders, bundle selection should always be in
         OrderAction table. YBU-1037 */
      IF Order.OrderType EQ 2 THEN NEXT OFFERITEM_LOOP.

      /* in case of multiple BundleItem, OrderAction contains selection */
      IF CAN-FIND(FIRST bOfferItem NO-LOCK WHERE
                        bOfferItem.Brand = gcBrand AND
                        bOfferItem.Offer = OfferItem.Offer AND
                        bOfferItem.ItemType = OfferItem.ItemType AND
                        bOfferItem.EndStamp >= ldOfferStamp  AND
                        bOfferItem.BeginStamp <= ldOfferStamp AND
                        ROWID(bOfferItem) NE ROWID(OfferItem)
      USE-INDEX ItemType) THEN NEXT OFFERITEM_LOOP.
   
      /* If BundleItem is defined in OrderAction, prevent duplicate creation */
      FIND FIRST OrderAction WHERE
                 OrderAction.Brand = gcBrand AND
                 OrderAction.OrderId = Order.OrderId AND
                 OrderAction.ItemType = OfferItem.ItemType NO-LOCK NO-ERROR.
      IF AVAIL OrderAction THEN NEXT OFFERITEM_LOOP.
      
      RUN pPeriodicalContract.

   END.
   OTHERWISE NEXT OFFERITEM_LOOP.
   END CASE.

   /* don't abort if an error occurred */
   IF RETURN-VALUE BEGINS "ERROR:" THEN DO:
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                       "MobSub",
                       STRING(MobSub.MsSeq),
                       MobSub.AgrCust,
                       "Offer " + OfferItem.ItemType,
                       "Creation failed. " + RETURN-VALUE).
   END.   
END.

RETURN "".

/******** Main end *********/


/* make a topup recharge request */
PROCEDURE pTopup:
   
   DEF VAR ldVatPerc      AS DEC  NO-UNDO.
   DEF VAR ldAmount       AS DEC  NO-UNDO.
   DEF VAR ldVatAmt       AS DEC  NO-UNDO. 
   DEF VAR lcTaxZone      AS CHAR NO-UNDO.

   FOR FIRST TopupScheme NO-LOCK WHERE
             TopupScheme.Brand       = gcBrand AND
             TopupScheme.TopupScheme = OfferItem.ItemKey AND
             TopupScheme.ToDate     >= ldaOfferDate AND
             TopupScheme.FromDate   <= ldaOfferDate,
       FIRST TopupSchemeRow NO-LOCK WHERE
             TopupSchemeRow.Brand       = gcBrand AND
             TopupSchemeRow.TopupScheme = TopupScheme.TopupScheme AND
             TopupSchemeRow.EndStamp   >= ldOfferStamp AND
             TopupSchemeRow.BeginStamp <= ldOfferStamp,
       FIRST BillItem NO-LOCK WHERE
             BillItem.Brand    = gcBrand AND
             BillItem.BillCode = TopupSchemeRow.BillCode:
         
      /* taxcode */
      ASSIGN
         lcTaxZone = fRegionTaxZone(Customer.Region)
         ldVatPerc = fTaxPerc(lcTaxZone,BillItem.TaxClass, TODAY).
      
      /* amount excluding vat and vat amount separately */
      IF TopupScheme.VatIncl THEN ASSIGN 
         ldAmount = ROUND(TopupSchemeRow.Amount / (1 + ldVatPerc / 100),2)
         ldVatAmt = TopupSchemeRow.Amount - ldAmount.
      ELSE ASSIGN 
         ldAmount = TopupSchemeRow.Amount
         ldVatAmt = ROUND(TopupSchemeRow.Amount * ldVatPerc / 100,2).

      liRequest = fCreateTopUpRequest(MobSub.MsSeq,
                                      MobSub.CLI,
                                      "RefillTRequest",
                                      TopupScheme.TopupSource,
                                      "RefillTRequest",
                                      TopupScheme.PPReqPrefix,
                                      "O:" + Offer.Offer, /* reference */
                                      lcTaxZone,
                                      0,
                                      ldAmount * 100,
                                      ldVatAmt * 100).

   END.

   IF liRequest = 0 THEN RETURN "ERROR:Topup request was not created".
   
   RETURN "".
   
END PROCEDURE.  /* pTopup */ 

PROCEDURE pFatime:

   /* YOT-1739/YDR-468 */
   IF OfferItem.ItemKey EQ "IPL8CPACT2" OR
      OfferItem.ItemKey EQ "IPL15CPACT" THEN DO:
      
      FIND FIRST SalesMan NO-LOCK WHERE
                 SalesMan.Brand = gcBrand AND
                 SalesMan.SalesMan = Order.SalesMan NO-ERROR.

      IF AVAIL SalesMan AND
         LOOKUP(SalesMan.Reseller,
                "PHONE HOUSE,YOIGO PHONE HOUSE,TELECOR,PH,TC,TP") > 0
         THEN RETURN "".
   END.

   RUN creafat (MobSub.CustNum,
                MobSub.MsSeq,
                OfferItem.ItemKey,      /* FatGroup  */
                "",
                "",
                OfferItem.Amount,
                0,
                OfferItem.VatIncl,
                YEAR(MobSub.ActivationDate) * 100 + 
                   MONTH(MobSub.ActivationDate),
                999999,
                OUTPUT lcResult).

   IF lcResult > "" THEN RETURN "ERROR:" + lcResult.

   RETURN "".
   
END PROCEDURE.  /* pFatime */

PROCEDURE pPeriodicalContract:
 
   DEF VAR llCreateFees         AS LOG  NO-UNDO.
   DEF VAR ldContrStamp         AS DEC  NO-UNDO.
   DEF VAR ldaContrDate         AS DATE NO-UNDO.
   DEF VAR liContrTime          AS INT  NO-UNDO.
   DEF VAR ldaDiscountStartDate AS DATE NO-UNDO.
   DEF VAR ldaDiscountEndDate   AS DATE NO-UNDO.
   DEF VAR ldeResidualFee       AS DEC NO-UNDO. 

   DEF BUFFER bActRequest FOR MsRequest.
   
   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand   = gcBrand AND
              DayCampaign.DCEvent = OfferItem.ItemKey
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE DayCampaign THEN 
      RETURN "ERROR: Unknown periodical contract " + OfferItem.ItemKey.

   IF Order.OrderType = 2 AND
     LOOKUP(DayCampaign.DCType,"3,5") > 0 THEN llCreateFees = TRUE.
   ELSE llCreateFees = (DayCampaign.FeeModel > "").

   ldContrStamp = (IF Order.OrderType EQ {&ORDER_TYPE_STC} THEN fMakeTS()
                   ELSE IF Order.OrderType NE 2 THEN MobSub.ActivationTS
                   ELSE IF Order.OrderChannel BEGINS "Retention"
                   THEN fMakeTS()
                   ELSE IF DayCampaign.DCType = "3"
                   THEN Order.CrStamp ELSE ldActStamp).

   /* YOT-2233 - Exclude Term Penalty if possible */
   IF DayCampaign.DCType = "3" AND
      Order.OrderType = 2 AND
      CAN-FIND (FIRST OrderAction WHERE
                      OrderAction.Brand    = gcBrand AND
                      OrderAction.OrderId  = Order.OrderId AND
                      OrderAction.ItemType = "ExcludeTermPenalty" NO-LOCK)
   THEN llCreateFees = FALSE.

   IF DayCampaign.DCType EQ {&DCTYPE_INSTALLMENT} AND
      OfferItem.Amount > 0 THEN ldeResidualFee = OfferItem.Amount.
            
   liRequest = fPCActionRequest(MobSub.MsSeq,
                                OfferItem.ItemKey,
                                (IF Order.OrderType = 2 AND
                                     DayCampaign.DCType EQ {&DCTYPE_DISCOUNT}
                                 THEN "recreate" ELSE "act"),
                                ldContrStamp,
                                llCreateFees,
                                icSource,
                                "",
                                iiOrigRequest,
                                FALSE,
                                "",
                                ldeResidualFee,
                                0,
                                OUTPUT lcResult).
 
   IF liRequest = 0 THEN 
      RETURN "ERROR:Periodical contract not created; " + lcResult.
   ELSE IF DayCampaign.DCEvent BEGINS "PAYTERM" THEN DO:

      FIND FIRST MsRequest NO-LOCK WHERE
                 MsRequest.MsSeq = MobSub.MsSeq AND
                 MsRequest.ReqType = 9 AND
                 MsRequest.ReqCParam3 BEGINS "PAYTERM" AND
          LOOKUP(STRING(MsRequest.ReqStatus),
                 {&REQ_INACTIVE_STATUSES} + ",3") = 0 NO-ERROR.

      IF AVAIL MsRequest THEN DO:
         find first bActRequest where
                    bActRequest.msrequest = liRequest
              exclusive-lock no-error.
         if avail bActRequest then
            bActRequest.ReqIParam2 = MsRequest.msrequest.
         RELEASE bActRequest.
      END.
   END.

   IF OfferItem.ItemKey BEGINS "PAYTERM" THEN DO:
      FIND FIRST bOfferItem NO-LOCK WHERE
                 bOfferItem.Brand = gcBrand AND
                 bOfferItem.Offer = OfferItem.Offer AND
                 bOfferItem.ItemType = "DiscountPlan" AND
                 bOfferItem.EndStamp >= ldOfferStamp  AND
                 bOfferItem.BeginStamp <= ldOfferStamp AND
                 LOOKUP(bOfferItem.ItemKey,lcIPhoneDiscountRuleIds) > 0 NO-ERROR.
      IF AVAIL bOfferItem THEN DO:
         fSplitTS(ldContrStamp,OUTPUT ldaContrDate,OUTPUT liContrTime).

         IF DAY(ldaContrDate) <> 1 THEN
            ldaDiscountEndDate = fcontract_end_date(OfferItem.ItemKey,
                                 ldaContrDate - DAY(ldaContrDate) + 1).
         ELSE
            ldaDiscountEndDate = fcontract_end_date(OfferItem.ItemKey,
                                                    ldaContrDate).

         ldaDiscountStartDate = DATE(MONTH(ldaDiscountEndDate),1,
                                     YEAR(ldaDiscountEndDate)).
         liRequest = fAddDiscountPlanMember(MobSub.MsSeq,
                                            bOfferItem.ItemKey,
                                            bOfferItem.Amount,
                                            ldaDiscountStartDate,
                                            bOfferItem.Periods,
                                            0, /* OrderId */
                                            OUTPUT lcResult).

         IF liRequest NE 0 THEN 
            RETURN "ERROR:Discount not created; " + lcResult.
      END.
   END. /* IF OfferItem.ItemKey BEGINS "PAYTERM" THEN DO: */
      
   RETURN "".
   
END PROCEDURE.  /* pPeriodicalContract */

PROCEDURE pServicePackage:
 
   DEF VAR liNumComp AS INT  NO-UNDO.
   DEF VAR liResume  AS LOG  NO-UNDO.
   DEF VAR lcInfo    AS CHAR NO-UNDO.

   /* Don't create activation request if BB service is already active */
   IF OfferItem.ItemKey = "BB" THEN DO:
      FIND FIRST SubSer WHERE
                 SubSer.ServCom = OfferItem.ItemKey AND
                 SubSer.MsSeq   = MobSub.MsSeq      AND
                 SubSer.SsDate <= TODAY NO-LOCK NO-ERROR.
      IF AVAILABLE SubSer THEN DO:
         IF SubSer.SSStat = 1 THEN RETURN "".
         ELSE IF SubSer.SSStat = 2 THEN liResume = TRUE.
      END. /* IF AVAILABLE SubSer THEN DO: */

      IF NOT CAN-FIND(FIRST OrderAction WHERE
                      OrderAction.Brand = gcBrand AND
                      OrderAction.OrderId = iiOrderID AND
                      OrderAction.ItemType = "BundleItem" AND
                      LOOKUP(OrderAction.ItemKey,
         lcPostpaidDataBundles + "," + lcPrePaidDataBundles + ",CONTS15") > 0) AND
         LOOKUP(MobSub.CLIType,lcDataBundleCLITypes) = 0 AND
         fGetActOngoingDataBundles(INPUT Mobsub.MsSeq,
                                   INPUT fMakeTS()) = ""
      THEN RETURN "ERROR: BB service can not be activated since no data bundle active".

   END. /* IF OfferItem.ItemKey = "BB" THEN DO: */

   IF liResume THEN
   liNumComp = fServiceRequest(MobSub.MsSeq,
                               OfferItem.ItemKey,
                               1,
                               "3",
                               fMakeTS(),
                               "",
                               TRUE,      /* fees */
                               TRUE,      /* sms */
                               "",
                               icSource,
                               iiOrigRequest, /* father request */
                               FALSE, /* mandatory for father request */
                               OUTPUT lcInfo).

   ELSE
   RUN pCopyPackage(MobSub.CLIType,
                    OfferItem.ItemKey, /* servpac */
                    "",
                    MobSub.MSSeq,
                    (IF Order.OrderType NE 2 THEN MobSub.ActivationDate
                     ELSE ldaOfferDate),
                    ?,    /* all changed ones, force it  */
                    TRUE, /* Fees */
                    TRUE,   /* solog (provisioning) */
                    iiOrigRequest,   /* Father request */
                    FALSE,
                    OUTPUT liNumComp).
 
    IF liNumComp = 0 THEN 
       RETURN "ERROR: No Service Components copied from ServPac " + 
              OfferItem.ItemKey. 
    

    RETURN "".

END PROCEDURE.  /* pServicePackage */

PROCEDURE pDiscountPlanMember:

   DEF VAR lcErrorReason AS CHAR NO-UNDO.
   DEF VAR lcDiscPlan    AS CHAR NO-UNDO.

   IF LOOKUP(OfferItem.ItemKey,lcIPhoneDiscountRuleIds) > 0 THEN RETURN "".

   /*
      YPR-2044
      Temporary solution to change the pending orders with the
      "TariffMarchDISC" discount plan on the fly to correct one.
      -- Implemented on 26/03/2015
   */

   lcDiscPlan = OfferItem.ItemKey.

   IF OfferItem.ItemKey = "TariffMarchDISC" THEN DO:
      CASE Order.CLIType:
         WHEN "CONT9"  THEN lcDiscPlan = "CONT9DISC".
         WHEN "CONT15" THEN lcDiscPlan = "CONT15DISC".
         WHEN "CONT24" THEN lcDiscPlan = "CONT24DISC".
         WHEN "CONT23" THEN lcDiscPlan = "CONT23DISC".
      END CASE.
   END.

   liRequest = fAddDiscountPlanMember(MobSub.MsSeq,
                                      lcDiscPlan, /* OfferItem.ItemKey */
                                      OfferItem.Amount,
                                      TODAY,
                                      OfferItem.Periods,
                                      0, /* OrderId */
                                      OUTPUT lcErrorReason).

   IF liRequest NE 0 THEN 
      RETURN lcErrorReason.

   RETURN "".

END PROCEDURE. /* pDiscountPlanMember */
