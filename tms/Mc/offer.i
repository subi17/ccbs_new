/* offer.i     22.01.09/aam 
*/
&IF "{&OFFER_I}" NE "YES"
&THEN
&GLOBAL-DEFINE OFFER_I YES

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
DEFINE TEMP-TABLE ttOffer NO-UNDO LIKE Offer.
DEFINE TEMP-TABLE ttOfferItem NO-UNDO LIKE OfferItem.
DEFINE TEMP-TABLE ttOfferCriteria NO-UNDO LIKE OfferCriteria.

FUNCTION fCriteriaMatch RETURNS LOGIC
   (icCriteria AS CHAR):

   IF OfferCriteria.IncludedValue > "" AND 
      OfferCriteria.IncludedValue NE "*" AND 
      LOOKUP(icCriteria,OfferCriteria.IncludedValue) = 0
   THEN RETURN FALSE.
   
   IF OfferCriteria.ExcludedValue > "" AND 
      (OfferCriteria.ExcludedValue = "*" OR
       LOOKUP(icCriteria,OfferCriteria.ExcludedValue) > 0)
   THEN RETURN FALSE.
   
   RETURN TRUE.
   
END FUNCTION.


FUNCTION fGetOffer RETURNS CHARACTER
   (icOffer      AS CHAR,
    idaOfferDate AS DATE):
    
   DEF VAR llPerContract AS LOG  NO-UNDO.
   DEF VAR llMatch       AS LOG  NO-UNDO.
   DEF VAR lcBillCode    AS CHAR NO-UNDO.
   
   DEF BUFFER bTerminal FOR OrderAccessory.
   
   lcBillCode = "".
   FOR EACH bTerminal OF Order WHERE
            bTerminal.TerminalType = ({&TERMINAL_TYPE_PHONE}) NO-LOCK:
     
      /* pos-orders don't have a billing item, only imei code */
      IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 AND
         bTerminal.IMEI > "" THEN DO:
         /* get billcode based on tac code */
         FOR FIRST IMEIRegister NO-LOCK WHERE
                   IMEIRegister.Brand = gcBrand AND
                   IMEIRegister.IMEI  = SUBSTRING(bTerminal.IMEI,1,8):
            lcBillCode = IMEIRegister.BillCode.       
         END.
      END.
      
      ELSE IF bTerminal.ProductCode > "" THEN 
         lcBillCode = bTerminal.ProductCode.
         
      IF lcBillCode > "" AND  
         NOT CAN-FIND(FIRST BillItem WHERE
                            BillItem.Brand    = gcBrand AND
                            BillItem.BillCode = lcBillCode AND
                            BillItem.BIGroup  = "7") 
      THEN lcBillCode = "".
      
      IF lcBillCode > "" THEN LEAVE.
   END.
   
   CASE icOffer:
   /* no penalty fee */
   WHEN "1" THEN llPerContract = FALSE.
   /* penalty fee */
   WHEN "2" THEN llPerContract = TRUE.
   OTHERWISE RETURN "".
   END CASE.

   IF NOT AVAILABLE Order THEN RETURN "".   

   FindOffer:
   FOR EACH Offer NO-LOCK WHERE
            Offer.Brand     = gcBrand AND
            Offer.ToDate   >= idaOfferDate AND
            Offer.FromDate <= idaOfferDate AND
            Offer.Active    = TRUE
   BY Offer.Priority:
   
      IF lcBillCode > "" THEN DO:
         IF NOT CAN-FIND(FIRST OfferItem WHERE
                               OfferItem.Brand      = gcBrand        AND
                               OfferItem.Offer      = Offer.Offer    AND
                               OfferItem.ItemType   = "BillItem"     AND
                               OfferItem.ItemKey    = lcBillCode     AND
                               OfferItem.EndStamp   >= Order.CrStamp AND
                               OfferItem.BeginStamp <= Order.CrStamp)
         THEN NEXT.
      END.
      
      llMatch = TRUE.
      
      FOR EACH OfferCriteria NO-LOCK WHERE
               OfferCriteria.Brand = gcBrand AND
               OfferCriteria.Offer = Offer.Offer AND
               OfferCriteria.EndStamp   >= Order.CrStamp AND
               OfferCriteria.BeginStamp <= Order.CrStamp:
               
         CASE OfferCriteria.CriteriaType:
         WHEN "CLIType"   THEN 
            llMatch = fCriteriaMatch(Order.CLIType).
         WHEN "OrderType" THEN 
            llMatch = fCriteriaMatch(STRING(Order.OrderType)).
         WHEN "PayType"   THEN 
            llMatch = fCriteriaMatch(STRING(INT(Order.PayType) + 1)).
         WHEN "OrderChannel" THEN 
            llMatch = fCriteriaMatch(Order.OrderChannel).
         WHEN "NumberType" THEN 
            llMatch = fCriteriaMatch(STRING(Order.MNPStatus > 0,"MNP/NEW")).
         END CASE.
         
         IF NOT llMatch THEN NEXT FindOffer.
      END.

      /* contract for penalty fee */
      FIND FIRST OfferItem WHERE
                 OfferItem.Brand      = gcBrand        AND
                 OfferItem.Offer      = Offer.Offer    AND
                 OfferItem.ItemType   = "PerContract"  AND
                 OfferItem.EndStamp   >= Order.CrStamp AND
                 OfferItem.BeginStamp <= Order.CrStamp NO-LOCK NO-ERROR.
      IF AVAILABLE OfferItem AND NOT llPerContract THEN NEXT.
      IF NOT AVAILABLE OfferItem AND llPerContract THEN NEXT.

      RETURN Offer.Offer.
   END.
   
   RETURN "".
   
END FUNCTION.

/* RPC uses this */
FUNCTION fValidateOffer RETURNS INT 
   (INPUT TABLE ttOffer,
    INPUT ilNew AS LOG,
    OUTPUT ocError AS CHAR):

   DEF BUFFER bOffer FOR Offer.
 
   IF ttOffer.fromdate > ttOffer.todate THEN DO:
      ocError = "Valid from date cannot be after valid to date".
      RETURN 1.
   END.
   
   IF ttOffer.Offer = "" THEN DO:
      ocError = "Missing offer id".
      RETURN 1.
   END.
   
   IF LENGTH(ttOffer.Description) > 180 THEN DO:
      ocError = "Maximum 180 characters is allowed for offer description".
      RETURN 1.
   END.

   IF ilNew THEN DO:
      IF CAN-FIND(Offer NO-LOCK WHERE
         Offer.Brand = gcBrand AND
         Offer.Offer = ttOffer.Offer) THEN DO:
         ocError = SUBST("Offer &1 already exists", ttOffer.Offer).
         RETURN 1.
      END.
      IF ttOffer.fromdate < TODAY THEN DO:
         ocError = "Valid from date cannot be set to past".
         RETURN 1.
      END.
      IF LENGTH(ttOffer.Offer) > 15 THEN DO:
         ocError = "Maximum 15 characters is allowed for offer".
         RETURN 1.
      END.
   END.
   ELSE DO: 
      FIND bOffer NO-LOCK WHERE 
           bOffer.Brand = gcBrand AND
           bOffer.Offer = ttOffer.Offer NO-ERROR.
      
      IF NOT AVAIL bOffer THEN DO:
         ocError = SUBST("Offer &1 was not found", ttOffer.Offer).
         RETURN 1.
      END. 
      
      IF ttOffer.FromDate NE bOffer.FromDate AND
         ttOffer.FromDate < TODAY THEN DO:
         ocError = "Cannot set valid from date to past".
         RETURN 1.
      END.
      
      IF bOffer.FromDate <= TODAY THEN DO:
         
         IF ttOffer.FromDate NE bOffer.FromDate OR  
            ttOffer.VatIncl  NE bOffer.VatIncl OR
            ttOffer.OfferAmount NE bOffer.OfferAmount THEN DO:
            ocError = "Cannot change active or past value".
            RETURN 1.
         END.
      END.
   END.

   RETURN 0.
END FUNCTION. 

FUNCTION fGetOfferItemName RETURN CHARACTER
   (INPUT pcOfferItemType AS CHARACTER,
    INPUT pcOfferItemValue AS CHARACTER):

   DEFINE VARIABLE lcItemName AS CHARACTER NO-UNDO. 
   lcItemName = "".
   
   CASE pcOfferItemType:
   WHEN "BillItem" THEN DO:
      FIND FIRST BillItem WHERE
                 BillItem.Brand    = gcBrand AND
                 BillItem.BillCode = pcOfferItemValue NO-LOCK NO-ERROR.
      IF AVAILABLE BillItem THEN lcItemName = BillItem.BIName.
   END.
   WHEN "FATime" THEN DO:
      FIND FIRST FatGroup WHERE
                 FatGroup.Brand = gcBrand AND
                 FatGroup.FtGrp = pcOfferItemValue NO-LOCK NO-ERROR.
      IF AVAILABLE FatGroup THEN lcItemName = FatGroup.FtgName.
   END.
   WHEN "Topup" THEN DO:
      FIND FIRST TopupScheme WHERE
                 TopupScheme.Brand = gcBrand AND
                 TopupScheme.TopupScheme = pcOfferItemValue NO-LOCK NO-ERROR.
      IF AVAILABLE TopupScheme THEN
         lcItemName = TopupScheme.Description.
   END.                                            
   WHEN "PerContract" THEN DO:
      FIND FIRST DayCampaign WHERE
                 DayCampaign.Brand   = gcBrand AND
                 DayCampaign.DCEvent = pcOfferItemValue NO-LOCK NO-ERROR.
      IF AVAILABLE DayCampaign THEN lcItemName = DayCampaign.DCName.
   END.
   WHEN "ServicePackage" THEN DO: 
       FIND ServPac WHERE
            ServPac.Brand   = gcBrand AND
            ServPac.ServPac = pcOfferItemValue NO-LOCK NO-ERROR.
       IF AVAILABLE ServPac THEN lcItemName = ServPac.SPName. 
   END.
   WHEN "DiscountPlan" THEN DO:
       FIND FIRST DiscountPlan WHERE
                  DiscountPlan.Brand = gcBrand AND
                  DiscountPlan.DPRuleID  = pcOfferItemValue NO-LOCK NO-ERROR.
       IF AVAILABLE DiscountPlan THEN 
          lcItemName = DiscountPlan.DPName.
   END.
   WHEN "BundleItem" THEN lcItemName = fCParamC(pcOfferItemValue).
   WHEN "OptionalBundleItem" THEN lcItemName = fCParamC(pcOfferItemValue).
   OTHERWISE lcItemName = "".
   END CASE.

   RETURN lcItemName.
END.


FUNCTION fGetOfferItemTypeName RETURNS CHARACTER
   (pcItemType AS CHAR):

   DEFINE VARIABLE lcType AS CHARACTER NO-UNDO. 
   IF pcItemType > "" THEN 
      lcType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "OfferItem",
                                "ItemType",
                                pcItemType).
   ELSE lcType = "".
   RETURN lcType.                                
END FUNCTION.

/* RPC uses this */
FUNCTION fValidateOfferItem RETURNS INT 
   (INPUT TABLE ttOfferItem,
    ilNew AS LOGICAL,
    OUTPUT ocError AS CHAR):
   
   DEF BUFFER bOfferItem FOR OfferItem.
   
   IF fGetOfferItemTypeName(ttOfferItem.ItemType) EQ "" THEN DO:
      ocError = "Offer item type " + ttOfferItem.ItemType + " is not supported".
      RETURN 1.
   END.

   IF fGetOfferItemName(ttOfferItem.ItemType,ttOfferItem.ItemKey) EQ "" THEN DO:
      ocError = "Offer item type " + ttOfferItem.ItemType + " does not have item name " + ttOfferItem.ItemKey.
      RETURN 1.
   END.

   IF NOT CAN-FIND(Offer WHERE 
                   Offer.Brand = gcBrand AND
                   Offer.Offer = ttOfferItem.Offer) THEN DO:
      ocError = "Offer " + ttOfferItem.Offer + " does not exist".
      RETURN 1.
   END.

   IF ttOfferItem.beginstamp > ttOfferItem.endstamp THEN DO:
      ocError = "Begin stamp cannot be after end stamp".
      RETURN 1.
   END.
   
   /* Overlapping timeintervals between set/new offeritem and an existing one */
   FOR EACH bOfferItem WHERE
      bOfferItem.Brand = gcBrand AND
      bOfferItem.Offer = ttOfferItem.Offer AND
      bOfferItem.ItemType = ttOfferItem.ItemType AND
      bOfferItem.ItemKey = ttOfferItem.ItemKey AND
      bOfferItem.OfferItemId NE ttOfferItem.OfferItemId 
      NO-LOCK USE-INDEX ItemType:
      
   /* 1. begin time cannot be inside the timeinterval of an
         existing OfferItem */
      IF (bOfferItem.EndStamp >= ttOfferItem.BeginStamp AND
         bOfferItem.BeginStamp <= ttOfferItem.BeginStamp) 
         OR
   /* 2. end time cannot be inside the the timeinterval of an
         existing OfferItem */
         (bOfferItem.EndStamp >= ttOfferItem.EndStamp AND
         bOfferItem.BeginStamp <= ttOfferItem.EndStamp) 
         OR
   /* 3. timeinterval may not contain an existing OfferItem
         timeinterval */
         (bOfferItem.EndStamp <= ttOfferItem.EndStamp AND
         bOfferItem.BeginStamp >= ttOfferItem.BeginStamp) THEN DO:

         ocError = "Offer items of one offer may not overlap when item type and item key are the same".
         RETURN 1.
      END.
   
   END.

   IF ilNew THEN DO:
   END.
   ELSE DO:

      FIND bOfferItem WHERE bOfferItem.OfferItemId = ttOfferItem.OfferItemId NO-LOCK NO-ERROR.
      IF NOT AVAIL bOfferItem THEN DO:
         ocError = SUBST("Offer item &1 was not found", ttOfferItem.OfferItemId).
         RETURN 1.
      END.

      IF ttOfferItem.Offer NE bOfferItem.Offer THEN DO:
         ocError = "Cannot change offer of offer item".
         RETURN 1.
      END.

      IF ttOfferItem.ItemType NE bOfferItem.ItemType THEN DO:
         ocError = "Cannot change item type of offer item".
         RETURN 1.
      END.

      IF ttOfferItem.ItemKey NE bOfferItem.ItemKey THEN DO:
         ocError = "Cannot change item key of offer item".
         RETURN 1.
      END.
   END.

   RETURN 0.

END FUNCTION. 


FUNCTION fCriteriaType RETURNS CHAR
   (icCriteriaType AS CHAR):

   DEF VAR lcType AS CHARACTER NO-UNDO.

   IF icCriteriaType > "" THEN 
      lcType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "OfferCriteria",
                                "CriteriaType",
                                icCriteriaType).
   ELSE lcType = "".
                                
   RETURN lcType.
   
END FUNCTION.

/* RPC uses this */
FUNCTION fValidateOfferCriteria RETURNS INT 
   (INPUT TABLE ttOffercriteria,
    INPUT ilNew AS LOG,
    OUTPUT ocError AS CHAR):
   
   DEF BUFFER bOfferCriteria FOR OfferCriteria.

   DEFINE VARIABLE lcType AS CHARACTER NO-UNDO.

   IF NOT CAN-FIND(Offer NO-LOCK WHERE
      Offer.Brand = gcBrand AND
      Offer.Offer = ttOfferCriteria.Offer) THEN DO:
      ocError = SUBST("Offer &1 not found", ttOfferCriteria.Offer).
      RETURN 1.
   END.

   IF ttOfferCriteria.BeginStamp > ttOfferCriteria.EndStamp THEN DO:
      ocError = "Valid from date cannot be after valid to date".
      RETURN 1.
   END.

   IF ttOfferCriteria.ExcludedValue NE "" AND
      ttOfferCriteria.IncludedValue NE "" THEN DO:
      ocError = "Both excluded and included values may not be defined at the same time".
      RETURN 1.
   END.
   
   lcType = fCriteriaType(ttOfferCriteria.CriteriaType).
   IF lcType = "" THEN DO:
      ocError = "Unknown offer criteria type".
      RETURN 1.
   END.
 
   /* Overlapping timeintervals between set/new offercriteria and an existing one */
   FOR EACH bOfferCriteria WHERE
      bOfferCriteria.Brand = gcBrand AND
      bOfferCriteria.Offer = ttOfferCriteria.Offer AND
      bOfferCriteria.CriteriaType = ttOfferCriteria.CriteriaType AND
      bOfferCriteria.OfferCriteriaId NE ttOfferCriteria.OfferCriteriaId 
      NO-LOCK:

      /* 1. begin time cannot be inside the timeinterval of an
         existing OfferCriteria */
      IF (bOfferCriteria.EndStamp >= ttOfferCriteria.BeginStamp AND
         bOfferCriteria.BeginStamp <= ttOfferCriteria.BeginStamp) 
         OR
         /* 2. end time cannot be inside the the timeinterval of an
         existing OfferCriteria */
         (bOfferCriteria.EndStamp >= ttOfferCriteria.EndStamp AND
         bOfferCriteria.BeginStamp <= ttOfferCriteria.EndStamp) 
         OR
         /* 3. timeinterval may not contain an existing OfferCriteria
         timeinterval */
         (bOfferCriteria.EndStamp <= ttOfferCriteria.EndStamp AND
         bOfferCriteria.BeginStamp >= ttOfferCriteria.BeginStamp) THEN DO:

         ocError = "Offer criteria of one offer may not overlap when criteria type is the same".
         RETURN 1.
      END.
   END.

   IF ilNew THEN DO:
      
      IF ttOfferCriteria.includedvalue = "" AND 
         ttOfferCriteria.excludedvalue = "" THEN DO:
         ocError = "Must give either included or excluded values".
         RETURN 1.
      END.
   END.
   ELSE DO:
      FIND bOfferCriteria NO-LOCK WHERE
         bOfferCriteria.OfferCriteriaID = ttOfferCriteria.OfferCriteriaID NO-ERROR.
      IF NOT AVAIL bOfferCriteria THEN DO: 
         ocError = "Offer criteria &1 not found".
         RETURN 1.
      END.

      IF ttOfferCriteria.BeginStamp NE bOfferCriteria.BeginStamp AND
         ttOfferCriteria.BeginStamp < fMakeTS() THEN DO:
         ocError = "Cannot set valid from date to past".
         RETURN 1.
      END.
    
      DEFINE VARIABLE deCurTime AS DECIMAL NO-UNDO. 
      deCurTime = fMakeTs().
      IF ttOfferCriteria.BeginStamp < deCurTime OR 
         bOfferCriteria.BeginStamp < deCurTime THEN
      DO:
         IF ttOfferCriteria.BeginStamp    NE bOfferCriteria.BeginStamp OR  
            ttOfferCriteria.CriteriaType  NE bOfferCriteria.CriteriaType OR
            ttOfferCriteria.IncludedValue NE bOfferCriteria.IncludedValue OR
            ttOfferCriteria.ExcludedValue NE bOfferCriteria.ExcludedValue THEN DO:
            ocError = "Cannot change active or past value".
            RETURN 1.
         END.
      END.
   END.

   RETURN 0.

END FUNCTION. 
    
/* YDR-328 */
FUNCTION fGetOfferDeferredPayment RETURNS DECIMAL
   (icOffer AS CHAR,
    ideOfferTs AS DEC,
    OUTPUT odeMonhlyFee AS DEC,
    OUTPUT oiMonths     AS INT,
    OUTPUT odeFinalFee  AS DEC):

   DEF BUFFER OfferItem FOR OfferItem.
   DEF BUFFER DayCampaign FOR DayCampaign.
   DEF BUFFER FeeModel FOR FeeModel.
   DEF BUFFER FMItem FOR FMItem.
      
   FOR EACH OfferItem NO-LOCK WHERE
            OfferItem.Brand = gcBrand AND
            OfferItem.Offer = icOffer AND
            OfferItem.BeginStamp <= ideOfferTS AND
            OfferItem.EndStamp >= ideOfferTS AND
            OfferItem.ItemType = "PerContract", 
      FIRST DayCampaign NO-LOCK WHERE
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = OfferItem.ItemKey AND
            DayCampaign.DCType = {&DCTYPE_INSTALLMENT},
      FIRST FeeModel NO-LOCK WHERE
            FeeModel.Brand = gcBrand AND
            FeeModel.FeeModel = DayCampaign.FeeModel,
      FIRST FMItem NO-LOCK WHERE
            FMItem.Brand = gcBrand AND
            FMItem.FeeModel = FeeModel.FeeModel:

      ASSIGN
         odeMonhlyFee = FMItem.Amount
         oiMonths = FMItem.FFItemQty
         odeFinalFee = OfferItem.Amount.

      RETURN (FMItem.Amount * FMItem.FFItemQty).
   END.
   
   RETURN 0.
END.

PROCEDURE pGetBundleInfo:
   DEF INPUT  PARAMETER liOrderID      AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER ocBundleInfo   AS CHAR NO-UNDO.

   FOR EACH OrderAction WHERE
            OrderAction.Brand    = gcBrand   AND
            OrderAction.OrderId  = liOrderID AND
            OrderAction.ItemType = "BundleItem" NO-LOCK:
      ocBundleInfo = ocBundleInfo + (IF ocBundleInfo > "" THEN "," ELSE "") + 
                     OrderAction.ItemKey + "=0.0". /* hard coded zero price */
   END. /* FOR EACH OrderAction WHERE */

END PROCEDURE. /* PROCEDURE pGetBundleInfo: */

&ENDIF
