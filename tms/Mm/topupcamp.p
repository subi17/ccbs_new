/* ----------------------------------------------------------------------
  MODULE .......: topupcamp.p
  TASK .........: create topup according to a valid campaign
  CREATED ......: 02.12.06/aam 
  CHANGED ......: 03.01.07/aam pass amounts as cents to fAddTopUp
                  22.05.07/aam TaxZone to fAddTopup  
  Version ......: yoigo
-------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/ftaxdata.i}
{Func/ftopup.i}
{Func/cparam2.i}

DEF INPUT  PARAMETER iiMsSeq   AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiRequest AS INT  NO-UNDO. 

DEF VAR ldVatPerc      AS DEC  NO-UNDO.
DEF VAR ldAmount       AS DEC  NO-UNDO.
DEF VAR ldVatAmt       AS DEC  NO-UNDO. 
DEF VAR lcTaxZone      AS CHAR NO-UNDO.
DEF VAR lcCampaign     AS CHAR NO-UNDO.
DEF VAR lcParam        AS CHAR NO-UNDO.
DEF VAR lcPrefix       AS CHAR NO-UNDO.
DEF VAR ldaOrderDate   AS DATE NO-UNDO.
DEF VAR liTime         AS INT  NO-UNDO.

FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN RETURN "ERROR:Subscription not available".

/* normal orders */
ASSIGN
   lcParam      = "InitialTopUpSim"
   lcPrefix     = "997"
   ldaOrderDate = MobSub.ActivationDate.
   
FIND FIRST Order WHERE Order.MsSeq = iiMsSeq NO-LOCK NO-ERROR.

/* special cases */
IF AVAILABLE Order THEN DO:

   fSplitTS(Order.CrStamp, 
            OUTPUT ldaOrderDate, 
            OUTPUT liTime).

   FOR EACH OrderAccessory OF Order WHERE
            OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE}) NO-LOCK:

      /* pos-orders don't have a billing item, only imei code */
      IF (LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_INDIRECT}) > 0 
          AND OrderAccessory.IMEI > "") 
          OR
         (OrderAccessory.ProductCode > "" AND 
          CAN-FIND(FIRST BillItem WHERE
                         BillItem.Brand    = gcBrand AND
                         BillItem.BillCode = OrderAccessory.ProductCode AND
                         BillItem.BIGroup  = "7")) 
      THEN DO:
         lcParam = "InitialTopup".
         LEAVE.
      END.
   END.
   
   IF Order.OrderChannel = "Yoigo" AND Order.Salesman = "gift" THEN ASSIGN 
      lcParam  = "GiftOrderTopup"
      lcPrefix = "993".
   ELSE IF Order.OrderChannel = "pre-act" THEN ASSIGN 
      lcParam  = "PreActOrderTopup"
      lcPrefix = "992".
END.
      
lcCampaign = fCParamC(lcParam).
IF lcCampaign = ? THEN lcCampaign = "".

/* add initial topup for prepaid subscriptions */
FOR EACH CampRow NO-LOCK WHERE
         CampRow.Brand    = gcBrand        AND
         CampRow.CLIType  = MobSub.CLIType AND
         CampRow.CRowType = 5,
   FIRST Campaign OF CampRow NO-LOCK WHERE
         Campaign.ToDate   >= ldaOrderDate AND
         Campaign.FromDate <= ldaOrderDate AND
         (IF lcCampaign > "" 
          THEN Campaign.Campaign = lcCampaign
          ELSE TRUE),
    EACH FMItem NO-LOCK WHERE
         FMItem.Brand     = gcBrand               AND
         FMItem.FeeModel  = CampRow.CRowItem      AND
         FMItem.PriceList = "TopUp"               AND
         FMItem.ToDate   >= ldaOrderDate AND
         FMItem.FromDate <= ldaOrderDate,
   FIRST BillItem NO-LOCK WHERE
         BillItem.Brand    = gcBrand AND
         BillItem.BillCode = FMItem.BillCode:
         
   /* take vat out from amount */
   FIND PriceList WHERE
        PriceList.Brand     = gcBrand AND
        PriceList.PriceList = FMItem.PriceList NO-LOCK.
           
   /* taxcode */
   FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK.
   ASSIGN 
      lcTaxZone = fRegionTaxZone(Customer.Region)
      ldVatPerc = fTaxPerc(lcTaxZone,BillItem.TaxClass, TODAY).
      
   /* amount excluding vat and vat amount separately */
   IF PriceList.InclVat
   THEN ASSIGN ldAmount = ROUND(FMItem.Amount / (1 + ldVatPerc / 100),2)
               ldVatAmt = FMItem.Amount - ldAmount.
   ELSE ASSIGN ldAmount = FMItem.Amount
               ldVatAmt = ROUND(FMItem.Amount * ldVatPerc / 100,2).

   oiRequest = fAddTopUp(MobSub.MsSeq,
                         MobSub.CLI,
                         "RefillTRequest",
                         "WEB Order",
                         "RefillTRequest",
                         lcPrefix,
                         lcTaxZone,
                         ldAmount * 100,
                         ldVatAmt * 100).
         
END.
 
