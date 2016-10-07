/* ----------------------------------------------------------------------
  MODULE .......: order_data.i
  TASK .........: Various functions to get order data
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 31.03.15
  Version ......: xfera
----------------------------------------------------------------------- */
&IF "{&ORDER_DATA_I}" NE "YES"
&THEN
&GLOBAL-DEFINE ORDER_DATA_I YES

{commali.i}
{date.i}
{cparam2.i}
{fbundle.i}
{transname.i}
{ftaxdata.i}
{tmsconst.i}
{offer.i}
{terminal_financing.i}
{mnp.i}

FUNCTION fGetOrderDeliveryDateEstimation RETURNS DATE
  (INPUT iiOrderId AS INT):

   DEF VAR liCount AS INT NO-UNDO.
   DEF VAR liDays AS INT NO-UNDO.
   DEF VAR ldaDate AS DATE NO-UNDO.

   DEF BUFFER OrderCustomer FOR OrderCustomer.

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand = gcBrand AND
              OrderCustomer.OrderId = iiOrderId AND
              OrderCustomer.RowType = 1 NO-ERROR.

   IF AVAIL OrderCustomer THEN CASE OrderCustomer.Region:
      WHEN "07" THEN liDays = 5.
      WHEN "38" OR WHEN "35" OR WHEN "51" OR WHEN "52" THEN liDays = 7.
      OTHERWISE liDays = 3.
   END.
   ELSE liDays = 3.

   ldaDate = TODAY.
   DO liCount = 1 TO liDays:
      ldaDate = ldaDate + 1.
      ldaDate = fMNPHoliday(ldaDate,TRUE).
   END.

   RETURN ldaDate.

END.

FUNCTION fGetOfferSMSValues RETURNS LOGICAL
  (INPUT iiOrderId AS INT,
   OUTPUT ocTariffName AS CHAR,
   OUTPUT odeMFWithTax AS DEC,
   OUTPUT ocTaxZone AS CHAR,
   OUTPUT ocDeviceName AS CHAR,
   OUTPUT odeDeviceFee AS DEC):

   DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.
   DEF VAR lcTariff AS CHAR NO-UNDO.
   DEF VAR liLanguage AS INT NO-UNDO.
   DEF VAR ldaOrderDate AS DATE NO-UNDO.
   DEF VAR ldeTaxPerc AS DEC NO-UNDO.

   DEF BUFFER Order fOR Order.
   DEF BUFFER OrderCustomer fOR OrderCustomer.
   DEF BUFFER OrderAccessory fOR OrderAccessory.

   lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand = gcBrand AND
              Order.Orderid = iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN RETURN FALSE.

   FIND FIRST OrderCustomer OF Order NO-LOCK WHERE
              OrderCustomer.RowType = 1 NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN RETURN FALSE.

   fTs2Date(Order.Crstamp, OUTPUT ldaOrderDate).

   IF LOOKUP(Order.CLIType,lcBundleCLITypes) > 0 THEN
      lcTariff = fGetDataBundleInOrderAction(Order.OrderID,Order.CLIType).
   ELSE lcTariff  = Order.CLIType.

   find clitype NO-LOCK where
        clitype.brand = "1" and
        clitype.clitype = lcTariff no-error.

   liLanguage = INTEGER(OrderCustomer.Language) NO-ERROR.
   
   /* temp code */
   liLanguage = 1.

   ldeTaxPerc = fRegionTaxPerc(OrderCustomer.Region,
                               "1",
                               ldaOrderDate).

   ocTariffName = fGetItemName(gcBrand,
                               "CLIType",
                               lcTariff,
                               liLanguage,
                               ldaOrderDate).

   ocTaxZone = fGetItemName(gcBrand,
                            "TaxZone",
                            Region.TaxZone,
                            liLanguage,
                             ldaOrderDate).

   IF CLIType.CLIType EQ "TARJ7" OR 
      CLIType.CLIType EQ "TARJ9" THEN
      odeMFWithTax = (1 + ldeTaxPerc / 100) * CLIType.CommercialFee.
   ELSE IF CLiType.CompareFee > 0 THEN
      odeMFWithTax = (1 + ldeTaxPerc / 100) * CLIType.CompareFee.

   odeMFWithTax = ROUND(odeMFWithTax,2).
   
   IF CLIType.CLIType EQ "TARJ9" AND
      odeMFWithTax EQ 9.99 THEN odeMFWithTax = 10. /* 9.99 -> 10 */ 

   FIND FIRST OrderAccessory NO-LOCK WHERE
              OrderAccessory.Brand = gcBrand AND
              OrderAccessory.OrderId = Order.OrderId AND
              OrderAccessory.TerminalType = {&TERMINAL_TYPE_PHONE} NO-ERROR.
   IF AVAIL OrderAccessory THEN ASSIGN
      ocDeviceName = fGetItemName(gcBrand,
                               "BillItem",
                               OrderAccessory.ProductCode,
                               liLanguage,
                               ldaOrderDate)
      odeDeviceFee = OrderAccessory.Amount.

   ELSE ocDeviceName = "Solo SIM".

END.


FUNCTION fGetOrderInstallmentData RETURNS LOGICAL
  (INPUT iiOrderId AS INT,
   OUTPUT odeInstallmentMF AS DEC,
   OUTPUT odeRedidualFee AS DEC,
   OUTPUT oiMonths AS INT,
   OUTPUT odeTAE AS DEC,
   OUTPUT oiTIN AS INT,
   OUTPUT ocBankName AS CHARACTER,
   OUTPUT odeCommFee AS DEC):

   DEF VAR ldeRVPerc AS DEC NO-UNDO.
   DEF VAR ldeDeferredPayment AS DEC NO-UNDO.
   DEFINE VARIABLE ldaOrderDate AS DATE NO-UNDO.
   DEFINE VARIABLE lcBankCode AS CHARACTER NO-UNDO.
   DEF VAR lcDCEvent AS CHAR NO-UNDO.

   DEF BUFFER Order FOR Order.
   DEF BUFFER Reseller FOR Reseller.
   DEF BUFFER ResellerTF FOR ResellerTF.
   DEF BUFFER TFConf FOR TFConf.

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand = gcBrand AND
              Order.Orderid = iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN RETURN FALSE.

   fTs2Date(Order.Crstamp, OUTPUT ldaOrderDate).

   ldeDeferredPayment = fGetOfferDeferredPayment(Order.Offer,
                              Order.CrStamp,
                              OUTPUT odeInstallmentMF,
                              OUTPUT oiMonths,
                              OUTPUT odeRedidualFee).
   IF ldeDeferredPayment EQ 0 THEN RETURN FALSE.

   IF fOrderContainsFinancedTerminal(Order.OrderId,"PAYTERM") NE
      {&TF_STATUS_YOIGO} THEN DO:

   IF INDEX(Order.OrderChannel, "POS") = 0 THEN
      lcBankCode = {&TF_BANK_CETELEM}.
   ELSE
      FOR FIRST Reseller NO-LOCK WHERE
                Reseller.Brand = gcBrand AND
                Reseller.Reseller = Order.Reseller,
          FIRST ResellerTF NO-LOCK USE-INDEX ResellerTF WHERE
                ResellerTF.Brand = Reseller.Brand AND
                ResellerTF.Reseller = Reseller.Reseller AND
                ResellerTF.ValidFrom <= ldaOrderDate:
         lcBankCode = ResellerTF.TFBank.
      END.

      IF lcBankCode EQ "" THEN lcBankCode = "0000".

      ocBankName = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                    "FixedFee",
                                    "TFBank",
                                    lcBankCode).

      ldeRVPerc = TRUNC(odeRedidualFee /
                        (ldeDeferredPayment + odeRedidualFee) * 100 + 0.05,1).

      FOR FIRST TFConf NO-LOCK WHERE
                TFConf.RVPercentage = ldeRVPerc AND
                TFConf.ValidTo >= ldaOrderDate AND
                TFConf.ValidFrom <= ldaOrderDate:

         odeCommFee = ROUND((TFConf.CommFeePerc / 100) *
                            (ldeDeferredPayment + odeRedidualFee),2).
         odeTAE = TFConf.TAE.

      END.
      RETURN TRUE.
   END.
END FUNCTION.


FUNCTION fGetOrderOfferSMS RETURNS CHAR
  (INPUT iiOrderId AS INT,
   ilFirstMessage AS LOG):

   DEF VAR lcTariffName AS CHAR format "x(20)".
   DEF VAR lcDeviceName AS CHAR format "x(20)".
   DEF VAR ldeMFWithTax AS DEC.
   DEF VAR lcTaxZone AS CHAR.
   DEF VAR ldeDeviceFee AS DECIMAL NO-UNDO.
   DEF VAR ldeInstallmentAmt AS DEC.
   DEF VAR liInstallmentMonths AS INT.
   DEF VAR ldeTAEPerc AS DEC.
   DEF VAR liTIN AS INT.
   DEF VAR ldeCommFee AS DEC.
   DEF VAR lcBankName AS CHARACTER NO-UNDO.
   DEF VAR ldeResidualFee AS DECIMAL NO-UNDO.
   DEF VAR ldaDeliveryDate AS DATE NO-UNDO.
   DEF VAR liPermancyLength AS INTEGER NO-UNDO.
   DEF VAR ldePermanencyAmount AS DECIMAL NO-UNDO.

   DEF VAR lcTemplate AS CHARACTER NO-UNDO.
   DEF VAR lcRenewal AS CHARACTER NO-UNDO.
   DEF VAR lcTelesales AS CHARACTER NO-UNDO.
   DEF VAR lcTariffData AS CHARACTER NO-UNDO.
   DEF VAR lcInitialPayment AS CHARACTER NO-UNDO.
   DEF VAR lcInstallment AS CHARACTER NO-UNDO.
   DEF VAR lcPermanency AS CHARACTER NO-UNDO.
   DEF VAR lcOrderType AS CHARACTER NO-UNDO.
   DEF VAR ldReqStamp AS DECIMAL NO-UNDO.
   DEF VAR lcDiscount AS CHARACTER NO-UNDO.
   DEF VAR liLang AS INTEGER NO-UNDO.

   DEF BUFFER Order FOR Order.
   DEF BUFFER OrderCustomer FOR OrderCustomer.
   DEF BUFFER OfferItem FOR OfferItem.
   DEF BUFFER DiscountPlan FOR DiscountPlan.

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand = gcBrand AND
              Order.OrderId = iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN RETURN "".

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand = Order.Brand AND
              OrderCustomer.OrderID = Order.OrderID AND
              OrderCustomer.RowType = 1 NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN RETURN "".

   IF OrderCustomer.Language EQ "5"
   THEN liLang = 5.
   ELSE liLang = 1.

   /* temp code */
   liLang = 1.

   lcTemplate = fGetSMSTxt("OfferSMS",
                          TODAY,
                          liLang,
                          OUTPUT ldReqStamp).
   IF lcTemplate EQ ? OR lcTemplate EQ "" THEN RETURN "". 

   fGetOfferSMSValues(order.orderid,
                      OUTPUT lcTariffName,
                      OUTPUT ldeMFWithTax,
                      OUTPUT lcTaxZone,
                      OUTPUT lcDeviceName,
                      OUTPUT ldeDeviceFee).

   RUN offer_penaltyfee.p(Order.OrderID,
                        Output liPermancyLength,
                        OUTPUT ldePermanencyAmount).

   IF fGetOrderInstallmentData(
      order.orderid,
      OUTPUT ldeInstallmentAmt,
      OUTPUT ldeResidualFee,
      OUTPUT liInstallmentMonths,
      OUTPUT ldeTAEPerc,
      OUTPUT liTIN,
      OUTPUT lcBankName,
      OUTPUT ldeCommFee) THEN DO:

      lcInstallment = SUBST((IF liLang EQ 5
         THEN " &1&2 E/month x &3 months + &4 E Residual value."
         ELSE " &1&2 E/mes x &3 meses + &4 E Pago Final."),
         (IF ldeDeviceFee > 0 THEN "+" ELSE ""),
         ldeInstallmentAmt,
         liInstallmentMonths,
         ldeResidualFee).

       IF lcBankName NE "Yoigo" THEN
         lcInstallment = lcInstallment +
            SUBST((IF liLang EQ 5
                  THEN " Financed by &1 BANK, TAE &2% TIN &3, Comission &4%"
                  ELSE " Financiacion con &1 BANK, TAE &2% TIN &3, Comision aplazamiento &4E."),
                  lcBankName,
                  ldeTAEPerc,
                  liTIN,
                  TRIM(STRING(ldeCommFee, "->>9.99"))).
   END.

   IF Order.OrderType NE {&ORDER_TYPE_MNP} THEN
      lcOrderType = " Alta nueva".
   ELSE
      IF Order.OldPayType EQ FALSE AND
         Order.PayType EQ FALSE THEN lcOrderType = " Portabilidad de Contrato".
      ELSE lcOrderType = SUBST(" Portabilidad de &1 a &2",
         (IF Order.OldPayType THEN "Tarjeta" ELSE "Contrato"),
         (IF Order.PayType THEN "Tarjeta" ELSE "Contrato")).

   ASSIGN
      lcInitialPayment = SUBST((IF liLang EQ 5
                               THEN " &1 E Initial payment."
                               ELSE " &1 E Pago Inicial."), ldeDeviceFee) WHEN ldeDeviceFee > 0
      lcPermanency = SUBST((IF liLang EQ 5
                            THEN " with Permanency of &1 months because of the discount of &2 E made to the terminal"
                            ELSE " con Permanencia de &1 meses en Contrato por el descuento de &2 E el movil"),
                           liPermancyLength,
                           ldePermanencyAmount) WHEN ldePermanencyAmount > 0.

   ldaDeliveryDate = fGetOrderDeliveryDateEstimation(Order.OrderId).

   IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL} THEN
      lcRenewal =  (IF liLang EQ 5
                    THEN " Renewal"
                    ELSE " Renuevo").
   ELSE ASSIGN 
      lcTelesales = (If liLang EQ 5 THEN " of Yoigo is"
                     ELSE " de Yoigo es")
      lcTariffData = SUBST((IF liLang EQ 5
                            THEN " Tariff &1 &2 E/month &3 incl.+ "
                            ELSE " Tarifa &1 &2 E/mes &3 incl.+"),
                           lcTariffName,
                           ldeMFWithTax,
                           lcTaxZone).

   IF NOT ilFirstMessage THEN lcTemplate = lcTemplate +
      ". Si no nos contestas en 48h cancelaremos tu pedido".

   FOR FIRST OfferItem NO-LOCK WHERE
             OfferItem.Brand = gcBrand AND
             OfferItem.Offer  = Order.Offer AND
             OfferItem.ItemType = "DiscountPlan" AND
             OfferItem.BeginStamp <= Order.CrStamp AND
             OfferItem.EndStamp >= Order.CrStamp,
       FIRST DiscountPlan NO-LOCK WHERE
             DiscountPlan.Brand = gcBrand AND
             DiscountPlan.DPRuleID = OfferItem.ItemKey:

      IF OfferItem.Amount > 0 AND
         OfferItem.Periods > 0 THEN
      lcDiscount = SUBST((IF liLang EQ 5
                    THEN " Billing Promo discount &1 &2 x &3 months."
                    ELSE " Promo descuento en factura &1 &2 x &3 meses."),
                    OfferItem.Amount,
                    (IF DiscountPlan.DPUnit EQ "Percentage"
                     THEN "%"
                     ELSE "E"),
                    OfferItem.Periods).
   END.

   ASSIGN
      lcTemplate = REPLACE(lcTemplate,"#RENEWAL", lcRenewal)
      lcTemplate = REPLACE(lcTemplate,"#TELESALES", lcTelesales)
      lcTemplate = REPLACE(lcTemplate,"#CONTRACTID", Order.ContractId)
      lcTemplate = REPLACE(lcTemplate,"#TARIFF_DATA", lcTariffData)
      lcTemplate = REPLACE(lcTemplate,"#DEVICE_NAME", lcDeviceName)
      lcTemplate = REPLACE(lcTemplate,"#INITIAL_PAYMENT", lcInitialPayment)
      lcTemplate = REPLACE(lcTemplate,"#INSTALLMENTS", lcInstallment)
      lcTemplate = REPLACE(lcTemplate,"#PERMANENCY", lcPermanency)
      lcTemplate = REPLACE(lcTemplate,"#ORDERTYPE", lcOrderType)
      lcTemplate = REPLACE(lcTemplate,"#DISCOUNT", lcDiscount)
      lcTemplate = REPLACE(lcTemplate,"#DELIVERY_DATE",
                           string(ldaDeliveryDate,"99/99/9999")).

   RETURN lcTemplate.
END.

&ENDIF
