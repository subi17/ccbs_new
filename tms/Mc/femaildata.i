/*
  femaildata.i    - 16.3.2015 - ilkkasav & kariaika.

  Module contains functions for fetching data to notification emails.
  Logic of function names:
  GetREQUIRED_DATA provides information that is marked in email with 
  bookmark #REQUIRED_DATA.
  Common parameters in functions:
    liLang - 1 =  Spanish, 5 = English
    olgErr - TRUE if error is detected  

  Modified:
*/

{Syst/commali.i}
{Func/refcode.i}
{Mm/fbundle.i}
{Mc/offer.i}
{Func/fbankdata.i}
{Mnp/mnp.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Func/profunc.i}
{Func/transname.i}
{Func/fixedlinefunc.i}
&SCOPED-DEFINE ORDERTYPE_MNP_EN "Portability"
&SCOPED-DEFINE ORDERTYPE_MNP_SP "Portabilidad"
&SCOPED-DEFINE ORDERTYPE_NEW_EN "New number"
&SCOPED-DEFINE ORDERTYPE_NEW_SP "Número nuevo"

DEF VAR ldeDeferredPayment AS DEC NO-UNDO.
DEF VAR liMsSeq       AS INT NO-UNDO.
DEF VAR ldeMonthlyFee AS DEC NO-UNDO.
DEF VAR liMonths      AS INT NO-UNDO.
DEF VAR ldeFinalFee   AS DEC NO-UNDO.
DEF VAR lcRegion      AS CHAR NO-UNDO.
DEF VAR lcDelRegion   AS CHAR NO-UNDO.
DEF VAR lcRegionName  AS CHAR NO-UNDO.
DEF VAR lcCRegionName AS CHAR NO-UNDO.
DEF VAR lcDelRegionName AS CHAR NO-UNDO.
DEF VAR liCustNum     AS INT NO-UNDO.
DEF VAR lcCustAddress AS CHAR NO-UNDO.
DEF VAR liLang        AS INT NO-UNDO.
DEF VAR lcCLIType     AS CHAR NO-UNDO.
/*for HTML tags*/
DEF VAR lcListBeginTag AS CHAR NO-UNDO.
DEF VAR lcBeginTag AS CHAR NO-UNDO.
DEF VAR lcMiddleTag AS CHAR NO-UNDO.
DEF VAR lcEndTag AS CHAR NO-UNDO.
DEF VAR ldeInvoiceTotal AS DEC NO-UNDO.

def buffer companycustomer for ordercustomer.
def buffer deliverycustomer for ordercustomer.

/*HTML tags for item list*/
lcListBeginTag = '<tr bgcolor="#f0f0f0"><td style="width:155px; text-align:left; padding:10px 5px 0 20px;"><tr>'.
lcBeginTag = '<tr bgcolor="#f0f0f0"><td style="width:155px; text-align:left; padding:0 0 0 20px;">'.
lcMiddleTag = '</td><td style="width:34px; text-align:right; padding:0 5px 0 0 ;"><strong>'.
lcEndTag = '&euro;</strong></td></tr>'.

ASSIGN Syst.Var:gcBrand = "1".


FUNCTION fGetOrderData RETURNS CHAR ( INPUT iiOrderId AS INT):
/* Check if order is needed to get */
   IF NOT AVAILABLE Order THEN DO:
      FIND Order WHERE
           Order.Brand   = Syst.Var:gcBrand AND
           Order.OrderID = iiOrderId NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Order THEN DO:
         RETURN ("Order " + STRING(iiOrderId) + " was not found.").
      END.

      FIND FIRST OrderCustomer NO-LOCK WHERE
                 OrderCustomer.Brand   = Syst.Var:gcBrand       AND
                 OrderCustomer.OrderID = Order.OrderID AND
                 OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.

      ASSIGN
         liCustNum = Order.CustNum
         lcRegion = OrderCustomer.Region
         lcDelRegion = OrderCustomer.Region   /* init to same as lcRegion */
         liLang = 1. /* INT(OrderCustomer.Language). Decision only spanish 
                        supported. YDR-1637 YTS-7046 */
         lcCustAddress = OrderCustomer.Address.
      
      FIND FIRST Region WHERE
         Region.Region = OrderCustomer.Region NO-LOCK NO-ERROR.

      IF AVAIL Region THEN
         lcRegionName = Region.RgName.
      ELSE lcRegionName = "".

      /* Check if complementary address line exist */
      /*IF OrderCustomer.AddressComp > "" THEN
         lcCustAddress = lcCustAddress + CHR(10) +
                         OrderCustomer.AddressComp.*/
      IF Order.MsSeq > 0 THEN DO:
         FIND MobSub WHERE MobSub.MsSeq = Order.MsSeq NO-LOCK NO-ERROR.
         IF AVAILABLE MobSub
         THEN ASSIGN liMsSeq = Mobsub.MsSeq
                     liCustNum = MobSub.CustNum.
      END.

      IF Order.CLIType > "" THEN lcCLIType = Order.CLIType.
      ldeDeferredPayment = fGetOfferDeferredPayment(Order.Offer,
                                                       Order.CrStamp,
                                                       OUTPUT ldeMonthlyFee,
                                                       OUTPUT liMonths,
                                                       OUTPUT ldeFinalFee).

      FIND FIRST OrderAccessory NO-LOCK WHERE
                 OrderAccessory.Brand  = Syst.Var:gcBrand AND
                 OrderAccessory.OrderID = Order.OrderID NO-ERROR.
      
      FIND FIRST companycustomer WHERE
                 companycustomer.Brand   = Order.Brand AND
                 companycustomer.OrderId = Order.OrderID AND
                 companycustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT} NO-LOCK NO-ERROR.
      
      FIND FIRST DeliveryCustomer WHERE
                 DeliveryCustomer.Brand   = Order.Brand AND
                 DeliveryCustomer.OrderId = Order.OrderID AND
                 DeliveryCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_DELIVERY} NO-LOCK NO-ERROR.           
   
   END.
   IF NOT AVAIL OrderCustomer THEN
      FIND FIRST OrderCustomer NO-LOCK WHERE
                 OrderCustomer.Brand   = Syst.Var:gcBrand       AND
                 OrderCustomer.OrderID = Order.OrderID AND
                 OrderCustomer.RowType = 1 NO-ERROR.
   IF AVAIL companycustomer THEN DO:
      FIND FIRST Region WHERE
         Region.Region = companycustomer.Region NO-LOCK NO-ERROR.
      IF AVAIL Region THEN
         lcCRegionName = Region.RgName.
      ELSE lcCRegionName = "".
   END.
   IF AVAIL DeliveryCustomer THEN DO:
      IF DeliveryCustomer.region > "" THEN
         lcDelRegion = DeliveryCustomer.region. /* if available fill actual */
      FIND FIRST Region WHERE
         Region.Region = DeliveryCustomer.Region NO-LOCK NO-ERROR.

      IF AVAIL Region THEN
         lcDelRegionName = Region.RgName.
      ELSE lcRegionName = "".
   END.
   RETURN "".
END.

FUNCTION fParseHTMLRow RETURNS CHARACTER
   (icRow AS CHAR):

   DEF VAR lcFinalRow AS CHAR NO-UNDO.
   DEF VAR lcTxt AS CHAR NO-UNDO.
   DEF VAR lcNbr AS CHAR NO-UNDO.
   DEF VAR liNbrPos AS INT NO-UNDO.

   icRow = RIGHT-TRIM(icRow, " ").
   icRow = RIGHT-TRIM(icRow, chr(10)).

   IF(SUBSTRING(icRow, (LENGTH(icRow) - 2), 3 ) EQ "EUR") THEN
      icRow = RIGHT-TRIM(icRow, " EUR").
   ELSE IF(SUBSTRING(icRow, (LENGTH(icRow) - 2), 3 ) EQ "ro;") THEN
      icRow = RIGHT-TRIM(icRow, " &euro;"). /* ends with HTML &euro; */
   ELSE RETURN "".

   /*Seek next ' ' from end of the string*/
   liNbrPos = R-INDEX(icRow, ' ' ).
   lcNbr = SUBSTRING(icRow, liNbrPos + 1).
   lcTxt = SUBSTRING(icRow, 1, ( liNbrPos - 1) ).

   lcFinalRow =  lcBeginTag + lcTxt + 
                 lcMiddleTag + lcNbr + 
                 lcEndTag.

   RETURN lcFinalRow.

END FUNCTION.

FUNCTION fParseHTMLPrices RETURNS CHARACTER
   (icData AS CHAR).

   DEF VAR liIndex AS INT NO-UNDO.
   DEF VAR lcRow AS CHAR NO-UNDO.
   DEF VAR lcRet AS CHAR NO-UNDO.
   
   liIndex = 1.
   DO WHILE liIndex < NUM-ENTRIES(icData, chr(10)):
      lcRow = ENTRY(liIndex,icData,chr(10)).
      /* Remove some lines from summary. These are displayed
         separately in template. */
      IF (lcRow > "") AND NOT(lcRow BEGINS "Pago ini" OR 
                              lcRow BEGINS "Initial pay" OR 
                              lcRow BEGINS "Mensual" OR
                              lcRow BEGINS "Monthly" OR
                              (liIndex = 1 AND 
                              lcRow BEGINS "Coste contra")) THEN DO:
         lcRow = fParseHTMLRow(lcRow).
         lcRet = lcRet + lcRow.
      END.
      liIndex = liIndex + 1.
   END.

   RETURN lcListBeginTag + lcRet.

END FUNCTION.

FUNCTION fBIName RETURNS CHARACTER
   (idtDate AS DATE):

   DEF VAR lcBiName AS CHAR NO-UNDO.

   lcBiName = fTranslationName(Syst.Var:gcBrand,
                               1,
                               BillItem.BillCode,
                               liLang,
                               idtDate).

   IF lcBiName = "" OR lcBiName = ? THEN lcBiName = BillItem.BIName.

   RETURN lcBiName.

END FUNCTION.

FUNCTION fGetOFEES_internal RETURNS CHAR (INPUT iiOrderNBR AS INT,
                                 OUTPUT olgErr AS LOGICAL,
                                 OUTPUT odInvTot AS DECIMAL):

   DEF VAR lcList        AS CHAR NO-UNDO.
   DEF VAR lcErr         AS CHAR NO-UNDO.
   DEF VAR lcTopupItems  AS CHAR NO-UNDO.
   DEF VAR ldAmt         AS DEC NO-UNDO.
   DEF VAR lcValue       AS CHAR NO-UNDO.
   DEF VAR ldvatperc     AS DEC NO-UNDO. 
   DEF VAR ldtOrder      AS DATE NO-UNDO. 
   DEF VAR ldtEventDate  AS DATE NO-UNDO.
   DEF VAR lcBundlesInfo AS CHAR NO-UNDO.
   DEF VAR liNumEntries  AS INT NO-UNDO.
   DEF VAR liCount       AS INT NO-UNDO.
   DEF VAR lcBundleInfo  AS CHAR NO-UNDO.
   DEF VAR lcBundle      AS CHAR NO-UNDO.
   DEF VAR ldeBundleFee  AS DEC  NO-UNDO.
   DEF VAR lcBundleBillItem AS CHAR NO-UNDO.
   DEF VAR llDSSPromotion AS LOG  NO-UNDO.
   DEF VAR ldaDSSPromotionFromDate AS DATE NO-UNDO.
   DEF VAR ldaDSSPromotionEndDate AS DATE NO-UNDO.
   DEF VAR lcTemp        AS CHAR NO-UNDO.
   DEF VAR lcTemp2       AS CHAR NO-UNDO.
   DEF VAR lipos         AS INT NO-UNDO.
   DEF VAR ldInvTot AS DEC NO-UNDO.

   lcErr = fGetOrderData (INPUT iiOrderNBR).
   ldtEventDate = TODAY.


         ASSIGN
            lcList       = ""
            ldInvTot     = 0
            odInvtot     = ldInvTot
            lcTopupItems = fCParamC("OrderTopUp") + "," +
                           fCParamC("OrderTopUpDisc").
         ldeDeferredPayment = fGetOfferDeferredPayment(Order.Offer,
                                                       Order.CrStamp,
                                                       OUTPUT ldeMonthlyFee,
                                                       OUTPUT liMonths,
                                                       OUTPUT ldeFinalFee).
         IF ldeFinalFee = ? THEN ldeFinalFee = 0.

         /* cash invoice has already been created */
         IF Order.InvNum > 0 THEN
         FOR FIRST Invoice NO-LOCK WHERE
                   Invoice.InvNum = Order.InvNum,
              EACH InvRow OF Invoice NO-LOCK,
             FIRST BillItem NO-LOCK WHERE
                   BillItem.Brand    = Syst.Var:gcBrand AND
                   BillItem.BillCode = InvRow.BillCode
         BY InvRow.InvRowNum:

            /* campaign topups are not displayed */
            IF LOOKUP(InvRow.BillCode,lcTopUpItems) > 0 THEN NEXT.

            /* amount with vat */
            ldAmt = InvRow.Amt.
            IF Invoice.VatIncl = FALSE THEN
               ldAmt = ROUND(ldAmt * (1 + InvRow.VatPerc / 100),2).

            /* billing item name */
            lcValue = fBIName(IF InvRow.ToDate NE ?
                              THEN InvRow.ToDate
                              ELSE Invoice.ToDate).

            /* Check if TopUpScheme has DisplayAmount to show */
            IF Order.CliType BEGINS "TARJ7" OR
               Order.CliType BEGINS "TARJ9" OR 
               Order.CliType BEGINS "TARJ10" OR 
               Order.CliType BEGINS "TARJ11" OR 
               Order.CliType BEGINS "TARJ12" OR
               Order.CliType BEGINS "TARJ13" THEN DO:

               IF InvRow.Amt >= 0 THEN
                  FOR EACH TopUpSchemeRow NO-LOCK WHERE
                           TopUpSchemeRow.BillCode = InvRow.BillCode AND
                           TopUpSchemeRow.Amount   = InvRow.Amt AND
                           TopUpSchemeRow.EndStamp >= Order.CrStamp AND
                           TopUpSchemeRow.BeginStamp <= Order.CrStamp AND
                           TopUpSchemeRow.DisplayAmount > 0:
                     ldAmt = TopUpSchemeRow.DisplayAmount.
                     LEAVE.
                  END.
               ELSE 
                  FOR EACH TopUpSchemeRow NO-LOCK WHERE
                           TopUpSchemeRow.DiscountBillCode = InvRow.BillCode AND
                           TopUpSchemeRow.DiscountAmount   = -1 * InvRow.Amt AND
                           TopUpSchemeRow.EndStamp >= Order.CrStamp AND
                           TopUpSchemeRow.BeginStamp <= Order.CrStamp AND
                           TopUpSchemeRow.DisplayAmount > 0:
                      ldAmt = -1 * TopUpSchemeRow.DisplayAmount.
                      LEAVE.
                  END.
            END.

            ASSIGN
               ldInvTot = ldInvTot + ldAmt
               odInvtot = ldInvTot
               ldAmt = ldAmt + ldeDeferredPayment + ldeFinalFee
                  WHEN BillItem.BiGroup EQ {&BITEM_GRP_TERMINAL} AND
                       ldeDeferredPayment > 0
               lcList   = lcList + STRING(lcValue,"X(40)") +
                                   STRING(ldAmt,"->>>>>9.99") +
                                   " &euro;" + CHR(10).
         END.
         /* fees have been created but amounts have been zero */
         ELSE IF
            CAN-FIND(FIRST SingleFee USE-INDEX HostTable WHERE
                           SingleFee.Brand     = Syst.Var:gcBrand AND
                           SingleFee.HostTable = "Order" AND
                           SingleFee.KeyValue  = STRING(Order.OrderID) AND
                           SingleFee.CalcObj   = "CASHFEE")
         THEN DO:
            FOR EACH SingleFee USE-INDEX HostTable WHERE
                     SingleFee.Brand     = Syst.Var:gcBrand AND
                     SingleFee.HostTable = "Order" AND
                     SingleFee.KeyValue  = STRING(Order.OrderID) AND
                     SingleFee.CalcObj   = "CASHFEE",
               FIRST BillItem NO-LOCK WHERE
                     BillItem.Brand    = Syst.Var:gcBrand AND
                     BillItem.BillCode = SingleFee.BillCode
               BY SingleFee.BillCode:

               /* campaign topups are not displayed */
               IF LOOKUP(SingleFee.BillCode,lcTopUpItems) > 0 THEN NEXT.

               /* amount with vat */
               ldAmt = SingleFee.Amt.
               IF SingleFee.VatIncl = FALSE THEN DO:
                  /*FIND FIRST OrderCustomer OF Order NO-LOCK WHERE
                             OrderCustomer.RowType = 1 NO-ERROR.*/
                  ldVatPerc = fRegionTaxPerc(IF AVAILABLE OrderCustomer
                                             THEN OrderCustomer.Region
                                             ELSE "",
                                             BillItem.TaxClass,
                                             ldtOrder).
                  ldAmt = ROUND(ldAmt * (1 + ldVatPerc / 100),2).
               END.

               /* billing item name */
               lcValue = fBIName(ldtEventDate).

               ASSIGN
                  ldInvTot = ldInvTot + ldAmt
                  odInvtot = ldInvTot
                  ldAmt = ldAmt + ldeDeferredPayment + ldeFinalFee
                     WHEN BillItem.BiGroup EQ {&BITEM_GRP_TERMINAL} AND
                          ldeDeferredPayment > 0
                  lcList   = lcList + STRING(lcValue,"X(40)") +
                                      STRING(ldAmt,"->>>>>9.99") +
                                     " &euro;" + CHR(10).
            END.
         END.

         /* fees have not been created yet */
         ELSE DO:
            /* make virtual creation */
            RUN Mc/cashfee.p (Order.OrderID,
                         4,     /* leave out campaign topups */
                         OUTPUT lcList,
                         OUTPUT ldInvTot,
                         OUTPUT lcErr).
            odInvtot = ldInvTot.
            IF lcErr <> "" THEN
               Func.Common:mWriteMemo("Order",
                                STRING(Order.OrderID),
                                0,
                                "Confirmation Email Cash Invoice Data Failed",
                                lcErr).
         END.

         FOR FIRST OfferItem WHERE
                   OfferItem.Brand       = Syst.Var:gcBrand          AND
                   OfferItem.Offer       = Order.Offer      AND
                   OfferItem.ItemType    = "ServicePackage" AND
                   OfferItem.ItemKey     = "BB"             AND
                   OfferItem.EndStamp   >= Order.CrStamp    AND
                   OfferItem.BeginStamp <= Order.CrStamp NO-LOCK:

             lcList  = lcList + STRING(fTeksti(513,liLang),"X(40)") +
                       STRING(OfferItem.Amount,"->>>>>9.99") +
                       " &euro;" + CHR(10).
         END. /* FOR FIRST OfferItem WHERE */

         /* Bundle Information in product list */
         RUN pGetBundleInfo(INPUT Order.OrderID, OUTPUT lcBundlesInfo).

         liNumEntries = NUM-ENTRIES(lcBundlesInfo).
         DO liCount = 1 TO liNumEntries:
            lcBundleInfo = ENTRY(liCount,lcBundlesInfo).
            IF lcBundleInfo > "" AND NUM-ENTRIES(lcBundleInfo,"=") = 2 THEN DO:
               ASSIGN lcBundle     = ENTRY(1,lcBundleInfo,"=")
                      ldeBundleFee = DECIMAL(ENTRY(2,lcBundleInfo,"=")) NO-ERROR.
               IF ERROR-STATUS:ERROR OR ldeBundleFee = ? THEN ldeBundleFee = 0.

               CASE lcBundle:
                  WHEN {&DSS} THEN
                     lcList = lcList + STRING(fTeksti(526,liLang),"X(40)") +
                              STRING(ldeBundleFee,"->>>>>9.99") + " &euro;" + CHR(10).
                  WHEN "MDUB2" OR WHEN "MDUB3" OR WHEN "MDUB4" OR
                  WHEN "CONTDATA" OR WHEN "CONTD2" OR WHEN "CONTD4" THEN
                     llDSSPromotion = TRUE.
                  OTHERWISE NEXT.
               END CASE. /* CASE lcBundle: */
            END. /* IF lcBundleInfo > "" THEN DO: */
         END. /* DO liCount = 1 TO liNumEntries: */

         /* YDR-470 - DSS Free Promotion */
         IF INDEX(lcBundlesInfo,{&DSS}) > 0 AND llDSSPromotion AND
            ldtOrder >= ldaDSSPromotionFromDate AND
            ldtOrder <= ldaDSSPromotionEndDate THEN
            lcList = lcList + fTeksti(531,liLang) + CHR(10).

         /* total row */
         IF lcList > "" THEN
            lcList = lcList + (IF ldeDeferredPayment > 0 THEN CHR(10) ELSE "") +
                              STRING(fTeksti((IF ldeDeferredPayment > 0 THEN
                                              515 ELSE 80),liLang),"X(40)") +
                              STRING(ldInvTot,"->>>>>9.99") +
                              " &euro;".

         IF ldeDeferredPayment > 0 THEN DO:
            IF ldeFinalFee > 0 THEN
               lcTemp = fTeksti(556,liLang).
            ELSE
               lcTemp = fTeksti(516,liLang).

            ASSIGN
               lcTemp = REPLACE(lcTemp,"#AMOUNT",
                             TRIM(STRING(ldeMonthlyFee,"->>>>>9.99")))
               lcTemp = REPLACE(lcTemp,"#MONTHS", STRING(liMonths))
               lcTemp = REPLACE(lcTemp,"#XX", STRING(liMonths + 1))
               lcTemp2 = "".

            /* split to fixed with lines */
            DO liPos = 1 to num-entries(lcTemp,CHR(10)):
               lcTemp2 = lcTemp2 + CHR(10) +
                         STRING(ENTRY(liPos,lcTemp,CHR(10)),"X(40)").
               IF liPos = 2 THEN
                  lcTemp2 = lcTemp2 +
                            STRING(ldeDeferredPayment,"->>>>>9.99") + " &euro;".
               ELSE IF liPos = 3 AND ldeFinalFee > 0 THEN
                  lcTemp2 = lcTemp2 +
                            STRING(ldeFinalFee,"->>>>>9.99") + " &euro;".
            END.
            lcList = lcList + lcTemp2.
         END.

         /* YDR-737 & YDR-1065 */
         IF INDEX(lcList,"iPhone") > 0 AND Order.CrStamp < 20130701 THEN
            FOR FIRST OfferItem NO-LOCK WHERE
                      OfferItem.Brand = Syst.Var:gcBrand AND
                      OfferItem.Offer = Order.Offer AND
                      OfferItem.ItemType = "Percontract" AND
                      OfferItem.ItemKey = "PAYTERM24_25" AND
                      OfferItem.EndStamp >= Order.CrStamp AND
                      OfferItem.BeginStamp <= Order.CrStamp:
               lcTemp = fTeksti(534,liLang).
               IF lcTemp > "" THEN
                  lcList = lcList + CHR(10) + CHR(10) + lcTemp.
            END.
         ldeInvoiceTotal = ldInvTot.
         lcList = REPLACE(lcList,"euros","&euro;"). 
         RETURN lcList.

END.

PROCEDURE pGetLanguage:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   lcResult = STRING(liLang).
END.

PROCEDURE pGetContactNbr:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderCustomer THEN
      lcResult = OrderCustomer.mobilenumber.
END.

PROCEDURE pGetOContactNbr:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderCustomer THEN
      lcResult = OrderCustomer.FixedNumber.
END.

PROCEDURE pGetCompany:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN
      lcResult = OrderCustomer.company.
END.

PROCEDURE pGetCompanyAddr:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL companycustomer THEN
      lcResult = companycustomer.Address.
   ELSE IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN
      lcResult = OrderCustomer.Address.
END.

PROCEDURE pGetDelContactNbr:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL DeliveryCustomer THEN
      lcResult = DeliveryCustomer.mobilenumber.
END.

PROCEDURE pGetDelLastName:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL DeliveryCustomer THEN
      lcResult = DeliveryCustomer.surname1 + " " + DeliveryCustomer.surname2.
   ELSE IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN
      lcResult = OrderCustomer.surname1 + " " + OrderCustomer.surname2.
END.

PROCEDURE pGetDelFirstName:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL DeliveryCustomer THEN
      lcResult = DeliveryCustomer.firstname.
   ELSE IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN
      lcResult = OrderCustomer.firstname.
END.

PROCEDURE pGetDelCustId:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL DeliveryCustomer THEN
      lcResult = DeliveryCustomer.custId.
END.

PROCEDURE pGetDelCustIdType:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL DeliveryCustomer THEN
      lcResult = DeliveryCustomer.custIdType.
END.

PROCEDURE pGetTerminal:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderAccessory THEN 
      lcResult = OrderAccessory.model + " " + 
             OrderAccessory.modelcolor.
END.

PROCEDURE pGetTerminalAmount:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderAccessory AND OrderAccessory.Amount > 0 THEN
      lcResult = STRING(OrderAccessory.Amount).
END.

PROCEDURE pGetTerminalDiscount:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderAccessory AND OrderAccessory.Discount > 0 THEN
      lcResult = STRING(OrderAccessory.Discount).
END.

PROCEDURE pGetFIRSTNAME:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderCustomer THEN
      lcResult = OrderCustomer.firstname.
END.

PROCEDURE pGetLastName:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderCustomer THEN
      lcResult = OrderCustomer.surname1 + " " + OrderCustomer.surname2.
END.

PROCEDURE pGetCustId:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErrtxt AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderCustomer THEN
      lcResult = OrderCustomer.custId.
END.

PROCEDURE pGetCustIdType:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErrtxt AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderCustomer THEN
      lcResult = OrderCustomer.custIdType.
END.


PROCEDURE pGetCIDType:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderCustomer THEN
      lcResult = OrderCustomer.custIdType.
END.

PROCEDURE pGetEmail:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderCustomer THEN
      lcResult = OrderCustomer.email.
END.

PROCEDURE pGetDelEmail:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL DeliveryCustomer THEN
      lcResult = DeliveryCustomer.email.
END.

PROCEDURE pGetCFIRSTNAME:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL companycustomer THEN
      lcResult = companycustomer.firstname.
   ELSE IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN
      lcResult = ordercustomer.firstname.  
END.

PROCEDURE pGetCLastName:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL companycustomer THEN
      lcResult = companycustomer.surname1 + " " + companycustomer.surname2.
   ELSE IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN
      lcResult = OrderCustomer.surname1 + " " + OrderCustomer.surname2.
END.

PROCEDURE pGetCMOBILE:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErrtxt AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL companycustomer THEN
      lcResult = companycustomer.MobileNumber.
   ELSE IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN
      lcResult = OrderCustomer.MobileNumber.   
END.

PROCEDURE pGetCAddress:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR compAddr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL companycustomer THEN DO:
      /*compAddr = CompanyCustomer.address.
      IF CompanyCustomer.AddressComp > "" THEN
                  compAddr = compAddr + CHR(10) +
                             CompanyCustomer.AddressComp.*/
      lcResult = CompanyCustomer.address.
   END.
   ELSE IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN
      lcResult = lcCustAddress.  
END.

PROCEDURE pGetCCustPost:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR lcCoRegionName AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   
   IF AVAIL companycustomer THEN DO:
      lcResult =  companycustomer.zipcode + " " +
             companycustomer.postOffice + " " + lcCRegionName.
   END.
   ELSE IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN DO:
      FIND FIRST Region WHERE
         Region.Region = OrderCustomer.Region NO-LOCK NO-ERROR.

      IF AVAIL Region THEN
         lcCoRegionName = Region.RgName.
      ELSE lcCoRegionName = "".

      lcResult =  OrderCustomer.zipcode + " " +
             OrderCustomer.postOffice + " " + lcCoRegionName.
   END.
END.

PROCEDURE pGetCEmail:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL companycustomer THEN
      lcResult =  companycustomer.email.
   ELSE IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN
      lcResult = OrderCustomer.email.  
END.

PROCEDURE pGetOBankdata:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcBankAcc AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF (Order.paytype) THEN lcBankAcc = "". /* Info needed only for postpaid */
   ELSE DO: /* postpaid */
      /* format bank account */
      IF AVAIL OrderCustomer THEN
         lcBankAcc = OrderCustomer.BankCode.
      IF LENGTH(lcBankAcc) = 20 THEN
         lcBankAcc = SUBSTRING(lcBankAcc,1,4) +
                        " **** ** ******" +
                        SUBSTRING(lcBankAcc,17,4).
      ELSE IF LENGTH(lcBankAcc) = 24 THEN
         lcBankAcc = SUBSTRING(lcBankAcc,1,4) +
                        " **** **** ** ******" +
                        SUBSTRING(lcBankAcc,21,4).
   END.
   lcResult = lcBankAcc.
  
END.

PROCEDURE pGetMandate:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcBankAcc AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR lcMandateId AS CHAR NO-UNDO.
   DEF VAR ldaMandateDate AS DATE NO-UNDO.

   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF (Order.paytype) THEN lcBankAcc = "". /* Info needed only for postpaid */
   ELSE DO: /* postpaid */
      /* format bank account */
      
      fGetOrderMandateId(BUFFER Order,
                         OUTPUT lcMandateId,
                         OUTPUT ldaMandateDate).
   END.
   lcResult = lcMandateId.

END.

PROCEDURE pGetCustAddr:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).  
   lcResult = lcCustAddress.
END.

PROCEDURE pGetCustCompAddr:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderCustomer THEN
      lcResult = OrderCustomer.addressComp.
END.

PROCEDURE pGetCompCustId:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL companycustomer THEN
      lcResult = companycustomer.CustId.
   ELSE IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN
      lcResult = OrderCustomer.CustId.
END.

PROCEDURE pGetCompCustIdType:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL companycustomer THEN
      lcResult = companycustomer.CustIdType.
   ELSE IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN
      lcResult = OrderCustomer.CustIdType.
END.

PROCEDURE pGetCustPost:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderCustomer THEN
      lcResult =  OrderCustomer.zipcode + " " +
             OrderCustomer.postOffice + " " + lcRegionName.
END.

PROCEDURE pGetCustRegion:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL OrderCustomer THEN
      lcResult = OrderCustomer.region.
END.

PROCEDURE pGetFoundate:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF AVAIL companycustomer THEN
      lcResult = STRING(companycustomer.FoundationDate,"99-99-9999").
   ELSE IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF" THEN
      lcResult = STRING(OrderCustomer.FoundationDate,"99-99-9999").
END.

PROCEDURE pGetOrderType:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   
   IF Order.Ordertype = {&ORDER_TYPE_MNP} THEN DO: 
      IF (liLang = 5) THEN lcResult = {&ORDERTYPE_MNP_EN}.
      ELSE lcResult = {&ORDERTYPE_MNP_SP}.
   END.
   ELSE DO:
      IF (liLang = 5) THEN lcResult = {&ORDERTYPE_NEW_EN}.
      ELSE lcResult = {&ORDERTYPE_NEW_SP}.
   END. 
END.


/* Invoice deliver type */
/*Todo: Think if the invoice type texts should be added to table HdrText
        for fetching correct text in correct language*/
PROCEDURE pGetDeltype:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

/*/* Invoice delivery types */
&GLOBAL-DEFINE INV_DEL_TYPE_PAPER 1
&GLOBAL-DEFINE INV_DEL_TYPE_EMAIL 2
&GLOBAL-DEFINE INV_DEL_TYPE_FUSION_EMAIL 3
&GLOBAL-DEFINE INV_DEL_TYPE_SMS 4
&GLOBAL-DEFINE INV_DEL_TYPE_NO_DELIVERY 10
&GLOBAL-DEFINE INV_DEL_TYPE_EMAIL_PENDING 11
&GLOBAL-DEFINE INV_DEL_TYPE_NO_TRAFFIC 12
&GLOBAL-DEFINE INV_DEL_TYPE_FUSION_EMAIL_PENDING 13
*/
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   
   IF NOT(Order.paytype) THEN DO: /* only for postpaid */
      IF OrderCustomer.DelType EQ {&INV_DEL_TYPE_PAPER} THEN DO:
            IF liLang = 5 THEN lcResult = "paper".
            ELSE lcResult = "en papel".
      END.   
      ELSE IF OrderCustomer.DelType EQ {&INV_DEL_TYPE_EMAIL} OR
              OrderCustomer.DelType EQ {&INV_DEL_TYPE_FUSION_EMAIL} OR
              OrderCustomer.DelType EQ {&INV_DEL_TYPE_SMS} OR
              OrderCustomer.DelType EQ {&INV_DEL_TYPE_ESI} THEN DO:
         IF liLang = 5 THEN lcResult = "electronic".
         ELSE lcResult =  "electrónica".
      END.
   END.
END.

PROCEDURE pGetMSISDN:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   lcResult = Order.CLI.
END.

PROCEDURE pGetRECOMMENDER:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   lcResult = Order.Referee.
END.

PROCEDURE pGetICCID:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF (Order.Ordertype = {&ORDER_TYPE_MNP} AND (Order.Oldpaytype)) THEN 
      lcResult = Order.oldICC.
END.

/*Financed payment: #LEGALFINANCING 
(only in the case of financed payment. If there is no financed payment, do not include this)*/
PROCEDURE pGetLEGALFINANCING:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   DEF VAR ldeMonthlyFee AS DECIMAL NO-UNDO.
   DEF VAR liMonths AS INTEGER NO-UNDO.
   DEF VAR lcList        AS CHAR NO-UNDO.
   DEF VAR ldeRVPerc AS DECIMAL NO-UNDO.
   DEF VAR ldtOrder AS DATE NO-UNDO.
   DEF VAR liOrderTime AS INT NO-UNDO.
   DEF VAR ldeCommFee AS DECIMAL NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).

   Func.Common:mSplitTS(Order.CrStamp,
               OUTPUT ldtOrder,
               OUTPUT liOrderTime).


   lcList = "".
   IF ldeDeferredPayment > 0 THEN DO:
      /*FIND FIRST OrderCustomer OF Order NO-LOCK WHERE
                 OrderCustomer.RowType = 1 NO-ERROR.*/
      IF AVAIL OrderCustomer AND OrderCustomer.CustIdType = "CIF"
      THEN DO:
         IF ldeFinalFee > 0 THEN
            ASSIGN lcList = CHR(10) +  fTeksti(570,liLang)
                   lcList = REPLACE(lcList,"#XX",STRING(liMonths + 1)).
         ELSE
            ASSIGN lcList = CHR(10) + fTeksti(569,liLang).
      END.
      ELSE DO:
         ldeRVPerc = TRUNC(ldeFinalFee /
                           (ldeDeferredPayment + ldeFinalFee) * 100 + 0.05,1).
         FIND FIRST TFConf NO-LOCK WHERE
                    TFConf.RVPercentage = ldeRVPerc AND
                    TFConf.ValidTo >= ldtOrder AND 
                    TFConf.ValidFrom <= ldtOrder NO-ERROR.
         IF NOT AVAIL TFConf THEN
            lcList = "".
         ELSE DO:
            ldeCommFee = ROUND((TFConf.CommFeePerc / 100) *
                               (ldeDeferredPayment + ldeFinalFee),2).
            IF ldeFinalFee > 0 THEN
               ASSIGN lcList = CHR(10) + fTeksti(571,liLang).
            ELSE
                ASSIGN lcList = CHR(10) + fTeksti(572,liLang).
                ASSIGN lcList = REPLACE(lcList,"#LOAN_FEE",TRIM(STRING(ldeDeferredPayment + ldeFinalFee,"->>9.99")))
                       lcList = REPLACE(lcList,"#TAE",TRIM(STRING(TFConf.TAE,"9.99")))
                       lcList = REPLACE(lcList,"#CF_PER",TRIM(STRING(TFConf.CommFeePerc,"9.99")))
                       lcList = REPLACE(lcList,"#CF_FEE",TRIM(STRING(ldeCommFee,"->>9.99")))
                       lcList = REPLACE(lcList,"#TOTAL_FEE",TRIM(STRING(ldeDeferredPayment + ldeFinalFee + ldeCommFee,"->>9.99"))).
         END.
      END.
   END.
   lcResult = lcList.
END.


/*Data handling functions:*/

PROCEDURE pGetORDER_DATE:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR ldtOrderDate AS DATETIME NO-UNDO.
   DEF VAR liOrderTime AS INT NO-UNDO.
   DEF VAR lcMonth AS CHAR NO-UNDO.
   DEF VAR lcDate AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).

   olgErr = FALSE.
   Func.Common:mSplitTS(Order.CrStamp,
               OUTPUT ldtOrderDate,
               OUTPUT liOrderTime).

   lcMonth = fTeksti(542 + MONTH(ldtOrderDate), liLang ).
   lcDate = SUBST("&1 &2 &3", DAY(ldtOrderDate), LC(lcMonth), YEAR(ldtOrderDate)).

   lcResult = lcDate.
END. /*GetORDER_DATA*/

PROCEDURE pGetCONTACT:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).

   IF fGetSegment(liCustNum, Order.OrderID) BEGINS "SOHO" OR
      fGetSegment(liCustNum, Order.OrderID) BEGINS "PRO-SOHO" THEN
      lcResult = REPLACE(fTeksti(577, liLang),"1707","1726").
   ELSE lcResult = fTeksti(577, liLang).

END. /*GetCONTACT*/

PROCEDURE pGetRETURN_RIGHTS:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).

   IF fGetSegment(liCustNum, Order.OrderID) BEGINS "SOHO" OR
      fGetSegment(liCustNum, Order.OrderID) BEGINS "PRO-SOHO" THEN
      lcResult = "".
   ELSE lcResult = fTeksti(578, liLang).

END. /*getRETURN_RIGHT*/

PROCEDURE pGetBOTTOM_BAR:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).

   IF fGetSegment(liCustNum, Order.OrderID) BEGINS "SOHO" OR
      fGetSegment(liCustNum, Order.OrderID) BEGINS "PRO-SOHO" THEN
      lcResult = fTeksti(580, liLang).
   ELSE lcResult = fTeksti(579, liLang).

END. /*GetBOTTOM_BAR*/


PROCEDURE pGetDELIVERY_DATE:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   DEF VAR liDays AS INT NO-UNDO.
   DEF VAR ldaDate AS DATE NO-UNDO.
   DEF VAR liCount AS INT  NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR ldePortingTime AS DEC NO-UNDO.
   DEF VAR lcProduct AS CHAR NO-UNDO.
   DEF VAR lcMonth AS CHAR NO-UNDO.
   DEF VAR ldaMNP AS DATE NO-UNDO. 
   DEF VAR lcSIMonlyMNP  AS CHAR NO-UNDO.
   lcSIMonlyMNP =   TRIM(fCParamC("SIMonlyMNPorder")).

   lcErr = fGetOrderData (INPUT iiOrderNBR).

   IF fIsConvergenceTariff(Order.CLIType) THEN
      lcResult = (IF liLang EQ 5
                  THEN "For new number fixed line takes 9-10 days and for portability number it takes 12-17 days"
                  ELSE "Si es nueva numeración fija, de 9-10 días y de 12-17 días si es portabilidad").
   ELSE IF Order.OrderType = 1 THEN DO:
      IF Order.PortingDate <> ? THEN
         ldePortingTime = Func.Common:mMake2DT(Order.PortingDate,0).
      IF AVAIL OrderAccessory THEN
         lcProduct = "T".
      ELSE
         lcProduct = "S".

      IF ldePortingTime <= Func.Common:mMakeTS() THEN
         ldamnp = fmnpchangewindowdate(
                             Func.Common:mMakeTS(),
                             order.orderchannel,
                             lcDelRegion,
                             lcProduct,
                             Order.CliType,
                             Order.DeliveryType).
      ELSE ldaMNP = Order.PortingDate.
      ldaDate = ldaMNP - 1.
      ldaDate = fMNPHoliday(ldaDate,FALSE).
      lcResult = Func.Common:mDateFmt(ldaDate,"dd/mm/yy").
   END.
   ELSE DO:
      IF lcDelRegion > "" THEN CASE lcDelRegion:
         WHEN "07"
         THEN DO:
            IF Order.DeliveryType = {&ORDER_DELTYPE_POS}
            THEN liDays = 4.
            ELSE liDays = 5.
         END.
         WHEN "38" OR WHEN "35" OR WHEN "51" OR WHEN "52"
         THEN DO:
            IF Order.DeliveryType = {&ORDER_DELTYPE_POS}
            THEN liDays = 6.
            ELSE liDays = 7.
         END.
         OTHERWISE DO:
            IF Order.DeliveryType = {&ORDER_DELTYPE_POS}
            THEN liDays = 2.
            ELSE liDays = 3.
         END.
      END. 
      ELSE DO:
         IF Order.DeliveryType = {&ORDER_DELTYPE_POS}
         THEN liDays = 6.
         ELSE liDays = 3.
      END.
      ldaDate = TODAY.
      DO liCount = 1 TO liDays:
         ldaDate = ldaDate + 1.
         ldaDate = fMNPHoliday(ldaDate,TRUE).
      END.
      lcResult = Func.Common:mDateFmt(ldaDate,"dd/mm/yy").
   END.

END. /*GetDELIVERY_DATE*/

PROCEDURE pGetLINETYPE:
   
   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   IF fIsConvergenceTariff(Order.CLIType) THEN
      lcResult = (IF liLang EQ 5 THEN " for mobile"
                  ELSE " del móvil").
   ELSE lcResult = "".

END.

PROCEDURE pGetMNP_DATE:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR lcMonth AS CHAR NO-UNDO.
   DEF VAR ldaMNP AS DATE NO-UNDO.
   DEF VAR lcList AS CHAR NO-UNDO.
   DEF VAR ldePortingTime AS DEC NO-UNDO.
   DEF VAR lcProduct AS CHAR NO-UNDO.

   lcErr = fGetOrderData (INPUT iiOrderNBR).

   IF Order.PortingDate <> ? THEN
      ldePortingTime = Func.Common:mMake2DT(Order.PortingDate,0).

   IF AVAIL OrderAccessory THEN
      lcProduct = "T".
   ELSE
      lcProduct = "S".

   IF ldePortingTime <= Func.Common:mMakeTS() THEN
      ldamnp = fmnpchangewindowdate(
                          Func.Common:mMakeTS(),
                          order.orderchannel,
                          lcDelRegion,
                          lcProduct,
                          Order.CliType,
                          Order.DeliveryType).
   ELSE ldaMNP = Order.PortingDate.

   lcMonth = fTeksti(542 + MONTH(ldaMNP),(IF liLang EQ 5 THEN 5 ELSE 1)).
   lcList = SUBST("&1 &2 &3", DAY(ldaMNP), LC(lcMonth), YEAR(ldaMNP)).

   lcResult = lcList + " 02:00".
END.

PROCEDURE pGetDELADDR:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   DEF VAR lcDelAddress AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).

   IF Order.DeliverySecure EQ 1 OR
      Order.DeliverySecure EQ 2 OR
      Order.DeliveryType EQ {&ORDER_DELTYPE_POST} OR
      Order.DeliveryType EQ {&ORDER_DELTYPE_KIALA} OR
      Order.DeliveryType EQ {&ORDER_DELTYPE_POS} THEN
      lcDelAddress = "". /* YPR-2660 */
   ELSE DO:
      /* separate delivery address */
      IF AVAILABLE DeliveryCustomer THEN DO:
         lcDelAddress = DeliveryCustomer.Address.
         /*IF DeliveryCustomer.AddressComp > "" THEN
            lcDelAddress = lcDelAddress + CHR(10) +
                           DeliveryCustomer.AddressComp.*/
      END.
      ELSE lcDelAddress = "".
   END.
   IF (lcDelAddress EQ lcCustAddress) THEN lcDelAddress = "". 

   lcResult = lcDelAddress.
END. /*GetDELADDR*/

PROCEDURE pGetDELPOST:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   DEF VAR lcDelAddress AS CHAR NO-UNDO.
   DEF VAR lcDelPost AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.

   IF AVAIL DeliveryCustomer THEN DO:
      lcErr = fGetOrderData (INPUT iiOrderNBR).

      IF Order.DeliverySecure EQ 1 OR
         Order.DeliverySecure EQ 2 OR
         Order.DeliveryType EQ {&ORDER_DELTYPE_POST} OR
         Order.DeliveryType EQ {&ORDER_DELTYPE_KIALA} OR
         Order.DeliveryType EQ {&ORDER_DELTYPE_POS} THEN
         lcDelPost = "".
      ELSE DO:
         /* separate delivery address */
         IF AVAILABLE DeliveryCustomer THEN ASSIGN 
            lcDelAddress = DeliveryCustomer.Address    
            lcDelPost    = DeliveryCustomer.ZipCode + " " +
                           DeliveryCustomer.PostOffice + " " +
                           lcDelRegionName.
         ELSE lcDelPost = "".
      END.
      /* Also post number is not needed if delivery address is same
         as orderer address. */
      IF (lcDelAddress EQ lcCustAddress) THEN lcDelPost = "". 
   END.
   lcResult = lcDelPost.
END. /*GetDELPOST*/

PROCEDURE pGetUPSHOURS:   /* UPS and Correos open hours */

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   DEF VAR lcUPSHours    AS CHAR NO-UNDO.
   DEF VAR lcErr         AS CHAR NO-UNDO.
   DEF VAR liCount       AS INT  NO-UNDO.
   DEF VAR liTimeCount   AS INT  NO-UNDO.
   DEF VAR lcDailyHours  AS CHAR NO-UNDO.
   DEF VAR lcTempHours   AS CHAR NO-UNDO.
   DEF VAR lcDay         AS CHAR NO-UNDO.
   DEF VAR lcHours       AS CHAR NO-UNDO.
   DEF VAR lcHoursText   AS CHAR NO-UNDO.
   DEF VAR lcOpenHour    AS CHAR NO-UNDO.
   DEF VAR lcCloseHour   AS CHAR NO-UNDO.
   DEF VAR lcUseEntries  AS CHAR NO-UNDO.
   DEF VAR lcDayList     AS CHAR NO-UNDO INIT "L a V|Lun|Mar|Mie|Jue|Vie|Sab|Dom|Vacaciones".

   DEF BUFFER OrderAction FOR OrderAction.

   lcErr = fGetOrderData (INPUT iiOrderNBR).

   FIND FIRST OrderAction NO-LOCK WHERE
              OrderAction.Brand = Syst.Var:gcBrand AND
              OrderAction.OrderId  = iiOrderNBR AND
              OrderAction.ItemType = "UPSHours" NO-ERROR.
   IF NOT AVAIL OrderAction THEN RETURN.

   /* Check that includes at least separator characters */
   IF INDEX(OrderAction.ItemKey,";") > 0 AND 
      NUM-ENTRIES(OrderAction.itemKey,";") = 9 THEN DO:
      IF Order.deliverytype = {&ORDER_DELTYPE_KIALA} OR
         Order.deliverytype = {&ORDER_DELTYPE_POS} THEN DO: /* UPS */
         DO liCount = 2 TO NUM-ENTRIES(OrderAction.ItemKey,";"):
            lcUseEntries = lcUseEntries + STRING(liCount) + "|".
         END.
         lcUPSHours = "<b>".
      END.
      /* Correos */
      ELSE IF Order.deliverytype = {&ORDER_DELTYPE_POST} THEN DO:
         /* valid itemkey should have at least 8 entries */
         lcUseEntries = "1|7|8".
         lcUPSHours = "<b>Oficina de Correos de ".
      END.
      lcUPSHours = lcUPSHours + 
                   DeliveryCustomer.company + "</b> - " +
                   DeliveryCustomer.address + " " +
                   DeliveryCustomer.ZipCode + " " +
                   DeliveryCustomer.postoffice + /* " " + 
                   lcDelRegionName + */ "<br /><br />" +
                   IF Order.deliverytype = {&ORDER_DELTYPE_KIALA} OR
                      Order.deliverytype = {&ORDER_DELTYPE_POS} THEN 
                   "<b>Horarios:</b><br /><table border='0'>" ELSE
                   "<b>Horarios:</b><br />".
      lcUseEntries = RIGHT-TRIM(lcUseEntries,"|"). /* remove last separator */
      /* get needed visible days */
      DO liCount = 1 TO NUM-ENTRIES(lcUseEntries,"|"):
         lcHoursText = "".
         lcDailyHours = ENTRY(INT(ENTRY(liCount,lcUseEntries,"|")),
                        OrderAction.ItemKey,";").
         /*remove possible extra ; */
         lcDailyHours = LEFT-TRIM(lcDailyHours, ";").
         /* handle several times for day */
         IF Order.deliverytype = {&ORDER_DELTYPE_KIALA} OR
            Order.deliverytype = {&ORDER_DELTYPE_POS} THEN DO: /* UPS */
            lcHoursText = REPLACE(lcDailyHours, "h",":").
         END.   
         ELSE IF Order.deliverytype = {&ORDER_DELTYPE_POST} THEN DO:
            DO liTimeCount = 1 TO NUM-ENTRIES(lcDailyHours,"/"):
               lcTempHours = ENTRY(liTimeCount,lcDailyHours,"/").
               IF INDEX(lcTempHours,"-") > 0 AND INDEX(lcTempHours,"h") > 0 AND
                  liTimeCount < 9 THEN DO: /* open times exists */
                  lcOpenHour = REPLACE(ENTRY(1,lcTempHours,"-"),"h",":").
                  lcCloseHour = REPLACE(ENTRY(2,lcTempHours,"-"),"h",":").
                  lcHoursText = lcHoursText + "De " + lcOpenHour + " a " + 
                                lcCloseHour + "/ ".
               END.
               ELSE lcHoursText = lcDailyHours. /* Closed texts */
               
            END.
         END.
         /* Remove extra spaces and / at the end of the string */
         lcHoursText = RIGHT-TRIM(lcHoursText).
         lcHoursText = RIGHT-TRIM(lcHoursText,"/").
         /* Correos need different day name syntax */
         IF Order.deliverytype = {&ORDER_DELTYPE_POST} /* Correos */ AND
            INT(ENTRY(liCount,lcUseEntries,"|")) = 7 THEN 
            lcDay = "S".
         ELSE IF Order.deliverytype = {&ORDER_DELTYPE_POST} AND
            INT(ENTRY(liCount,lcUseEntries,"|")) = 8 THEN
            lcDay = "Festivos".
         ELSE   
            lcDay = ENTRY(INT(ENTRY(liCount,lcUseEntries,"|")),lcDayList,"|").
         IF Order.deliverytype = {&ORDER_DELTYPE_KIALA} OR
            Order.deliverytype = {&ORDER_DELTYPE_POS} THEN
            lcUPSHours = lcUPSHours + "<tr><td><b>" + lcDay + 
                         "</b>:</td> <td>" + lcHoursText + " </td></tr> ".
         ELSE
            lcUPSHours = lcUPSHours + "<b>" + lcDay + "</b>: " + 
                         lcHoursText + "<br />".
      END.
      IF Order.deliverytype = {&ORDER_DELTYPE_KIALA} OR
         Order.deliverytype = {&ORDER_DELTYPE_POS} THEN
         lcUPSHours = lcUPSHours + "</table>".
   END.

   lcResult = lcUPSHours.
END. /*GetUPSHOURS*/

PROCEDURE pGetPAYMENT_METHOD:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).

   FIND FIRST OrderPayment NO-LOCK WHERE
              OrderPayment.Brand   = Syst.Var:gcBrand       AND
              OrderPayment.OrderID = Order.OrderID NO-ERROR.
   IF AVAILABLE OrderPayment THEN
      IF OrderPayment.method = 1 THEN
         IF liLang = 5 THEN lcResult = "Payment on delivery". 
         ELSE lcResult =  "Contrareembolso".
      ELSE IF OrderPayment.method = 6 THEN
         lcResult = "Paypal".
      ELSE
         IF liLang = 5 THEN lcResult = "Card".
         ELSE lcResult = "Tarjeta".
END.



/*Current operator*/
PROCEDURE pGetOMNPDATA:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   
   DEF VAR lcValue AS CHAR NO-UNDO.   
   DEF VAR lcList AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).

   lcList = "".
   IF Order.MNPStatus > 0 THEN DO:
      ASSIGN
      lcValue = fTeksti(287,liLang)
                 lcList  = CHR(10) +
                            lcValue + CHR(10) +
                            FILL("-",LENGTH(lcValue) + 2) + CHR(10).

      lcValue = Order.CurrOper. 

      lcList = lcList + fTeksti(288,liLang) + " " +
                        lcValue + CHR(10).

      lcValue = fTeksti(263,liLang).
      IF lcValue = "" OR lcValue = ? THEN lcValue = "Prepaid/Postpaid".
      lcList = lcList + fTeksti(289,liLang) + " " +
                        STRING(Order.OldPayType,lcValue).

      IF Order.OldPayType THEN lcList = lcList + CHR(10) +
                  fTeksti(290,liLang) + " " + Order.OldICC.
   END.
   lcResult = Order.CurrOper.
END. /*GetORDER_DATA*/


/*Current payment method*/
/*To be checked in testing: this and the previous function handle Pre/Post printing.*/
PROCEDURE pGetOPMETHOD:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcValue AS CHAR NO-UNDO.   
   DEF VAR lcList AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   lcList = "".
   IF Order.Ordertype = {&ORDER_TYPE_MNP} THEN DO: 
            /*Fetch text 'Payment method' ENG or SPA*/
      ASSIGN
      lcValue = fTeksti(292,liLang)
      lcList  = CHR(10) +
                lcValue + CHR(10) +
                FILL("-",LENGTH(lcValue) + 2) + CHR(10).

      lcValue = fTeksti(263,liLang).
      IF lcValue = "" OR lcValue = ? THEN lcValue = "Prepaid/Postpaid".

      lcList = STRING(Order.PayType,lcValue).
   END.   
   lcResult = lcList.
END.

PROCEDURE pGetSERVICE_AVAILABLE:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR htmlText AS CHAR NO-UNDO.
   htmlText = "<strong>".
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   IF Order.Ordertype <> {&ORDER_TYPE_MNP} THEN DO:
      htmlText = htmlText + fTeksti((IF Order.ResignationPeriod
         THEN 563 ELSE 562), liLang).
      htmlText = REPLACE(htmlText,":",":</strong><br /><br />").
      lcResult = htmlText.
   END.          
END.

PROCEDURE pGetPENALTYFEE:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR liTermMonths AS INT NO-UNDO.
   DEF VAR lcList AS CHAR NO-UNDO.
   DEF VAR ldAmt AS DEC NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR lcTariffType AS CHAR NO-UNDO.
   DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.
   DEF VAR lcText AS CHAR NO-UNDO. 
   DEF VAR lcTermName AS CHAR NO-UNDO.

   lcErr = fGetOrderData (INPUT iiOrderNBR).  
   
   RUN Mc/offer_penaltyfee.p(Order.OrderID,
                        Output liTermMonths,
                        OUTPUT ldAmt).

   IF ldAmt NE 0 AND Order.PayType = FALSE THEN DO:

      lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").
      lcList = lcList + CHR(10) + fTeksti(510,liLang).

      assign lcList = REPLACE(lcList,"#xxx",TRIM(STRING(ldAmt,"->>>>>9")))
             lcList = REPLACE(lcList,"#yy",TRIM(STRING(liTermMonths))).
   END. /* IF ldAmt NE 0 AND Order.PayType = FALSE THEN DO: */

   IF CAN-FIND(FIRST CLIType WHERE
                     CLIType.Brand = Syst.Var:gcBrand AND
                     CLIType.CLItype = Order.CliType AND
                    (CLIType.LineType EQ {&CLITYPE_LINETYPE_MAIN} OR
                     CLIType.LineType EQ {&CLITYPE_LINETYPE_ADDITIONAL})) THEN DO:

      IF LOOKUP(Order.CLIType,lcBundleCLITypes) > 0 THEN
         lcTariffType = fGetDataBundleInOrderAction(Order.OrderId,
                                                    Order.CLIType).
      ELSE lcTariffType = Order.CLIType.

      IF CAN-FIND(FIRST CLIType NO-LOCK WHERE
                        CLIType.Brand = Syst.Var:gcBrand AND
                        CLIType.CLItype = lcTariffType AND
                        CLItype.LineType = {&CLITYPE_LINETYPE_ADDITIONAL}) THEN
         lcList = lcList + CHR(10) + fTeksti(539,liLang).
   END.
         
   IF fIsConvergenceTariff(Order.CLIType) AND
     (Order.OrderType EQ {&ORDER_TYPE_NEW} OR
      Order.OrderType EQ {&ORDER_TYPE_MNP})
   THEN DO: 

      lcText = fTeksti(532,liLang).
            
      IF INDEX(Order.Orderchannel, "pro") EQ 0 AND
         Order.CrStamp < 20171101 THEN liMonths = 8.
      ELSE liMonths = 12.
      lcText = REPLACE(lcText,"#MONTHS",STRING(liMonths)).

      /*YCO-279 + refactoring text 532*/
      /*before this 532 was hardcoded with 100E + 12 months*/
      lcErr =  fSelectFTERMFee(Order.OrderId,
                               OUTPUT ldAmt,
                               OUTPUT lcTermName).
      IF lcErr EQ "" THEN                         
         lcText = REPLACE(lcText,"#AMOUNT",STRING(ldAmt)).
      ELSE
         lcText = REPLACE(lcText,"#AMOUNT",STRING(100)).

      lcList = lclist + CHR(10) + lcText.

   END.
                  
   lcList = REPLACE(lcList,"euros","&euro;"). 
   lcResult = lcList.

END.

PROCEDURE pGetAMOUNT:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   
   IF ldeDeferredPayment > 0 THEN DO:   
      DEF VAR lcErr AS CHAR NO-UNDO.
      lcErr = fGetOrderData (INPUT iiOrderNBR).
      lcResult = STRING(ldeMonthlyFee,"->>9.99").
   END.

END.

PROCEDURE pGetMONTHS:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   IF ldeDeferredPayment > 0 THEN DO:
      DEF VAR lcErr AS CHAR NO-UNDO.
      lcErr = fGetOrderData (INPUT iiOrderNBR).
      lcResult = STRING(liMonths).
   END.

END.

PROCEDURE pGetFINALFEE:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   IF ldeDeferredPayment > 0 THEN DO:
      DEF VAR lcErr AS CHAR NO-UNDO.
      lcErr = fGetOrderData (INPUT iiOrderNBR).
      IF (ldeFinalFee > 0) THEN
         lcResult = STRING(ldeFinalFee,"->>9.99").
   END.

END.

PROCEDURE pGetOCONTRID:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   lcResult = Order.ContractID.
END.

PROCEDURE pGetPOD:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR lcList AS CHAR NO-UNDO.
   DEF VAR ldInvTot AS DEC NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
         
   RUN Mc/cashfee.p (Order.OrderID,
                  4,     /* leave out campaign topups */
                  OUTPUT lcList,
                  OUTPUT ldInvTot,
                  OUTPUT lcErr).
   IF INDEX(lcList,"Payment on delivery")>0 THEN DO:
      /* Remove not needed text and white spaces" */
      lcList = SUBSTRING(lcList, INDEX(lcList,"Payment on delivery")).
      lcList = REPLACE(lcList,"Payment on delivery","").
      lcList = REPLACE(lcList,"EUR","").
      lcList = TRIM(lcList).
   END. 
   ELSE IF INDEX(lcList,"reembolso")>0 THEN DO:
      /* Remove not needed text and white spaces" */
      lcList = SUBSTRING(lcList, INDEX(lcList,"reembolso")).      
      lcList = REPLACE(lcList,"reembolso","").
      lcList = REPLACE(lcList,"EUR","").
      lcList = TRIM(lcList).
   END.
   ELSE DO: 
      lcList = "".
   END.
   lcResult = lcList.
END.

PROCEDURE pGetTOTAL_FEE:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   DEF VAR lcList AS CHAR NO-UNDO.
   DEF VAR ldeRVPerc AS DECIMAL NO-UNDO.
   DEF VAR ldtOrder AS DATE NO-UNDO.
   DEF VAR liOrderTime AS INT NO-UNDO.
   DEF VAR ldeCommFee AS DECIMAL NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).

   Func.Common:mSplitTS(Order.CrStamp,
               OUTPUT ldtOrder,
               OUTPUT liOrderTime).
   ldeRVPerc = TRUNC(ldeFinalFee /
                           (ldeDeferredPayment + ldeFinalFee) * 100 + 0.05,1).

   FIND FIRST TFConf NO-LOCK WHERE
         TFConf.RVPercentage = ldeRVPerc AND
                 TFConf.ValidTo >= ldtOrder AND
                 TFConf.ValidFrom <= ldtOrder NO-ERROR.
   IF NOT AVAIL TFConf THEN
         ldeCommFee = 0.
   ELSE DO:
         ldeCommFee = ROUND((TFConf.CommFeePerc / 100) *
                            (ldeDeferredPayment + ldeFinalFee),2).
   END.
   lcList = TRIM(STRING(ldeDeferredPayment + ldeFinalFee + ldeCommFee, "->>9.99")).

   lcResult = lcList.

END.

PROCEDURE pGetPERCONTR:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   DEF VAR lcText        AS CHAR NO-UNDO.
   DEF VAR lcDCEvent     AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).

   lcDCEvent = fCParamC("PerContractID").
   lcText = "#PERCONTR".
   FOR FIRST DCCLI NO-LOCK WHERE
             DCCLI.Brand      = Syst.Var:gcBrand   AND
             DCCLI.DCEvent    = lcDCEvent AND
             DCCLI.MsSeq      = liMsSeq   AND
             DCCLI.ValidTo   >= TODAY     AND
             DCCLI.ValidFrom <= TODAY,
       FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand   = Syst.Var:gcBrand AND
             DayCampaign.DCEvent = DCCLI.DCEvent:

      lcText = REPLACE(lcText,"#PERCONTR",  
                              DayCampaign.DCName + ", " +
                              fTeksti(227,liLang) + " " +
                              STRING(DCCLI.ValidFrom,"99.99.9999") + " - " +
                              STRING(DCCLI.ValidTo,"99.99.9999") +
                              CHR(10) +
                              fTeksti(228,liLang)).
   END.

   /* no contracts */
   IF INDEX(lcText,"#PERCONTR") > 0 THEN
      lcResult = "".
   ELSE lcResult =  lcText.

END.

/*Tariff name*/
/*Tariff: #CTNAME [tariff prices as specified on summary and confirmation page, with customer VAT zone]*/
PROCEDURE pGetCTNAME:

   DEF INPUT  PARAMETER iiOrderNBR AS INT     NO-UNDO.
   DEF OUTPUT PARAMETER olgErr     AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult   AS CHAR    NO-UNDO.

   DEF VAR lcValue                 AS CHAR NO-UNDO.   
   DEF VAR lcList                  AS CHAR NO-UNDO.
   DEF VAR lcTagCTName             AS CHAR NO-UNDO.
   DEF VAR liNumEntries            AS INT  NO-UNDO.
   DEF VAR llDSSPromotion          AS LOG  NO-UNDO.
   DEF VAR ldtEventDate            AS DATE NO-UNDO.
   DEF VAR liMsSeq                 AS INT  NO-UNDO.
   DEF VAR lcTitle                 AS CHAR NO-UNDO.
   DEF VAR llSmall                 AS LOG  NO-UNDO.
   DEF VAR lcFinTxt                AS CHAR NO-UNDO.
   DEF VAR lcDivTxt                AS CHAR NO-UNDO.
   DEF VAR liSmallLength           AS INT  NO-UNDO.
   DEF VAR lcErrTxt                AS CHAR NO-UNDO.
   DEF VAR ldeTaxPerc              AS DEC  NO-UNDO.
   DEF VAR lcFATGroup              AS CHAR NO-UNDO.
   DEF VAR lcDCEvent               AS CHAR NO-UNDO.
   DEF VAR liCount                 AS INT  NO-UNDO.
   DEF VAR lcBundleCLITypes        AS CHAR NO-UNDO.
   DEF VAR lcBundle                AS CHAR NO-UNDO.
   DEF VAR lcBundlesInfo           AS CHAR NO-UNDO.
   DEF VAR lcBundleInfo            AS CHAR NO-UNDO.
   DEF VAR ldeBundleFee            AS DEC  NO-UNDO.
   DEF VAR lcBundleBillItem        AS CHAR NO-UNDO.
   DEF VAR liTime                  AS INT  NO-UNDO.
   DEF VAR ldtOrder                AS DATE NO-UNDO.
   DEF VAR lcErr                   AS CHAR NO-UNDO.
   DEF VAR llgExtraLine            AS LOG  NO-UNDO. 
   DEF VAR lcSegment               AS CHAR NO-UNDO.
   DEF VAR lcVAT                   AS CHAR NO-UNDO.
   DEF VAR ldeProFee               AS DEC  NO-UNDO.
   DEF VAR lcProFeeModel           AS CHAR NO-UNDO.

   DEFINE BUFFER lbELOrder   FOR Order.
   DEFINE BUFFER lbMLOrder   FOR Order.
   DEFINE BUFFER lbELCLIType FOR CLIType.
   DEFINE BUFFER lbMLCLIType FOR CLIType.

   ASSIGN 
      lcErr        = fGetOrderData (INPUT iiOrderNBR)
      ldtEventDate = TODAY.

   ASSIGN 
      lcBundleCLITypes        = fCParamC("BUNDLE_BASED_CLITYPES").

   Func.Common:mSplitTS(Order.CrStamp,
            OUTPUT ldtOrder,
            OUTPUT liTime).
   ldeTaxPerc = fRegionTaxPerc(OrderCustomer.Region,
                               "1",
                               ldtOrder).
   /* Bundle Information in product list */
   RUN pGetBundleInfo(INPUT Order.OrderID, OUTPUT lcBundlesInfo).
   lcSegment = fGetSegment(liCustNum, Order.OrderID).

   liNumEntries = NUM-ENTRIES(lcBundlesInfo).
   DO liCount = 1 TO liNumEntries:
      lcBundleInfo = ENTRY(liCount,lcBundlesInfo).
      IF lcBundleInfo > "" AND NUM-ENTRIES(lcBundleInfo,"=") = 2 THEN DO:
         ASSIGN lcBundle     = ENTRY(1,lcBundleInfo,"=")
                ldeBundleFee = DECIMAL(ENTRY(2,lcBundleInfo,"=")) NO-ERROR.
         IF ERROR-STATUS:ERROR OR ldeBundleFee = ? THEN ldeBundleFee = 0.

         CASE lcBundle:
            WHEN {&DSS} THEN
               lcList = lcList + STRING(fTeksti(526,liLang),"X(40)") +
                        STRING(ldeBundleFee,"->>>>>9.99") + " &euro;" + CHR(10).
            WHEN "MDUB2" OR WHEN "MDUB3" OR WHEN "MDUB4" OR
            WHEN "CONTDATA" OR WHEN "CONTD2" OR WHEN "CONTD4" THEN
               llDSSPromotion = TRUE.
            OTHERWISE NEXT.
         END CASE. /* CASE lcBundle: */
      END. /* IF lcBundleInfo > "" THEN DO: */
   END. /* DO liCount = 1 TO liNumEntries: */

   /* clitype and language have now their final values */
   IF  lcCLIType > "" THEN DO:
      lcBundle = "".
      IF LOOKUP(lcCLIType ,lcBundleCLITypes) > 0 THEN DO:
         lcBundle = fGetDataBundleInOrderAction(Order.OrderID,lcCLIType).
         lcBundleBillItem = fConvBundleToBillItem(lcBundle).
         lcTagCTName = fTranslationName(Syst.Var:gcBrand,
                                        1,
                                        lcBundleBillItem,
                                        liLang,
                                        ldtEventDate).
      END. /* IF LOOKUP(icCLIType,lcBundleCLITypes) > 0 THEN DO: */
      ELSE DO:
         lcTagCTName = fTranslationName(Syst.Var:gcBrand,
                                        9,
                                        lcCLIType,
                                        liLang,
                                        ldtEventDate).
         /* CONT8 (La del Cero + [Internet]) */
         IF lcCLIType EQ "CONT8" AND
            Order.UsageType EQ "DATA" THEN
            lcTagCTName = lcTagCTName + " Internet".
      END.

      IF lcTagCTName = ? OR lcTagCTName = "" THEN DO:
         FIND CLIType WHERE
              CLIType.Brand   = Syst.Var:gcBrand AND
              CLIType.CLIType = lcCLIType NO-LOCK NO-ERROR.
         IF AVAILABLE CLIType THEN lcTagCTName = CLIType.CLIName.
      END.
   END. /* lcCLIType */

   /*YPRO-21*/
   IF fIsProOrder(iiOrderNBR) EQ TRUE THEN DO:
      lcTagCTName = lcTagCTName + " + BENEFICIOS PRO".
      lcProFeemodel = fgetPROFeemodel(lcClitype).
      FIND FIRST FMItem no-lock WHERE
                 FMItem.brand EQ "1" AND
                 FMItem.feemodel EQ lcProFeemodel AND
                 FMItem.pricelist EQ "PRO_" + lcClitype NO-ERROR.
      IF AVAIL FMItem THEN           
         ldeProFee = fmitem.Amount.
   END.
   IF lcErrTxt NE "" THEN DO:
      olgErr = TRUE.
      lcResult = (lcErrTxt).
   END.

   FOR FIRST CLIType NO-LOCK WHERE
             CLIType.Brand = Syst.Var:gcBrand AND
             CLIType.CLIType = (IF lcBundle > "" THEN lcBundle ELSE lcCLIType):

      DEF VAR ldeCallPrice AS DEC NO-UNDO.
      DEF VAR ldeMF AS DEC NO-UNDO.
      DEF VAR ldeMFNoDisc  AS DEC NO-UNDO.
      DEF VAR ldeDiscount  AS DEC NO-UNDO.

      DEFINE VARIABLE ldtOrderDate AS DATE NO-UNDO.
      DEFINE VARIABLE ldiOrderDate AS INT  NO-UNDO.
      DEFINE VARIABLE llgOrderDate AS LOG  NO-UNDO.
      DEFINE VARIABLE lcMFText     AS CHAR No-UNDO.
      DEFINE VARIABLE llgEmailText AS LOG  NO-UNDO. 
      DEFINE VARIABLE llAddLineDiscount AS LOG NO-UNDO.
      DEFINE VARIABLE ldDiscValue  AS DEC  NO-UNDO.

      llgOrderDate = Func.Common:mSplitTS(Order.CrStamp,
                              OUTPUT ldtOrderDate,
                              OUTPUT ldiOrderDate).
      
      /*YDR-2347 removed the hard coded values AND now fetching the values from Commercial Fee itself*/
      IF lcSegment BEGINS "PRO-SOHO" THEN DO:
         ASSIGN
            ldeMF = CLIType.CommercialFee + ldeProFee.
            lcVat = IF liLang EQ 5 THEN " VAT. excl" ELSE " imp. no incl.".
      END.
      ELSE DO:
         ASSIGN
            ldeMF = (1 + ldeTaxPerc / 100) * (CLIType.CommercialFee + ldeProFee).
            lcVat = IF liLang EQ 5 THEN " VAT. incl" ELSE " imp. incl.".
      END.
      CASE CLIType.CLIType:
         WHEN "CONT9" OR WHEN "CONT10" OR WHEN "CONT15" THEN lcList = "0 cent/min".
         WHEN "TARJ7" THEN lcList = "1 cent/min".
         WHEN "TARJ8" THEN lcList = "6,05 cent/min".
         WHEN "TARJ9" THEN lcList = "1 cent/min".
         WHEN "TARJ10" THEN lcList = "20 min/mes gratis,".
         WHEN "TARJ11" THEN lcList = "50 min/mes gratis,".
         WHEN "TARJ12" THEN lcList = "100 min/mes gratis,".
         WHEN "TARJ13" THEN lcList = "Llamadas Ilimitadas,".
         OTHERWISE lcList = "".
      END.

       IF LOOKUP(Order.CLIType, "CONT9,CONT10,CONT15,CONT24,CONT23,CONT25,CONT26,CONT27") > 0 OR fIsConvergenceTariff(Order.CLIType) THEN DO:

          llAddLineDiscount = FALSE.

          /* Additional line discount information in emails */
          FOR FIRST OrderAction NO-LOCK WHERE
                    OrderAction.Brand    = Syst.Var:gcBrand       AND
                    OrderAction.OrderID  = Order.OrderID AND
                    OrderAction.ItemType = "AddLineDiscount":

             FOR FIRST DiscountPlan NO-LOCK WHERE
                       DiscountPlan.DPRuleID = OrderAction.ItemKey,
                 FIRST DPRate WHERE
                       DPRate.DPId       = DiscountPlan.DPId AND
                       DPRate.ValidFrom <= ldtOrderDate      AND
                       DPRate.ValidTo   >= ldtOrderDate      NO-LOCK:
             
                 llAddLineDiscount = TRUE.
                 ldeMFNoDisc       = ldeMF.
                 ldDiscValue       = DPRate.DiscValue.
                 ldeMF             = ldeMF - ((DPRate.DiscValue / 100) * ldeMF).

             END.

          END. /* ADDITIONAL-LINE */

          IF NOT llAddLineDiscount THEN
             FOR FIRST OfferItem WHERE
                       OfferItem.Brand       = Syst.Var:gcBrand             AND
                       OfferItem.Offer       = Order.Offer         AND
                       OfferItem.ItemType    = "discountplan"      AND
                       LOOKUP(OfferItem.ItemKey,
                       "TariffMarchDISC,CONT9DISC,CONT10DISC,CONT15DISC,CONT24DISC,CONT23DISC,CONT25DISC,CONT26DISC,CONVDISC,CONVDISC20_3") > 0 AND
                       OfferItem.BeginStamp <= Order.CrStamp       AND
                       OfferItem.EndStamp   >= Order.CrStamp     NO-LOCK,
                 FIRST DiscountPlan WHERE
                       DiscountPlan.Brand    = Syst.Var:gcBrand AND
                       DiscountPlan.DPRuleId = OfferItem.ItemKey NO-LOCK,
                 FIRST DPRate WHERE
                       DPRate.DPId = DiscountPlan.DPId AND
                       DPRate.ValidFrom <= ldtOrderDate AND
                       DPRate.ValidTo   >= ldtOrderDate NO-LOCK:

                lcMFText = STRING(DiscountPlan.ValidPeriods)                    +
                           (IF liLang EQ 5 THEN " months. " ELSE " meses. ") +
                           "<br/>" +
                           (IF liLang EQ 5 THEN "After " ELSE "Después ")    +
                           TRIM(STRING(ldeMF,"->>>>>>>9.99"))             + " &euro;/" +
                           (IF liLang EQ 5 THEN "month" ELSE "mes") + lcVat.
                /*Offeritem values must be used if such is available.
                  Otherwise the value is taken from discountplan settings.*/
                IF Offeritem.Amount > 0 THEN DO:
                   IF DiscountPlan.DPUnit EQ "Percentage" THEN
                      ldeMF = ldeMF - ((Offeritem.amount / 100) * ldeMF).
                   ELSE IF DiscountPlan.DPUnit EQ "Fixed" THEN DO:
                      IF lcSegment BEGINS "PRO-SOHO" THEN
                         ldeDiscount = Offeritem.amount / (1 + ldeTaxPerc / 100).
                      ELSE
                         ldeDiscount = Offeritem.amount.

                      ldeMF = ldeMF - ldeDiscount.
                   END.   
 
                END.
                ELSE DO:
                   IF DiscountPlan.DPUnit EQ "Percentage" THEN
                      ldeMF = ldeMF - ((DPRate.DiscValue / 100) * ldeMF).
                   ELSE IF DiscountPlan.DPUnit EQ "Fixed" THEN
                      IF lcSegment BEGINS "PRO-SOHO" THEN
                         ldeDiscount = DPRate.DiscValue / (1 + ldeTaxPerc / 100).
                      ELSE
                         ldeDiscount = DPRate.DiscValue.

                      ldeMF = ldeMF - ldeDiscount. 
                END.
             END.
       END.
       
       /* TV Service */
       TVSERVICE_DETAILS:
       FOR EACH OrderAction WHERE OrderAction.Brand    = Syst.Var:gcBrand       AND
                                  OrderAction.OrderId  = Order.Orderid AND
                                  OrderAction.ItemType = "BundleItem"  NO-LOCK,
           FIRST DayCampaign WHERE DayCampaign.Brand        = Syst.Var:gcBrand              AND 
                                   DayCampaign.DCEvent      = OrderAction.ItemKey  AND 
                                   DayCampaign.BundleTarget = {&TELEVISION_BUNDLE} NO-LOCK:

           FIND FIRST FMItem WHERE FMItem.Brand     = Syst.Var:gcBrand              AND 
                                   FMItem.FeeModel  = DayCampaign.FeeModel AND
                                   FMItem.BillCode  = DayCampaign.BillCode AND
                                   FMItem.PriceList > ""                   AND 
                                   FMItem.FromDate <= TODAY                AND
                                   FMItem.ToDate   >= TODAY                NO-LOCK NO-ERROR. 
                                   
           ASSIGN 
              lcMFText        = lcMFText + 
                                "<br/>Agile TV, " + 
                                (IF AVAIL FMItem THEN 
                                   (REPLACE(STRING(FMItem.Amount,"z9.99"),".",",") + " &euro;/" + (IF liLang EQ 5 THEN "month" ELSE "mes")) 
                                 ELSE 
                                   "").
           LEAVE TVSERVICE_DETAILS.                     
       END.                                    

       FIND FIRST DiscountPlan NO-LOCK WHERE
                  DiscountPlan.DPRuleId = "BONO7DISC" NO-ERROR.

       IF AVAIL DiscountPlan THEN DO:

          llgEmailText = FALSE.
           
          FIND FIRST DPMember WHERE
                     DPMember.DPId = DiscountPlan.DPId AND
                     DPMember.hosttable = "MobSub" AND
                     DPMember.keyValue = STRING(order.msseq)  AND
                     DPMember.validFrom <= ldtOrderDate AND
                     DPMember.validTo >= ldtOrderDate NO-LOCK NO-ERROR.
          
          IF AVAIL DPMember THEN    
             llgEmailText = TRUE.   
          ELSE DO:
             FIND FIRST Orderaction NO-LOCK where
                        Orderaction.brand = Syst.Var:gcBrand AND
                        orderaction.orderid = order.orderid AND
                      ((orderaction.itemtype = "discount" AND
                        orderaction.itemkey = STRING(DiscountPlan.DPID)) OR
                       (orderaction.itemtype = "discountplan" AND
                        orderaction.itemkey = DiscountPlan.DPRuleId)) NO-ERROR.
              IF AVAIL orderaction THEN    
                 llgEmailText = TRUE.
          END.

          IF llgEmailText THEN DO:
             lcMFText = lcMFText + (IF liLang EQ 5 THEN "<br/>3 GB/mes extra free during 6 months" 
                        ELSE "<br/>3 GB/mes extra gratis durante 6 meses"). 
          
          END.
       END.
       
       /* YDR-2294 */
       FIND FIRST DiscountPlan NO-LOCK WHERE
                  DiscountPlan.DPRuleId = "BONO6WEBDISC" NO-ERROR.

       IF AVAIL DiscountPlan THEN DO:

          llgEmailText = FALSE.
           
          FIND FIRST DPMember WHERE
                     DPMember.DPId = DiscountPlan.DPId AND
                     DPMember.hosttable = "MobSub" AND
                     DPMember.keyValue = STRING(order.msseq)  AND
                     DPMember.validFrom <= ldtOrderDate AND
                     DPMember.validTo >= ldtOrderDate NO-LOCK NO-ERROR.
          
          IF AVAIL DPMember THEN    
             llgEmailText = TRUE.   
          ELSE DO:
             FIND FIRST Orderaction NO-LOCK where
                        Orderaction.brand = Syst.Var:gcBrand AND
                        orderaction.orderid = order.orderid AND
                      ((orderaction.itemtype = "discount" AND
                        orderaction.itemkey = STRING(DiscountPlan.DPID)) OR
                       (orderaction.itemtype = "discountplan" AND
                        orderaction.itemkey = DiscountPlan.DPRuleId)) NO-ERROR.
              IF AVAIL orderaction THEN    
                 llgEmailText = TRUE.
          END.

          IF llgEmailText THEN DO:
                lcMFText = lcMFText + (IF liLang EQ 5 THEN "<br/>1 GB/mes extra free during 6 months"
                                       ELSE "<br/>1 GB/mes extra gratis durante 6 meses"). 
          END.
       END.
       /*YPR-5616: Adding promotion text for Azul orders (that do not have discount plan for the campaign)*/
       /*Dirty and temporary solution. Can be removed after march 2017*/
       IF (Order.CliType EQ "CONTDSL59" OR
           Order.CliType EQ "CONTFH59_50" OR  
           Order.CliType EQ "CONTFH69_300") 
           AND
          (Order.Crstamp > fCParamDe("March2017PromoFromDate") AND 
           Order.Crstamp < fCParamDe("March2017PromoToDate")) THEN DO:
          lcMFText = lcMFText + (IF liLang EQ 5 THEN "<br/>25 GB/mes extra free during 6 months"
                              ELSE "<br/>25 GB/mes extra gratis durante 6 meses").
       END.
       ELSE IF Order.CliType EQ "CONT25" AND /* April promotion */
               NOT llAddLineDiscount     AND
               Order.Ordertype = {&ORDER_TYPE_MNP} AND
              (Order.Crstamp >= fCParamDe("March2017AprilFromDate") AND /* 20170403 */
               Order.Crstamp < fCParamDe("March2017PromoToDate")) THEN DO:
          lcMFText = lcMFText + (IF liLang EQ 5 THEN "<br/>25 GB/mes extra free during 6 months"
                              ELSE "<br/>25 GB/mes extra gratis durante 6 meses").
       END.

       /* Extra lines text */
       IF fCLITypeIsMainLine(Order.CLIType) AND 
          Order.MultiSimId                      <> 0 AND
          Order.MultiSimType                     = {&MULTISIMTYPE_PRIMARY} THEN DO:
      
          FIND FIRST lbELOrder NO-LOCK WHERE
                     lbELOrder.Brand        = Syst.Var:gcBrand          AND
                     lbELOrder.OrderId      = Order.MultiSimId AND
                     lbELOrder.MultiSimId   = Order.OrderId    AND
                     lbELOrder.MultiSimType = {&MULTISIMTYPE_EXTRALINE} NO-ERROR.
          
          IF AVAIL lbELOrder THEN DO:
             
             FIND FIRST lbELCLIType NO-LOCK WHERE 
                        lbELCLIType.Brand   = Syst.Var:gcBrand           AND 
                        lbELCLIType.CLIType = lbELOrder.CLIType NO-ERROR.

             IF AVAIL lbELCLIType THEN 
                lcTagCTName = lcTagCTName + " con " + lbELCLIType.CLIName. 
          
          END.

       END.
       ELSE IF fCLITypeIsExtraLine(Order.CLIType) AND 
               Order.MultiSimId  <> 0 AND
               Order.MultiSimType = {&MULTISIMTYPE_EXTRALINE} THEN DO:
          
          FIND FIRST lbMLOrder NO-LOCK WHERE
                     lbMLOrder.Brand   = Syst.Var:gcBrand AND
                     lbMLOrder.OrderId = Order.MultiSimId NO-ERROR. 
 
         IF AVAIL lbMLOrder THEN DO:

            FIND FIRST lbMLCLIType NO-LOCK WHERE 
                       lbMLCLIType.Brand   = Syst.Var:gcBrand AND 
                       lbMLCLIType.CLIType = lbMLOrder.CLIType NO-ERROR.

            IF AVAIL lbMLCLIType THEN 
               ASSIGN 
                  lcTagCTName  = lcTagCTNAme + " de " + lbMLCLIType.CLIName
                  ldeMF        = 0
                  llgExtraLine = TRUE
                  lcList       = lcList + " 0 " + "&euro;/mes" + fTeksti(581,liLang).
                  
         END.

       END.

       IF ldeMF > 0 THEN DO:
          IF llAddLineDiscount THEN
             lcList = lcList + (IF lcList > "" THEN ",<br/>" ELSE "") + "<del>" + TRIM(STRING(ldeMFNoDisc,"->>>>>>>9.99")) + " &euro;</del>" + " " +
                      TRIM(STRING(ldeMF,"->>>>>>>9.99")) + " &euro;/" +
                      (IF liLang EQ 5 THEN "month" ELSE "mes") + " IVA incl.<br/>" + TRIM(STRING(ldDiscValue,"99"))+ "% DTO. para siempre".
          ELSE
             /* YBU-4648 LENGTH check added for fitting one line */
             lcList = lcList + (IF LENGTH(lcList +  TRIM(STRING(ldeMF,
                                   "->>>>>>>9.99")) + " &euro;/" + (IF liLang 
                                   EQ 5 THEN "month" ELSE "mes")) > 36 THEN 
                                   ",<br/>" ELSE " ") +
                      TRIM(STRING(ldeMF,"->>>>>>>9.99")) + " &euro;/" +
                      (IF liLang EQ 5 THEN "month" ELSE "mes").
       END.

       IF lcList > "" THEN DO:
         IF llAddLineDiscount OR 
            llgExtraLine      THEN
            lcTagCTName = lcTagCTName + ",<br/> " + lcList + "<br/>".
         ELSE
            lcTagCTName = lcTagCTName + ",<br/> " + lcList + lcVat.
       END.

        IF lcMFText NE ""  THEN
           lcTagCTName = lcTagCTName + " " + lcMFText.

   END.

   /* YOT-1512/YOT-1732/YDR-468 */
   IF lcBundle EQ "CONTD3"      THEN lcFATGroup = "IPL8CPACT2".
   ELSE IF lcBundle EQ "CONTD4" THEN lcFATGroup = "IPL15CPACT".

   IF lcFATGroup > "" THEN DO:
      FIND FIRST OfferItem WHERE
                 OfferItem.Brand = Syst.Var:gcBrand AND
                 OfferItem.Offer = Order.Offer AND
                 OfferItem.ItemType = "Fatime" AND
                 OfferItem.ItemKey  = lcFATGroup AND
                 OfferItem.BeginStamp <= Order.CrStamp AND
                 OfferItem.EndStamp >= Order.CrStamp NO-LOCK NO-ERROR.
      IF AVAIL OfferItem THEN ASSIGN
         lcTagCTName = lcTagCTName + CHR(9) +
         FILL(" ",36) + fTeksti(521,liLang).
   END. /* IF lcFATGroup > "" THEN DO: */

   IF (Order.CLIType = "TARJ7" OR Order.CLIType = "TARJ9") AND
       Order.OrderType < 2 THEN DO:
       FIND FIRST OrderAction NO-LOCK WHERE
              OrderAction.Brand    = Syst.Var:gcBrand        AND
              OrderAction.OrderId  = Order.OrderID  AND
              OrderAction.ItemType = "Promotion"    AND
              OrderAction.ItemKey  = Order.CLIType  NO-ERROR.

       lcTagCTName = lcTagCTName + CHR(9) + CHR(9) + fTeksti(540,liLang) +
                     IF AVAILABLE OrderAction THEN
                        CHR(9) + CHR(9) + fTeksti(568,liLang) + CHR(9)
                     ELSE "".
   END.

   lcTagCTName = REPLACE(lcTagCTName,"E/mes","&euro;/mes").
   lcTagCTName = REPLACE(lcTagCTName,"E/month","&euro;/month").
   lcTagCTName = REPLACE(lcTagCTName,",00","").

   lcResult = lcTagCTName.
END. /*Tariff name pGetCTNAME*/



PROCEDURE pGetORDERSUMMARY:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.

   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR lcList AS CHAR NO-UNDO.
   DEF VAR ldInvTot AS DEC NO-UNDO.
   DEF VAR lgErr AS LOG NO-UNDO.
   DEF VAR ldTotalFee AS DEC NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   
    lcList = fGetOFEES_internal( INPUT iiOrderNBR,
                                 OUTPUT lgErr,
                                 OUTPUT ldTotalFee ).
   /*RUN Mc/cashfee.p (Order.OrderID,
                  4,     /* leave out campaign topups */
                  OUTPUT lcList,
                  OUTPUT ldInvTot,
                  OUTPUT lcErr). */
   /* parse name and price and add needed HTML tags for view */
   lcList = fParseHTMLPrices(lcList).
   /* corrected right writing form to received text */
   lcList = REPLACE(lcList,"Contra reembolso","Contrareembolso").
   lcResult = lcList.
END.

PROCEDURE pGetINITIALFEE:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   DEF VAR lcList AS CHAR NO-UNDO.
   DEF VAR ldTotalFee AS DEC NO-UNDO.
   DEF VAR lcError AS CHAR NO-UNDO.
   DEF VAR lcTmp AS CHAR NO-UNDO.
   DEF VAR lgErr AS LOG NO-UNDO.

   lcErr = fGetOrderData (INPUT iiOrderNBR).
   /*RUN Mc/cashfee.p (Order.OrderID,
                         4,     /* leave out campaign topups */
                         OUTPUT lcList,
                         OUTPUT ldInvTot,
                         OUTPUT lcError).*/
   IF ldeDeferredPayment > 0 THEN DO:
      lcTmp = fGetOFEES_internal( INPUT iiOrderNBR,
                                  OUTPUT lgErr,
                                  OUTPUT ldTotalFee ).
      lcResult = STRING(ldTotalFee,"->>>>>9.99").
   END.
END.

PROCEDURE pGetTOTALFEE:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR ldeTotalFee AS DECIMAL NO-UNDO.
   DEF VAR lcTmp AS CHAR NO-UNDO.
   DEF VAR lgErr AS LOGICAL NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   lcTmp = fGetOFEES_internal( INPUT iiOrderNBR, 
                               OUTPUT lgErr, 
                               OUTPUT ldeTotalFee ).
   ldeInvoiceTotal = IF ldeDeferredPayment > 0 THEN
                     ldeInvoiceTotal + ldeDeferredPayment + ldeFinalFee
                     ELSE ldeTotalFee. 
   olgErr = lgErr.
   lcResult = TRIM(STRING(ldeInvoiceTotal,"->>>>>9.99")).
   

END.

PROCEDURE pGetPicture:

   DEF INPUT PARAMETER iiOrderNBR AS INT NO-UNDO.
   DEF OUTPUT PARAMETER olgErr AS LOGICAL NO-UNDO.
   DEF OUTPUT PARAMETER lcResult AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.
   lcErr = fGetOrderData (INPUT iiOrderNBR).
   /* productcode / billitem defines ordered terminal */
   IF AVAIL OrderAccessory THEN
      lcResult = "pictures/terminals/" + OrderAccessory.productcode + "/small".
   ELSE DO:
      FIND FIRST OrderAction WHERE
                             OrderAction.Brand    = Syst.Var:gcBrand AND
                             OrderAction.OrderId  = Order.OrderId AND
                             OrderAction.ItemType = "SIMType" NO-LOCK NO-ERROR.
      IF AVAIL OrderAction AND OrderAction.ItemKey > "" THEN DO:
         IF OrderAction.ItemKey EQ "Plug_IN" THEN
            lcResult = "images/img-mini-sim.jpg". /* only Regular SIM ordered */
         ELSE IF OrderAction.ItemKey EQ "micro" THEN
            lcResult = "images/img-micro-sim.jpg". /* only Micro SIM ordered */
         ELSE IF OrderAction.ItemKey EQ "nano" THEN
            lcResult = "images/img-nano-sim.jpg". /* only Nano SIM ordered */
         ELSE IF OrderAction.ItemKey EQ "universal" THEN
            lcResult = "images/img-universal-sim.jpg". /* only universal 
                                                          SIM ordered */
      END.
	  ELSE lcResult = "images/img-mini-sim.jpg". /* otherwise return picture of normal SIM card (unknown case) */
   END.
END.

PROCEDURE pGetEXTRA_LINE_INFO:

   DEF INPUT  PARAMETER iiOrderNBR AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER olgErr     AS LOG  NO-UNDO.
   DEF OUTPUT PARAMETER lcResult   AS CHAR NO-UNDO.

   DEF VAR lcList                  AS CHAR NO-UNDO. 

   DEFINE BUFFER lbOrder   FOR Order.
   DEFINE BUFFER lbMLOrder FOR Order.
   DEFINE BUFFER lbELOrder FOR Order.
   
   ASSIGN
      lcList                  = "".

   FIND lbOrder NO-LOCK WHERE
      lbOrder.Brand   = Syst.Var:gcBrand AND
      lbOrder.OrderId = iiOrderNBR
   NO-ERROR.

   IF AVAIL lbOrder                       AND 
      lbOrder.MultiSimId <> 0             AND
      (fCLITypeIsMainLine(lbOrder.CLIType) OR
       fCLITypeIsExtraLine(lbOrder.CLIType)) THEN DO: 
      CASE lbOrder.MultiSimType:
         WHEN {&MULTISIMTYPE_PRIMARY} THEN DO:
            FIND FIRST lbELOrder NO-LOCK WHERE
                       lbELOrder.Brand        = Syst.Var:gcBrand            AND
                       lbELOrder.OrderId      = lbOrder.MultiSimId AND
                       lbELOrder.MultiSimId   = lbOrder.OrderId    AND
                       lbELOrder.MultiSimType = {&MULTISIMTYPE_EXTRALINE} NO-ERROR.

            IF AVAIL lbELOrder THEN
               ASSIGN 
                  lcList   = fTeksti(579,liLang)
                  lcResult = REPLACE(lcList,"#ORDERID", STRING(lbELOrder.ContractId)).
         END.
         WHEN {&MULTISIMTYPE_EXTRALINE} THEN DO:
            FIND FIRST lbMLOrder NO-LOCK WHERE
                       lbMLOrder.Brand   = Syst.Var:gcBrand   AND
                       lbMLOrder.OrderId = lbOrder.MultiSimId NO-ERROR. 

            IF AVAIL lbMLOrder THEN
               ASSIGN 
                  lcList   = fTeksti(580,liLang)
                  lcResult = REPLACE(lcList,"#ORDERID", STRING(lbMLOrder.ContractId)).
         END.
         OTHERWISE .
      END CASE.
   END.
   ELSE lcResult = "".

END PROCEDURE.
