/* ----------------------------------------------------------------------
  MODULE .......: create_logistics_file.p (based of Class/iccclass.cls)
  TASK .........: Created logistics file for Dextra
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 07.06.10
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

&GLOBAL-DEFINE MailTitleSpaces Allow

{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".

{Syst/tmsconst.i}
{Func/fwebuser.i}
{Func/transname.i}
{Func/ftaxdata.i}
{Syst/eventlog.i}
{Func/log.i}
{Func/ftransdir.i}
{Func/tmsparam4.i}
{Func/forderstamp.i}
{Func/orderfunc.i}
{Mm/fbundle.i}
{Mm/active_bundle.i}
{Mnp/mnp.i}
{Func/email.i}
{Mc/orderfusion.i}
{Func/financed_terminal.i}
{Func/fixedlinefunc.i}

DEFINE VARIABLE lcLogFile          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBundleCLITypes   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcContractTARFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcErrorLogFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE llCreateErrLogFile AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcTARSpoolDir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTAROutDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcContractsDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcContractsOutDir  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcErrorLogDir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcEmailConfDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRiftpDir         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lhTable            AS HANDLE    NO-UNDO.
DEFINE VARIABLE lhField            AS HANDLE    NO-UNDO.
DEFINE VARIABLE liLoop1            AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcLine             AS CHARACTER NO-UNDO.
DEFINE VARIABLE liNewDelay         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcICC              AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLargestId         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTarOption        AS CHARACTER NO-UNDO.
DEF VAR ldaCont15PromoFrom         AS DATE NO-UNDO. 
DEF VAR ldaCont15PromoEnd          AS DATE NO-UNDO. 
DEFINE VARIABLE ocResult           AS CHAR      NO-UNDO. 
DEFINE VARIABLE oiCustomer         AS INTEGER   NO-UNDO.
DEFINE VARIABLE llCorporate        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcError            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCustID           AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liMainlineOrderId  AS INTEGER   NO-UNDO. 
DEFINE VARIABLE llDespacharValue   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcTerminalBillCode AS CHAR      NO-UNDO.  
DEFINE VARIABLE lhOrder            AS HANDLE    NO-UNDO.
DEFINE VARIABLE ocTerminalCode     AS CHAR      NO-UNDO.

DEFINE BUFFER AgreeCustomer   FOR OrderCustomer.
DEFINE BUFFER ContactCustomer FOR OrderCustomer.
DEFINE BUFFER DelivCustomer   FOR OrderCustomer.
DEFINE BUFFER bBillItem       FOR BillItem.

DEFINE TEMP-TABLE ttOutputText 
   FIELD cText AS CHARACTER
   FIELD id AS INTEGER
   INDEX idx id.

DEFINE VARIABLE lcBrand AS CHARACTER NO-UNDO.

DO ON ERROR UNDO, THROW:
   lcBrand = CAPS(multitenancy.TenantInformation:mGetEffectiveBrand()).

   /* Handler code for any error condition. */
   CATCH anyErrorObject AS Progress.Lang.Error:
      MESSAGE "create_logistic_file: unable to fetch brand name".    
      MESSAGE anyErrorObject:GetMessage(1).
      RETURN.
   END CATCH.
END.


ASSIGN
   lcSpoolDir         = fCParam("Logistics","OutSpoolDir") 
   lcRiftpDir         = fCParam("Logistics","OutDir")
   liNewDelay         = fCParamI4(Syst.Var:gcBrand, "Logistics","NewOrderDelay")
   lcTARSpoolDir      = fCParam("Logistics","TARSpoolDir")
   lcTAROutDir        = fCParam("Logistics","TAROutDir")
   lcContractsDir     = fCParam("Logistics","ContractsDir")
   lcContractsOutDir  = fCParam("Logistics","ContractsOutDir")
   lcErrorLogDir      = fCParam("Logistics","DextraErrorLogDir")
   lcFileName         = lcBrand + "_ccbs_" + Func.Common:mDateFmt(TODAY,"ddmmyyyy") + 
                        REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt"
   lcContractTARFile  = lcTARSpoolDir +
                        lcBrand + "_contracts_ccbs_" + Func.Common:mDateFmt(TODAY,"ddmmyyyy") +
                        REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".tar"
   lcErrorLogFileName = lcErrorLogDir +
                        lcBrand + "_error_ccbs_" + Func.Common:mDateFmt(TODAY,"ddmmyyyy") + 
                        REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".log"
   lcBundleCLITypes   = fCParamC("BUNDLE_BASED_CLITYPES")
   ldaCont15PromoFrom = fCParamDa("CONT15PromoFromDate")
   ldaCont15PromoEnd  = fCParamDa("CONT15PromoEndDate").

DEFINE STREAM sICC.
DEFINE STREAM sLog.
DEFINE STREAM sErr.

DEFINE TEMP-TABLE ttOneDelivery NO-UNDO
   FIELD RowNum        AS INTEGER             
   /* Skip this OrderId field when generating dextra file: 
      important for internal reason: logging, etc.   */
   FIELD OrderId       AS INTEGER                   
   FIELD OrderType     AS INTEGER
   FIELD RequestID     AS CHARACTER FORMAT "X(8)"
   FIELD ActionID      AS CHARACTER FORMAT "X(2)"
   FIELD ProductID     AS CHARACTER FORMAT "X(9)"
   FIELD ContractId    AS CHARACTER FORMAT "X(10)"
   FIELD NIE           AS CHARACTER FORMAT "X(9)"
   FIELD NIF           AS CHARACTER FORMAT "X(9)"
   FIELD CIF           AS CHARACTER FORMAT "X(9)"
   FIELD PassPort      AS CHARACTER FORMAT "X(10)"
   FIELD SubsType      AS CHARACTER FORMAT "X(10)"
   /* 10 */
   FIELD ICCNum        AS CHARACTER FORMAT "X(13)"
   FIELD MSISDN        AS CHARACTER FORMAT "X(10)"
   FIELD TmpMSISDN     AS CHARACTER FORMAT "X(10)" /*YPR-6059: content changed*/
   FIELD MNPState      AS CHARACTER FORMAT "X(10)"
   FIELD VoiceMail     AS CHARACTER FORMAT "X(1)"
   FIELD XFUserID      AS CHARACTER FORMAT "X(10)"
   FIELD XFPWD         AS CHARACTER FORMAT "X(8)"
   FIELD Company       AS CHARACTER FORMAT "X(60)"
   FIELD Name          AS CHARACTER FORMAT "X(60)"
   FIELD SurName1      AS CHARACTER FORMAT "X(60)"
   /* 20 */
   FIELD SurName2      AS CHARACTER FORMAT "X(60)"
   FIELD DelivCO       AS CHARACTER FORMAT "X(30)"
   FIELD DelivAddr     AS CHARACTER FORMAT "X(60)"
   FIELD DelivCity     AS CHARACTER FORMAT "X(60)"
   FIELD DelivZip      AS CHARACTER FORMAT "X(5)"
   FIELD DelivRegi     AS CHARACTER FORMAT "X(60)"
   FIELD DelivCoun     AS CHARACTER FORMAT "X(20)"
   FIELD CustAddr      AS CHARACTER FORMAT "X(60)"
   FIELD CustCity      AS CHARACTER FORMAT "X(60)"
   FIELD CustZip       AS CHARACTER FORMAT "X(5)"
   /* 30 */
   FIELD CustRegi      AS CHARACTER FORMAT "X(60)"
   FIELD CustCoun      AS CHARACTER FORMAT "X(20)"
   FIELD MobConNum     AS CHARACTER FORMAT "X(15)"
   FIELD FixConNum     AS CHARACTER FORMAT "X(15)"
   FIELD EMail         AS CHARACTER FORMAT "X(60)"
   FIELD PaymInfo      AS CHARACTER FORMAT "X(1)"
   FIELD TermAmt       AS CHARACTER FORMAT "X(7)"
   FIELD TopUp         AS CHARACTER FORMAT "X(7)"
   FIELD MNPTransT     AS CHARACTER FORMAT "X(12)"
   FIELD InvRefNum     AS CHARACTER FORMAT "X(20)"
   /* 40 */
   FIELD InvNum        AS CHARACTER FORMAT "X(20)"
   FIELD TaxInfoServ   AS CHARACTER FORMAT "X(4)"
   FIELD TaxInfoTerm   AS CHARACTER FORMAT "X(4)"
   FIELD SalesChan     AS CHARACTER FORMAT "X(2)"
   FIELD DelivPaym     AS CHARACTER FORMAT "X(7)"
   FIELD DelivCost     AS CHARACTER FORMAT "X(7)"
   FIELD ServTotal     AS CHARACTER FORMAT "X(7)"
   FIELD TermTotal     AS CHARACTER FORMAT "X(7)"
   FIELD TaxService    AS CHARACTER FORMAT "X(7)"
   FIELD TaxTerminal   AS CHARACTER FORMAT "X(7)"
   /* 50 */
   FIELD InvoiceTotal  AS CHARACTER FORMAT "X(7)"
   FIELD DiscountTotal AS CHARACTER FORMAT "X(7)"
   .

DEFINE TEMP-TABLE ttInvRow NO-UNDO
   FIELD RowNum      AS INTEGER
   FIELD ProductId   AS CHARACTER FORMAT "X(9)"
   FIELD ProductDesc AS CHARACTER FORMAT "X(80)"
   FIELD UnitPrice   AS CHARACTER FORMAT "X(7)"
   FIELD Quantity    AS CHARACTER FORMAT "X(2)"
   FIELD Discount    AS CHARACTER FORMAT "X(7)"
   FIELD TotalPrice  AS CHARACTER FORMAT "X(7)"
   
   INDEX RowNum AS PRIMARY RowNum.

DEFINE TEMP-TABLE ttExtra NO-UNDO
   FIELD RowNum      AS INTEGER
   /* 106 */
   FIELD DataService AS CHARACTER FORMAT "X(9)"
   FIELD PayTerm     AS CHARACTER FORMAT "X(12)"
   FIELD OrderDate   AS CHARACTER FORMAT "X(8)"
   FIELD ResidualAmount AS CHARACTER FORMAT "X(7)"
   FIELD DeliveryType AS CHAR FORMAT "X(1)"
   FIELD KialaCode   AS CHAR FORMAT "X(16)"
   FIELD ContractFileName AS CHAR FORMAT "X(14)"
   FIELD Despachar   AS CHAR FORMAT "X(2)" /* Can delivery be started: 
                                              01=true, 02=false. */
   FIELD MainOrderID AS CHAR FORMAT "X(8)" /* Main line orderId for additional line cases */
   INDEX RowNum AS PRIMARY RowNum.

DEFINE TEMP-TABLE ttTax NO-UNDO
   FIELD Percentage AS DECIMAL
   FIELD NetAmount  AS DECIMAL
   FIELD VatAmount  AS DECIMAL
   FIELD TaxClass   AS CHARACTER.

DEFINE BUFFER bMLOrder         FOR Order.
DEFINE BUFFER bMLOrderCustomer FOR OrderCustomer.
DEFINE BUFFER bALOrderCustomer FOR OrderCustomer.
DEFINE BUFFER bALOrder         FOR Order.
DEFINE BUFFER bALOrderGroup    FOR OrderGroup.
DEFINE BUFFER bMLttExtra       FOR ttExtra.

FUNCTION fRowVat RETURNS DECIMAL
  (INPUT llInvVat   AS LOGICAL,
   INPUT ldeRowAmt  AS DECIMAL,
   INPUT ldeRowPerc AS DECIMAL):

   DEFINE VARIABLE ldeVatAmt AS DECIMAL NO-UNDO.
   
   CASE llInvVat:
      WHEN TRUE THEN DO:
         ldeVatAmt = ROUND(ldeRowAmt * ldeRowPerc / (100 + ldeRowPerc),2).
      END.
      WHEN FALSE THEN DO:
         ldeVatAmt = ROUND(ldeRowAmt * ldeRowPerc / 100,2).
      END.
   END.

   RETURN ldeVatAmt.

END FUNCTION.

FUNCTION fTaxAmount RETURNS LOG
  (INPUT ldeVatPerc AS DECIMAL,
   INPUT ldeVatAmt  AS DECIMAL,
   INPUT ldeRowAmt  AS DECIMAL,
   INPUT lcTaxClass AS CHARACTER):
   
   
   FIND FIRST ttTax WHERE
              ttTax.Percentage = ldeVatPerc AND
              ttTax.TaxClass   = lcTaxClass
   EXCLUSIVE-LOCK NO-ERROR.

   IF NOT AVAIL ttTax THEN DO:
      CREATE ttTax.
      ASSIGN
         ttTax.Percentage = ldeVatPerc
         ttTax.TaxClass   = lcTaxClass.
   END.
   
   ASSIGN
      ttTax.VatAmount = ttTax.VatAmount + ldeVatAmt
      /* amount without vat */
      ttTax.NetAmount = ttTax.NetAmount + ldeRowAmt.

END FUNCTION.

FUNCTION fIsInstallmentConsumerOrder RETURNS LOG:

   /* Prepaid Order */
   IF Order.PayType = TRUE THEN RETURN FALSE.

   /* Non-Consumer */
   IF LOOKUP(AgreeCustomer.CustIdType,"NIF,NIE") = 0 THEN RETURN FALSE.

   /* Make sure orders before deployment should be financed by Yoigo */
   IF (AgreeCustomer.Profession = "" OR AgreeCustomer.Profession = ?) AND
      NOT fIsDirectChannelCetelemOrder(BUFFER Order) THEN RETURN FALSE.

   /* Check Installment contract */
   FOR EACH OfferItem NO-LOCK WHERE
            OfferItem.Brand = Syst.Var:gcBrand AND
            OfferItem.Offer = Order.Offer AND
            OfferItem.BeginStamp <= Order.CrStamp AND
            OfferItem.EndStamp >= Order.CrStamp AND
            OfferItem.ItemType = "PerContract", 
      FIRST DayCampaign NO-LOCK WHERE
            DayCampaign.Brand = Syst.Var:gcBrand AND
            DayCampaign.DCEvent = OfferItem.ItemKey AND
            DayCampaign.DCType = {&DCTYPE_INSTALLMENT}:

      RETURN TRUE.
   END.

   RETURN FALSE.
END FUNCTION.

FUNCTION fErrorLogLine RETURNS LOG(icMessage AS CHAR):

   IF NOT llCreateErrLogFile THEN DO:
      llCreateErrLogFile = TRUE.
      OUTPUT STREAM sErr TO VALUE(lcErrorLogFileName).
   END.

   IF llCreateErrLogFile THEN
      PUT STREAM sErr UNFORMATTED icMessage SKIP.

END FUNCTION.
   
/* must be global for fDelivSIM */
DEFINE VARIABLE liRowNum        AS INTEGER   NO-UNDO.

/* YPR-6059: *Function finds if subscription has orderaction for voice bundle */
/* Here issupport for multiple bundles but it is important to remember 
   field length limitations (10 chars) */
FUNCTION fVoiceBundle RETURNS CHAR
   (iiOrderId AS INT):
   DEF VAR lcVoiceBundles AS CHAR NO-UNDO.
   DEF VAR lcOut AS CHAR NO-UNDO.
   lcVoiceBundles = fcParamC("VOICE_BONO_CONTRACTS").
   DEF BUFFER bOrderaction FOR Orderaction.
   FOR EACH bOrderaction NO-LOCK WHERE
            bOrderaction.Brand EQ Syst.Var:gcBrand AND
            bOrderaction.Orderid EQ iiOrderId AND
            LOOKUP(bOrderaction.ItemKey, lcVoiceBundles) > 0:
     IF lcOut NE "" THEN  lcOut = lcOut + ",".
     lcOut = lcOut + bOrderaction.ItemKey.
   END.
RETURN lcOut.
END.

/* liOrderId = Mainline mobile / Additionaline / Extraline orderid */  
FUNCTION fCreateOrderGroup RETURNS LOGICAL
   (INPUT liOrderId        AS INT, 
    INPUT liMLOrderId      AS INT,
    INPUT llDespacharValue AS LOG):

   CREATE OrderGroup.
   ASSIGN OrderGroup.OrderId   = liOrderId
          OrderGroup.GroupId   = liMLOrderID
          OrderGroup.GroupType = {&OG_LOFILE}
          OrderGroup.Info      = IF llDespacharValue THEN
                                    "01" + CHR(255) + lcFileName
                                 ELSE "02" + CHR(255) + lcFileName
          OrderGroup.CrStamp   = Func.Common:mMakeTS().

   RETURN TRUE.

END FUNCTION.

FUNCTION fDelivSIM RETURNS LOG
   (INPUT liOrderId          AS INT,
    INPUT llgDespacharStatus AS LOG,
    INPUT lcMainOrderId      AS CHAR,
    INPUT lcDespachar        AS CHAR):
   
   DEFINE VARIABLE lcVatInfo1      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcVatInfo2      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOrderChannel  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldeDelivPayment AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeDelivCost    AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeAmtTopUp     AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE liLoop1         AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liLoop2         AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcBIName        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcMNPTime       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcAccProd       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcBillCode      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCustRegi      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcDeliRegi      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldeAccAmt       AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lcUID           AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcPWD           AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldeRowVat       AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeServTotal    AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeServTax      AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeTermTotal    AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeTermTax      AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeAmtVat0      AS DECIMAL   NO-UNDO.

   DEFINE VARIABLE lcPaytermContract  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSMSSender        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldaOrderDate       AS DATE      NO-UNDO. 
   DEFINE VARIABLE liTime             AS INTEGER   NO-UNDO.
   DEFINE VARIABLE ldePayTermFeeTotal AS DECIMAL   NO-UNDO. 
   DEFINE VARIABLE ldePayTermFee      AS DECIMAL   NO-UNDO. 
   DEFINE VARIABLE ldePayTermTax      AS DECIMAL   NO-UNDO. 
   DEFINE VARIABLE ldeUsageVATPerc    AS DECIMAL   NO-UNDO. 
   DEFINE VARIABLE llAddPayTermFee    AS LOGICAL   NO-UNDO. 
   DEFINE VARIABLE lcCLIType          AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcDataBundle       AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcDataService      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcItemKey          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcError            AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldeAmount          AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lcOrderDate        AS CHARACTER NO-UNDO.

   DEFINE VARIABLE ldeResidualAmountTotal    AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeResidualAmountVATExcl  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeResidualAmountVAT      AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE liDelType                 AS INT       NO-UNDO.
   DEFINE VARIABLE lcKialaCode               AS CHAR      NO-UNDO.
   DEFINE VARIABLE lcTerminalBillCode        AS CHAR      NO-UNDO.
   DEFINE VARIABLE lcContractFileName        AS CHAR      NO-UNDO.
   DEFINE VARIABLE lcContractFile            AS CHAR      NO-UNDO.
   DEFINE VARIABLE lcPaymInfo                AS CHAR      NO-UNDO.
   DEFINE VARIABLE lcTaxZone                 AS CHAR      NO-UNDO.
   DEFINE VARIABLE ldeVatPerc                AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeOfferItemAmt           AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE llConsumerWithInstallment AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE llDextraInvoice           AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE ldeCurrAmt                AS DEC       NO-UNDO. 
   DEFINE VARIABLE ldtermdiscamt             AS DEC       NO-UNDO. 
   DEFINE VARIABLE lcTermDiscItem            AS CHAR      NO-UNDO.

   DEFINE BUFFER bufRow         FOR InvRow.
   DEFINE BUFFER bufItem        FOR BillItem.
   DEFINE BUFFER bufGroup       FOR BItemGroup.
   DEFINE BUFFER bufSIM         FOR SIM.
   DEFINE BUFFER bMLOrder       FOR Order. 
   DEFINE BUFFER bOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bOrderAction   FOR OrderAction.
   DEFINE BUFFER bufCLIType     FOR CliType. 
   DEFINE BUFFER bOrderGroup    FOR OrderGroup.   

   RELEASE Invoice.

   FIND FIRST Order NO-LOCK WHERE 
              Order.Brand   = Syst.Var:gcBrand AND 
              Order.OrderId = liOrderId        NO-ERROR.

   IF NOT AVAIL Order THEN RETURN FALSE.            

   /* skip those in control or already closed */
   IF Order.StatusCode = "4" OR
      LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 THEN RETURN FALSE.

   IF llgDespacharStatus THEN 
      IF LOOKUP(STRING(Order.MNPStatus),"5,8") > 0 THEN RETURN FALSE.

   IF CAN-FIND(FIRST bOrderGroup NO-LOCK WHERE
                     bOrderGroup.OrderId   EQ Order.OrderId                   AND
                     bOrderGroup.GroupType EQ {&OG_LOFILE}                    AND
               ENTRY(1,bOrderGroup.Info,CHR(255)) EQ {&DESPACHAR_TRUE_VALUE}) THEN 
   RETURN FALSE.
 
   FIND FIRST MNPProcess WHERE 
      MNPProcess.OrderID = Order.OrderId AND
      (MNPProcess.StatusCode EQ 5 OR 
      MNPProcess.StatusCode EQ 6) NO-LOCK NO-ERROR.
   IF AVAIL MNPProcess THEN DO:
      /* Is MNP Transfer time known ? */
      FIND FIRST MNPSub WHERE
                 MNPSub.MNPSeq = MNPProcess.MNPSeq AND
                 MNPSub.MsSeq  = Order.MsSeq NO-LOCK NO-ERROR.

      IF AVAIL MNPSub THEN DO:
         IF MNPSub.PortingTime NE 0 THEN DO:
            ASSIGN
               lcMNPTime = Func.Common:mTS2HMS(MNPSub.PortingTime)
               lcMNPTime = REPLACE(lcMNPTime," ","")
               lcMNPTime = REPLACE(lcMNPTime,".","")
               lcMNPTime = REPLACE(lcMNPTime,":","")
               lcMNPTime = SUBSTR(lcMNPTime,5,4) + lcMNPTime.
            SUBSTR(lcMNPTime,9,4) = "".
         END.
         ELSE RETURN FALSE.
      END.
   END.

   Func.Common:mSplitTS(Order.CrStamp, OUTPUT ldaOrderDate, OUTPUT liTime).
   lcOrderDate = STRING(ldaOrderDate,"99999999").

   FIND FIRST AgreeCustomer WHERE
              AgreeCustomer.Brand   = Order.Brand   AND
              AgreeCustomer.OrderId = Order.OrderId AND
              AgreeCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
   NO-LOCK NO-ERROR.
   IF NOT AVAIL AgreeCustomer THEN RETURN FALSE.

   /* Dextra will generate the Sales Invoice for Postpaid Terminal Orders */
   llDextraInvoice = fIsTerminalOrder(Order.OrderId,OUTPUT lcTerminalBillCode).

   IF NOT llDextraInvoice THEN DO:

      /* YDR-1034-Move the Sales invoice creation
         from order process to Dextra */
      IF Order.InvNum = 0 OR Order.InvNum = ? THEN DO:
         RUN Mc/cashfee.p(Order.OrderID,
                       1,                     /* action 1=create fees */
                       OUTPUT lcError,
                       OUTPUT ldeAmount,
                       OUTPUT lcError).

         IF lcError BEGINS "Error" THEN DO:
            Func.Common:mWriteMemo("Order",
                             STRING(Order.OrderID),
                             0,
                             "CASH INVOICE FAILED",
                             lcError).
            RETURN FALSE.
         END.
      END. /* IF Order.InvNum = 0 OR Order.InvNum = ? THEN DO: */

      FIND FIRST Invoice WHERE
                 Invoice.InvNum = Order.InvNum
           NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Invoice THEN RETURN FALSE.
   END.
   ELSE DO:
      /* Dextra Terminal Price */
      FIND FIRST TerminalConf WHERE
                 TerminalConf.TerminalCode = lcTerminalBillCode AND
                 TerminalConf.ValidFrom <= ldaOrderDate AND
                 TerminalConf.ValidTo >= ldaOrderDate NO-LOCK NO-ERROR.
      IF NOT AVAIL TerminalConf OR TerminalConf.DextraPrice = 0 OR
         TerminalConf.DextraPrice = ? THEN DO:
         fErrorLogLine(STRING(Order.OrderId) +
                       "|ERROR:Terminal dextra price is not defined").
         RETURN FALSE.
      END.

      FIND FIRST OrderPayment NO-LOCK WHERE
                 OrderPayment.Brand = Syst.Var:gcBrand AND
                 OrderPayment.OrderId = Order.OrderId NO-ERROR.
      IF AVAIL OrderPayment THEN DO:
         IF OrderPayment.Method EQ {&ORDERPAYMENT_M_CREDIT_CARD} THEN 
            lcPaymInfo = "1". /*credit card*/
         ELSE IF OrderPayment.Method EQ {&ORDERPAYMENT_M_PAYPAL} THEN 
            lcPaymInfo = "6". /*paypal*/
         ELSE lcPaymInfo = "0". /*payment on delivery*/
      END.
      ELSE lcPaymInfo = "1".

      /* Check Postpaid Consumer order with Installment */
      llConsumerWithInstallment = fIsInstallmentConsumerOrder().

      /* Send PDF contract to Dextra */
      IF llConsumerWithInstallment AND 
         llgDespacharStatus        THEN DO:

         ASSIGN
            lcContractFileName = STRING(Order.OrderId) + ".pdf"
            lcContractFile = lcContractsDir + lcContractFileName.

         IF SEARCH(lcContractFile) = ? THEN DO:
            fErrorLogLine(STRING(Order.OrderId) +
                          "|ERROR:PDF contract not found").
            RETURN FALSE.
         END.

         /* Add contract file to TAR file */
         FILE-INFO:FILE-NAME = lcContractTARFile.
         IF FILE-INFO:FILE-TYPE BEGINS "F" THEN lcTarOption = "r".
         ELSE lcTarOption = "c".

         UNIX SILENT VALUE("tar -" + lcTarOption + 
                           "f " + lcContractTARFile +
                           " -C " + lcContractsDir + " " + 
                           lcContractFileName).

         /* Now move contract file to done directory */
         UNIX SILENT VALUE("mv " + lcContractFile + " " + lcContractsOutDir).
      END.
   END.

   lcOrderChannel = STRING(LOOKUP(Order.OrderChannel,
                                  "Self,TeleSales,POS,CC,,,Emission"),"99").
   CASE Order.OrderChannel:
      WHEN "fusion_self" THEN lcOrderChannel = "01".
      WHEN "fusion_telesales" THEN lcOrderChannel = "02".
      WHEN "fusion_pos" OR WHEN "fusion_pos_pro" OR WHEN "pos_pro" THEN lcOrderChannel = "03".
      WHEN "fusion_cc" THEN lcOrderChannel = "04".
      WHEN "fusion_emission" THEN lcOrderChannel = "07".
      WHEN "Telesales_PRO" OR 
      WHEN "Fusion_Telesales_PRO" THEN lcOrderChannel = "08".
      WHEN "CC_PRO" OR
      WHEN "Fusion_CC_PRO" THEN lcOrderChannel = "09".
      WHEN "Emission_PRO" OR
      WHEN "Fusion_Emission_PRO" THEN lcOrderChannel = "10".
   END CASE.

   FIND FIRST DelivCustomer WHERE
              DelivCustomer.Brand   = Order.Brand   AND
              DelivCustomer.OrderId = Order.OrderId AND
              DelivCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_DELIVERY}
   NO-LOCK NO-ERROR.
   
   IF NOT AVAIL DelivCustomer THEN DO:

      FIND FIRST DelivCustomer WHERE
                 DelivCustomer.Brand   = Order.Brand   AND
                 DelivCustomer.OrderId = Order.OrderId AND
                 DelivCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
      NO-LOCK NO-ERROR.
   
   END.

   FIND FIRST ContactCustomer WHERE
              ContactCustomer.Brand   = Order.Brand AND
              ContactCustomer.OrderId = Order.OrderId AND
              ContactCustomer.Rowtype = {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT}
   NO-LOCK NO-ERROR.
   
   IF NOT AVAIL ContactCustomer THEN DO:

      FIND FIRST ContactCustomer WHERE
                 ContactCustomer.Brand   = Order.Brand AND
                 ContactCustomer.OrderId = Order.OrderId AND
                 ContactCustomer.Rowtype = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
      NO-LOCK NO-ERROR.
   
   END.
   
   FIND FIRST OrderAccessory WHERE
              OrderAccessory.Brand   = Order.Brand AND
              OrderAccessory.OrderId = Order.OrderId AND
              OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE})
   NO-LOCK NO-ERROR.
   IF AVAIL OrderAccessory THEN
      lcAccProd = OrderAccessory.ProductCode.
   ELSE lcAccProd = "".
   
   IF Order.Offer > "" THEN
      FOR FIRST Offer NO-LOCK WHERE
                Offer.Brand = Syst.Var:gcBrand AND
                Offer.Offer = Order.Offer:

          /* Check Bono in Order Action */
          lcItemKey = fGetDataBundleInOrderAction(Order.OrderID,"BONO").

          fConvBundleToCommName(INPUT lcItemKey,
                                OUTPUT lcDataService,
                                OUTPUT lcSMSSender).
        
          IF CAN-FIND(FIRST OfferItem NO-LOCK WHERE
                            OfferItem.Brand      = Syst.Var:gcBrand          AND
                            OfferItem.Offer      = Offer.Offer      AND
                            OfferItem.ItemType   = "ServicePackage" AND
                            OfferItem.ItemKey    = "BB"             AND
                            OfferItem.EndStamp   >= Order.CrStamp   AND
                            OfferItem.BeginStamp <= Order.CrStamp) THEN DO:
             IF lcDataService > "" THEN
                lcDataService = lcDataService + "-" + "BB".
             ELSE
                lcDataService = "BB".
          END. /* IF CAN-FIND(FIRST OfferItem NO-LOCK WHERE */

          IF lcAccProd = "" THEN
             FOR EACH OfferItem NO-LOCK WHERE
                      OfferItem.Brand      = Syst.Var:gcBrand        AND
                      OfferItem.Offer      = Offer.Offer    AND
                      OfferItem.ItemType   = "BillItem"     AND
                      OfferItem.EndStamp   >= Order.CrStamp AND
                      OfferItem.BeginStamp <= Order.CrStamp,
                 FIRST BillItem NO-LOCK WHERE
                       BillItem.Brand = Syst.Var:gcBrand AND
                       BillItem.BillCode = OfferItem.ItemKey AND
                       BillItem.BIGroup = "7":
                 CASE BillItem.BillCode: /* YPR-314 */
                     WHEN "P045520A3" THEN lcAccProd = "P045520A2".
                     WHEN "P075MF663" THEN lcAccProd = "P075MF662".
                     WHEN "P075MF6A3" THEN lcAccProd = "P075MF6A2".
                     WHEN "P043E3673" THEN lcAccProd = "P043E3672".
                     WHEN "P043E36M3" THEN lcAccProd = "P043E36M2".
                     OTHERWISE lcAccProd = BillItem.BillCode.
                 END.
                 LEAVE.
             END. /* FOR EACH OfferItem NO-LOCK WHERE */
      END. /* FOR FIRST Offer NO-LOCK WHERE */
   
   ldeAccAmt = 0.

   FIND FIRST Region WHERE
              Region.Region = AgreeCustomer.Region
   NO-LOCK NO-ERROR.
   IF AVAIL Region THEN
      lcCustRegi = Region.RgName.

   FIND FIRST Region WHERE
              Region.Region = DelivCustomer.Region
   NO-LOCK NO-ERROR.
   IF AVAIL Region THEN
      lcDeliRegi = Region.RgName.
   
   get_account_data(Order.CustNum, OUTPUT lcUID, OUTPUT lcPWD).

   liRowNum = liRowNum + 1.

   /* Get IPL/FLAT Tariff CLIType */
   IF LOOKUP(Order.CLIType,lcBundleCLITypes) > 0 THEN DO:
      lcDataBundle = fGetDataBundleInOrderAction(Order.OrderID,Order.CLIType).
      lcCLIType = fConvBundleToCLIType(lcDataBundle).
      IF lcCLIType = "" THEN lcCLIType = Order.CLIType.
   END.
   ELSE lcCLIType = Order.CLIType.
   
   IF Order.CLIType EQ "CONT15" THEN DO:
      
      IF Order.OrderType EQ 2 THEN DO:
         IF fGetCurrentSpecificBundle(Order.MsSeq, "VOICE") EQ "VOICE100" THEN
            lcCLIType = "CONT15V100".
      END.
      ELSE DO:
      
         IF ldaCont15PromoFrom NE ? AND
            ldaCont15PromoEnd NE ? AND
            ldaOrderDate >= ldaCont15PromoFrom AND
            ldaOrderDate <= ldaCont15PromoEnd THEN
            lcCLIType = "CONT15V100".
      END.
   END.

   FOR EACH OfferItem NO-LOCK WHERE
            OfferItem.Brand = Syst.Var:gcBrand AND
            OfferItem.Offer = Order.Offer AND
            OfferItem.BeginStamp <= Order.CrStamp AND
            OfferItem.EndStamp >= Order.CrStamp AND
            OfferItem.ItemType = "PerContract", 
      FIRST DayCampaign NO-LOCK WHERE
            DayCampaign.Brand = Syst.Var:gcBrand AND
            DayCampaign.DCEvent = OfferItem.ItemKey AND
            DayCampaign.DCType = {&DCTYPE_INSTALLMENT}:

      lcPaytermContract = DayCampaign.DCEvent.

      ldeUsageVATPerc = fRegionTaxPerc(AgreeCustomer.Region,
                                       "1",
                                       (IF AVAIL Invoice THEN Invoice.InvDate
                                        ELSE ldaOrderDate)).

      IF OfferItem.Amount > 0 THEN DO:
         ASSIGN 
            ldeResidualAmountTotal = OfferItem.Amount
            ldeResidualAmountVATExcl = ROUND(ldeResidualAmountTotal /
                                             (1 + (ldeUsageVATPerc / 100)),2)
            ldeResidualAmountVAT = ldeResidualAmountTotal - ldeResidualAmountVATExcl.
      END.

      FIND FIRST FMItem NO-LOCK WHERE
                 FMItem.Brand     = Syst.Var:gcBrand AND
                 FMItem.FeeModel  = DayCampaign.FeeModel AND
                 FMItem.ToDate   >= ldaOrderDate AND
                 FMItem.FromDate <= ldaOrderDate NO-ERROR.
      IF AVAIL FMItem THEN
         ASSIGN
            ldePayTermFeeTotal = fmitem.FFItemQty * fmitem.Amount.
            ldePayTermFee = ROUND(ldePayTermFeeTotal /
                                  (1 + (ldeUsageVATPerc / 100)),2).
            ldePayTermTax = ldePayTermFeeTotal - ldePayTermFee.
            llAddPayTermFee = TRUE.

      LEAVE.
   END.

   /* try to update SIM status, these are for the orders which are created 
      before deployment is done - GAP phase 2*/
   FIND FIRST SIM EXCLUSIVE-LOCK WHERE
              SIM.Brand   EQ Syst.Var:gcBrand AND 
              SIM.ICC     EQ Order.ICC        AND 
              SIM.SimStat EQ {&SIM_SIMSTAT_CHOSEN_TO_ORDER} NO-ERROR NO-WAIT.

   CREATE ttOneDelivery.
   ASSIGN
      ttOneDelivery.RowNum        = liRowNum
      ttOneDelivery.OrderId       = Order.OrderId
      ttOneDelivery.OrderType     = Order.OrderType
      ttOneDelivery.RequestID     = IF Order.CrStamp > 20080910.21600
                                    THEN STRING(Order.OrderID)
                                    ELSE STRING(Order.MsSeq)
      ttOneDelivery.ActionID      = "0" 
      ttOneDelivery.ProductID     = lcAccProd
      ttOneDelivery.ContractID    = STRING(Order.ContractID)
      ttOneDelivery.NIE           = AgreeCustomer.CustId WHEN AgreeCustomer.CustIdType = "NIE"
      ttOneDelivery.NIF           = AgreeCustomer.CustId WHEN AgreeCustomer.CustIdType = "NIF"
      ttOneDelivery.CIF           = AgreeCustomer.CustId WHEN AgreeCustomer.CustIdType = "CIF"
      ttOneDelivery.PassPort      = AgreeCustomer.CustId WHEN AgreeCustomer.CustIdType = "PassPort"
      ttOneDelivery.SubsType      = IF lcCLIType BEGINS "CONTFH" THEN SUBSTRING(lcClitype,5) ELSE lcCLIType
      ttOneDelivery.ICCNum        = IF AVAIL SIM THEN SUBSTR(SIM.ICC,7) ELSE SUBSTR(Order.ICC,7)
      ttOneDelivery.MSISDN        = Order.CLI
      ttOneDelivery.TmpMSISDN     = fVoiceBundle(Order.OrderID) /*YPR-6059*/
      ttOneDelivery.MNPState      = STRING(Order.MNPStatus = 0,"0/1")
      ttOneDelivery.VoiceMail     = "633633633"
      ttOneDelivery.XFUserID      = lcUID
      ttOneDelivery.XFPWD         = lcPWD
      ttOneDelivery.Company       = AgreeCustomer.Company
      ttOneDelivery.Name          = ContactCustomer.FirstName
      ttOneDelivery.SurName1      = ContactCustomer.SurName1
      ttOneDelivery.SurName2      = ContactCustomer.SurName2
      ttOneDelivery.DelivCO       = ENTRY(1,Order.Campaign,";")
      ttOneDelivery.DelivAddr     = DelivCustomer.Address
      ttOneDelivery.DelivCity     = DelivCustomer.PostOffice
      ttOneDelivery.DelivZip      = DelivCustomer.ZIP
      ttOneDelivery.DelivRegi     = lcDeliRegi
      ttOneDelivery.DelivCoun     = DelivCustomer.Country
      ttOneDelivery.CustAddr      = AgreeCustomer.Address
      ttOneDelivery.CustCity      = AgreeCustomer.PostOffice
      ttOneDelivery.CustZip       = AgreeCustomer.ZIP
      ttOneDelivery.CustRegi      = lcCustRegi
      ttOneDelivery.CustCoun      = AgreeCustomer.Country
      ttOneDelivery.MobConNum     = ContactCustomer.Mobile
      ttOneDelivery.FixConNum     = ContactCustomer.Fixed
      ttOneDelivery.EMail         = ContactCustomer.eMail
      ttOneDelivery.PaymInfo      = 
         (IF llDextraInvoice THEN lcPaymInfo
          ELSE IF AVAIL Invoice AND Invoice.ChargeType = 6  THEN "6"
          ELSE IF AVAIL Invoice AND Invoice.InvType = 6     THEN "0"
          ELSE IF AVAIL Invoice AND Invoice.InvType = 7     THEN "1"
          ELSE "")
      ttOneDelivery.TopUp         = STRING(ldeAmtTopUp,"zzz9.99")
      ttOneDelivery.MNPTransT     = lcMNPTime
      ttOneDelivery.InvRefNum     = Invoice.RefNum WHEN AVAIL Invoice
      ttOneDelivery.InvNum        = Invoice.ExtInvId WHEN AVAIL Invoice
      ttOneDelivery.SalesChan     = lcOrderChannel
      ttOneDelivery.DelivPaym     = STRING(ldeDelivPayment)
      ttOneDelivery.DelivCost     = STRING(ldeDelivCost)
      ttOneDelivery.ServTotal     = "0"
      ttOneDelivery.TermTotal     = "0"
      ttOneDelivery.TaxService    = "0"
      ttOneDelivery.TaxTerminal   = "0"
      ttOneDelivery.InvoiceTotal  = (IF AVAIL Invoice THEN STRING(Invoice.CurrAmt) ELSE "0")
      ttOneDelivery.DiscountTotal = (IF AVAIL Invoice THEN STRING(Invoice.DirDisc) ELSE "0")
      .

   /* YDR-896: Add admin id in case of CIF order customer */
   IF AgreeCustomer.CustIdType = "CIF" THEN
   ASSIGN
      ttOneDelivery.NIE           = AgreeCustomer.AuthCustId WHEN AgreeCustomer.AuthCustIdType = "NIE"
      ttOneDelivery.NIF           = AgreeCustomer.AuthCustId WHEN AgreeCustomer.AuthCustIdType = "NIF"
      ttOneDelivery.PassPort      = AgreeCustomer.AuthCustId WHEN AgreeCustomer.AuthCustIdType = "PassPort".

   IF Order.OrderType eq 2 THEN DO:
      /* Overwrite certain expeptional values */
      ASSIGN
         ttOneDelivery.MobConNum = Order.CLI
         ttOneDelivery.FixConNum = ContactCustomer.Mobile.

      /* If ICC change not requested with Renewal Order */
      IF AVAIL SIM AND SIM.ICC = MobSub.ICC THEN ttOneDelivery.ICCNum = "".

      /* Channel information */
      IF LOOKUP(Order.OrderChannel,"renewal_telesales,retention,renewal_ctc") > 0 THEN
         ttOneDelivery.SalesChan = "06".
      ELSE
         ttOneDelivery.SalesChan = "05".
   END.

   liLoop1 = 0.
   
   /* SIM row */
   IF AVAIL Invoice THEN
   FOR EACH InvRow OF Invoice NO-LOCK,
      FIRST BillItem NO-LOCK WHERE
            BillItem.Brand    = Syst.Var:gcBrand AND
            BillItem.BillCode = InvRow.BillCode,
      FIRST BitemGroup NO-LOCK WHERE
            BitemGroup.Brand   = BillItem.Brand   AND
            BitemGroup.BIGroup = BillItem.BIGroup AND
            BItemGroup.BIGroup = "9":
   
      lcBIName = fTranslationName(Syst.Var:gcBrand,
                                  1,
                                  InvRow.BillCode,
                                  INT(AgreeCustomer.Language),
                                  IF InvRow.ToDate NE ?
                                  THEN InvRow.ToDate
                                  ELSE Invoice.ToDate).
         
      IF lcBIName = ? THEN lcBIName = BillItem.BIName.

      liLoop1 = liLoop1 + 1.

      ldeRowVat = fRowVat(Invoice.VatIncl,InvRow.Amt,InvRow.VatPerc).

      /* row amount without vat */
      ldeAmtVat0 = InvRow.Amt - 
                   (IF Invoice.VatIncl THEN ldeRowVat ELSE 0).
      fTaxAmount(InvRow.VatPerc,ldeRowVat,ldeAmtVat0,BillItem.TaxClass).

      /* Renewal: ProductId Renove Targeta and Renove Contrato */

      CREATE ttInvRow.
      ASSIGN
         ttInvRow.RowNum      = ttOneDelivery.RowNum
         ttInvRow.ProductId   = InvRow.BillCode
         ttInvRow.ProductDesc = lcBIName
         ttInvRow.UnitPrice   = STRING(ldeAmtVat0)
         ttInvRow.Quantity    = STRING(InvRow.Qty)
         ttInvRow.Discount    = STRING(0.0)
         ttInvRow.TotalPrice  = STRING(ldeAmtVat0).

   END.
   
   /* TopUp rows: invoiced + discount */
   IF AVAIL Invoice THEN
   FOR EACH InvRow OF Invoice NO-LOCK,
      FIRST BillItem NO-LOCK WHERE
            BillItem.Brand    = Syst.Var:gcBrand AND
            BillItem.BillCode = InvRow.BillCode,
      FIRST BitemGroup NO-LOCK WHERE
            BitemGroup.Brand   = BillItem.Brand   AND
            BitemGroup.BIGroup = BillItem.BIGroup AND
            BItemGroup.BIGroup = "12":

      /* YDR-868 - Exclude extra initial topup of TARJ6 */
      IF BillItem.BillCode = "TS0000054" THEN NEXT.

      lcBIName = fTranslationName(Syst.Var:gcBrand,
                                  1,
                                  InvRow.BillCode,
                                  INT(AgreeCustomer.Language),
                                  IF InvRow.ToDate NE ?
                                  THEN InvRow.ToDate
                                  ELSE Invoice.ToDate).

      IF lcBIName = ? THEN lcBIName = BillItem.BIName.

      CREATE ttInvRow.
      ASSIGN
         liLoop1              = liLoop1 + 1
         ttInvRow.RowNum      = ttOneDelivery.RowNum
         ttInvRow.ProductId   = InvRow.BillCode
         ttInvRow.ProductDesc = lcBIName
         ttInvRow.UnitPrice   = STRING(InvRow.Amt)
         ttInvRow.Quantity    = STRING(InvRow.Qty)
         ttInvRow.Discount    = STRING(0.0)
         ttInvRow.TotalPrice  = STRING(InvRow.Amt).

      FOR EACH bufRow OF Invoice NO-LOCK WHERE
               SUBSTRING(bufRow.BillCode,1,3) =
               SUBSTRING(InvRow.BillCode,1,3),
         FIRST bufItem NO-LOCK WHERE        
               bufItem.Brand    = Syst.Var:gcBrand AND
               bufItem.BillCode = bufRow.BillCode,
         FIRST bufGroup NO-LOCK WHERE
               bufGroup.Brand   = bufItem.Brand   AND
               bufGroup.BIGroup = bufItem.BIGroup AND
               bufGroup.BIGroup = "10":

         /* YDR-868 - Exclude extra initial topup discount of TARJ6 */
         IF BillItem.BillCode = "TS0DISC54" THEN NEXT.

         ASSIGN
            ttInvRow.TotalPrice = STRING(DEC(ttInvRow.TotalPrice) - DEC(InvRow.Amt))
            ttInvRow.Discount   = STRING(ABS(InvRow.Amt)).

      END.

   END.

   /* others */
   IF AVAIL Invoice THEN
   FOR EACH InvRow OF Invoice NO-LOCK,
      FIRST BillItem NO-LOCK WHERE
            BillItem.Brand    = Syst.Var:gcBrand AND
            BillItem.BillCode = InvRow.BillCode,
      FIRST BitemGroup NO-LOCK WHERE
            BitemGroup.Brand   = BillItem.Brand AND
            BitemGroup.BIGroup = BillItem.BIGroup:

      IF LOOKUP(BItemGroup.BIGroup,"9,12,10") = 0 THEN DO:
                 
         IF BItemGroup.BIGroup EQ "7" THEN
            CASE BillItem.BillCode: /* YPR-314 */
                WHEN "P045520A3" THEN lcBillCode = "P045520A2".
                WHEN "P075MF663" THEN lcBillCode = "P075MF662".
                WHEN "P075MF6A3" THEN lcBillCode = "P075MF6A2".
                WHEN "P043E3673" THEN lcBillCode = "P043E3672".
                WHEN "P043E36M3" THEN lcBillCode = "P043E36M2".
                OTHERWISE lcBillCode = BillItem.BillCode.
            END.
         ELSE lcBillCode = BillItem.BillCode.

         ASSIGN
            liLoop1  = liLoop1 + 1
            lcBIName = fTranslationName(Syst.Var:gcBrand,
                                        1,
                                        lcBillCode,
                                        INT(AgreeCustomer.Language),
                                        IF InvRow.ToDate NE ?
                                        THEN InvRow.ToDate
                                        ELSE Invoice.ToDate).
            ldeRowVat = fRowVat(Invoice.VatIncl,InvRow.Amt,InvRow.VatPerc).

         /* row amount without vat */
         ldeAmtVat0 = InvRow.Amt - 
                      (IF Invoice.VatIncl THEN ldeRowVat ELSE 0).
         fTaxAmount(InvRow.VatPerc,ldeRowVat,ldeAmtVat0,BillItem.TaxClass).               

         IF lcBIName = ? THEN lcBIName = BillItem.BIName.

         CREATE ttInvRow.
         ASSIGN
            ttInvRow.RowNum      = ttOneDelivery.RowNum
            ttInvRow.ProductId   = lcBillCode
            ttInvRow.ProductDesc = lcBIName
            ttInvRow.UnitPrice   = STRING(ldeAmtVat0)
            ttInvRow.Quantity    = STRING(InvRow.Qty)
            ttInvRow.Discount    = STRING(0.0)
            ttInvRow.TotalPrice  = STRING(ldeAmtVat0).

         /* terminal and its discount */ 
         IF BillItem.BiGroup = "7" THEN DO:

            ASSIGN
               ldeAccAmt = ldeAccAmt + ldeAmtVat0.
         
            IF llAddPayTermFee THEN ASSIGN
               ttInvRow.UnitPrice = STRING(ldeAmtVat0 + ldePayTermFee + ldeResidualAmountVATExcl)
               ttInvRow.TotalPrice  = STRING(ldeAmtVat0 + ldePayTermFee + ldeResidualAmountVATExcl)
               llAddPayTermFee = FALSE.
         END.
      END.

   END.

   /* SIM, payment on delivery, Terminal product row */
   IF llDextraInvoice THEN DO:
      lcTaxZone = fRegionTaxZone(AgreeCustomer.Region).
      /* SIM only */
      FIND FIRST OrderAction WHERE
                 OrderAction.Brand    = Syst.Var:gcBrand AND
                 OrderAction.OrderId  = Order.OrderId AND
                 OrderAction.ItemType = "SIMType" NO-LOCK NO-ERROR.
      IF AVAIL OrderAction AND Order.ICC EQ "" THEN DO:
         lcBillCode = fGetSIMBillItem(OrderAction.ItemKey,Order.PayType).
         FOR FIRST BillItem NO-LOCK WHERE
                   BillItem.Brand    = Syst.Var:gcBrand AND
                   BillItem.BillCode = lcBillCode,
             FIRST BitemGroup NO-LOCK WHERE
                   BitemGroup.Brand   = BillItem.Brand AND
                   BitemGroup.BIGroup = BillItem.BIGroup:

            ASSIGN
               liLoop1  = liLoop1 + 1
               lcBIName = fTranslationName(Syst.Var:gcBrand,
                                           1,
                                           lcBillCode,
                                           INT(AgreeCustomer.Language),
                                           ldaOrderDate).
            ASSIGN
               ldeOfferItemAmt = 0
               ldeVatPerc = fTaxPerc(lcTaxZone,BillItem.TaxClass,ldaOrderDate)
               ldeRowVat  = fRowVat(TRUE,ldeOfferItemAmt,ldeVatPerc).

            /* row amount without vat */
            ldeAmtVat0 = ldeOfferItemAmt - ldeRowVat.
            fTaxAmount(ldeVatPerc,ldeRowVat,ldeAmtVat0,BillItem.TaxClass). 

            IF lcBIName = ? THEN lcBIName = BillItem.BIName.

            CREATE ttInvRow.
            ASSIGN
               ttInvRow.RowNum      = ttOneDelivery.RowNum
               ttInvRow.ProductId   = lcBillCode
               ttInvRow.ProductDesc = lcBIName
               ttInvRow.UnitPrice   = STRING(ldeAmtVat0)
               ttInvRow.Quantity    = "1"
               ttInvRow.Discount    = STRING(0.0)
               ttInvRow.TotalPrice  = STRING(ldeAmtVat0).
         END.
      END.

      /* payment on delivery */
      IF lcPaymInfo = "0" AND Order.FeeModel > "" THEN DO:
         FOR FIRST FeeModel WHERE
                   FeeModel.Brand = Syst.Var:gcBrand AND
                   FeeModel.FeeModel = Order.FeeModel NO-LOCK,
             FIRST FMItem NO-LOCK WHERE
                   FMItem.Brand     = Syst.Var:gcBrand           AND
                   FMITem.FeeModel  = FeeModel.FeeModel AND
                   FMItem.FromDate <= ldaOrderDate      AND
                   FMItem.ToDate   >= ldaOrderDate,
             FIRST BillItem NO-LOCK WHERE
                   BillItem.Brand    = Syst.Var:gcBrand AND
                   BillItem.BillCode = FMItem.BillCode,
             FIRST BitemGroup NO-LOCK WHERE
                   BitemGroup.Brand   = Syst.Var:gcBrand AND
                   BitemGroup.BIGroup = BillItem.BIGroup,
             FIRST PriceList NO-LOCK WHERE
                   PriceList.Brand     = Syst.Var:gcBrand AND
                   PriceList.PriceList = FMItem.PriceList:

             ASSIGN
               liLoop1  = liLoop1 + 1
               lcBIName = fTranslationName(Syst.Var:gcBrand,
                                           1,
                                           BillItem.BillCode,
                                           INT(AgreeCustomer.Language),
                                           ldaOrderDate).

            ASSIGN
               ldeVatPerc = fTaxPerc(lcTaxZone,BillItem.TaxClass,ldaOrderDate)
               ldeRowVat  = fRowVat(PriceList.InclVat,FMItem.Amount,ldeVatPerc).

            /* row amount without vat */
            ldeAmtVat0 = FMItem.Amount - ldeRowVat.
            fTaxAmount(ldeVatPerc,ldeRowVat,ldeAmtVat0,BillItem.TaxClass).

            IF lcBIName = ? THEN lcBIName = BillItem.BIName.

            CREATE ttInvRow.
            ASSIGN
               ttInvRow.RowNum      = ttOneDelivery.RowNum
               ttInvRow.ProductId   = BillItem.BillCode
               ttInvRow.ProductDesc = lcBIName
               ttInvRow.UnitPrice   = STRING(ldeAmtVat0)
               ttInvRow.Quantity    = "1"
               ttInvRow.Discount    = STRING(0.0)
               ttInvRow.TotalPrice  = STRING(ldeAmtVat0)
               ldeCurrAmt = ldeCurrAmt + FMItem.Amount.

         END.
      END.

      /* Terminal row */
      FOR EACH OfferItem NO-LOCK WHERE
               OfferItem.Brand = Syst.Var:gcBrand AND
               OfferItem.Offer = Order.Offer AND
               OfferItem.BeginStamp <= Order.CrStamp AND
               OfferItem.EndStamp >= Order.CrStamp AND
               OfferItem.ItemType = "BillItem",
         FIRST BillItem NO-LOCK WHERE
               BillItem.Brand    = Syst.Var:gcBrand AND
               BillItem.BillCode = OfferItem.ItemKey,
         FIRST BitemGroup NO-LOCK WHERE
               BitemGroup.Brand   = Syst.Var:gcBrand AND
               BitemGroup.BIGroup = BillItem.BIGroup:

         IF LOOKUP(BItemGroup.BIGroup,"9,12,10") = 0 THEN DO:
                 
            IF BItemGroup.BIGroup EQ "7" THEN
               CASE BillItem.BillCode: /* YPR-314 */
                  WHEN "P045520A3" THEN lcBillCode = "P045520A2".
                  WHEN "P075MF663" THEN lcBillCode = "P075MF662".
                  WHEN "P075MF6A3" THEN lcBillCode = "P075MF6A2".
                  WHEN "P043E3673" THEN lcBillCode = "P043E3672".
                  WHEN "P043E36M3" THEN lcBillCode = "P043E36M2".
                  OTHERWISE lcBillCode = BillItem.BillCode.
               END.
            ELSE lcBillCode = BillItem.BillCode.

            ASSIGN
               liLoop1  = liLoop1 + 1
               lcBIName = fTranslationName(Syst.Var:gcBrand,
                                           1,
                                           lcBillCode,
                                           INT(AgreeCustomer.Language),
                                           ldaOrderDate).

            ASSIGN
               ldeVatPerc = fTaxPerc(lcTaxZone,BillItem.TaxClass,ldaOrderDate)
               ldeRowVat  = fRowVat(OfferItem.VatIncl,OfferItem.Amount,ldeVatPerc).

            /* row amount without vat */
            ldeAmtVat0 = OfferItem.Amount - 
                         (IF OfferItem.VatIncl THEN ldeRowVat ELSE 0).
            fTaxAmount(ldeVatPerc,ldeRowVat,ldeAmtVat0,BillItem.TaxClass).

            IF lcBIName = ? THEN lcBIName = BillItem.BIName.

            CREATE ttInvRow.
            ASSIGN
               ttInvRow.RowNum      = ttOneDelivery.RowNum
               ttInvRow.ProductId   = lcBillCode
               ttInvRow.ProductDesc = lcBIName
               ttInvRow.UnitPrice   = STRING(ldeAmtVat0)
               ttInvRow.Quantity    = "1"
               ttInvRow.Discount    = STRING(0.0)
               ttInvRow.TotalPrice  = STRING(ldeAmtVat0)
               ldeCurrAmt = ldeCurrAmt + OfferItem.Amount.

            /* terminal and its discount */ 
            IF BillItem.BiGroup = "7" THEN DO:
               ldeAccAmt = ldeAccAmt + ldeAmtVat0.
               IF NOT (lcBillCode BEGINS "CPDISC") THEN DO:
                  ttInvRow.TotalPrice = STRING(TerminalConf.DextraPrice).

                  IF llAddPayTermFee THEN
                     ASSIGN
                        ttInvRow.UnitPrice = STRING(ldeAmtVat0 + ldePayTermFee +
                                                    ldeResidualAmountVATExcl)
                        llAddPayTermFee = FALSE.
               END.

               /* Terminal discount */
               if ldtermdiscamt eq 0 then
               for first orderaccessory of order NO-LOCK where
                         orderaccessory.terminaltype = ({&terminal_type_phone}) and
                         orderaccessory.discount ne 0:

                  ldtermdiscamt = orderaccessory.discount.

                  IF offeritem.amount < ldTermDiscAmt THEN
                     ldTermDiscAmt = offeritem.amount.

                   ldTermDiscAmt = ldTermDiscAmt * -1.
                  
                  ASSIGN
                     liLoop1  = liLoop1 + 1
                     lcTermDiscItem = fCParamC("OrderTermDisc").
               
                  lcBIName = fTranslationName(Syst.Var:gcBrand,
                                              1,
                                              lcTermDiscItem,
                                              INT(AgreeCustomer.Language),
                                              ldaOrderDate).
                  IF lcBIName = ? THEN lcBIName = lcTermDiscItem.
                  
                  FIND FIRST bBillItem NO-LOCK WHERE
                             bBillItem.Brand    = Syst.Var:gcBrand AND
                             bBillItem.BillCode = lcTermDiscItem NO-ERROR.
            
                  ASSIGN
                     ldeVatPerc = fTaxPerc(lcTaxZone,bBillItem.TaxClass,ldaOrderDate)
                     ldeRowVat  = fRowVat(OfferItem.VatIncl,ldTermDiscAmt,ldeVatPerc).

                  /* row amount without vat */
                  ldeAmtVat0 = ldTermDiscAmt - 
                               (IF OfferItem.VatIncl THEN ldeRowVat ELSE 0).
                  fTaxAmount(ldeVatPerc,ldeRowVat,ldeAmtVat0,bBillItem.TaxClass).
                  
                  CREATE ttInvRow.
                  ASSIGN
                     ttInvRow.RowNum      = ttOneDelivery.RowNum
                     ttInvRow.ProductId   = lcTermDiscItem
                     ttInvRow.ProductDesc = lcBIName
                     ttInvRow.UnitPrice   = STRING(ldeAmtVat0)
                     ttInvRow.Quantity    = "1"
                     ttInvRow.Discount    = STRING(0.0)
                     ttInvRow.TotalPrice  = STRING(ldeAmtVat0)
                     ldeCurrAmt = ldeCurrAmt + ldTermDiscAmt.
               end.
            END.
         END.
      END.
      
      ttOneDelivery.InvoiceTotal = STRING(ldeCurrAmt).
   END.

   DO liLoop2 = liLoop1 TO 8:

      CREATE ttInvRow.
     
      ASSIGN
         ttInvRow.RowNum      = ttOneDelivery.RowNum
         ttInvRow.ProductId   = ""
         ttInvRow.ProductDesc = ""
         ttInvRow.UnitPrice   = ""
         ttInvRow.Quantity    = ""
         ttInvRow.Discount    = ""
         ttInvRow.TotalPrice  = "".

   END.

   ASSIGN
      ldeServTotal = 0
      ldeServTax   = 0
      ldeTermTotal = 0
      ldeTermTax   = 0
      lcVatInfo1   = ""
      lcVatInfo2   = "".
   
   FOR EACH ttTax NO-LOCK:

      IF ttTax.TaxClass = "1" THEN DO:

         ASSIGN
            ldeServTotal = ldeServTotal + ttTax.NetAmount
            ldeServTax   = ldeServTax   + ttTax.VatAmount.

         IF lcVatInfo1 = "" THEN ASSIGN
            lcVatInfo1 = STRING(ttTax.Percentage,"99.9")
            lcVatInfo1 = REPLACE(lcVatInfo1,SESSION:NUMERIC-DECIMAL-POINT,"")
            lcVatInfo1 = fRegionTaxZone(AgreeCustomer.Region) + lcVatInfo1.

      END.
      ELSE IF ttTax.TaxClass = "2" THEN DO:

         ASSIGN
            ldeTermTotal = ldeTermTotal + ttTax.NetAmount
            ldeTermTax   = ldeTermTax   + ttTax.VatAmount.

         IF lcVatInfo2 = "" THEN ASSIGN
            lcVatInfo2 = STRING(ttTax.Percentage,"99.9")
            lcVatInfo2 = REPLACE(lcVatInfo2,SESSION:NUMERIC-DECIMAL-POINT,"")
            lcVatInfo2 = fRegionTaxZone(AgreeCustomer.Region) + lcVatInfo2.

      END. 

   END.

   EMPTY TEMP-TABLE ttTax.
      
   IF lcVatInfo2 = "" THEN lcVatInfo2 = lcVatInfo1.
   IF lcVatInfo1 = "" THEN lcVatInfo1 = lcVatInfo2.

   IF SUBSTR(lcVatInfo1,1,1) = "4" THEN SUBSTR(lcVatInfo1,1,1) = "3".
   IF SUBSTR(lcVatInfo2,1,1) = "4" THEN SUBSTR(lcVatInfo2,1,1) = "3".

   ASSIGN
      ttOneDelivery.TaxInfoServ = lcVatInfo1
      ttOneDelivery.TaxInfoTerm = lcVatInfo2
      ttOneDelivery.TermTotal   = STRING(ldeTermTotal + ldePayTermFee + ldeResidualAmountVATExcl)
      ttOneDelivery.TaxTerminal = STRING(ldeTermTax + ldePayTermTax + ldeResidualAmountVAT)
      ttOneDelivery.ServTotal   = STRING(ldeServTotal)
      ttOneDelivery.TaxService  = STRING(ldeServTax)
      ttOneDelivery.TermAmt     = STRING(ldeAccAmt + ldePayTermFee + ldeResidualAmountVATExcl,"zzz9.99").


   IF Order.DeliverySecure EQ 1
   THEN liDelType = {&ORDER_DELTYPE_POST_SECURE}.
   ELSE IF Order.DeliverySecure EQ 2
   THEN liDelType = {&ORDER_DELTYPE_POS_SECURE}.
   ELSE IF Order.DeliveryType EQ 0 THEN liDelType = {&ORDER_DELTYPE_COURIER}.
   ELSE liDelType = Order.DeliveryType.

   IF llgDespacharStatus  AND
      lcMainOrderId EQ "" AND 
      lcDespachar   EQ "" THEN DO:

      FIND FIRST bOrderGroup NO-LOCK WHERE
                 bOrderGroup.OrderId   EQ ttOneDelivery.OrderId           AND
                 bOrderGroup.GroupType EQ {&OG_LOFILE}                    AND
           ENTRY(1,bOrderGroup.Info,CHR(255)) EQ {&DESPACHAR_FALSE_VALUE} NO-ERROR.

      IF AVAIL bOrderGroup THEN 
         ASSIGN lcMainOrderId = STRING(bOrderGroup.GroupId)
                lcDespachar   = "01".

   END.   

   /* Create Temp-table for DataService (OR extra fields in future) */
   CREATE ttExtra.
   ASSIGN ttExtra.RowNum          = ttOneDelivery.RowNum
          ttExtra.DataService     = lcDataService
          ttExtra.PayTerm         = lcPayTermContract
          ttExtra.OrderDate       = lcOrderDate
          ttExtra.ResidualAmount  = (IF ldeResidualAmountTotal > 0 THEN
                                        STRING(ldeResidualAmountTotal) 
                                     ELSE "")
          ttExtra.DeliveryType     = STRING(liDelType)
          ttExtra.KialaCode        = DelivCustomer.KialaCode WHEN Order.DeliveryType = {&ORDER_DELTYPE_POS}
          ttExtra.ContractFileName = lcContractFileName
          ttExtra.Despachar        = lcDespachar
          ttExtra.MainOrderID      = lcMainOrderId.

   /* update SimStat when all skipping are checked */
   IF NOT (Order.OrderType eq 2) AND 
      llgDespacharStatus         THEN DO:
      
      IF AVAIL SIM THEN 
         SIM.SimStat = 21.
      
      /* Ordergroup record is used to trace the logistics 
         delivery status with despachar and file values */
      fCreateOrderGroup(ttOneDelivery.OrderId,
                        INT(lcMainOrderId),
                        TRUE).
 
   END.

   RETURN TRUE.
END.

FUNCTION pCheckTextSection RETURNS LOG (INPUT-OUTPUT pcText AS CHARACTER,
                                         INPUT plLast AS LOGICAL):
  IF LENGTH(pcText) > 30000 OR plLast THEN
  DO:
     CREATE ttOutputText.
     ASSIGN ttOutputText.cText = REPLACE(pcText, CHR(10), " ")
            ttOutputText.cText = REPLACE(pcText, "+", " ")
            ttOutputText.id = iLargestId.
     iLargestId = iLargestId + 1.
     pcText = "".

     RETURN TRUE.
  END.
  RETURN FALSE.
END.

FUNCTION pLog RETURNS LOG (INPUT pcLogContent AS CHARACTER):
   IF lcLogFile <> "" AND lcLogFile <> ? THEN
      fLog(pcLogContent, "DEXTRA-INFO").
END.

FUNCTION fDelivDevice RETURNS LOG
   (INPUT icDevice  AS CHAR):

   DEFINE VARIABLE lcDeliRegi      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCustRegi      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOrderChannel  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOrderDate     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldaOrderDate    AS DATE      NO-UNDO.
   DEFINE VARIABLE liTime          AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liLoop1         AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liLoop2         AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liTempRegion    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcAddressFields AS CHARACTER NO-UNDO.

   FIND FIRST AgreeCustomer WHERE
              AgreeCustomer.Brand   = Order.Brand   AND
              AgreeCustomer.OrderId = Order.OrderId AND
              AgreeCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
   NO-LOCK NO-ERROR.
   IF NOT AVAIL AgreeCustomer THEN RETURN FALSE.

   FIND FIRST DelivCustomer WHERE
              DelivCustomer.Brand   = Syst.Var:gcBrand   AND
              DelivCustomer.OrderId = Order.OrderId AND
              DelivCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
   NO-LOCK NO-ERROR.

   IF AVAIL DelivCustomer THEN DO:
      /* YTS-9922: Checking additional address fields */
      IF LENGTH(TRIM(DelivCustomer.Block)) > 0 THEN
         lcAddressFields = lcAddressFields + " " +
                           DelivCustomer.Block.

      IF LENGTH(TRIM(DelivCustomer.Door)) > 0 THEN
         lcAddressFields = lcAddressFields + " " +
                           DelivCustomer.Door.

      IF LENGTH(TRIM(DelivCustomer.Letter)) > 0 THEN
         lcAddressFields = lcAddressFields + " " +
                           DelivCustomer.Letter.

      IF LENGTH(TRIM(DelivCustomer.Stair)) > 0 THEN
         lcAddressFields = lcAddressFields + " " +
                           DelivCustomer.Stair.

      IF LENGTH(TRIM(DelivCustomer.Floor)) > 0 THEN
         lcAddressFields = lcAddressFields + " " +
                           DelivCustomer.Floor.

      IF LENGTH(TRIM(DelivCustomer.Hand)) > 0 THEN
         lcAddressFields = lcAddressFields + " " +
                           DelivCustomer.Hand.

      IF LENGTH(TRIM(DelivCustomer.Km)) > 0 THEN
         lcAddressFields = lcAddressFields + " " +
                           DelivCustomer.Km.
      /* Verification */ 
      IF LENGTH(TRIM(lcAddressFields)) = 0 THEN lcAddressFields = "".
   END.
   ELSE DO: /*IF NOT AVAIL DelivCustomer THEN DO: */

      FIND FIRST DelivCustomer WHERE
                 DelivCustomer.Brand   = Order.Brand   AND
                 DelivCustomer.OrderId = Order.OrderId AND
                 DelivCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
      NO-LOCK NO-ERROR.
   END.
   FIND FIRST ContactCustomer WHERE
              ContactCustomer.Brand   = Order.Brand AND
              ContactCustomer.OrderId = Order.OrderId AND
              ContactCustomer.Rowtype = {&ORDERCUSTOMER_ROWTYPE_CIF_CONTACT}
   NO-LOCK NO-ERROR.

   IF NOT AVAIL ContactCustomer THEN DO:

      FIND FIRST ContactCustomer WHERE
                 ContactCustomer.Brand   = Order.Brand AND
                 ContactCustomer.OrderId = Order.OrderId AND
                 ContactCustomer.Rowtype = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
      NO-LOCK NO-ERROR.

   END.
   IF AgreeCustomer.Region > "" THEN DO: 
      FIND FIRST Region WHERE
                 Region.Region = AgreeCustomer.Region
      NO-LOCK NO-ERROR.
      IF AVAIL Region THEN
         lcCustRegi = Region.RgName.
   END.

   IF DelivCustomer.Region > "" THEN DO:
      /* Done because fixed line install address might include
         region as normal text */
      ASSIGN liTempRegion = INT(DelivCustomer.Region) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
         lcDeliRegi = DelivCustomer.Region.
      ELSE DO:   
         FIND FIRST Region WHERE
                    Region.Region = DelivCustomer.Region
         NO-LOCK NO-ERROR.
         IF AVAIL Region THEN
            lcDeliRegi = Region.RgName.
      END.
   END.
   lcOrderChannel = STRING(LOOKUP(Order.OrderChannel,
                                  "Self,TeleSales,POS,CC,,,Emission"),"99").
   CASE Order.OrderChannel:
      WHEN "fusion_self" THEN lcOrderChannel = "01".
      WHEN "fusion_telesales" OR WHEN "Fusion_Telesales_PRO" THEN lcOrderChannel = "02".
      WHEN "fusion_pos" OR WHEN "fusion_pos_pro" OR WHEN "pos_pro" THEN lcOrderChannel = "03".
      WHEN "fusion_cc" THEN lcOrderChannel = "04".
      WHEN "fusion_emission" THEN lcOrderChannel = "07".
   END CASE.
   Func.Common:mSplitTS(Order.CrStamp, OUTPUT ldaOrderDate, OUTPUT liTime).
   lcOrderDate = STRING(ldaOrderDate,"99999999").

   liRowNum = liRowNum + 1.
   CREATE ttOneDelivery.
   ASSIGN
      ttOneDelivery.RowNum        = liRowNum
      ttOneDelivery.OrderId       = Order.OrderId
      ttOneDelivery.OrderType     = Order.OrderType
      ttOneDelivery.RequestID     = STRING(Order.OrderId)
      ttOneDelivery.ActionID      = (IF icDevice = "Router" THEN "1" ELSE "2")
      ttOneDelivery.ProductID     = (IF icDevice = "Router" THEN "R075A67W2" ELSE "G050DTVN2")
      ttOneDelivery.ContractID    = STRING(Order.ContractID)
      ttOneDelivery.NIE           = AgreeCustomer.CustId WHEN AgreeCustomer.CustIdType = "NIE"
      ttOneDelivery.NIF           = AgreeCustomer.CustId WHEN AgreeCustomer.CustIdType = "NIF"
      ttOneDelivery.CIF           = AgreeCustomer.CustId WHEN AgreeCustomer.CustIdType = "CIF"
      ttOneDelivery.PassPort      = AgreeCustomer.CustId WHEN AgreeCustomer.CustIdType = "PassPort"
      ttOneDelivery.SubsType      = order.clitype
      ttOneDelivery.MSISDN        = Order.CLI
      ttOneDelivery.Company       = AgreeCustomer.Company
      ttOneDelivery.Name          = ContactCustomer.FirstName
      ttOneDelivery.SurName1      = ContactCustomer.SurName1
      ttOneDelivery.SurName2      = ContactCustomer.SurName2
      ttOneDelivery.DelivAddr     = DelivCustomer.Address + lcAddressFields
      ttOneDelivery.DelivCity     = DelivCustomer.PostOffice
      ttOneDelivery.DelivZip      = DelivCustomer.ZIP
      ttOneDelivery.DelivRegi     = lcDeliRegi
      ttOneDelivery.DelivCoun     = DelivCustomer.Country
      ttOneDelivery.CustAddr      = AgreeCustomer.Address
      ttOneDelivery.CustCity      = AgreeCustomer.PostOffice
      ttOneDelivery.CustZip       = AgreeCustomer.ZIP
      ttOneDelivery.CustRegi      = lcCustRegi
      ttOneDelivery.CustCoun      = AgreeCustomer.Country
      ttOneDelivery.MobConNum     = ContactCustomer.Mobile
      ttOneDelivery.FixConNum     = ContactCustomer.Fixed
      ttOneDelivery.EMail         = ContactCustomer.eMail
      ttOneDelivery.SalesChan     = lcOrderChannel.

   CREATE ttInvRow.
   ASSIGN
      ttInvRow.RowNum      = ttOneDelivery.RowNum
      ttInvRow.ProductId   = (IF icDevice = "Router" THEN "R075A67W2" ELSE "G050DTVN2") 
      ttInvRow.Quantity    = "1"
      liLoop1              = 1.

   DO liLoop2 = liLoop1 TO 8:

      CREATE ttInvRow.

      ASSIGN
         ttInvRow.RowNum      = ttOneDelivery.RowNum
         ttInvRow.ProductId   = ""
         ttInvRow.ProductDesc = ""
         ttInvRow.UnitPrice   = ""
         ttInvRow.Quantity    = ""
         ttInvRow.Discount    = ""
         ttInvRow.TotalPrice  = "".

   END.

   CREATE ttExtra.
   ASSIGN ttExtra.RowNum       = ttOneDelivery.RowNum
          ttExtra.OrderDate    = lcOrderDate
          ttExtra.DeliveryType = STRING({&ORDER_DELTYPE_COURIER})
          ttExtra.Despachar    = "" 
          ttExtra.MainOrderID  = "".

   RETURN TRUE.

END FUNCTION.

FUNCTION fUpdateOrderLogisticsValue RETURNS LOGICAL
   (INPUT liOrderId AS INT):

   DEFINE BUFFER bufOrder FOR Order. 

   FIND FIRST bufOrder EXCLUSIVE-LOCK WHERE
              bufOrder.Brand   EQ Syst.Var:gcBrand AND 
              bufOrder.OrderId EQ liOrderId        NO-ERROR.

   IF ERROR-STATUS:ERROR OR LOCKED(bufOrder) THEN
      RETURN FALSE.

   bufOrder.Logistics = lcFileName.
   fMarkOrderStamp(bufOrder.OrderID,"SendToLogistics",0.0). /* Timestamp for Logistics Operator Change Dextra->Netkia */
   RELEASE bufOrder.

   RETURN TRUE.

END FUNCTION. 

fBatchLog("START",lcSpoolDir + lcFileName).

OUTPUT STREAM sICC TO VALUE(lcSpoolDir + lcFileName).

DEFINE BUFFER xOrder FOR Order.

/* ordinary dextra */
FOR EACH Order NO-LOCK WHERE 
         Order.Brand      EQ Syst.Var:gcBrand          AND 
         Order.StatusCode EQ {&ORDER_STATUS_SENDING_TO_LO}:

   /* handle only NEW or MNP orders */
   IF Order.OrderType NE 0 AND
      Order.OrderType NE 1 THEN NEXT.
   
   /* Do not create LO file in migration */
   IF Order.Orderchannel BEGINS "migration" THEN NEXT.

   /* YOT-867 */
   IF Order.MNPStatus EQ 0 AND 
      liNewDelay      NE ? AND
      Func.Common:mOffSet(Order.CrStamp, 24 * liNewDelay) > Func.Common:mMakeTS() THEN NEXT.

   IF fDelivSIM(Order.OrderId,
                TRUE,
                "",
                "") THEN DO: 
      fUpdateOrderLogisticsValue(Order.OrderId).

      IF llDoEvent THEN DO:
         lhOrder = BUFFER Order:HANDLE.
         RUN StarEventInitialize(lhOrder).
         RUN StarEventSetOldBuffer(lhOrder).
      END.

      fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_PENDING_ICC_FROM_LO}).
      
      IF llDoEvent THEN DO:
         RUN StarEventMakeModifyEvent(lhOrder).
         fCleanEventObjects().
      END.
   
   END.

END.

DEFINE BUFFER bufOrderGroup FOR OrderGroup.

/* Order Terminal info sent to LO in case of Fiber 
   convergent orders where SIM will be assigned by INSTALLER */
FOR EACH Order NO-LOCK WHERE
         Order.Brand    = Syst.Var:gcBrand AND
         Order.CrStamp >= 20171201         AND
         Order.CLIType BEGINS "CONTFH"     AND
         Order.ICC      <> "":

   IF Order.OrderType EQ 2 THEN NEXT.

   IF LOOKUP(Order.StatusCode,"7,8,9") > 0 THEN NEXT.

   IF LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) EQ 0 THEN NEXT.

   IF CAN-FIND(FIRST bufOrderGroup NO-LOCK WHERE
                     bufOrderGroup.OrderId   EQ Order.OrderId  AND
                     bufOrderGroup.GroupType EQ {&OG_LOFILE}   AND
             ENTRY(1,bufOrderGroup.Info,CHR(255)) EQ {&DESPACHAR_TRUE_VALUE}) THEN NEXT.

   IF Order.Logistics NE "" THEN NEXT.

   IF Order.OrderType EQ {&ORDER_TYPE_MNP} AND
     NOT (Order.MNPStatus EQ 6 OR
          Order.MNPStatus EQ 7)            THEN NEXT.
      
   IF NOT fIsTerminalOrder(Order.OrderId,ocTerminalCode) THEN NEXT.

   IF fDelivSIM(Order.OrderId,
                TRUE,
                "",
                "") THEN  
      fUpdateOrderLogisticsValue(Order.OrderId).

END.

/* Order has to be second time when order was already sent with
   despachar value "02" - Previously order was sent twice when 
   sim status is "20" */ 
FOR EACH OrderGroup NO-LOCK WHERE 
         OrderGroup.GroupType      EQ {&OG_LOFILE}         AND
 ENTRY(1,OrderGroup.Info,CHR(255)) EQ {&DESPACHAR_FALSE_VALUE}:

   IF CAN-FIND(FIRST bufOrderGroup NO-LOCK WHERE
                     bufOrderGroup.OrderId   EQ OrderGroup.OrderId  AND
                     bufOrderGroup.GroupId   EQ OrderGroup.GroupId  AND
                     bufOrderGroup.GroupType EQ {&OG_LOFILE}        AND
             ENTRY(1,bufOrderGroup.Info,CHR(255)) EQ {&DESPACHAR_TRUE_VALUE}) THEN NEXT. 

   FIND FIRST Order NO-LOCK WHERE 
              Order.Brand     EQ Syst.Var:gcBrand            AND 
              Order.OrderId   EQ OrderGroup.OrderId          AND 
              Order.Logistics EQ ""                          AND 
       LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) EQ 0 NO-ERROR.

   IF AVAIL Order THEN DO: 
      
      IF Order.OrderType EQ {&ORDER_TYPE_MNP} AND
        NOT (Order.MNPStatus EQ 6 OR
             Order.MNPStatus EQ 7)            THEN NEXT.

      /* For convergent + terminal order, LO info should not be sent
         if fixed line is not yet installed */
      IF fIsConvergenceTariff(Order.CLIType)         AND
         fIsTerminalOrder(Order.OrderId,
                          OUTPUT lcTerminalBillCode) THEN DO:

         IF NOT CAN-FIND(FIRST OrderFusion NO-LOCK WHERE
                               OrderFusion.Brand        EQ Syst.Var:gcBrand AND
                               OrderFusion.OrderId      EQ Order.OrderId    AND
                               OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_FINALIZED}) THEN
            NEXT.

      END.

      IF fDelivSIM(Order.OrderId,
                   TRUE,
                   STRING(OrderGroup.GroupId),
                   "01") THEN DO:

         fUpdateOrderLogisticsValue(Order.OrderId).

         /* If convergent order fixed line is not installed, and even if (Router + Mobile) info 
            is sent to logistics - Order status should not be changed from 77 */
         IF fIsConvergenceTariff(Order.CLIType) THEN
            IF NOT CAN-FIND(FIRST OrderFusion NO-LOCK WHERE
                                  OrderFusion.Brand        EQ Syst.Var:gcBrand AND
                                  OrderFusion.OrderId      EQ Order.OrderId    AND
                                  OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_FINALIZED}) THEN
               NEXT.
         
         IF llDoEvent THEN DO:
            lhOrder = BUFFER Order:HANDLE.
            RUN StarEventInitialize(lhOrder).
            RUN StarEventSetOldBuffer(lhOrder).
         END.

         fSetOrderStatus(Order.OrderId,{&ORDER_STATUS_PENDING_ICC_FROM_LO}).
         
         IF llDoEvent THEN DO:
            RUN StarEventMakeModifyEvent(lhOrder).
            fCleanEventObjects().
         END.

      END.

   END.    

END.  

/* Renewal dextra */
RENEWAL_LOOP:
FOR EACH Order NO-LOCK WHERE  
         Order.Brand = Syst.Var:gcBrand AND
         Order.StatusCode = "78" AND 
         Order.OrderType = 2:

   IF Order.OrderChannel BEGINS "renewal_pos" THEN NEXT RENEWAL_LOOP. 
    
   ocResult = "".

   FIND MobSub WHERE 
        MobSub.MsSeq = Order.MsSeq NO-LOCK NO-ERROR.
   IF AVAIL MobSub THEN DO:

      lcICC = MobSub.ICC.
      IF Order.ICC > "" THEN lcICC = Order.ICC.
             
      FIND xOrder WHERE 
           xOrder.Brand   = Syst.Var:gcBrand AND
           xOrder.OrderId = Order.OrderId 
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF ERROR-STATUS:ERROR OR LOCKED(xOrder) THEN NEXT RENEWAL_LOOP.
       
      FIND SIM WHERE
           SIM.Brand = Syst.Var:gcBrand AND 
           SIM.ICC = lcICC NO-LOCK NO-ERROR.
      IF AVAILABLE SIM THEN DO:
         
         IF fDelivSIM(xOrder.OrderId,
                      TRUE,
                      "",
                      "") THEN DO:

            fAfterSalesRequest(
               xOrder.MsSeq,
               xOrder.OrderId,
               Syst.Var:katun,
               Func.Common:mMakeTS(),
               "7",
               OUTPUT ocResult
               ).

            IF ocResult > "" THEN DO:
               Func.Common:mWriteMemo("Order",
                                STRING(xOrder.OrderID),
                                0,
                                "After Sales Request creation failed - LO",
                                ocResult).
            END.

            ASSIGN
               xOrder.Logistics = lcFileName
               xOrder.SendToROI = 1.
 
            fSetOrderStatus(xOrder.OrderID,"12").
            fMarkOrderStamp(xOrder.OrderID,"SendToLogistics",0.0).
         END. /* IF fDelivSIM( SIM.ICC ) THEN DO: */
      END. /* IF AVAILABLE SIM THEN DO: */
       
      RELEASE xOrder.
   END.
END.

/* YPR-4983 COFF logistic file for router */

FOR EACH FusionMessage EXCLUSIVE-LOCK WHERE 
         FusionMessage.source EQ "MasMovil" AND
         FusionMessage.messagestatus EQ {&FUSIONMESSAGE_STATUS_NEW} AND
         FusionMessage.messagetype EQ "Logistics":
   FIND FIRST Order WHERE
              Order.brand EQ Syst.Var:gcBrand AND
              Order.orderId EQ FusionMessage.orderId NO-ERROR.
   IF NOT AVAIL Order OR INDEX(Order.orderchannel, "pos") > 0 THEN DO:
      ASSIGN
         FusionMessage.UpdateTS = Func.Common:mMakeTS()
         FusionMessage.messagestatus = {&FUSIONMESSAGE_STATUS_ERROR}.
      NEXT.
   END.

   IF LOOKUP(order.statuscode,{&ORDER_CLOSE_STATUSES}) > 0 THEN DO:
      ASSIGN
         FusionMessage.UpdateTS = Func.Common:mMakeTS()
         FusionMessage.FixedStatusDesc = "Invalid order status"
         FusionMessage.messagestatus = {&FUSIONMESSAGE_STATUS_ERROR}.
      NEXT.
   END.
   
   FIND orderfusion NO-LOCK where
        orderfusion.brand = Syst.Var:gcBrand AND
        orderfusion.orderid = order.orderid NO-ERROR.
   IF AVAIL orderfusion AND
      OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_CANCELLED}
      THEN DO:
      ASSIGN
         FusionMessage.UpdateTS = Func.Common:mMakeTS()
         FusionMessage.FixedStatusDesc = "Pending fixed line cancellation"
         FusionMessage.messagestatus = {&FUSIONMESSAGE_STATUS_ERROR}.
      NEXT.
   END.

   FIND FIRST CliType WHERE
              Clitype.brand EQ Syst.Var:gcBrand AND
              Clitype.clitype EQ order.clitype NO-LOCK NO-ERROR.
   IF Clitype.fixedlinetype NE {&FIXED_LINE_TYPE_ADSL} THEN DO:
      ASSIGN
         FusionMessage.UpdateTS = Func.Common:mMakeTS()
         FusionMessage.messagestatus = {&FUSIONMESSAGE_STATUS_ERROR}.
      NEXT.   
   END.
   IF fDelivDevice("Router") THEN ASSIGN
      FusionMessage.UpdateTS = Func.Common:mMakeTS()
      FusionMessage.messagestatus = {&FUSIONMESSAGE_STATUS_SENT}.
END.

/* Third Party Device Logistics */
FOR EACH TPService WHERE TPService.MsSeq > 0 AND TPService.Operation = {&TYPE_ACTIVATION} AND TPService.ServStatus = {&STATUS_NEW} NO-LOCK:
      
   FIND FIRST MobSub WHERE MobSub.MsSeq = TPService.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN
   DO: 
       fTPServiceError(BUFFER TPService,"Contract not found").
       NEXT.
   END.

   FIND FIRST Order WHERE Order.brand EQ Syst.Var:gcBrand AND Order.MsSeq EQ MobSub.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL Order THEN 
   DO:
      fTPServiceError(BUFFER TPService,"Failed to identify associated order during logistics initiation").
      NEXT.
   END.
   
   IF fDelivDevice(TPService.ServType) THEN 
       fCreateTPServiceMessage(TPService.MsSeq, TPService.ServSeq, {&SOURCE_TMS}, {&STATUS_LOGISTICS_INITIATED}).
END.

iLargestId = 1.

lcLogFile = fCParamC("DextraLogFile").

IF lcLogFile <> "" AND lcLogFile <> ? AND fIsOutputFile(lcLogFile) THEN
DO:
   fSetLogFileName(lcLogFile).
   fSetLogEntryTypes("").
   fSetGlobalLoggingLevel(1).
END.
ELSE
  lcLogFile = ?.

DEF VAR liMLRowNum  AS INT NO-UNDO. 
DEF VAR liMLOrderId AS INT NO-UNDO. 

/* link Extra/Additional line mainline order to orders if available */
ADDITIONAL:
FOR EACH ttOneDelivery NO-LOCK WHERE
         ttOneDelivery.ActionID EQ "1" TRANSACTION:

   FIND FIRST bMLOrder NO-LOCK WHERE   
              bMLOrder.Brand   = Syst.Var:gcBrand      AND 
              bMLOrder.OrderId = ttOneDelivery.Orderid NO-ERROR.

   IF NOT AVAIL bMLOrder THEN NEXT.

   ASSIGN liMLOrderId = 0
          liMLRowNum  = 0 
          liMLOrderId = ttOneDelivery.Orderid
          liMLRowNum  = ttOneDelivery.RowNum.

   FIND FIRST bMLOrderCustomer NO-LOCK WHERE 
              bMLOrderCustomer.Brand   = Syst.Var:gcBrand      AND 
              bMLOrderCustomer.OrderId = ttOneDelivery.OrderID AND
              bMLOrderCustomer.RowType = 1                     NO-ERROR.

   IF NOT AVAIL bMLOrderCustomer THEN NEXT. 
  
   IF fIsTerminalOrder(liMLOrderId,
                       OUTPUT lcTerminalBillCode) OR 
      (bMLOrder.DeliverySecure > 0)               THEN
      llDespacharValue = FALSE.
   ELSE llDespacharValue = TRUE. 

   /* Mainline Convergent order is with OrderStatus 77, 
      include mobile part of mainline in logistics file */
   IF bMLOrder.OrderType NE {&ORDER_TYPE_STC} THEN DO: 
  
      IF NOT fDelivSIM(ttOneDelivery.OrderId,
                       llDespacharValue,
                       STRING(liMLOrderID),
                       IF llDespacharValue THEN "01"
                       ELSE "02") THEN 
         UNDO ADDITIONAL, NEXT. 

      IF llDespacharValue THEN 
         fUpdateOrderLogisticsValue(liMLOrderId).
   END.

   FIND FIRST bMLttExtra EXCLUSIVE-LOCK WHERE
              bMLttExtra.RowNum = liMLRowNum NO-ERROR.

   IF NOT AVAIL bMLttExtra THEN 
      UNDO ADDITIONAL, NEXT.

   ASSIGN bMLttExtra.MainOrderID  = STRING(liMLOrderID)
          bMLttExtra.Despachar    = "01".

   IF NOT CAN-FIND(FIRST OrderGroup NO-LOCK WHERE
                         OrderGroup.OrderId        EQ liMLOrderID              AND
                         OrderGroup.GroupType      EQ {&OG_LOFILE}             AND
                 ENTRY(1,OrderGroup.Info,CHR(255)) EQ {&DESPACHAR_TRUE_VALUE}) THEN
      fCreateOrderGroup(liMLOrderId,
                        liMLOrderID,
                        llDespacharValue).
   
   /* Reset Despachar Value for Additional/Extra lines orders */
   llDespacharValue = FALSE.

   FOR EACH bALOrderCustomer NO-LOCK WHERE  
            bALOrderCustomer.Brand      EQ Syst.Var:gcBrand             AND 
            bALOrderCustomer.CustId     EQ bMLOrderCustomer.CustId      AND
            bALOrderCustomer.CustIdType EQ bMLOrderCustomer.CustIdType  AND
            bALOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bALOrder NO-LOCK WHERE
            bALOrder.Brand      EQ Syst.Var:gcBrand                  AND
            bALOrder.OrderId    EQ bALOrderCustomer.OrderId          AND
            bALOrder.StatusCode EQ {&ORDER_STATUS_PENDING_MAIN_LINE} AND
            bALOrder.OrderType  NE {&ORDER_TYPE_RENEWAL}:

      IF CAN-FIND(FIRST bALOrderGroup NO-LOCK WHERE 
                        bALOrderGroup.OrderId   EQ bALOrder.OrderId           AND 
                        bALOrderGroup.GroupId   EQ ttOneDelivery.OrderID      AND 
                        bALOrderGroup.GroupType EQ {&OG_LOFILE}               AND 
          (ENTRY(1,bALOrderGroup.Info,CHR(255)) EQ {&DESPACHAR_TRUE_VALUE} OR 
           ENTRY(1,bALOrderGroup.Info,CHR(255)) EQ {&DESPACHAR_FALSE_VALUE})) THEN NEXT.

      IF fIsTerminalOrder(liMLOrderId,
                          OUTPUT lcTerminalBillCode) OR 
         (bALOrder.DeliverySecure > 0)               THEN 
         llDespacharValue = FALSE.
      ELSE llDespacharValue = TRUE.
     
      IF NOT fDelivSIM(bALOrder.OrderId,
                       llDespacharValue,
                       STRING(liMLOrderID),
                       IF llDespacharValue THEN "01"
                       ELSE "02") THEN 
         UNDO ADDITIONAL, NEXT.    

      IF llDespacharValue THEN
         fUpdateOrderLogisticsValue(bALOrder.OrderId).
      
      IF NOT CAN-FIND(FIRST OrderGroup NO-LOCK WHERE
                            OrderGroup.OrderId        EQ bALOrder.OrderId         AND
                            OrderGroup.GroupType      EQ {&OG_LOFILE}             AND
                    ENTRY(1,OrderGroup.Info,CHR(255)) EQ {&DESPACHAR_TRUE_VALUE}) THEN
         fCreateOrderGroup(bALOrder.OrderId,
                           liMLOrderID,
                           llDespacharValue).

   END. /* FOR EACH bALOrderCustomer */

END. /* FOR EACH ttOneDelivery */

FOR EACH ttOneDelivery NO-LOCK BREAK BY ttOneDelivery.RowNum:

   lhTable = BUFFER ttOneDelivery:HANDLE.
   lcLine = "".

   /*YDR-2313 Create Cust. Nbr just when the order is placed*/
   oiCustomer = 0.

   FIND FIRST Order WHERE
              Order.Brand   = Syst.Var:gcBrand AND
              Order.OrderID = ttOneDelivery.OrderID AND
              Order.CustNum = 0 NO-LOCK NO-ERROR.
   IF AVAILABLE Order THEN
   DO:
      /* YTS-10537 Update Customer information only when order is finished */
      RUN Mm/createcustomer.p(INPUT ttOneDelivery.OrderId,1,FALSE,FALSE,OUTPUT oiCustomer).

      IF RETURN-VALUE NE "not updated existing customer" THEN DO:

		   llCorporate = CAN-FIND(OrderCustomer WHERE
                        OrderCustomer.Brand      = Syst.Var:gcBrand      AND
								OrderCustomer.OrderID    = ttOneDelivery.OrderID AND
								OrderCustomer.RowType    = 1                     AND
								OrderCustomer.CustIdType = "CIF").

	      FOR EACH OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand   = Syst.Var:gcBrand AND
				  OrderCustomer.OrderID = ttOneDelivery.OrderID:
			IF llCorporate AND (OrderCustomer.RowType = 1 OR OrderCustomer.RowType = 5) THEN
			DO:
			   RUN Mm/createcustcontact.p(OrderCustomer.OrderID,
										  oiCustomer,
										  OrderCustomer.RowType,
										  OUTPUT lcError).
			   IF lcError > "" THEN DO:
                           Func.Common:mWriteMemo("Order",
									STRING(OrderCustomer.OrderID),
									oiCustomer,
									"CUSTOMER CONTACT CREATION FAILED",
									lcError).
			   END.
		    END.
	     END.
      END.

      /* YTS-10537 Update Customer information only when order is finished */
      RUN Mm/createcustomer.p(INPUT ttOneDelivery.OrderId,3,FALSE,FALSE,OUTPUT oiCustomer).
   END.

   DO liLoop1 = 1 TO lhTable:NUM-FIELDS:

      lhField = lhTable:BUFFER-FIELD(liLoop1).

      IF lhField:NAME NE "RowNum"    AND 
         lhField:NAME NE "OrderId"   AND 
         lhField:NAME NE "OrderType" THEN
         lcLine  = lcLine + STRING(lhField:BUFFER-VALUE,lhField:FORMAT).
      IF lhField:NAME EQ "OrderId" THEN
          pLog("OrderId : " + STRING(lhField:BUFFER-VALUE)).
      IF lhField:NAME EQ "ICCNum" THEN
          pLog("ICCNum : " + lhField:BUFFER-VALUE).
      IF lhField:NAME EQ "MSISDN" THEN
          pLog("MSISDN: " + lhField:BUFFER-VALUE).

      pCheckTextSection(INPUT-OUTPUT lcLine, FALSE).

   END.

   FOR EACH ttInvRow NO-LOCK WHERE
            ttInvRow.RowNum = ttOneDelivery.RowNum:

      lhTable = BUFFER ttInvRow:HANDLE.

      DO liLoop1 = 1 TO lhTable:NUM-FIELDS:

         lhField = lhTable:BUFFER-FIELD(liLoop1).
     

         IF lhField:NAME NE "RowNum" THEN
            lcLine  = lcLine + STRING(lhField:BUFFER-VALUE,lhField:FORMAT).
         IF liLoop1 = lhTable:NUM-FIELDS THEN
            pCheckTextSection(INPUT-OUTPUT lcLine, TRUE).
         ELSE
            pCheckTextSection(INPUT-OUTPUT lcLine, FALSE).
      
      END.

   END.
   
   /* Write the DataService Field in the last of each line */
   FOR FIRST ttExtra WHERE
             ttExtra.RowNum = ttOneDelivery.RowNum NO-LOCK:

      lhTable = BUFFER ttExtra:HANDLE.

      DO liLoop1 = 1 TO lhTable:NUM-FIELDS:
         lhField = lhTable:BUFFER-FIELD(liLoop1).

         IF lhField:NAME = "RowNum" THEN NEXT.
      
         lcLine  = lcLine + STRING(lhField:BUFFER-VALUE,lhField:FORMAT).
         IF liLoop1 = lhTable:NUM-FIELDS THEN
            pCheckTextSection(INPUT-OUTPUT lcLine, TRUE).
         ELSE
            pCheckTextSection(INPUT-OUTPUT lcLine, FALSE).
      END. /* DO liLoop1 = 1 TO lhTable:NUM-FIELDS: */
   END. /* FOR FIRST ttExtra WHERE */

   FOR EACH ttOutputText:
      PUT STREAM sICC UNFORMATTED ttOutputText.cText.
   END. 
   EMPTY TEMP-TABLE ttOutputText.

   PUT STREAM sICC UNFORMATTED CHR(10).

END.

EMPTY TEMP-TABLE ttOneDelivery.
EMPTY TEMP-TABLE ttInvRow.
EMPTY TEMP-TABLE ttExtra.

IF VALID-HANDLE(lhField) THEN DELETE OBJECT lhField.
IF VALID-HANDLE(lhTable) THEN DELETE OBJECT lhTable.

OUTPUT STREAM sICC CLOSE.

IF lcLogFile <> "" AND lcLogFile <> ? THEN
   fCloseLog().

UNIX SILENT VALUE("mv " + lcSpoolDir + lcFileName + " " + lcRiftpDir).

/* Transfer TAR file to final directory */
IF lcTarOption > "" THEN
   fTransDir(lcContractTARFile,".tar",lcTAROutDir).

/* Send email with error log file */
IF llCreateErrLogFile AND
   LOOKUP(lcMailHost,{&HOSTNAME_STAGING}) = 0 THEN DO:
   OUTPUT STREAM sErr CLOSE.

   lcEmailConfDir = fCParamC("RepConfDir").

   /* mail recipients AND actual sending */
   GetRecipients(lcEmailConfDir + "create_logistics_file.email").

   IF xMailAddr > "" THEN DO:
      ASSIGN xMailAttach = lcErrorLogFileName
             lcErrorLogFileName = "/tmp/create_logistics_file_errmsg.txt".

      OUTPUT STREAM sErr TO VALUE(lcErrorLogFileName).
      PUT STREAM sErr UNFORMATTED
          "Dextra file is finished at " + Func.Common:mTS2HMS(Func.Common:mMakeTS()) SKIP
          "Errors occurred! See the attached log file" SKIP(1).
      OUTPUT STREAM sErr CLOSE.

      SendMail(lcErrorLogFileName,xMailAttach).
   END.
END.

fBatchLog("FINISH",lcRiftpDir + lcFileName).
