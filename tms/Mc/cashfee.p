/* ---------------------------------------------------------------------------
  MODULE .......: CASHFEE
  FUNCTION .....: Create single fees for cash invoice
  APPLICATION ..: TMS
  CREATED ......: 27.11.06/aam
  MODIFIED .....: 16.08.07/aam better cleaning of eventlog
                  27.08.07/aam discount for terminal,
                               additional topup from OrderTopUp
  VERSION ......: yoigo
  -------------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{tmsconst.i}
{eventval.i}
{fcustpl.i}
{ftaxdata.i}
{transname.i}
{timestamp.i}
{offer.i}
{orderfunc.i}

DEF INPUT  PARAMETER iiOrder  AS INT  NO-UNDO. 
DEF INPUT  PARAMETER iiAction AS INT  NO-UNDO.  
DEF OUTPUT PARAMETER ocList   AS CHAR NO-UNDO. 
DEF OUTPUT PARAMETER odTotAmt AS DEC  NO-UNDO. 
DEF OUTPUT PARAMETER ocError  AS CHAR NO-UNDO. 

/* iiAction: 1=create fees and invoice
             2=just make a list of fees, don't create anything 
             3=like 2, but leave out campaign topup rows
             4=like 3, but for conf emails use always Spanish translation
*/             

DEF VAR ldAmount        AS DEC  NO-UNDO. 
DEF VAR liCashCust      AS INT  NO-UNDO. 
DEF VAR liBillPeriod    AS INT  NO-UNDO. 
DEF VAR liConcerns      AS INT  NO-UNDO. 
DEF VAR liQty           AS INT  NO-UNDO. 
DEF VAR lcTermPList     AS CHAR NO-UNDO.
DEF VAR lcCustPlist     AS CHAR NO-UNDO.
DEF VAR lcTerminal      AS CHAR NO-UNDO.
DEF VAR lcFeeModel      AS CHAR NO-UNDO.
DEF VAR lcPicked        AS CHAR NO-UNDO.
DEF VAR lcCalcObj       AS CHAR NO-UNDO INIT "CASHFEE".
DEF VAR lcPriceLists    AS CHAR NO-UNDO. 
DEF VAR lcRegion        AS CHAR NO-UNDO.
DEF VAR llCreateInv     AS LOG  NO-UNDO.
DEF VAR liInvType       AS INT  NO-UNDO. 
DEF VAR liCnt           AS INT  NO-UNDO.
DEF VAR ldAmt           AS DEC  NO-UNDO. 
DEF VAR ldTotAmt        AS DEC  NO-UNDO. 
DEF VAR liLanguage      AS INT  NO-UNDO. 
DEF VAR ldTermDiscAmt   AS DEC  NO-UNDO.
DEF VAR lcTermDiscItem  AS CHAR NO-UNDO.
DEF VAR ldTopupAmt      AS DEC  NO-UNDO.
DEF VAR lcTopupItem     AS CHAR NO-UNDO.
DEF VAR lcTopupDiscItem AS CHAR NO-UNDO. 
DEF VAR llVatIncl       AS LOG  NO-UNDO. 
DEF VAR ldaOrderDate    AS DATE NO-UNDO. 
DEF VAR liOrderTime     AS INT  NO-UNDO. 

/* YTS-3277 */
RUN pInitializeReturnValue.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}

   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
END.
 

FUNCTION fCreateSingleFee RETURNS LOGICAL
   (icBillCode  AS CHAR,
    idAmount    AS DEC,
    ilVatIncl   AS LOG,
    icFeeModel  AS CHAR):
   
   DO TRANS: 
      CREATE SingleFee.

      ASSIGN
      SingleFee.Brand       = gcBrand 
      SingleFee.FMItemId    = NEXT-VALUE(bi-seq)
      SingleFee.CustNum     = liCashCust    
      SingleFee.BillTarget  = 1
      SingleFee.CalcObj     = lcCalcObj
      SingleFee.BillCode    = icBillCode       
      SingleFee.BillPeriod  = liBillPeriod     /* billing Period   */
      SingleFee.Concerns[1] = liConcerns       /* period concerned */
      SingleFee.Amt         = idAmount         /* Payment          */
      SingleFee.Memo[1]     = ""
      SingleFee.Memo[2]     = ""
      SingleFee.HostTable   = "Order"
      SingleFee.KeyValue    = STRING(Order.OrderID)
      SingleFee.BillType    = "SF"
      SingleFee.Contract    = ""
      SingleFee.Active      = TRUE
      SingleFee.FeeModel    = icFeeModel
      SingleFee.VATIncl     = ilVatIncl
      odTotAmt              = odTotAmt + idAmount.

      /* always create the invoice */
      llCreateInv = TRUE.
   
      IF llDoEvent THEN
         RUN StarEventMakeCreateEventWithMemo(lhSingleFee,
                                              katun,
                                              "CashInvoiceCreation").

      RELEASE SingleFee.
   END.
   
END FUNCTION.

FUNCTION fCreateDispRow RETURNS LOGIC
   (idAmount    AS DEC,
    ilVatIncl   AS LOG,
    idtDate AS DATE):

   DEF VAR lcBiName   AS CHAR NO-UNDO. 
   DEF VAR ldVatPerc  AS DEC  NO-UNDO. 
   DEF VAR ldeMonthlyFee AS DEC NO-UNDO. 
   DEF VAR liMonths   AS INT NO-UNDO.
   DEF VAR ldeFinalFee AS DEC  NO-UNDO.

   DEF BUFFER OfferItem FOR OfferItem.
   DEF BUFFER DayCampaign FOR DayCampaign.
   DEF BUFFER FeeModel FOR FeeModel.
   DEF BUFFER FMItem FOR FMItem.
         
   /* get amount with vat */
   IF ilVatIncl = FALSE THEN DO:
      ldVatPerc = fRegionTaxPerc(lcRegion,
                                 BillItem.TaxClass,
                                 idtDate).
      idAmount = ROUND(idAmount * (1 + ldVatPerc / 100),2).    
   END.
   
   odTotAmt = odTotAmt + idAmount.
   
   /* Add monthly payment to:shell YDR-328 */
   IF BillItem.BiGroup EQ {&BITEM_GRP_TERMINAL} THEN DO:
      idAmount = idAmount + fGetOfferDeferredPayment(Order.Offer,
                                                     Order.CrStamp,
                                                     OUTPUT ldeMonthlyFee,
                                                     OUTPUT liMonths,
                                                     OUTPUT ldeFinalFee).
      IF ldeFinalFee > 0 THEN
         idAmount = idAmount + ldeFinalFee.
   END.
         
   lcBiName = fTranslationName(gcBrand,
                               1,
                               BillItem.BillCode,
                               liLanguage,
                               idtDate).
                        
   IF lcBiName = "" OR lcBiName = ? THEN lcBiName = BillItem.BIName.      
   
   ASSIGN ocList  = ocList +
                    STRING(lcBIName,"X(40)") + 
                    STRING(idAmount,"->>>>>9.99") +
                    " EUR" + CHR(10).

END FUNCTION.


/******* Main start *********/

IF iiAction = 1 AND llDoEvent THEN DO:
   RUN StarEventInitialize(lhSingleFee).
END.
 
RUN pMakeCashInvoice.

fCleanEventObjects().

/******* Main end *********/


PROCEDURE pMakeCashInvoice:

   /* fees may have already been created (e.g. an mnp order) */
   IF iiAction = 1 THEN DO: 

      IF CAN-FIND(FIRST SingleFee USE-INDEX HostTable WHERE
                        SingleFee.Brand     = gcBrand AND
                        SingleFee.HostTable = "Order" AND
                        SingleFee.KeyValue  = STRING(iiOrder) AND
                        SingleFee.CalcObj   = lcCalcObj)
      THEN DO:

         ocError = "Already done".

         /* if invoice already created and subscription has now been created
            then transfer invoice to the actual customer */
         FIND Order WHERE 
              Order.Brand   = gcBrand AND
              Order.OrderID = iiOrder NO-LOCK NO-ERROR.
         IF AVAILABLE Order AND Order.InvNum > 0 THEN DO:
         
            FIND MobSub WHERE MobSub.MsSeq = Order.MsSeq NO-LOCK NO-ERROR.
            FIND Invoice WHERE Invoice.InvNum = Order.InvNum NO-LOCK NO-ERROR.
         
            IF AVAILABLE MobSub AND AVAILABLE Invoice AND 
               Invoice.CustNum NE MobSub.InvCust
            THEN DO TRANS:

               IF llDoEvent THEN DO:
                  RUN StarEventInitialize(lhInvoice).
               END.
             
               FOR EACH SingleFee EXCLUSIVE-LOCK USE-INDEX HostTable WHERE
                        SingleFee.Brand     = gcBrand AND
                        SingleFee.HostTable = "Order" AND
                        SingleFee.KeyValue  = STRING(iiOrder) AND
                        SingleFee.CalcObj   = lcCalcObj:
                  IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSingleFee).
                  SingleFee.CustNum = MobSub.InvCust.      
                  IF llDoEvent THEN RUN StarEventMakeModifyEventWithMemo(
                                          lhSingleFee,
                                          katun,
                                          "CashInvoiceCreation").
               END.

               FIND CURRENT Invoice EXCLUSIVE-LOCK.
               IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).
               Invoice.CustNum = MobSub.InvCust.
               IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).
            
               FOR EACH Payment OF Invoice EXCLUSIVE-LOCK:
                  Payment.CustNum = Invoice.CustNum.
               END.
              
               ocError = ocError + ", invoice transferred". 
            END.    
         END.
      
         RETURN.
      END.

   END.

   FIND Order NO-LOCK WHERE 
        Order.Brand   = gcBrand AND
        Order.OrderID = iiOrder NO-ERROR.
   IF NOT AVAILABLE Order THEN DO:
      ocError = "Error:Unknown order".
      RETURN.
   END.

   fSplitTS(INPUT Order.CrStamp, OUTPUT ldaOrderDate, OUTPUT liOrderTime).

   /* not for pos,pre-activated and vip  */
   IF LOOKUP(Order.OrderChannel,"pos,pre-act,vip,renewal_pos,renewal_pos_stc,fusion_pos") > 0
   THEN DO:
      ocError = Order.OrderChannel + " order".
      RETURN.
   END.

   /* region determines tax zone, which determines the customer to be used */
   FIND FIRST OrderCustomer WHERE
              OrderCustomer.Brand   = gcBrand       AND
              OrderCustomer.OrderID = Order.OrderID AND
              OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE OrderCustomer THEN DO:
      ocError = "Error:Customer data not available".
      RETURN.
   END.

   /* has customer been created already */
   IF OrderCustomer.CustNum > 0 THEN DO:

      FIND Customer WHERE Customer.CustNum = OrderCustomer.CustNum 
         NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN ASSIGN 
         lcRegion   = Customer.Region 
         liCashCust = Customer.CustNum
         liLanguage = Customer.Language.
   END.

   /* order has not been delivered, create customer for invoicing */
   IF liCashCust = 0 THEN DO:

      IF iiAction EQ 1 THEN DO:

         RUN createcustomer.p(Order.OrderId, 
                            1,
                            FALSE,
                            FALSE, /* do not update existing customer */
                            OUTPUT liCashCust).
         IF liCashCust = ? OR liCashCust = 0 THEN DO:
            ocError = "Error:Customer has not been created".
            RETURN.
         END.
         
         FIND Customer NO-LOCK WHERE
              Customer.Brand   = gcBrand AND
              Customer.CustNum = liCashCust NO-ERROR.
         IF NOT AVAILABLE Customer THEN DO:
            ocError = "Error:Unknown customer " + STRING(liCashCust).
            RETURN. 
         END.
      END.
  
      lcRegion = IF OrderCustomer.Region > "" 
                 THEN OrderCustomer.Region
                 ELSE SUBSTRING(OrderCustomer.ZipCode,1,2).
          
      liLanguage = INTEGER(OrderCustomer.Language) NO-ERROR. 

   END.

   IF iiAction EQ 4 THEN
      liLanguage = 1. /* yts-7046 only Spanish supported HTML conf emails. */

   IF liLanguage = 0 THEN liLanguage = 1. 

   ASSIGN 
      /* pricelist for terminals */
      lcTermPList = fCParamC("TerminalPriceList")
      llCreateInv = FALSE.
   
   ASSIGN 
      liBillPeriod  = YEAR(TODAY) * 100 + MONTH(TODAY)
      liConcerns    = liBillPeriod * 100 + DAY(TODAY)
      lcTerminal    = ""
      lcPicked      = ""
      ldTermDiscAmt = 0
      ldTopupAmt    = 0.

   /* one terminal per order */
   FOR FIRST OrderAccessory OF ORDER WHERE
             OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE}) NO-LOCK:
      ASSIGN 
         lcTerminal    = OrderAccessory.ProductCode
         ldTermDiscAmt = OrderAccessory.Discount.
   END.

   /* discount billing item for terminal */
   IF ldTermDiscAmt NE 0 THEN DO:
      lcTermDiscItem = fCParamC("OrderTermDisc").
   
      IF lcTermDiscItem = ? OR lcTermDiscItem = "" THEN DO:
         ocError = "Error:Terminal discount item missing".
         IF iiAction = 1 THEN fCleanEventObjects(). 
         RETURN. 
      END.
   END.

   /* additional topup given */
   FOR EACH OrderTopup OF Order NO-LOCK WHERE
            OrderTopup.Amount > 0:
      ldTopupAmt = ldTopupAmt + OrderTopup.Amount + OrderTopup.VatAmount.       
   END. 

   IF ldTopupAmt > 0 THEN DO:
      ASSIGN 
         lcTopupItem     = fCParamC("OrderTopUp")
         lcTopupDiscItem = fCParamC("OrderTopUpDisc").
   
      IF lcTopupItem = ? OR lcTopupItem = "" OR
         lcTopupDiscItem = ? OR lcTopupDiscItem = ""
      THEN DO:
         ocError = "Error:Topup item missing".
         RETURN. 
      END.
   END.

   /* new offer method */
   IF Order.Offer > "" THEN RUN pUseOffer(Order.Offer,
                                          Order.CrStamp).

   /* current method */
   IF Order.Offer = "" OR Order.FeeModel > "" THEN  
      RUN pUseFeeModel.

   IF RETURN-VALUE BEGINS "ERROR:" THEN DO:
      ocError = RETURN-VALUE.
      RETURN.
   END.

   /* campaign events: additional topup */
   IF ldTopupAmt NE 0 AND iiAction NE 3 AND iiAction NE 4 THEN DO:

      ldAmt = ldTopupAmt.
      
      /* create fees */
      IF iiAction = 1 THEN DO: 
 
         /* make single fee */
         fCreateSingleFee(lcTopupItem,
                          ldAmt,
                          llVatIncl,
                          "").
         fCreateSingleFee(lcTopupDiscItem,
                          ldAmt * -1,
                          llVatIncl,
                          "").
      END.
      
      /* rows for reporting */
      ELSE IF iiAction = 2 THEN DO:
    
         FIND BillItem WHERE 
              BillItem.Brand    = gcBrand AND
              BillItem.BillCode = lcTopupItem NO-LOCK NO-ERROR.
         
         fCreateDispRow(ldAmt,
                        llVatIncl,
                        TODAY).

         FIND BillItem WHERE 
              BillItem.Brand    = gcBrand AND
              BillItem.BillCode = lcTopupDiscItem NO-LOCK NO-ERROR.
         
         fCreateDispRow(ldAmt * -1,
                        llVatIncl,
                        TODAY).
      END. 

   END.


   /* create invoice */
   IF iiAction = 1 THEN DO:
      IF llCreateInv THEN DO: 

         /* cash receipt */ 
         liInvType = 7.
    
         /* payment on delivery */ 
         FOR FIRST OrderPayment OF Order NO-LOCK WHERE
                OrderPayment.Method = {&ORDERPAYMENT_M_POD}:
            liInvType = 6.
         END.
   
         RUN nnlamu_inc (liCashCust,
                         Order.OrderID,
                         "",
                         liInvType,
                         TRUE,
                         OUTPUT liQty).

         IF liQty = 0 THEN ocError = "Error:Invoice was not created".
      END.
   END.

   RETURN "".
   
END PROCEDURE.  /* pMakeCashInvoice */


PROCEDURE pUseFeeModel:

   ASSIGN
      lcFeeModel  = ""
      lcCustPlist = "".
    
   IF Order.Offer = "" THEN DO: 
      /* determine the feemodel from order data */
      lcFeeModel = (IF Order.PayType THEN "PRE" ELSE "POS") +
                   (IF Order.OrderType EQ 2 THEN "REN" ELSE
                   (IF Order.MNPStatus > 0 THEN "MNP" ELSE "NEW")).

      /* no terminal on order, only sim */             
      IF lcTerminal = "" THEN DO:
         IF INDEX(Order.CLIType,"CONTRD") > 0 
         THEN lcFeeModel = lcFeeModel + "DATA".
         ELSE lcFeeModel = lcFeeModel + "SIM".
      END.

      /* otherwise try to determine actual fee model through terminal product */
      ELSE IF lcTerminal > "" THEN DO:
         
         lcPicked = "".

         FOR EACH FeeModel NO-LOCK WHERE
                  FeeModel.Brand   = gcBrand      AND
                  FeeModel.FeeModel BEGINS lcFeeModel,
            FIRST FMItem NO-LOCK WHERE
                  FMItem.Brand     = gcBrand      AND
                  FMITem.FeeModel  = FeeModel.FeeModel AND
                  FMItem.BillCode  = lcTerminal   AND
                  FMItem.PriceList = lcTermPList  AND
                  FMItem.FromDate <= ldaOrderDate AND
                  FMItem.ToDate   >= ldaOrderDate:
            
            lcPicked = FeeModel.FeeModel.

            /* take the first one that matches */
            LEAVE.
         END.

         IF lcPicked > "" THEN lcFeeModel = lcPicked.
         ELSE DO:
            
            CREATE ErrorLog.
            ASSIGN ErrorLog.Brand     = gcBrand
                   ErrorLog.ActionID  = "CASHFEE"
                   ErrorLog.TableName = "Order"
                   ErrorLog.KeyValue  = STRING(Order.OrderId) 
                   ErrorLog.ErrorChar = lcTerminal 
                   ErrorLog.ErrorMsg  = "Terminal fee model was not found"
                   ErrorLog.UserCode  = katun.
                   ErrorLog.ActionTS  = fMakeTS().
             ocError = "Terminal fee model was not found".
             RETURN "ERROR:" + ocError.
         END.
     
      END.
      /* default pricelist for customer (used for other than terminals) */
      lcCustPList = fFeeModelPriceList(liCashCust,
                                       1,
                                       lcFeeModel,
                                       ldaOrderDate).
   END.
   
   /* additional fee (delivery charge) */
   IF Order.FeeModel > "" THEN 
      lcFeeModel = lcFeeModel + (IF lcFeeModel > "" THEN "," ELSE "") + 
                   Order.FeeModel.

   /* special case */
   IF LOOKUP(Order.OrderChannel,"Yoigo,Pre-act") > 0 THEN DO:
   
      lcFeeModel = Order.FeeModel.

      FOR FIRST FMItem NO-LOCK WHERE
                FMItem.Brand     = gcBrand           AND
                FMITem.FeeModel  = FeeModel.FeeModel AND
                FMItem.FromDate <= ldaOrderDate      AND
                FMItem.ToDate   >= ldaOrderDate:
         lcCustPList = FMItem.PriceList.
      END.
   END.

   lcPriceLists = lcTermPList + 
                  (IF lcTermPList > "" AND lcCustPList > ""
                   THEN "," 
                   ELSE "") +
                  lcCustPList. 


   DO liCnt = 1 TO NUM-ENTRIES(lcFeeModel):

      FOR EACH FMItem NO-LOCK WHERE
               FMItem.Brand     = gcBrand                 AND
               FMITem.FeeModel  = ENTRY(liCnt,lcFeeModel) AND
               LOOKUP(FMItem.PriceList,lcPriceLists) > 0  AND
               FMItem.FromDate <= ldaOrderDate            AND
               FMItem.ToDate   >= ldaOrderDate,
         FIRST BillItem NO-LOCK WHERE
               BillItem.Brand    = gcBrand AND
               BillItem.BillCode = FMItem.BillCode,
         FIRST PriceList NO-LOCK WHERE
               PriceList.Brand     = gcBrand AND
               PriceList.PriceList = FMItem.PriceList:

         llVatIncl = PriceList.InclVat.
      
         RUN pSingleFee(FMItem.BillCode,
                        FMItem.Amount,
                        PriceList.InclVat,
                        FMItem.FeeModel).
                        
         IF RETURN-VALUE BEGINS "ERROR:" THEN RETURN RETURN-VALUE.
      
      END.
   END.

   RETURN "".
   
END PROCEDURE.  /* pUseFeeModel */

PROCEDURE pUseOffer:

   DEF INPUT PARAMETER icOffer      AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idOfferStamp AS DEC  NO-UNDO.
    
   DEF VAR lcUseOffer AS CHAR NO-UNDO.
   DEF VAR lcSIMBillItem AS CHAR NO-UNDO.
   DEF VAR ldtopUpAmount AS DEC NO-UNDO.
   DEF VAR ldDiscAmount AS DEC NO-UNDO.

   DEF BUFFER bOfferItem FOR OfferItem.
   
   /* determine correct offer using 'permanencia' code */
   IF LOOKUP(icOffer,"1,2") > 0 THEN 
      lcUseOffer = fGetOffer(icOffer,
                             ldaOrderDate).
   ELSE lcUseOffer = icOffer.
   
   IF lcUseOffer = "" THEN RETURN "ERROR:Unknown offer ID".

   FOR FIRST Offer NO-LOCK WHERE
             Offer.Brand = gcBrand AND
             Offer.Offer = lcUseOffer:

      llVatIncl = Offer.VatIncl.

      /* Terminal Offer without SIM billing item */
      IF NOT CAN-FIND(FIRST bOfferItem WHERE
                            bOfferItem.Brand       = gcBrand      AND
                            bOfferItem.Offer       = Offer.Offer  AND
                            bOfferItem.ItemType    = "BillItem"   AND
         /* Plug-in, Micro, Nano, Universal SIM billitems */
         LOOKUP(bOfferItem.ItemKey,
                "TS00000R1,TS00000M1,TS00000N1,TS00000U1," +
                "TS00000R3,TS00000M3,TS00000N3,TS00000U3") > 0 AND
                bOfferItem.EndStamp   >= idOfferStamp AND
                bOfferItem.BeginStamp <= idOfferStamp NO-LOCK)
      THEN DO:
         FIND FIRST OrderAction WHERE
                    OrderAction.Brand    = gcBrand AND
                    OrderAction.OrderId  = Order.OrderId AND
                    OrderAction.ItemType = "SIMType" NO-LOCK NO-ERROR.
         IF AVAIL OrderAction THEN DO:
            RELEASE Sim.
            IF Order.icc > "" THEN DO:
               FIND FIRST Sim WHERE
                          Sim.icc EQ order.icc NO-LOCK NO-ERROR.
            END.
            IF AVAIL Sim THEN
               lcSIMBillItem = fGetSIMBillItem(Sim.SimArt,
                                               Order.PayType).
            ELSE
               lcSIMBillItem = fGetSIMBillItem(OrderAction.ItemKey,
                                               Order.PayType).
            IF lcSIMBillItem > "" THEN
               RUN pSingleFee(lcSIMBillItem,
                              0.0,
                              TRUE,
                              "O:" + Offer.Offer).
         END. /* IF AVAIL OrderAction THEN DO: */
      END. /* IF NOT CAN-FIND(FIRST bOfferItem WHERE */
      
      FOR EACH OfferItem NO-LOCK WHERE
               OfferItem.Brand       = gcBrand      AND
               OfferItem.Offer       = Offer.Offer  AND
               OfferItem.EndStamp   >= idOfferStamp AND
               OfferItem.BeginStamp <= idOfferStamp:
      
         CASE OfferItem.ItemType:
         WHEN "BillItem" THEN
            RUN pSingleFee(OfferItem.ItemKey,
                           OfferItem.Amount,
                           OfferItem.VatIncl,
                           "O:" + Offer.Offer).
         WHEN "Topup" THEN DO:
         
            FOR FIRST TopupScheme NO-LOCK WHERE
                      TopupScheme.Brand = gcBrand AND
                      TopupScheme.TopupScheme = OfferItem.ItemKey,
                FIRST TopupSchemeRow NO-LOCK WHERE
                      TopupSchemeRow.Brand       = gcBrand AND
                      TopupSchemeRow.TopupScheme = TopupScheme.TopupScheme AND
                      TopupSchemeRow.EndStamp   >= idOfferStamp AND
                      TopupSchemeRow.BeginStamp <= idOfferStamp:
               /* Check if TopUpScheme has DisplayAmount to show */
               IF ((Order.CliType EQ "TARJ7" OR
                   Order.CliType EQ "TARJ9") AND
                   TopUpSchemeRow.DisplayAmount > 0 AND
                   (iiAction EQ 3 OR iiAction EQ 4)) THEN DO:
                  ldtopupAmount = TopUpSchemeRow.DisplayAmount.
                  ldDiscAmount = TopUpSchemeRow.DisplayAmount * -1.
               END.
               ELSE DO:
                  ldtopupAmount = TopupSchemeRow.Amount.      
                  ldDiscAmount = TopupSchemeRow.DiscountAmount * -1.
               END.
               RUN pSingleFee(TopupSchemeRow.BillCode,
                              ldtopupAmount,
                              TopupScheme.VatIncl,
                              "O:" + Offer.Offer).
                              
               IF ldDiscAmount NE 0 THEN 
                  RUN pSingleFee(TopupSchemeRow.DiscountBillCode,
                                 ldDiscAmount,
                                 TopupScheme.VatIncl,
                                 "O:" + Offer.Offer).
                            
            END.
         END.
         END CASE.

         IF RETURN-VALUE BEGINS "ERROR:" THEN RETURN RETURN-VALUE.
      END.
      
      LEAVE.
   END.
      
   RETURN "".
   
END PROCEDURE.   /* pUseOffer */

PROCEDURE pSingleFee:
      
   DEF INPUT PARAMETER icBillCode AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idAmount   AS DEC  NO-UNDO.
   DEF INPUT PARAMETER ilVatIncl  AS LOG  NO-UNDO.
   DEF INPUT PARAMETER icSourceID AS CHAR NO-UNDO.
   
   FIND FIRST BillItem WHERE
              BillItem.Brand    = gcBrand AND
              BillItem.BillCode = icBillCode NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BillItem THEN DO:
      ocError = "Unknown billing item " + icBillCode.
      RETURN "ERROR:" + ocError.
   END.   
   
   IF BillItem.BIGroup = "7" AND lcTerminal = "" THEN 
      lcTerminal = BillItem.BillCode.
      
   /* create fees */
   IF iiAction = 1 THEN DO: 
      
      /* already done 
         (should invoice creation be tried if billed = false ?) */
      FOR FIRST SingleFee NO-LOCK USE-INDEX HostTable WHERE
                SingleFee.Brand     = gcBrand AND
                SingleFee.HostTable = "Order" AND
                SingleFee.KeyValue  = STRING(Order.OrderID) AND
                SingleFee.BillCode  = icBillCode            AND
                SingleFee.CalcObj   = lcCalcObj             AND
                SingleFee.Amt       = idAmount:
         ocError = "Fee already exists".
            
         IF iiAction = 1 THEN RETURN "ERROR:" + ocError.
      END.

      /* make single fee */
      fCreateSingleFee(icBillCode,
                       idAmount,
                       ilVatIncl,
                       icSourceID).
   END.
      
   /* just make rows for reporting */
   ELSE IF iiAction = 2 OR iiAction = 3 OR iiAction = 4 THEN DO:
      
      fCreateDispRow(idAmount,
                     ilVatIncl,
                     TODAY).
   END.
             
   /* get discount for terminal right after terminal row */               
   IF icBillCode = lcTerminal AND ldTermDiscAmt NE 0 THEN DO:
      
      IF idAmount < ldTermDiscAmt THEN DO:
         
         IF iiAction = 1 THEN DO:
            CREATE ErrorLog.
            ASSIGN ErrorLog.Brand     = gcBrand
                   ErrorLog.ActionID  = "CASHFEE"
                   ErrorLog.TableName = "Order"
                   ErrorLog.KeyValue  = STRING(Order.OrderId) 
                   ErrorLog.ErrorChar = lcTerminal 
                   ErrorLog.ErrorMsg  = "Too much discount: terminal price=" +
                                        STRING(idAmount) + 
                                        ", discount=" + STRING(ldTermDiscAmt) +
                                        ", campaign=" + Order.Campaign
                   ErrorLog.UserCode  = katun.
                   ErrorLog.ActionTS  = fMakeTS().
         END.
         
         ldTermDiscAmt = idAmount.
      END.
      
      RUN pTerminalDiscount (ilVatIncl).
   END.   

   RETURN "".
       
END PROCEDURE.   /* pSingleFee */

/* campaign events: discount for terminal */
PROCEDURE pTerminalDiscount:

   DEF INPUT PARAMETER ilVatIncl  AS LOG  NO-UNDO.

   IF ldTermDiscAmt = 0 THEN RETURN.

   ldAmt = ldTermDiscAmt * -1.
      
   /* create fees */
   IF iiAction = 1 THEN DO: 
    
      /* make single fee */
      fCreateSingleFee(lcTermDiscItem,
                       ldAmt,
                       ilVatIncl,
                       "").
   END.
      
   /* rows for reporting */
   ELSE IF iiAction = 2 OR iiAction = 3 OR iiAction = 4 THEN DO:
      
      FIND BillItem WHERE 
           BillItem.Brand    = gcBrand AND
           BillItem.BillCode = lcTermDiscItem NO-LOCK NO-ERROR.
         
      fCreateDispRow(ldAmt,
                     ilVatIncl,
                     TODAY).
   END. 

END PROCEDURE.   /* pTerminalDiscount */

PROCEDURE pInitializeReturnValue:
   RETURN "".
END PROCEDURE. 
