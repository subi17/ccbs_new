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

{Syst/commali.i}
{Func/cparam.i2}
{Syst/eventval.i}
{Func/fcustpl.i}
{Func/ftaxdata.i}
{invlang.i2}

DEF INPUT  PARAMETER iiOrder  AS INT  NO-UNDO. 
def input  parameter icextinvid as char no-undo.
DEF INPUT  PARAMETER iiAction AS INT  NO-UNDO.  
DEF OUTPUT PARAMETER ocList   AS CHAR NO-UNDO. 
DEF OUTPUT PARAMETER odTotAmt AS DEC  NO-UNDO. 
DEF OUTPUT PARAMETER ocError  AS CHAR NO-UNDO. 

/* iiAction: 1=create fees and invoice
             2=just make a list of fees, don't create anything 
             3=like 2, but leave out campaign topup rows
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
DEF VAR lcTaxZone       AS CHAR NO-UNDO.
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

/* fees may have already been created (e.g. an mnp order) 
   check here before initializing eventlog */
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
            FOR EACH SingleFee EXCLUSIVE-LOCK USE-INDEX HostTable WHERE
                     SingleFee.Brand     = gcBrand AND
                     SingleFee.HostTable = "Order" AND
                     SingleFee.KeyValue  = STRING(iiOrder) AND
                     SingleFee.CalcObj   = lcCalcObj:
               SingleFee.CustNum = MobSub.InvCust.      
            END.

            FIND CURRENT Invoice EXCLUSIVE-LOCK.
            Invoice.CustNum = MobSub.InvCust.
            
            FOR EACH Payment OF Invoice EXCLUSIVE-LOCK:
               Payment.CustNum = Invoice.CustNum.
            END.
              
            ocError = ocError + ", invoice transferred". 
         END.    
      END.
         
      RETURN.
   END.

   IF llDoEvent THEN DO:
      &GLOBAL-DEFINE STAR_EVENT_USER katun
   
      {lib/eventlog.i}
      
      DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
      lhSingleFee = BUFFER SingleFee:HANDLE.
      RUN StarEventInitialize(lhSingleFee).
   END.
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
   
      IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSingleFee).

      RELEASE SingleFee.
   END.
   
END FUNCTION.

FUNCTION fCreateDispRow RETURNS LOGIC
   (idAmount    AS DEC,
    ilVatIncl   AS LOG):

   DEF VAR lcBiName   AS CHAR NO-UNDO. 
   DEF VAR ldVatPerc  AS DEC  NO-UNDO. 
         
   IF NOT AVAILABLE BillItem THEN RETURN FALSE. 
         
   /* get amount with vat */
   IF ilVatIncl = FALSE THEN DO:
      ldVatPerc = fRegionTaxPerc(lcRegion,
                                 BillItem.TaxClass).
      idAmount = ROUND(idAmount * (1 + ldVatPerc / 100),2).    
   END.
         
   lcBiName = fInvLangC(liLanguage,
                        1,
                        BillItem.BillCode).
   IF lcBiName = "" OR lcBiName = ? THEN lcBiName = BillItem.BIName.      
   
   ASSIGN ocList  = ocList +
                    STRING(lcBIName,"X(30)") + 
                    STRING(idAmount,"->>>>>9.99") +
                    " EUR" + CHR(10).
         odTotAmt = odTotAmt + idAmount.

END FUNCTION.


        
FIND Order NO-LOCK WHERE 
     Order.Brand   = gcBrand AND
     Order.OrderID = iiOrder NO-ERROR.
IF NOT AVAILABLE Order THEN DO:
   ocError = "Error:Unknown order".
   IF iiAction = 1 THEN fCleanEventObjects(). 
   RETURN.
END.

/* not for pos and not for pre-activated  */
IF LOOKUP(Order.OrderChannel,"pos,pre-act") > 0 THEN DO:
   ocError = Order.OrderChannel + " order".
   IF iiAction = 1 THEN fCleanEventObjects(). 
   RETURN.
END.

/* region determines tax zone, which determines the customer to be used */
FIND FIRST OrderCustomer WHERE
           OrderCustomer.Brand   = gcBrand       AND
           OrderCustomer.OrderID = Order.OrderID AND
           OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
IF NOT AVAILABLE OrderCustomer THEN DO:
   ocError = "Error:Customer data not available".
   IF iiAction = 1 THEN fCleanEventObjects(). 
   RETURN.
END.

/* has subscription and customer been created already */
IF CAN-FIND(MobSub WHERE MobSub.MsSeq = Order.MsSeq) AND
    OrderCustomer.CustNum > 0 THEN DO:

   FIND Customer WHERE Customer.CustNum = OrderCustomer.CustNum 
      NO-LOCK NO-ERROR.
   IF AVAILABLE Customer THEN ASSIGN 
      lcRegion   = Customer.Region 
      liCashCust = Customer.CustNum
      liLanguage = Customer.Language.
END.

/* order has not been delivered, use a common customer */
IF liCashCust = 0 THEN DO:

   lcRegion = IF OrderCustomer.Region > "" 
              THEN OrderCustomer.Region
              ELSE SUBSTRING(OrderCustomer.ZipCode,1,2).
          
   liLanguage = INTEGER(OrderCustomer.Language) NO-ERROR. 
           
   lcTaxZone = "0".
   IF lcRegion > "" THEN DO:
      FIND Region WHERE Region.Region = lcRegion NO-LOCK NO-ERROR.
      IF AVAILABLE Region THEN lcTaxZone = Region.TaxZone.
   END.

   /* customer to which invoice is created */
   liCashCust  = fCParamI("CashCust" + lcTaxZone).

END.

IF liLanguage = 0 THEN liLanguage = 1. 

ASSIGN 
   /* pricelist for terminals */
   lcTermPList = fCParamC("TerminalPriceList")
   llCreateInv = FALSE.

IF liCashCust = ? OR liCashCust = 0 THEN DO:
   ocError = "Error:Cash customer has not been defined for tax zone " + 
             lcTaxZone.
   IF iiAction = 1 THEN fCleanEventObjects(). 
   RETURN.
END.

FIND Customer NO-LOCK WHERE
     Customer.Brand   = gcBrand AND
     Customer.CustNum = liCashCust NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   ocError = "Error:Unknown customer " + STRING(liCashCust).
   IF iiAction = 1 THEN fCleanEventObjects(). 
   RETURN. 
END.

ASSIGN 
   liBillPeriod  = YEAR(TODAY) * 100 + MONTH(TODAY)
   liConcerns    = liBillPeriod * 100 + DAY(TODAY)
   lcTerminal    = ""
   lcPicked      = ""
   ldTermDiscAmt = 0
   ldTopupAmt    = 0.

/* one terminal per order */
FOR FIRST OrderAccessory OF ORDER NO-LOCK:
   ASSIGN 
      lcTerminal    = OrderAccessory.ProductCode
      ldTermDiscAmt = OrderAccessory.Discount.
END.

/* discount billing item for terminal */
IF lcTerminal > "" AND ldTermDiscAmt NE 0 THEN DO:
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
      IF iiAction = 1 THEN fCleanEventObjects(). 
      RETURN. 
   END.

END.

/* determine the feemodel from order data */
lcFeeModel = (IF Order.PayType THEN "PRE" ELSE "POS") +
             (IF Order.MNPStatus > 0 THEN "MNP" ELSE "NEW") +
             (IF lcTerminal = "" THEN "SIM" ELSE "").

/* first try to determine actual fee model through terminal product */
IF lcTerminal > "" THEN 
FOR EACH FeeModel NO-LOCK WHERE
         FeeModel.Brand   = gcBrand      AND
         FeeModel.FeeModel BEGINS lcFeeModel,
   FIRST FMItem NO-LOCK WHERE
         FMItem.Brand     = gcBrand      AND
         FMITem.FeeModel  = FeeModel.FeeModel AND
         FMItem.BillCode  = lcTerminal   AND
         FMItem.PriceList = lcTermPList  AND
         FMItem.FromDate <= TODAY        AND
         FMItem.ToDate   >= TODAY:
         
   lcPicked = FeeModel.FeeModel.

   /* take the first one that matches */
   LEAVE.
END.

IF lcPicked > "" THEN lcFeeModel = lcPicked.

/* default pricelist for customer (used for other than terminals) */
lcCustPList = fFeeModelPriceList(liCashCust,
                                 1,
                                 lcFeeModel,
                                 TODAY).

/* additional fee (delivery charge) */
IF Order.FeeModel > "" THEN lcFeeModel = lcFeeModel + "," + Order.FeeModel.

/* special case */
IF LOOKUP(Order.OrderChannel,"Yoigo,Pre-act") > 0 THEN DO:
   
   lcFeeModel = Order.FeeModel.

   FOR FIRST FMItem NO-LOCK WHERE
             FMItem.Brand     = gcBrand           AND
             FMITem.FeeModel  = FeeModel.FeeModel AND
             FMItem.FromDate <= TODAY             AND
             FMItem.ToDate   >= TODAY:
      lcCustPList = FMItem.PriceList.
   END.
END.

lcPriceLists = lcTermPList + 
               (IF lcTermPList > "" AND lcCustPList > ""
                THEN "," 
                ELSE "") +
               lcCustPList. 

/*
message "feemodel" lcfeemodel
        "price" lcpricelists
        "cust" licashcust
        "region" lcregion
        "tax"    lctaxzone
        view-as alert-box.
*/

DO liCnt = 1 TO NUM-ENTRIES(lcFeeModel):

   FOR EACH FMItem NO-LOCK WHERE
            FMItem.Brand     = gcBrand                 AND
            FMITem.FeeModel  = ENTRY(liCnt,lcFeeModel) AND
            LOOKUP(FMItem.PriceList,lcPriceLists) > 0  AND
            FMItem.FromDate <= TODAY                   AND
            FMItem.ToDate   >= TODAY,
      FIRST BillItem NO-LOCK WHERE
            BillItem.Brand    = gcBrand AND
            BillItem.BillCode = FMItem.BillCode,
      FIRST PriceList NO-LOCK WHERE
            PriceList.Brand     = gcBrand AND
            PriceList.PriceList = FMItem.PriceList:

      llVatIncl = PriceList.InclVat.
      
      /* create fees */
      IF iiAction = 1 THEN DO: 
      
         /* already done 
            (should invoice creation be tried if billed = false ?) */
         FOR FIRST SingleFee NO-LOCK USE-INDEX HostTable WHERE
                   SingleFee.Brand     = gcBrand AND
                   SingleFee.HostTable = "Order" AND
                   SingleFee.KeyValue  = STRING(Order.OrderID) AND
                   SingleFee.BillCode  = FMItem.BillCode       AND
                   SingleFee.CalcObj   = lcCalcObj             AND
                   SingleFee.Amt       = FMItem.Amount:
            ocError = "Fee already exists".
            
            IF iiAction = 1 THEN fCleanEventObjects(). 
            RETURN.
         END.

         /* make single fee */
         fCreateSingleFee(FMItem.BillCode,
                          FMItem.Amount,
                          PriceList.InclVat,
                          FMItem.FeeModel).
      END.
      
      /* just make rows for reporting */
      ELSE IF iiAction = 2 OR iiAction = 3 THEN DO:
      
         fCreateDispRow(FMItem.Amount,
                        PriceList.InclVat).
      END.
             
      /* get discount for terminal right after terminal row */               
      IF FMItem.BillCode = lcTerminal THEN
         RUN pTerminalDiscount.
      
   END.
END.

/* campaign events: additional topup */
IF ldTopupAmt NE 0 AND iiAction NE 3 THEN DO:

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
                     llVatIncl).

      FIND BillItem WHERE 
           BillItem.Brand    = gcBrand AND
           BillItem.BillCode = lcTopupDiscItem NO-LOCK NO-ERROR.
         
      fCreateDispRow(ldAmt * -1,
                     llVatIncl).
   END. 

END.


/* create invoice */
IF iiAction = 1 THEN DO:
   IF llCreateInv THEN DO: 

      /* cash receipt */ 
      liInvType = 7.
    
      /* payment on delivery */ 
      FOR FIRST OrderPayment OF Order NO-LOCK WHERE
             OrderPayment.Method = 1:
         liInvType = 6.
      END.
   
      RUN /home/ari/work/nnlamu_givenbr.p (liCashCust,
                      Order.OrderID,
                      icextinvid,
                      "",
                      liInvType,
                      TRUE,
                      OUTPUT liQty).

      IF liQty = 0 THEN ocError = "Error:Invoice was not created".
   END.

   /* clean out dynamic temp-tables that eventlog created */
   fCleanEventObjects(). 
END.


/* campaign events: discount for terminal */
PROCEDURE pTerminalDiscount:

   IF ldTermDiscAmt = 0 THEN RETURN.

   ldAmt = ldTermDiscAmt * -1.
      
   /* create fees */
   IF iiAction = 1 THEN DO: 
    
      /* make single fee */
      fCreateSingleFee(lcTermDiscItem,
                       ldAmt,
                       llVatIncl,
                       "").
   END.
      
   /* rows for reporting */
   ELSE IF iiAction = 2 OR iiAction = 3 THEN DO:
      
      FIND BillItem WHERE 
           BillItem.Brand    = gcBrand AND
           BillItem.BillCode = lcTermDiscItem NO-LOCK NO-ERROR.
         
      fCreateDispRow(ldAmt,
                     llVatIncl).
   END. 

END PROCEDURE.






