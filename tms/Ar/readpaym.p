/* ---------------------------------------------------------------------------
  MODULE .......: READPAYM
  KUTSUVAMODULI : 
  FUNCTION .....: Book payments to invoices            
  SYSTEM .......: TMS
  AUTHOR .......: 
  CREATED ......: 26.06.1997
  CHANGED ......:
                  08.12.99 /pt Automatic recognition of payment of a
                               reminder fee
                  29.12.99 /kl No VATAmt on remider fees
                  14.01.00 /jp cparams added
                  24.01.00 /jp Overpayments; default EventType = 5, 
                               if DPaym then 1
                  09.04.01/aam -longer format for customer nbr
                               -parameter UnpaidToDeposit determines the max.
                                unpaid amount that can be transferred to 
                                customer's Deposit (Billed on next invoice)
                  04.06.01/aam give a note of an invoice that is transferred
                               to Intrum
                  20.08.01/aam handle also Interest amount from nnkosu.tmp
                  18.09.01/aam variables used with paymck changed to decimal,
                               update for TMSParam into a transaction
                  01.11.01/aam "kr"-references removed,
                               use AccDate
                  12.11.01/aam booking list according to AccDate,
                               show grand total to the list
                  22.11.01/aam check archive id,
                               ppp-references removed 
                  20.12.01/aam customer name added to payment line in nnocko
                  04.01.02/aam check that voucher nbr is free,
                               special reference nbr 20000913 -> 
                               find customer's unpaid invoices and pay them or
                               add to Deposit,
                               save the source of payment to Payment.PaymSrc
                  07.01.02/aam Bank AccNum to Payment.Memo
                  08.01.02/aam sort ttPayment by AccDate,
                               show total sums by error RepType in the end,
                               longer format for voucher in OverHdr 
                  25.01.02/aam check InterestPerm when calculating Interest 
                  29.01.02/aam create "kohsuor" for rejected events,
                               handle TITO-lines
                  28.02.02/aam printing of "ok"-lines optional,
                               claiming costs from Intrum
                  08.03.02/aam invoice types 3 & 4 (Deposit & adv.payment)
                  15.03.02/aam customer name from ttPayment in error cases,
                               get customer nbr from nnockointr.p
                  21.03.02/aam Transfer booking list to a directory defined
                               in RefPrintDir
                  08.04.02/aam use AdvPaymAcc for advance payments
                  10.04.02/ht  ImportStamp added
                  19.04.02/aam line counter was not updated after claim note
                  03.05.02/aam use PaymVouch (fvoucher.i)
                  31.05.02/aam tt-payments with empty ArchiveId are accepted
                  07.06.02/aam VAT-handling for AdvPaym
                  17.06.02/aam A/R AccNum is not used with Deposit invoices
                               (RepType 3)
                  22.07.02/aam use also booking Date when checking PaymArc 
                  02.09.02/aam parameter PaymFileList determines if customer
                               specification printed
                  10.09.02/aam check period lock 
                  26.09.02/aam customer balances in table CustBal 
                  11.10.02/aam check duplicates also when archiveid is empty
                  30.10.02/aam ttPayment.Interest to xTot-sums,
                               UnPaidToDeposit not used for claimed invoices
                  25.10.02/aam credit loss handling
                  14.11.02/jr  bankacc & filename from memo to own fiels 
                  22.11.02/aam vat handling for cl-payment 
                  03.01.03/aam vat-amount from old cl-payments noted
                  26.02.03/aam handle direct debit errors
                  25.03.03/aam show voucher range in summary box
                  02.04.03/aam in overpayment/adv.paym cases first try to find 
                               old unpaid invoices and target payment to them
                               (not for Intrum payments)
                  11.09.03/aam brand 
                  07.01.04/aam VatUsage
                  19.01.04/aam check if deposit invoice is linked to order
                  08.04.04/aam payment plan 
                  04.05.04/aam account with 6 digits
                  23.09.04/aam memo to customer from failed dd
                  30.09.04/aam addition to subscription balance (fat)
                  04.10.04/aam subtract correction (negative payment) first
                               from previous overpayment
                  29.11.04/aam new parameter to fcpfat-functions,
                               Brand was missing from PaymArc check
                  10.12.04/aam addition to subscription balance as gift (RG)   
                  12.01.05/aam CustNum to calcint.p
                  31.01.05/aam send posting lists via mail
                  01.04.05/aam change archiveid for op-payments (fOldUnPaid)
                  13.04.05/aam ttPayment.ErrCode,
                               overpayment specification removed
                  03.05.05/aam logic from nnkosu.p -> readpaym.p             
                  04.08.05/aam find routine for fatimes corrected,      
                               PaymType 2 for credit loss 
                  05.10.05/aam own account for credit loss cancellation
                  01.11.05/aam all negative payments to unregistered
                  28.11.05/aam account for saldo payment and egift from 
                               according billing item
                  03.01.06/aam use fTransDir for booking list             
                  16.06.06/aam ClaimState instead of ClaimQty
                  25.08.06/aam if period is locked then transfer acc.date
                               to next open period
                  09.03.07/aam CLI, Payment.ExtVoucher             
                  03.05.07/aam posting report to 2 streams
                  10.05.07/aam output ocInfo
                  16.05.07/aam make an empty posting file when required
  Version ......: M15
  ------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}                                               
{Syst/utumaa.i}
{Ar/bankrej.i}
{Func/fvoucher.i}
{Func/fapvat.i}
/* temp-table */
{Ar/paymfile.i}
{Func/faccper.i}
{Func/fcustbal.i}
{Func/fclvat.i}
{Func/finvbal.i}
{Func/fpplan.i}
{Func/fcpfat.i}
{Func/email.i}
{Func/ftransdir.i}
{Func/forderstamp.i}
{Func/orderfunc.i}


DEF INPUT  PARAMETER TABLE FOR ttPayment. 
DEF INPUT  PARAMETER icPaymFile AS CHAR NO-UNDO. 
DEF INPUT  PARAMETER iiFileType AS INT  NO-UNDO.
DEF INPUT  PARAMETER ilDispMsg  AS LOG  NO-UNDO. 
DEF INPUT  PARAMETER ilSendMail AS LOG  NO-UNDO. 
DEF OUTPUT PARAMETER oiRead     AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocInfo     AS CHAR NO-UNDO. 


DEF VAR op-tstamp like OPLog.CreStamp no-undo.
DEF VAR rc        AS Int   NO-UNDO.
DEF VAR balt      AS DE    NO-UNDO.
DEF VAR xPrintOk  AS INT   NO-UNDO INIT 1. 

DEF VAR PaidAmt     LIKE Invoice.PaidAmt NO-UNDO.
DEF VAR Inv         AS Int NO-UNDO.
DEF VAR RefNum      AS C   NO-UNDO.
DEF VAR ed-PaymDate as da  NO-UNDO.
DEF VAR AmtPaid     AS DE  NO-UNDO.
DEF VAR subtotal    AS DE  NO-UNDO.
DEF VAR discount    AS LO  NO-UNDO.
DEF VAR diff        AS DE  NO-UNDO.
DEF VAR Debt        AS DE  NO-UNDO.
DEF VAR EventType   AS I   NO-UNDO INIT 5.
DEF VAR IntAmt      AS DE  NO-UNDO.
DEF VAR x-amt       AS DE  NO-UNDO extent 11.
   /* 1 = money 
      2 = a/r
      3 = discount 
      4 = overpayment 
      5 = interest
      6 = unpaid to deposit
      7 = claiming cost
      8 = adv.payment vat (ap vat)
      9 = adv.payment vat (vat debt)
     10 = credit loss 
     11 = credit loss vat 
  */
DEF VAR x-acct     AS I  NO-UNDO extent 11.
DEF VAR err        AS I  NO-UNDO.
DEF VAR ErrorMsg   AS C  NO-UNDO.
DEF VAR AddMsg     AS C  NO-UNDO. 
DEF VAR ConVno     AS I  NO-UNDO.
DEF VAR i          AS I  NO-UNDO.
DEF VAR t          AS I  NO-UNDO.
DEF VAR DelDays    AS I  NO-UNDO.
DEF VAR InPerc     AS DE NO-UNDO.
DEF VAR rl         AS I  NO-UNDO.
DEF VAR sl         AS I  NO-UNDO.
DEF VAR rlx        AS I  NO-UNDO.
DEF VAR b-acc      AS LO  NO-UNDO.   /* update accounts ? */
DEF VAR liDepoAcc  AS I   NO-UNDO. 
DEF VAR DefRemFee  AS DE  NO-UNDO.
DEF VAR DefRemfAc  AS I   NO-UNDO.
DEF VAR Overpaym   AS LO  NO-UNDO.
DEF VAR llOk       AS LO  NO-UNDO.

/* Interests */
DEF NEW SHARED VAR intpro  AS DE EXTENT 10 NO-UNDO.
DEF NEW SHARED VAR intdays AS I  EXTENT 10 NO-UNDO.
DEF NEW SHARED VAR intsumm AS DE EXTENT 10 NO-UNDO.

DEF VAR Balance      AS DEC  NO-UNDO.
DEF VAR paybal       AS DEC  NO-UNDO.
DEF VAR extra        AS DEC  NO-UNDO.
DEF VAR head-CustNum  LIKE Customer.CustNum  NO-UNDO.
DEF VAR head-as-nimi  LIKE Customer.CustName NO-UNDO.

DEF VAR ReceivAcc   AS I   NO-UNDO.
DEF VAR DiscAcc     AS I   NO-UNDO.
DEF VAR OverPayAcc  AS I   NO-UNDO.
DEF VAR AdvPaymAcc  AS I   NO-UNDO. 
DEF VAR VatAcc      AS I   NO-UNDO.
DEF VAR RoundAcc    AS I   NO-UNDO.
DEF VAR ClaimCostAcc AS I  NO-UNDO. 
DEF VAR MinIntPay   AS DE  NO-UNDO.
DEF VAR OverTimeInt AS DE  NO-UNDO.
DEF VAR Imarg       AS DE  NO-UNDO.
DEF VAR IntCalcMet  AS I   NO-UNDO.
DEF VAR qdays       AS C   NO-UNDO.
DEF VAR qperc       AS C   NO-UNDO.
DEF VAR error       AS C   NO-UNDO.
DEF VAR OTIntAcc    AS I   NO-UNDO.
DEF VAR VatAccount    AS I  NO-UNDO.
DEF VAR grtotal       AS DE NO-UNDO.
DEF VAR AccTotal      AS DE NO-UNDO.

DEF VAR xUnpaidLimit  AS DE NO-UNDO.
DEF VAR xUnPaid       AS LO NO-UNDO.
DEF VAR xFileName     AS CH NO-UNDO.
DEF VAR xIntMark      AS CH NO-UNDO. 
DEF VAR xBookDir      AS CH NO-UNDO. 

DEF VAR xTotOk        AS DE   NO-UNDO.
DEF VAR xTotMiss      AS DE   NO-UNDO.
DEF VAR xTotOver      AS DE   NO-UNDO EXTENT 2.
DEF VAR xTotRej       AS DE   NO-UNDO.
DEF VAR xQtyOk        AS I    NO-UNDO.
DEF VAR xQtyMiss      AS I    NO-UNDO.
DEF VAR xQtyOver      AS I    NO-UNDO.
DEF VAR xQtyRej       AS I    NO-UNDO.
DEF VAR xQtyClaim     AS INT  NO-UNDO.
DEF VAR xTotClaim     AS DEC  NO-UNDO. 

DEF VAR ldArAmt       AS DEC  NO-UNDO. 
DEF VAR liLossAcc     AS INT  NO-UNDO.
DEF VAR ldCredLoss    AS DEC  NO-UNDO. 
DEF VAR ldVatAmt      AS DEC  NO-UNDO. 
DEF VAR llCredFound   AS LOG  NO-UNDO. 
DEF VAR lcVouchRange  AS CHAR NO-UNDO EXTENT 2. 
DEF VAR llPaymAdded   AS LOG  NO-UNDO. 
DEF VAR ldPaymOrder   AS INT  NO-UNDO.
DEF VAR lcSubBalFat   AS CHAR NO-UNDO.
DEF VAR lcSubBalGift  AS CHAR NO-UNDO. 
DEF VAR lcFat         AS CHAR NO-UNDO. 
DEF VAR liPer         AS INT  NO-UNDO. 
DEF VAR ldOpPosting   AS DEC  NO-UNDO.
DEF VAR ldOpBal       AS DEC  NO-UNDO. 
DEF VAR lcMailFile    AS CHAR NO-UNDO. 
DEF VAR lcConfDir     AS CHAR NO-UNDO. 
DEF VAR liArcSeq      AS INT  NO-UNDO. 
DEF VAR liCLCancel    AS INT  NO-UNDO.
DEF VAR liSaldoAcc    AS INT  NO-UNDO.
DEF VAR liEGiftAcc    AS INT  NO-UNDO.
DEF VAR lcCustName    AS CHAR NO-UNDO.
DEF VAR ldtAccDate    AS DATE NO-UNDO.
DEF VAR lcPref        AS CHAR NO-UNDO. 
DEF VAR liDDVoucher   AS INT  NO-UNDO.
DEF VAR lcPostList    AS CHAR NO-UNDO.
DEF VAR lcPostDir     AS CHAR NO-UNDO.
DEF VAR lcPrintLine   AS CHAR NO-UNDO. 
DEF VAR ldtFileDate   AS DATE NO-UNDO.
DEF VAR lcTableName   AS CHAR NO-UNDO.
DEF VAR lcKeyValue    AS CHAR NO-UNDO. 
DEF VAR liDDCancel    AS INT  NO-UNDO.
DEF VAR lrRecid       AS RECID NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO.
DEF VAR lcExtVoucher  AS CHAR NO-UNDO. 

DEF TEMP-TABLE ttFat NO-UNDO
   FIELD Fat AS INT.

DEF BUFFER bInvoice FOR Invoice.
DEF BUFFER bPayment FOR ttPayment. 

DEF STREAM sPost.

form
   xQtyOk              column-label "Qty"           format ">>>>>9"
   ttPayment.RefNum    column-label "Ref. no."      format "x(10)"
   ttPayment.Inv       column-label "Invoice"       format ">>>>>>>9"
   ttPayment.PaymDate  column-label "Date"          format "99.99.9999"
   ttPayment.AmtPaid   column-label "Paid"          format "->>>>,>>9.99"
   ttPayment.CustName  column-label "Cust. Name"    format "x(23)"
with
   overlay centered row 10 1 down color value(Syst.Var:cfc) title color value(Syst.Var:ctc)
   " Payments " frame log.

DEFINE VARIABLE ynimi AS CHARACTER NO-UNDO.
ynimi = Syst.Var:ynimi.

form header
   fill("=",122) format "x(122)" skip
   ynimi "SUMMARY OF RETURN FILE" at 45 "Page" at 113
      sl format "ZZZZ9" skip
   icPaymFile AT 1 FORMAT "X(100)" 
      string(TODAY,"99-99-99") to 122 skip
   fill ("=",122) format "x(122)" skip(1)

   "Date"           AT 1
   "Voucher"        AT 10
   "Invoice"        AT 23
   "Amount"         TO 47
   "DueDate"        AT 49
   "Customer"       TO 65
   "Name"           AT 67
   "Reason / Error" AT 84
   skip
   
   fill("-",122) format "x(122)" skip
   
with width 122 no-label no-box frame PageHdr.


FUNCTION fChgPage RETURNS LOGICAL
    (iAdd AS INT).

    IF rl + iAdd >= skayt1 THEN DO:

        IF sl GT 0 THEN 
           PUT STREAM tul UNFORMATTED chr(12).
        ASSIGN sl = sl + 1 rl = 7 rlx = 0.
        VIEW STREAM tul FRAME PageHdr.
    END.   

END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (iiError   AS INT,
    icMessage AS CHAR):

   ASSIGN 
      ErrorMsg = icMessage
      err      = iiError
      xTotRej  = xTotRej + ttPayment.AmtPaid + ttPayment.Interest
      xQtyRej  = xQtyRej + 1.
    
END FUNCTION.
    
    
FUNCTION fCreateOpLog RETURNS LOGICAL 
    (iType AS INT,
     iInv  AS INT,
     iAmt  AS DEC). 

    {Mf/opstamp.i}

    /* Make an overpayment transaction */   
    CREATE OPLog.
    ASSIGN
    OPLog.CreStamp  = op-tstamp
    OPLog.CustNum   = Customer.CustNum
    OPLog.EventDate = ttPayment.PaymDate
    OPLog.UserCode  = Syst.Var:katun
    OPLog.EventType = iType      
    OPLog.InvNum    = iInv
    OPLog.Voucher   = ConVno
    OPLog.Amt       = iAmt.

END FUNCTION.
   
FUNCTION fOldUnpaid RETURNS LOGICAL
   (idAmount AS DEC).

   IF idAmount = 0 THEN RETURN FALSE.
   
   /* before posting overpayment/adv.payment check if there are unpaid 
      invoices, and make payments to them instead 
   */

   /* start from negative -> new payments are handled first when new
      payment-loop begins (POrder is 0 for other payments) */
   ldPaymOrder = -50.

   FindUnpaid:
   FOR EACH bInvoice NO-LOCK WHERE
            bInvoice.Brand    = Syst.Var:gcBrand          AND
            bInvoice.CustNum  = Invoice.CustNum  AND
            bInvoice.CrInvNum = 0                AND
            bInvoice.InvNum  NE Invoice.InvNum   AND
            bInvoice.PrintState > 0              AND
            bInvoice.InvType < 3
   BY bInvoice.DueDate:

      paybal = fInvBal(BUFFER bInvoice,
                       TODAY + 1000).
      IF paybal <= 0 THEN NEXT.

      CREATE BPayment.
      BUFFER-COPY ttPayment TO BPayment.
      ASSIGN bPayment.AmtPaid   = MIN(paybal,idAmount)
             bPayment.ClaimCost = 0
             bPayment.Interest  = 0
             bPayment.Inv       = bInvoice.InvNum
             bPayment.Voucher   = 0
             /* amount is first booked to customer's overpayment and this
                payment takes it from there */
             bPayment.ocr-acct  = x-acct[4]
             bPayment.DivMess   = "From OP"
             liArcSeq           = liArcSeq + 1
             /* alter archive id so that this will not be rejected */
             bPayment.ArchiveId = bPayment.ArchiveId + "_" + STRING(liArcSeq)
             ldPaymOrder        = ldPaymOrder + 1
             bPayment.POrder    = ldPaymOrder
             idAmount           = idAmount - bPayment.AmtPaid
             llPaymAdded        = TRUE. 

      /* all were booked to invoices */
      IF idAmount = 0 THEN LEAVE FindUnpaid.
   END.

END FUNCTION.


ASSIGN 
    /* Reminder Fees */
    DefRemFee    = fCParamDE("DefRemindFee")
    DefRemFac    = fCParamI("DefRemFac")
    /* max. unpaid amount to be deposited */
    xUnPaidLimit = fCParamDE("UnpaidToDeposit") 
    /* print ok-lines to booking list */
    xPrintOk     = fCParamI("RefPrintOk")
    /* directory for booking list */
    xBookDir     = fCParamC("RefPrintDir")
    lcConfDir    = fCParamC("RepConfDir")
    
    ReceivAcc    = fCParamI("ReceivAcc") 
    DiscAcc      = fCParamI("DiscAcc")
    OverPayAcc   = fCParamI("OverPayAcc")  
    liDepoAcc    = fCParamI("ResDepositsAcc")
    AdvPaymAcc   = fCParamI("AdvPaymAcc") 
    VatAcc       = fCParamI("VatAcc") 
    MinIntPay    = fCParamI("MinIntPay") 
    OverTimeInt  = fCParamI("OverTimeInt") 
    Imarg        = fCParamDE("Imarg")
    IntCalcMet   = fCParamI("IntCalcMet") 
    RoundAcc     = fCParamI("RoundAcc")
    OTIntAcc     = fCParamI("OTIntAcc") 
    VatAccount   = fCParamI("VatAcc")  
    ClaimCostAcc = fCParamI("ClaimCostAcc") 
    lcSubBalFat  = fCParamC("SaldoAgreementAccount")
    lcSubBalGift = fCParamC("eGiftPayment")
    liCLCancel   = fCParamI("CreditLossCancel")
    lcPostList   = fCParamC("RefPostList")
    lcPostDir    = fCParamC("RefPostDir")
    liDDCancel   = fCParamI("DDCancelDays").    

IF lcPostList = ? THEN lcPostList = "".
IF lcPostDir = ?  THEN lcPostDir = "".
IF liDDCancel = ? THEN liDDCancel = 0.

/* account for saldo payment and egift */
IF lcSubBalFat > "" THEN 
FOR FIRST FatGroup NO-LOCK WHERE
          FatGroup.Brand = Syst.Var:gcBrand AND
          FatGroup.FtGrp = lcSubBalFat,
    FIRST BillItem NO-LOCK WHERE
          BillItem.Brand    = Syst.Var:gcBrand AND
          BillItem.BillCode = FatGroup.BillCode,
    FIRST CCRule NO-LOCK WHERE 
          CCRule.Brand      =  BillItem.Brand    AND 
          CCRule.Category   =  "*"               AND
          CCRule.BillCode   =  BillItem.BillCode AND 
          CCRule.CLIType    =  ""                AND 
          CCRule.ValidTo    >= TODAY :
   liSaldoAcc = CCRule.AccNum.
END.
IF lcSubBalGift > "" THEN 
FOR FIRST FatGroup NO-LOCK WHERE
          FatGroup.Brand = Syst.Var:gcBrand AND
          FatGroup.FtGrp = lcSubBalGift,
    FIRST BillItem NO-LOCK WHERE
          BillItem.Brand    = Syst.Var:gcBrand AND
          BillItem.BillCode = FatGroup.BillCode,
    FIRST CCRule NO-LOCK WHERE 
          CCRule.Brand      =   BillItem.Brand    AND 
          CCRule.Category   =   "*"               AND   
          CCRule.BillCode   =   BillItem.BillCode AND 
          CCRule.CLIType    =   ""                AND 
          CCRule.ValidTo    >=  TODAY :
   liEGiftAcc = CCRule.AccNum.
END.

ASSIGN
   x-acct[2] = ReceivAcc         /* Accounts receivable        */
   x-acct[3] = DiscAcc           /* Discounts                  */
   x-acct[5] = OTIntAcc          /* Interest                   */
   x-acct[7] = ClaimCostAcc      /* Claiming costs             */
   rl        = 999
   xTotOk    = 0
   xTotMiss  = 0
   xTotOver  = 0
   xTotRej   = 0
   xQtyOk    = 0
   xQtyMiss  = 0
   xQtyOver  = 0
   xQtyRej   = 0
   xQtyClaim = 0
   xTotClaim = 0.

IF lcPostList > "" THEN DO:

   FIND FIRST ttPayment NO-ERROR.
   IF AVAILABLE ttPayment 
   THEN ldtFileDate = ttPayment.AccDate.
   ELSE ldtFileDate = TODAY.
   
   lcPostList = REPLACE(lcPostList,"#DATE",STRING(YEAR(ldtFileDate)) +
                                           STRING(MONTH(ldtFileDate),"99") + 
                                           STRING(DAY(ldtFileDate),"99")).

   /* check that file doesn't exist and form the complete name */
   lcPostList = fUniqueFileName(lcPostList,
                                ".txt").
   OUTPUT STREAM sPost TO VALUE(lcPostList).
END.

/* outer loop for restarting the "for each ttPayment" in case new ttPayment-
   records have been added during the posting (in fOldUnpaid)
*/
IF icPaymFile NE "EMPTY" THEN DO WHILE TRUE:

payment:
for each ttPayment WHERE
         ttPayment.Voucher = 0 
BY ttPayment.AccDate
BY ttPayment.POrder:  

   /* nothing to do */
   IF ttPayment.AmtPaid   = 0 AND
      ttPayment.Interest  = 0 AND
      ttPayment.ClaimCost = 0
   THEN NEXT.
   
   /* First assume that everything goes OK */
   ASSIGN err         = 0 
          ErrorMsg    = "" 
          AddMsg      = ""
          IntAmt      = 0
          xIntMark    = ""
          overpaym    = FALSE 
          subtotal    = subtotal + ttPayment.AmtPaid
          grtotal     = grtotal  + ttPayment.AmtPaid
          llPaymAdded = FALSE
          liDDVoucher = 0
          oiRead      = oiRead + 1.

   /* error noticed already in reading */
   IF ttPayment.ErrorTxt > "" THEN DO:
       fError(3,ttPayment.ErrorTxt).
   END.
   
   /* from TITO -file */
   ELSE IF ttPayment.origin BEGINS "T" THEN DO:
       fError(1,"TITO-line").
   END.

   /* direct debit has not succeeded */
   ELSE IF ttPayment.DDError NE "" THEN DO:
      ASSIGN Errormsg = "DD: " + ttPayment.DDError
             err      = -3.
             
      FIND Invoice WHERE Invoice.InvNum = ttPayment.Inv NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Invoice OR Invoice.ExtInvID NE ttPayment.ExtInvID 
      THEN DO:
         fError(1,ErrorMsg).
         AddMsg = "Invoice not available".
      END.

      /* cancellation must be done within defined days */
      ELSE IF ttPayment.AccDate > Invoice.DueDate + liDDCancel THEN DO:
         fError(1,ErrorMsg).
         AddMsg = "Cancellation is overdue".
      END.
       
      /* already credited, cancel the pending refund requests */
      ELSE IF Invoice.CrInvNum > 0 THEN DO:
         fError(1,ErrorMsg).
         AddMsg   = "Invoice already credited".
      
         RUN Ar/refundcancel.p(0,
                          "DD",
                          Invoice.InvNum,
                          Invoice.CustNum,
                          "DD cancel CSB19",
                          4).
      END.
     
      IF err = -3 THEN DO: 
         FIND FIRST Payment NO-LOCK USE-INDEX InvNum WHERE
                    Payment.Brand      = Syst.Var:gcBrand        AND
                    Payment.InvNum     = Invoice.InvNum AND
                    Payment.PaymType   = 8              AND
                    Payment.PaymSrc    = "DD"           AND
                    Payment.Posting[1] = Invoice.InvAmt NO-ERROR.
         IF AVAILABLE Payment THEN liDDVoucher = Payment.Voucher.
         ELSE err = 11.
      END.
              
   END.

   /* negative payments to unregistered (nnocko should handle corrections) */
   ELSE IF ttPayment.AmtPaid < 0 THEN DO:
       fError(1,"Correction without original").
   END.
   
   /* if invoice nbr is 0 then add customer's Deposit */
   ELSE IF ttPayment.Inv     = 0 AND 
           ttPayment.CustNum NE 0 AND
           ttPayment.AmtPaid NE 0 
   THEN DO:
   
      /* RS = payment to subscription balance, RG = paid as gift */
      IF LOOKUP(ttPayment.Origin,"RS,RG") > 0 THEN DO:
         FIND FIRST MobSub NO-LOCK WHERE
                    MobSub.MsSeq = ttPayment.CustNum NO-ERROR.
         IF AVAILABLE MobSub AND MobSub.Brand = Syst.Var:gcBrand 
         THEN DO:
            err = -2.
            FIND Customer OF MobSub NO-LOCK.
         END. 
         ELSE ASSIGN ErrorMsg = "Unknown subscr. " + STRING(ttPayment.CustNum)
                     err      = 1.
      END.  

      /* adv. payment to customer */
      ELSE DO:
         FIND Customer WHERE Customer.CustNum = ttPayment.CustNum 
             NO-LOCK NO-ERROR.
         IF AVAILABLE Customer AND Customer.Brand = Syst.Var:gcBrand 
         THEN err = -1.

         ELSE ASSIGN ErrorMsg = "Unknown cust. " + STRING(ttPayment.CustNum)
                     err      = 1.
      END.            

      IF err > 0 THEN DO:
         fError(err,ErrorMsg).
      END.   
   END.

   ELSE IF ttPayment.Inv = 0 THEN DO:
      fError(1,"Invalid Reference nbr").
   END.

   IF ttPayment.Inv > 0 AND (err = 0 OR err = -3 OR err = 11) THEN DO:
        FIND Invoice where Invoice.InvNum = ttPayment.Inv 
             exclusive-lock no-error.

       IF not avail Invoice OR Invoice.InvAmt < 0 OR 
          Invoice.Brand NE Syst.Var:gcBrand
       then do:
          fError(1,"Unknown invoice nbr").
       END.

       ELSE DO:
          
          FIND Customer OF Invoice NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Customer THEN DO:
             fError(1,"Unknown cust. " + STRING(Invoice.CustNum)).
          END.   
       END. 
   END.

   IF err le 0 THEN DO:
   
      IF /* Intrum payments don't have an archive id */
         ttPayment.Origin NE "IN" AND
         ttPayment.ArchiveId NE ""
      THEN DO:
         /* is payment already Booked */
         for each Payment no-lock use-index PaymArc where
                  Payment.Brand   = Syst.Var:gcBrand              and
                  Payment.PaymArc = ttPayment.ArchiveId  and
                  Payment.InvNum  = ttPayment.Inv        and 
                  Payment.AccDate = ttPayment.AccDate:

            /* corrections may have the same archive id */
            IF (Payment.PaymAmt > 0 AND ttPayment.AmtPaid > 0) OR
               (Payment.PaymAmt < 0 AND ttPayment.AmtPaid < 0)
            THEN DO:
               fError(2,"Already Booked, vchr " + string(Payment.Voucher)).
               LEAVE.       
            END.   
         end.
      END.

      /* check without archive id */
      ELSE         
      FOR FIRST Payment NO-LOCK USE-INDEX InvNum WHERE
                Payment.Brand   = Syst.Var:gcBrand           AND
                Payment.InvNum  = ttPayment.Inv     AND
                Payment.AccDate = ttPayment.AccDate AND
                Payment.PaymAmt = -1 * ttPayment.AmtPaid AND
                Payment.BankAcc = ttPayment.BankAcc AND
                Payment.PaymSrc = ttPayment.Origin:
         fError(2,"Already booked, vchr " + string(Payment.Voucher)).
      END.                
   END.
   
   /* check period */
   IF err le 0 THEN DO:
      /* if period is locked then post the payment to the 1. day of the first
         open period */
      if fPeriodLocked(ttPayment.AccDate,FALSE) THEN DO:
      
         ldtAccDate = ttPayment.AccDate.
         
         FOR EACH AccPeriod NO-LOCK WHERE
                  AccPeriod.Brand     = Syst.Var:gcBrand AND
                  AccPeriod.FromDate  > ttPayment.AccDate AND
                  AccPeriod.PerLocked = FALSE
         BY AccPeriod.FromDate:
            ttPayment.AccDate = AccPeriod.FromDate.
            LEAVE.
         END.

         /* open period was not found */   
         IF ldtAccDate = ttPayment.AccDate THEN DO:
            fError(1,"Period locked for " + 
                     STRING(ttPayment.AccDate,"99.99.9999")).
         END.
      END.
   END.

   if err le 0 then do:  /* payment will be made */

      FIND InvGroup where 
           InvGroup.Brand    = Syst.Var:gcBrand AND 
           InvGroup.InvGroup = Customer.InvGroup 
         no-lock no-error.
      if avail InvGroup then
      ASSIGN b-acc = InvGroup.UpdCustBal.
      ELSE b-acc = TRUE. 

      /* How much is not yet paid ? */
      assign Debt        = 0
             ldCredLoss  = 0
             ldOpPosting = 0
             ldOpBal     = 0.

      IF err = 0 THEN DO:

         /* open balance */
         Debt = fCalcInvBal(BUFFER Invoice, 
                            TODAY + 1000,
                            FALSE).  /* disregard possible ePaym */

         /* has credit loss been posted */
         ldCredLoss = fCredLossPaid(BUFFER Invoice,
                                    TODAY,
                                    OUTPUT liLossAcc).
                                    
         /* previous overpayment */
         IF ttPayment.AmtPaid < 0 THEN DO:
            FOR EACH Payment OF Invoice NO-LOCK:
               DO i = 1 TO 10:
                  IF Payment.AccType[i] = 6 
                  THEN ldOpPosting = ldOpPosting + Payment.Posting[i].
               END.
            END.

            /* has op already been targeted to other invoices */
            ldOpBal = fGetCustBal(Invoice.CustNum,"TOTAL","OP").
            IF -1 * ldOpPosting > ldOpBal THEN ldOpPosting = -1 * ldOpBal.
         END.   
         
      END.                              

      /* DD has failed, make a reverse payment to invoice */
      ELSE IF err = -3 THEN DO:
         FIND Payment WHERE Payment.Voucher = liDDVoucher NO-LOCK.
         DO i = 1 TO 10:
            ASSIGN x-acct[i] = Payment.AccNum[i]
                   x-amt[i]  = -1 * Payment.Posting[i].
         END.          
      END.
      
      ELSE DO:
         /* calculate bookings into GenLedger */
         ASSIGN
         x-amt     = 0
         diff      = 0
         x-acct[1] = ttPayment.ocr-acct
         x-amt[1]  = ttPayment.AmtPaid + ttPayment.Interest + 
                     ttPayment.ClaimCost        /* Money received           */
         x-amt[2]  = 0 - ttPayment.AmtPaid      /* off the receivables      */
         x-amt[5]  = 0 - ttPayment.Interest     /* Interest amount          */
         x-amt[7]  = 0 - ttPayment.ClaimCost    /* claiming costs           */
         ErrorMsg  = "".                        /* no error MESSAGE         */
      END.
      
      discount  = false.                      /* no automatic discount    */

      IF ttPayment.AmtPaid <> Debt and 
         ttPayment.AmtPaid > 0     and
         Debt > 0                  and
         err = 0
      then do:

         /* difference between total debt and this payment */
         diff = Debt - ttPayment.AmtPaid.

         /* We assume that payment is always a positive (+) amount */

         /* too low or high payment */ 
         IF abs(diff) ne 0 and abs(diff) <= IMarg
         THEN DO:
            ASSIGN
            x-amt[2] = x-amt[2] - diff   /* subtract receivables */
            x-amt[3] = diff             /* discount(+/-)               */
            discount = true
            ErrorMsg = "Discount (+/-) " + TRIM(string(diff,"->,>>>,>>9.99")).
         END.

         /* Limit for transferring unpaid amount to customer's Deposit
            is given (discount is checked first, so that it can be given
            a lower Limit) */
         ASSIGN xUnPaid = FALSE.
         IF NOT discount AND diff GT 0 AND diff LE xUnPaidLimit AND
            (Invoice.ClaimStatus EQ "" OR Invoice.ClaimStatus BEGINS "0")
         THEN DO:
            /* negative overpaym to customer */
            ASSIGN 
            x-amt[6] = diff
            x-acct[6] = OverPayAcc
            /* subtract receivables */
            x-amt[2] = x-amt[2] - diff
            xUnPaid = TRUE
            ErrorMsg = "Deposited unpaid " + TRIM(string(diff,"->>>,>>9.99")).
         END.
         ELSE ASSIGN diff = 0.
      END.

      /* what to do with extra payment  */
      IF err NE -3 THEN 
      CASE err:
      WHEN -1 THEN x-acct[4] = AdvPaymAcc.
      WHEN -2 THEN x-acct[4] = IF ttPayment.Origin = "RG"
                               THEN liEGiftAcc
                               ELSE liSaldoAcc.
      OTHERWISE    x-acct[4] = OverPayAcc.
      END CASE.
      
      IF err NE -3 AND 
        ((NOT DISCOUNT and ttPayment.AmtPaid > Debt) OR
          /* subtract previous overpayment */
         (ttPayment.AmtPaid < 0 AND ldOpPosting < 0) OR
          err < 0
        )
      THEN DO:

         /* FIRST check IF credit loss has been posted */
         IF ldCredLoss > 0 AND err = 0 AND ttPayment.AmtPaid > 0 THEN DO:
            ASSIGN x-acct[10] = IF liCLCancel > 0
                                THEN liCLCancel
                                ELSE liLossAcc 
                   x-amt[10]  = -1 * MIN(ldCredLoss,ttPayment.AmtPaid - Debt)
                   AddMsg     = "Cr.loss reversed: " +
                                STRING(x-amt[10] * -1). 

            /* vat for credit loss */
            x-amt[11] = fCLVat(x-amt[10],
                               BUFFER Invoice,
                               OUTPUT i).

            IF x-amt[11] NE 0 THEN ASSIGN 
               x-acct[11] = i
               x-amt[10]  = x-amt[10] - x-amt[11].
         END.

         IF ttPayment.AmtPaid > 0 THEN ASSIGN 
            /* subtract the existing debt from receivables */
            x-amt[2] = 0 - Debt
            /* Part that exceeds the existing debt AND possible 
               earlier credit loss (note: x-amt10 is negative -> +) */
            x-amt[4] = 0 - (ttPayment.AmtPaid - Debt + x-amt[10] + x-amt[11]).

         /* negative sum -> correction to an earlier payment
            first check if overpayment has been posted from this invoice's
           payments and subtract that */
         ELSE DO:
            ASSIGN x-amt[4] = MIN(-1 * ldOpPosting,-1 * ttPayment.AmtPaid)
                   x-amt[2] = x-amt[2] - x-amt[4].
         END.

         /******************************************
         * IF there is just exactly an overpayment *
         * that equals DefaultReminder Fee AND     *
         * the reminder status of this invoice     *
         * indicates that customer has received a  *
         * reminder WE ASSUME that this extra sum  *
         * is a REMINDER FEE                       *
         ******************************************/

         Overpaym = TRUE.

         /* if sum matches and if the rem fee is set */
         IF (0 - x-amt[4]) = DefRemFee AND DefRemFee > 0 AND err = 0
         THEN DO:
            IF Invoice.ClaimStatus > "" AND
               Invoice.ClaimStatus NE "0.0" THEN ASSIGN
                Overpaym  = FALSE
                x-amt[4]  = 0 - DefRemFee   
                x-acct[4] = DefRemFAc
                ErrorMsg  = "+ Reminder Fee " +
                            TRIM(string(0 - x-amt[4],"->>>,>>9.99")).
         END.

         /* ... MESSAGE to the printout */
         IF overpaym THEN DO:

            /* if customer has old unpaid invoices -> payments to them */
            IF LOOKUP(ttPayment.Origin,"SC,IN,RD,RA,RV") = 0 AND
               ttPayment.AmtPaid > 0                      AND
               err = 0 
            THEN DO:
               fOldUnpaid(-1 * x-amt[4]).
            END.

            ASSIGN 
            ErrorMsg    = (IF err = -1 
                           THEN "Adv.payment " 
                           ELSE IF err = -2 
                                THEN "Subscr.balance "     
                                ELSE "Overpayment ") +
                           TRIM(string(0 - x-amt[4],"->>>,>>9.99"))
            xTotOver[1] = xTotOver[1] + ttPayment.AmtPaid
            xQtyOver    = xQtyOver + 1.
         END.
     END.

     /* underpayment */
     IF NOT DISCOUNT and NOT xUnPaid AND
        ttPayment.AmtPaid < Debt     AND
        ttPayment.AmtPaid > 0        AND
        err = 0
     THEN ASSIGN 
         ErrorMsg  = "Still missing " +
                     TRIM(string(Debt - ttPayment.AmtPaid,"->>,>>9.99"))
         xTotMiss = xTotMiss + ttPayment.AmtPaid
         xQtyMiss = xQtyMiss + 1.

     ELSE IF NOT overpaym then assign
         ErrorMsg = "OK" WHEN err NE -3
         xTotOk   = xTotOk + ttPayment.AmtPaid + ttPayment.Interest
         xQtyOk   = xQtyOk + 1.

     /* this message overrides others */
     IF ttPayment.DivMess NE "" 
     THEN Errormsg = ttPayment.DivMess. 

     /* Account for receivables (from the invoice record),
        for deposit invoices (type 3) a/r account is not used
     */
     IF err = -1 OR err = -2 THEN x-acct[2] = 0.
     ELSE IF err NE -3 THEN 
     CASE Invoice.InvType:
     WHEN 3 THEN x-acct[2] = liDepoAcc.
     WHEN 4 THEN x-acct[2] = AdvPaymAcc.
     OTHERWISE   x-acct[2] = Invoice.ARAccNum.
     END CASE.

      lcCustName = Func.Common:mDispCustName(BUFFER Customer).
      
      CREATE Payment.
      
      /* internal voucher nbr */ 
      REPEAT:

         Payment.Voucher = fGetIntVoucher() NO-ERROR.

         VALIDATE Payment NO-ERROR.

         /* another process has just used the same number */
         IF ERROR-STATUS:ERROR OR Payment.Voucher = 0 THEN NEXT.
         ELSE LEAVE.
      END.
      
      ASSIGN 
         Payment.Brand     = Syst.Var:gcBrand 
         Payment.CustNum   = Customer.CustNum
         Payment.CustName  = lcCustName
         Payment.TotAmt    = ttPayment.AmtPaid + Diff + ttPayment.Interest
         Payment.PaymAmt   = ttPayment.AmtPaid + ttPayment.Interest
         Payment.Discount  = Diff
         Payment.PaymDate  = ttPayment.PaymDate
         Payment.AccDate   = ttPayment.AccDate
         Payment.BankAcc   = ttPayment.BankAcc
         Payment.PaymSrc   = ttPayment.origin  
         Payment.PaymFile  = ttPayment.fname
         Payment.PaymArc   = ttPayment.ArchiveId
         Payment.Refnum    = ttPayment.RefNum
         Convno            = Payment.Voucher
         ttPayment.Voucher = Payment.Voucher.

      CASE err:
      WHEN -2 THEN Payment.PaymType = 7.
      WHEN -3 THEN Payment.PaymType = 8.
      OTHERWISE Payment.PaymType = 0.
      END CASE. 
      
      /* reduction of credit loss */
      IF ldCredLoss > 0 AND x-amt[10] + x-amt[11] NE 0
      THEN Payment.PaymType = 2.

      Payment.ImportStamp = Func.Common:mMakeTS().

      if err = 0 OR err = -3 then assign 
         Payment.InvNum   = Invoice.InvNum
         Payment.ExtInvID = Invoice.ExtInvID
         Payment.InvAmt   = Invoice.InvAmt
         Payment.InvDate  = Invoice.InvDate
         Payment.DueDate  = Invoice.DueDate.

      /* Accounts */
      ASSIGN t       = 0
             ldArAmt = 0.

      DO i = 1 TO 11:
         IF x-amt[i] = 0 then next.
      
         ASSIGN
            t = t + 1
            Payment.AccNum[t]  = x-acct[i]
            Payment.Posting[t] = x-amt[i].

         /* account type */
         FIND Account WHERE 
            Account.Brand  = Syst.Var:gcBrand AND
            Account.AccNum = Payment.AccNum[t] NO-LOCK NO-ERROR.
         IF AVAIL Account THEN 
             Payment.AccType[t] = Account.AccType.

         /* change to customer balance */
         IF Payment.AccType[t] = 1 
         THEN ldArAmt = ldArAmt + Payment.Posting[t].

         /* overpayment */
         ELSE IF Payment.AccType[t] = 6 THEN DO:
            fCustBal(Customer.CustNum,
                     "",
                     "OP",
                     -1 * Payment.Posting[t]). 
         
            /* transaction log */   
            fCreateOpLog(IF xUnPaid AND i = 6 THEN 9 ELSE 2,
                         IF err < 0 THEN 0 ELSE Invoice.InvNum,
                         -1 * Payment.Posting[t]).
         END.
         
         /* Deposit  */
         ELSE IF Payment.AccType[t] = 7 THEN DO:
            /* customer balance */
            fCustBal(Customer.CustNum,
                     "",
                     "DP",
                     -1 * Payment.Posting[t]). 

            /* transaction log */   
            fCreateOpLog(13,
                         IF err < 0 THEN 0 ELSE Invoice.InvNum,
                         -1 * Payment.Posting[t]).
         END.                 
      
         /* adv.payment */
         ELSE IF Payment.AccType[t] = 19 THEN DO:
 
            /* normal, customer related */
            IF err NE -2 THEN DO:
               /* customer balance */
               fCustBal(Customer.CustNum,
                        "",
                        "AP",
                        -1 * Payment.Posting[t]). 

               /* transaction log */   
               fCreateOpLog(10,
                            IF err < 0 THEN 0 ELSE Invoice.InvNum,
                            -1 * Payment.Posting[t]). 
            END.

            /* addition to subscription balance */
            ELSE DO:
               /* transaction log */   
               fCreateOpLog(21,
                            0,
                            -1 * Payment.Posting[t]). 
            END.               
             
            /* VAT for advance payment */
            IF (err = 0 AND Invoice.VATUsage < 3) OR
               (err < 0 AND Customer.VATUsage < 3) THEN
            ASSIGN  t                  = t + 1
                    Payment.Posting[t] = fAPVatAmt(Customer.Region,
                                                   -1 * Payment.Posting[t - 1],
                                                   Payment.AccDate)
                    Payment.AccNum[t]  = liAPVatAcc
                    Payment.AccType[t] = 5 /* should actually be checked from
                                              account, but this must be a 
                                              vat account .. */
                    t                  = t + 1
                    Payment.Posting[t] = -1 * Payment.Posting[t - 1]
                    Payment.AccNum[t]  = liAPVatDebtAcc
                    Payment.AccType[t] = 5. 
         END.
      END.

      /* Create an Interest Record */
      IF ttPayment.Interest = 0 AND
         err = 0                AND   
         ttPayment.PaymDate  - Invoice.DueDate > 0 AND
         Debt ne 0              AND
         Invoice.InterestPerm = true   AND
         ttPayment.AmtPaid > 0
      then do:
         /* calculate interest */
         RUN Ar/calcint.p(Invoice.DueDate,
                     ttPayment.PaymDate,
                     IntCalcMet,
                     ttPayment.AmtPaid,
                     Customer.CustNum,
                     output qdays,
                     output qperc,
                     output IntAmt).

         DelDays = Int(ENTRY(1,qdays)).
         InPerc  = Decimal(ENTRY(1,qperc)) / 100.                         

      END.

      /* IF Interest exceeds the limit, make an Interest record */
      IF err = 0 AND IntAmt > MinIntPay then do:
         create CustIntEvent.
         ASSIGN
            CustIntEvent.Brand    = Payment.Brand
            CustIntEvent.Voucher  = Payment.Voucher
            CustIntEvent.InvNum   = Invoice.InvNum
            CustIntEvent.CustNum  = Invoice.CustNum
            CustIntEvent.InvDate  = Invoice.InvDate
            CustIntEvent.DueDate  = Invoice.DueDate
            CustIntEvent.PaymDate = ttPayment.PaymDate
            CustIntEvent.LateDays = DelDays
            CustIntEvent.Amt      = IntAmt
            CustIntEvent.InvAmt   = Invoice.InvAmt
            CustIntEvent.PaidAmt  = ttPayment.AmtPaid 
            CustIntEvent.Percent  = InPerc
            xIntMark              = "#". 

            /* Interest arrays */
            DO i = 1 TO 10:
               ASSIGN
               CustIntEvent.IntPerc[i] = intpro[i]
               CustIntEvent.IntDays[i] = intdays[i]
               CustIntEvent.IntAmt[i]  = intsumm[i].
            END.
      END.

      if err = 0 then
      /* Customer's payment behavior and no. of payments */
      fPaymBehaviour(Customer.CustNum,
                     "",
                     ttPayment.PaymDate,
                     IF err = 0
                     THEN Invoice.DueDate
                     ELSE ?). 

      IF b-acc then DO:
         /* Customer's Balance */
         fCustBal(Customer.CustNum,
                  "",
                  "ARBAL",
                  ldArAmt). 

         /* Interest Debt */
         fCustBal(Customer.CustNum,
                  "",
                  "INT",
                  IntAmt). 
      END.

      /* invoice data */
      if err = 0 OR err = -3 then do:
          /* Date of payment */          
          IF err = -3 THEN Invoice.PaymDate = ?.
          ELSE IF Invoice.PaymDate = ? or Invoice.PaymDate < ttPayment.PaymDate           THEN Invoice.PaymDate = ttPayment.PaymDate.

          /* Payment status of invoice */
          paybal = fCalcInvBal(BUFFER Invoice,
                               TODAY + 1000,
                               FALSE).  /* disregard possible ePaym */
          Invoice.PaidAmt = Invoice.InvAmt - paybal.           

          /* credit loss */
          IF Invoice.PaymState = 3 THEN DO:
             /* reversed totally */
             IF ldCredLoss > 0 AND x-amt[10] + x-amt[11] = -1 * ldCredLoss 
             THEN Invoice.PaymState = 2. 
          END.
          ELSE DO:
             IF paybal = 0 THEN Invoice.PaymState = 2. 
             
             /* don't change payment state if invoice is in a paymplan */
             ELSE IF Invoice.PaymState NE 4 THEN DO:
                IF paybal = Invoice.InvAmt 
                THEN Invoice.PaymState = 0. 
                ELSE Invoice.PaymState = 1.
             END.    
          END.
          
          /* update possible payment plan */
          fPaymPlanPaid(Invoice.CustNum,
                        Invoice.InvNum,
                        -1 * ldArAmt).

      END.
     
      ASSIGN AccTotal = AccTotal + ttPayment.AmtPaid.

      /* special type of invoices, 3 & 4 */
      IF err = 0 THEN DO:

         /* check if invoice is created from order (if fully paid) */
         IF (Invoice.InvType = 3 OR Invoice.InvType = 4) AND
            Invoice.PaymState = 2 THEN 
         FOR FIRST SingleFee NO-LOCK WHERE
                   SingleFee.InvNum    = Invoice.InvNum AND
                   SingleFee.HostTable = "Order",
             FIRST Order EXCLUSIVE-LOCK WHERE
                   Order.Brand   = Invoice.Brand AND
                   Order.OrderID = INTEGER(SingleFee.KeyValue):
                      
            /* checked */
            Order.CredOK = TRUE.
            fSetOrderStatus(Order.OrderID,"3").
            fMarkOrderStamp(Order.OrderID,"Change",0.0). 
         END.
      
      END.          

      /* addition to subscription balance; check that fatime is created */
      ELSE IF err = -2 AND AVAILABLE MobSub THEN DO:

         EMPTY TEMP-TABLE ttFat.
         
         FOR EACH Payment NO-LOCK WHERE
                  Payment.Brand           = Syst.Var:gcBrand          AND
                  Payment.CustNum         = Customer.CustNum AND
                  Payment.PaymType        = 7                AND
                  MONTH(Payment.PaymDate) = MONTH(ttPayment.PaymDate) AND
                  YEAR(Payment.PaymDate)  = YEAR(ttPayment.PaymDate):
               
             ASSIGN llOk  = FALSE
                    /* take period from payment date */
                    liPer = YEAR(Payment.PaymDate) * 100 + 
                            MONTH(Payment.PaymDate)
                    lcFat = IF Payment.PaymSrc = "RG"
                            THEN lcSubBalGift
                            ELSE lcSubBalFat.         
                              
             FOR EACH Fatime NO-LOCK WHERE
                      Fatime.Brand   = Syst.Var:gcBrand           AND
                      Fatime.FtGrp   = lcFat             AND
                      Fatime.CustNum = Payment.CustNum   AND
                      Fatime.MsSeq   = MobSub.MsSeq      AND
                      Fatime.Period  = liPer             AND
                      Fatime.Amt     = Payment.PaymAmt:

                /* same sum may have been paid multiple times in one month */
                IF CAN-FIND(FIRST ttFat WHERE 
                                  ttFat.Fat = INTEGER(RECID(Fatime)))
                THEN NEXT.

                llOk = TRUE. 
                CREATE ttFat.
                ttFat.Fat = RECID(Fatime).
                LEAVE.
             END.

             IF NOT llOk THEN DO:
                /* if default rows have been defined to fatgroup 
                   then copy them */
                IF NOT fCopyDefFatime(lcFat,
                                      Customer.CustNum,
                                      MobSub.MsSeq,
                                      MobSub.CLI,
                                      Payment.PaymAmt,
                                      liPer,
                                      999999,
                                      "")
                THEN DO:
                   /* otherwise create one row with default values */
                   fCreateFatime(lcFat,
                                 Customer.CustNum,
                                 MobSub.MsSeq,
                                 MobSub.Cli,
                                 Payment.PaymAmt,
                                 liPer,
                                 "").
                END.
              
             END.
         END.

         /* replace msseq with custnum for listing purposes */   
         ttPayment.CustNum = Customer.CustNum.
         
      END.

      IF AVAILABLE Payment THEN DO:  
         ASSIGN
            lrRecid = RECID(Payment)
            liCount = 0.

         REPEAT:

            /* external voucher id */
            Payment.ExtVoucher = fGetAndUpdExtVoucher(Customer.InvGroup,
                                                      Payment.PaymType,
                                                      Payment.AccDate,
                                                      OUTPUT lcPref).

            IF Payment.ExtVoucher = "" THEN DO:
               liCount = 11000.
               LEAVE.
            END.
   
            FIND Payment WHERE RECID(Payment) = lrRecid EXCLUSIVE-LOCK.
   
            ASSIGN
               lcExtVoucher = Payment.ExtVoucher
               liCount      = liCount + 1.
  
            IF liCount > 10000 THEN LEAVE.

            IF NOT CAN-FIND(FIRST Payment WHERE
                                  Payment.Brand      = Syst.Var:gcBrand AND
                                  Payment.ExtVoucher = lcExtVoucher AND
                                  RECID(Payment) NE lrRecid)
            THEN LEAVE.
         END.


         IF lcVouchRange[1] = "" THEN lcVouchRange = Payment.ExtVoucher.
      
         ASSIGN lcVouchRange[1] = MIN(lcVouchRange[1],Payment.ExtVoucher)
                lcVouchRange[2] = MAX(lcVouchRange[2],Payment.ExtVoucher). 
      END.

   END. /* invoice was found */

   /* DD error */
   IF err = -3 OR err = 11 THEN DO:

      IF err = 11 THEN ASSIGN 
         xTotClaim = xTotClaim + ttPayment.AmtPaid 
         xQtyClaim = xQtyClaim + 1.
      
      IF err = -3 AND AVAILABLE Payment THEN ASSIGN 
         lcTableName = "Payment"
         lcKeyValue  = STRING(Payment.Voucher).
      ELSE ASSIGN 
         lcTableName = "Customer"
         lcKeyValue  = STRING(Customer.CustNum).

      Func.Common:mWriteMemo(lcTableName,
                             lcKeyValue,
                             Customer.CustNum,
                             "Failed Direct Debit",
                             ttPayment.DDError + CHR(10) +
                             "Invoice: " + ttPayment.ExtInvID + CHR(10) +
                             "Amount: " + STRING(ttPayment.AmtPaid)).

      IF ttPayment.Inv > 0 AND AVAILABLE Invoice THEN DO:
         FIND CURRENT Invoice EXCLUSIVE-LOCK.
         ASSIGN 
            Invoice.ClaimState = 1 + INTEGER(ttPayment.ErrorCode) / 10
            Invoice.ClaimStatus = REPLACE(STRING(Invoice.ClaimState),",",".")
            Invoice.DDState    = 2
            Invoice.ClaimStamp = Func.Common:mMakeTS().
               
         CREATE ClaimHist.
         ASSIGN 
            ClaimHist.Brand      = Invoice.Brand
            ClaimHist.InvNum     = Invoice.InvNum
            ClaimHist.CustNum    = Invoice.CustNum 
            ClaimHist.ClaimState = Invoice.ClaimState
            ClaimHist.Claim      = 10 * Invoice.ClaimState
            ClaimHist.Memo       = "DD cancelled"
            ClaimHist.ClaimDate  = IF ttPayment.PaymDate NE ?
                                   THEN ttPayment.PaymDate
                                   ELSE TODAY
            ClaimHist.ClaimAmt   = Invoice.InvAmt - Invoice.PaidAmt
            ClaimHist.Handler    = Syst.Var:katun.
      END.       
   END.
 
   /* Disp data on screen */
   IF ilDispMsg THEN DO:
      pause 0.
      IF (xQtyOk + xQtyMiss + xQtyOver + xQtyRej) MOD 10 = 0  THEN 
      disp (xQtyOk + xQtyMiss + xQtyOver + xQtyRej) ;& xQtyok 
          ttPayment.RefNum ttPayment.Inv ttPayment.PaymDate ttPayment.AmtPaid 
          ttPayment.CustName with frame log.
   END.
   
   fChgPage(IF sl = 0 
            THEN 999
            ELSE IF ttPayment.PaymDate ne ed-PaymDate and ed-PaymDate ne ? 
                 THEN 2 
                 ELSE 0).

   /* print out a sub-total by prev. day prior to first line of a new day */
   IF ttPayment.AccDate ne ed-PaymDate and ed-PaymDate ne ? then do:

      put stream tul unformatted
          ed-PaymDate format "99.99.99"   at 1
          "*********"                    at 10
          "TOTAL"                        at 21
          subtotal format "->>>>,>>9.99" to 42
          "********"                     at 45 
          skip(1).
      rl = rl + 2.
      subtotal = 0.
   END.

   ASSIGN ed-PaymDate = ttPayment.AccDate.

   /* info about payment */
   if ErrorMsg = "" THEN ASSIGN ErrorMsg = AddMsg
                                AddMsg   = "". 

   /* shall ok-lines be Printed */
   IF xPrintOk = 1           OR
      ErrorMsg NE "OK"       OR
      AddMsg NE ""           OR
      ttPayment.AmtPaid LT 0
   THEN DO:

       /* actual posting line is printed to two streams */
       lcPrintLine = STRING(ttPayment.AccDate,"99.99.99") + " ".
       
       IF err le 0 AND AVAILABLE Payment 
       THEN lcPrintLine = lcPrintLine + STRING(Payment.ExtVoucher,"X(12)").
       ELSE lcPrintLine = lcPrintLine + FILL("*",12).
        
       lcPrintLine = lcPrintLine + " " + 
                     STRING(ttPayment.ExtInvId,"X(12)") + " " +
                     STRING(ttPayment.AmtPaid + ttPayment.Interest +
                            ttPayment.ClaimCost,"->>>>,>>9.99") + " ".
                            
       IF ttPayment.Inv > 0 AND AVAILABLE Invoice 
       THEN lcPrintLine = lcPrintLine + STRING(Invoice.DueDate,"99.99.99").
       ELSE lcPrintLine = lcPrintLine + FILL(" ",8).
       
       IF (err LE 0 OR err = 11) AND AVAILABLE Customer THEN DO:
          IF err = 11 THEN 
             lcCustName = Func.Common:mDispCustName(BUFFER Customer).

          lcPrintLine = lcPrintLine + " " + 
                        STRING(Customer.CustNum,"zzzzzzz9") + " " +
                        STRING(lcCustName,"X(16)") + " ".
       END.
       ELSE lcPrintLine = lcPrintLine + " " +
                          STRING(ttPayment.CustNum,"zzzzzzz9") + " " +
                          STRING(ttPayment.CustName,"X(16)") + " ".
       

       PUT STREAM tul UNFORMATTED 
          lcPrintLine + STRING(ErrorMsg,"X(39)") SKIP.

       IF lcPostList > "" THEN PUT STREAM sPost UNFORMATTED 
          lcPrintLine + ErrorMsg + " " + AddMsg SKIP.
             
       IF AddMsg ne "" THEN DO:
          fChgPage(1). 
          
          PUT STREAM tul 
             AddMsg at 84 format "x(39)"
             SKIP.
 
          ASSIGN rl = rl + 1.
       END. 

       /* transferred to Intrum */
       IF err = 0 and available Invoice AND Invoice.ClaimBatch ne 0 THEN DO:

         fChgPage(1). 
         put stream tul unformatted 
            "Claimed, batch " AT 84
            Invoice.ClaimBatch
            SKIP.
         assign rl = rl + 1. 
       END.

       /* deposit/adv.paym invoice */
       IF err = 0 AND AVAILABLE Invoice AND 
          (Invoice.InvType = 3 OR Invoice.InvType = 4) 
       THEN DO:
          fChgPage(1). 
          put stream tul unformatted 
             (IF Invoice.InvType = 3 THEN "Deposit" ELSE "AdvPaym") +
             " Invoice" AT 84 SKIP.
          assign rl = rl + 1. 
       END.

       ASSIGN rl  = rl + 1 
              rlx = rlx + 1.
   END.

   IF err = 1                                                     AND 
      (not can-find(first UnregPaym where 
                     UnregPaym.Brand     = Syst.Var:gcBrand             and
                     UnregPaym.ArchiveId = ttPayment.ArchiveId and
                     UnregPaym.AccDate   = ttPayment.AccDate   and
                     UnregPaym.PaidAmt   = ttPayment.AmtPaid +
                                           ttPayment.ClaimCost +
                                           ttPayment.Interest)  or
       (ttPayment.Origin = "UR" and ttPayment.ArchiveId = "")   or 
       ttPayment.Origin = "IN")                                   AND
      /* Bank accounts that are rejected */            
      lookup(ttPayment.BankAcc,xRejBanks) = 0
   THEN DO:

      create UnregPaym.
      assign UnregPaym.UrSeq     = NEXT-VALUE(UrSeq)
             UnregPaym.Brand     = Syst.Var:gcBrand 
             UnregPaym.ArchiveId = ttPayment.ArchiveId
             UnregPaym.AccDate   = ttPayment.AccDate
             UnregPaym.PaymDate  = ttPayment.PaymDate
             UnregPaym.RefNum    = ttPayment.RefNum
             UnregPaym.Inv       = STRING(ttPayment.Inv)
             UnregPaym.PaidAmt   = ttPayment.AmtPaid + 
                                   ttPayment.ClaimCost +
                                   ttPayment.Interest
             UnregPaym.CustName  = ttPayment.CustName
             UnregPaym.BankAcc   = ttPayment.BankAcc
             UnregPaym.Memo      = (IF ttPayment.ClaimCost NE 0
                                    THEN "Includes claiming cost " +      
                                         STRING(ttPayment.ClaimCost) + ". "
                                    ELSE "") +
                                   (IF ttPayment.Interest NE 0
                                    THEN "Includes Interest " +
                                         STRING(ttPayment.Interest) + ". "
                                    ELSE "") +
                                   ttPayment.Memo
             UnregPaym.PaymSrc   = ttPayment.Origin
             UnregPaym.State     = 0.

   end.                      

   /* mark as handled even if payment was not created */
   IF ttPayment.Voucher = 0 THEN ttPayment.Voucher = -1.

   /* new payments were added in fOldUnpaid */
   IF llPaymAdded THEN LEAVE payment. 

END. /* payment */

IF NOT CAN-FIND(FIRST ttPayment WHERE ttPayment.Voucher = 0)
THEN LEAVE. 

END.  /* do while true */

IF icPaymFile NE "EMPTY" THEN DO:

   IF subtotal > 0 then do:
      fChgPage(1).

      /* last sub-total */
      IF grtotal = subtotal then 
         put stream tul unformatted
            "GRAND TOTAL" AT 1.
      ELSE 
         put stream tul unformatted
            ed-PaymDate format "99.99.99" at 1
            "*********"                  at 10
            " TOTAL"                     at 21.
      put stream tul 
         subtotal format "->,>>>,>>9.99" to 47
         "********"                      at 49 skip.
      rl = rl + 1.
   END.

   /* if more than 1 acc dates then print grand total */
   if grtotal gt subtotal then do:

      fChgPage(1).
      /* last sub-total */
      put stream tul unformatted
         skip(1)
         "GRAND TOTAL"                   at 1
         grtotal format "->,>>>,>>9.99"  to 47
         "********"                      at 49 skip.
         rl = rl + 2.                       
   end.

   fChgPage(7).

   put stream tul unformatted
      skip(1)
      "Voucher range ....: " at 1 
         lcVouchRange[1] + " - " + lcVouchRange[2] FORMAT "X(27)" SKIP
      "Qty of postings   : " at 1  xQtyOk      format ">>>>>9"
         "Sum: "                at 45 xTotOk      format "->>>>>>>9.99" skip
      "Qty of claimstates: " at 1  xQtyClaim   format ">>>>>9"
         "Sum: "                at 45 xTotClaim   format "->>>>>>>9.99" skip
      "Qty of rejections : " at 1  xQtyRej     format ">>>>>9"
         "Sum: "                at 45 xTotRej     format "->>>>>>>9.99" 
       skip(1).
   assign rl = rl + 7.                

   ocInfo = "Qty of postings   : " + STRING(xQtyOk,">>>>>9") +
               "  Sum: " + STRING(xTotOk,"->>>>>>>9.99") + CHR(10) + 
            "Qty of claimstates: " + STRING(xQtyClaim,">>>>>9") + 
               "  Sum: " + STRING(xTotClaim,"->>>>>>>9.99") + CHR(10) + 
            "Qty of rejections : " + STRING(xQtyRej,">>>>>9") + 
               "  Sum: " + STRING(xTotRej,"->>>>>>>9.99").
         
   /* Booked amount */
   fChgPage(1).
   put stream tul unformatted
      "POSTED TOTAL"                   at 1
      AccTotal format "->,>>>,>>9.99"  to 47        
      "********"                       at 49 skip.
   rl = rl + 1.

   {Syst/uprfeed.i rl}

   IF str1 > "" THEN DO:
      ASSIGN tila = false.
      {Syst/utuloste.i}
   END.
   ELSE OUTPUT STREAM tul CLOSE.
END.


IF lcPostList > "" THEN 
   OUTPUT STREAM sPost CLOSE.

/* send the booking list via mail */
IF ilSendMail AND iiFileType >= 1 AND iiFileType <= 3 THEN DO:
    
   /* mail recipients */
   GetRecipients(lcConfDir + 
                 "payment_book_type" + STRING(iiFileType) +
                 ".email").
                  
   IF xMailAddr > "" THEN DO:
    
      ASSIGN lcMailFile = "/tmp/payment_" +
                          STRING(YEAR(TODAY),"9999") +
                          STRING(MONTH(TODAY),"99")  +
                          STRING(DAY(TODAY),"99")    + 
                          "_" + STRING(TIME) + ".txt".
                                    
      OUTPUT STREAM tul TO VALUE(lcMailFile).
      PUT STREAM tul UNFORMATTED
         "Payments booked on " 
         STRING(TODAY,"99.99.9999") 
         " from file " icPaymFile
         SKIP.
      OUTPUT STREAM tul CLOSE. 

      SendMail(lcMailFile,
               oso).
   END.
                
END.

/* move the booking list to the Transfer directory */
IF icPaymFile NE "EMPTY" AND xBookDir NE "" AND SEARCH(oso) NE ? THEN DO:

   fTransDir(oso,
             ".txt",
             xBookDir).
END.

/* move the other posting list also */
IF lcPostList > "" AND lcPostDir > "" THEN DO:
   fTransDir(lcPostList,
             ".txt",
             lcPostDir).
END.

IF ilDispMsg THEN 
MESSAGE skip
        "Voucher range: "  string(lcVouchRange[1]) + " - " +
                           string(lcVouchRange[2]) skip(1)
        "Qty of postings   : "  string(xQtyOk,     ">>>>>9")      
        "Sum: "                 string(xTotOk,     "->>>>>>9.99") skip
        "Qty of claimstates: "  string(xQtyMiss,   ">>>>>9")
        "Sum: "                 string(xTotMiss,   "->>>>>>9.99") skip
        "Qty of rejections : "  string(xQtyRej,    ">>>>>9")     
        "Sum: "                 string(xTotRej,    "->>>>>>9.99") skip(1)
        "             Total: "  string(xQtyOk + xQtyMiss + 
                                       xQtyRej,    ">>>>>9")
        "     "                 string(xTotOk + xTotMiss +
                                       xTotRej, "->>>>>>9.99")
        skip(1)
VIEW-AS ALERT-BOX
TITLE " Payments ".

HIDE frame log no-pause.
HIDE MESSAGE.


