/*------------------------------------------------------------------
  MODULE .......: NNSUSY.p
  KUTSUVA MODULI: NN.P
  FUNCTION .....: Suoritusten syOttO
  APPLICATION ..: VP
  AUTHOR .......: TT
  CREATED ......: 24.02.1997
  changePVM ....: 12.03.1997 pt synkr. as-limi[4] oikein myOs kun korj.suoritus
                  27.05.1997 pt koronlasku 365-kaavalla
                  18.08.97 tt Ei koskaan lasketa korkoa laskutetulle korolle
                  23.10.97 pt rakenne miltei kokonaan uusiksi
                  06.11.97 pt accOinnin tarkistuksia
                  28.07.98 pt suoritus- ja kirjauspvm:n labelit
                  06.08.98 pt VALIDATE ALL acct numbers against AccNum PaymFile
                              removed default Bank acct #
                  08.10.98 pt print memo > F2
                  13.10.98 pt notes > F3
                  26.11.98 pt when exiting: clean up the Payment PaymFile from
                              those records where inv.no = 0 etc.
                  28.04.99 pt voluntary Interest payment does NOT affect
                              invoices 'total paid' nor customer's total debt
                  09.06.99 kl release nnohti 
                  18.11.99 pt xcustomer
                  20.01.00 pt AccType
                  24.01.00 jp cparams
                  28.01.00 jp Overpayments
                  03.02.00 jp DateLimit
                  06.03.00 jp Warning IF payment belongs in PP-Plan
                  06.04.00 pt RUN show-op
                  03.05.00 pt FIND invoice using VOUCHER no.
                  13.06.00 kl use invbal FOR existing payments
                  31.07.00 pt check IF AVAIL OPLog when deleting a payment
                  09.04.01/aam longer FORMAT FOR customer nbr -> name shorter
                  10.05.01 /pt get a/r AccNum no. from Invoice.ARAccNum
                  25.09.01/aam make the temporary vouchernbr FOR a NEW voucher
                               negative 
                  25.10.01/aam customer's Deposit sum was updated incorrectly 
                  21.11.01/aam smaller scope FOR TMSParam -> LOCK is released
                               after the payment is saved 
                  19.12.01/aam myoh-field was NOT shown correctly
                  25.01.02/aam check InterestPerm when calculating Interest
                  18.02.02/aam mark PaymSrc as "MP",
                               FOR old payments don't go directly into UPDATE
                               mode,
                               get the AccNum types before assigning
                               accounting sums,
                               don't UPDATE payment behaviour when changing
                               old payments
                  22.03.02/ht  Constant "MP" in the beginning of PaymSrc
                  10.04.02/ht  ImportStamp added
                  03.05.02/aam use PaymVouch (fvoucher.i),
                               FIND FOR OPLog used only EventType which is 
                               NOT indexed -> slow
                  05.06.02/aam use deposit[1] AND AdvPaym LIKE deposit[2],
                               VAT-handling FOR AdvPaym,
                               partly NEW logic,
                               cleanup FOR code etc.  
                  18.06.02 lp  when writing new Payment or updating old
                               always writing or updating memo too
                  29.07.02/aam change check for total paym versus open Balance 
                  26.08.02/aam VAT for AP had wrong sign
                  10.09.02/aam check period lock 
                  25.09.02/aam customer balances in table CustBal
                  14.09.02/aam show currency 
                  24.10.02/aam PaymentState 3 for credit loss
                  31.10.02/jr  Eventlog
                  12.11.02/jr  New Memos for Payment & Invoice
                  12.11.02/jr  Checks memo record instead memo extents
                  22.11.02/aam vat handling for cl-payment
                  07.01.03/jr  Removed f12 (local variable:s not datafield)
                  11.09.03/aam brand 
                  31.12.03/aam VatUsage
                  06.02.04 jp  custnumber for memos
                  12.03.04 aam check payment plan
                  23.03.04 aam update order status if deposit invoice
                  16.04.04 aam update payment plan
                  04.05.04/aam account with 6 digits
                  29.09.04/aam setoldbuffer for eventlog to correct place
                  11.10.04/aam allow dates prior to invoice date
                  04.11.04/aam disregard epayment when calculating due amt,
                               show warning if epayment active
                  31.12.04/aam handling of credit loss vat corrected           
                  12.01.05/aam CustNum to calcint.p
                  25.01.05/aam correct sum to fPaymPlanPaid
                  31.03.05/aam use fCredLossPaid for payment state
                  17.08.05/aam separate field for credit loss 
                  01.11.05/aam don't update CustBal.Interest from 'extra-amt'
                  29.09.06/aam vat from payment to adv.payment invoice,
                               fBalanceInvoicePaid()
                  12.01.07/aam CLI to fCustBal      
                  09.03.07/aam ExtVoucher
  Version ......: M15
  ------------------------------------------------------------------*/

{Syst/commali.i}
{Func/timestamp.i}
{Func/fvoucher.i}
{Func/fapvat.i}
{Func/faccper.i}
{Func/fcustbal.i}
{Syst/eventval.i} 
{Func/fclvat.i}
{Func/fpplan.i}
{Func/finvbal.i}
{Func/fpaymact.i}

DEF BUFFER bsuor     FOR Payment.

DEF VAR ok           as lo format "Yes/No" init TRUE NO-UNDO.
DEF VAR ldOldDue     as de format "zzzzzz9.99-"      NO-UNDO.
DEF VAR ldNewDue     AS DE                           NO-UNDO.
DEF VAR ldPaidAmt    LIKE ldOldDue                   NO-UNDO.
DEF VAR ldPayment    LIKE ldOldDue                   NO-UNDO.
DEF VAR ysuoyht      LIKE ldOldDue                   NO-UNDO.
DEF VAR suoryht      LIKE ldOldDue                   NO-UNDO.
DEF VAR ykorko       LIKE ldOldDue                   NO-UNDO.
DEF VAR lDisc        LIKE ldOldDue                   NO-UNDO.
DEF VAR dprop        LIKE ldOldDue                   NO-UNDO.
DEF VAR oprop        LIKE ldOldDue                   NO-UNDO.
DEF VAR aprop        LIKE ldOldDue                   NO-UNDO.
DEF VAR ldeBalProp   LIKE ldOldDue                   NO-UNDO. 
DEF VAR old_dprop    LIKE ldOldDue                   NO-UNDO.
DEF VAR old_oprop    LIKE ldOldDue                   NO-UNDO.
DEF VAR old_aprop    LIKE ldOldDue                   NO-UNDO.
DEF VAR myoh         as i  format "zzzz9"         NO-UNDO.
DEF VAR suopvm       as da format "99-99-99"      NO-UNDO.
DEF VAR deb          as de format "zzzzzzzz9.99-" NO-UNDO.
DEF VAR kre          as de format "zzzzzzzz9.99-" NO-UNDO.
DEF VAR ed-korko     AS DE                        NO-UNDO.
DEF VAR lastvou      as i  format "zzzzzzz9"      NO-UNDO.
DEF VAR kaccno       as i  format "zzzzzzzz"     EXTENT 10 NO-UNDO.
DEF VAR kaccmk       as de format "zzzzzz9.99-"  EXTENT 10 NO-UNDO.
DEF VAR kactype      AS i                        EXTENT 10 NO-UNDO.
DEF VAR AccNum       AS i                         NO-UNDO.
DEF VAR i            AS i                         NO-UNDO.
DEF VAR ii           AS i                         NO-UNDO.
DEF VAR pVouch       as c  format "x(8)"          NO-UNDO.
DEF VAR memory       AS RECID                     NO-UNDO.
DEF VAR visupvm      as da format "99-99-99"      NO-UNDO  init ?.
DEF VAR muispvm      as da format "99-99-99"      NO-UNDO.
DEF VAR kirjpvm      as da format "99-99-99"      NO-UNDO.
DEF VAR new_paym     AS lo                        NO-UNDO.
DEF VAR b-acc        AS lo                        NO-UNDO.
DEF VAR Interest     AS lo                        NO-UNDO.
DEF VAR extra-amt    AS DE                        NO-UNDO.
DEF VAR ac-ind       AS i                         NO-UNDO. 
DEF VAR opaym        as lo format "Y/N"                    init FALSE.
DEF VAR lisriv       AS lo                        NO-UNDO  init FALSE.
DEF VAR datelimit    AS DA                        NO-UNDO.
DEF VAR noovpm       AS lo                        NO-UNDO.
DEF VAR nopaym       AS lo                        NO-UNDO.
DEF VAR ftime        AS lo                        NO-UNDO  init TRUE.
DEF VAR op-tstamp    LIKE OPLog.CreStamp        NO-UNDO.
DEF VAR IntCalcMet   AS I                         NO-UNDO.

DEF VAR OverPayAcc   AS I                         NO-UNDO.
DEF VAR AdvPaymAcc   AS I                         NO-UNDO.
DEF VAR DepositAcc   AS I                         NO-UNDO. 
DEF VAR liCLossAcc   AS INT                       NO-UNDO. 
DEF VAR BankAcc      AS I                         NO-UNDO. 
DEF VAR OTIntAcc     AS I                         NO-UNDO.
DEF VAR ConseNo      AS I                         NO-UNDO.
DEF VAR OverTimeInt  AS DE                        NO-UNDO.
DEF VAR MinIntPay    AS DE                        NO-UNDO.
DEF VAR ReceivAcc    AS I                         NO-UNDO.
DEF VAR DiscAcc      AS I                         NO-UNDO.
DEF VAR qdays        AS C                         NO-UNDO.
DEF VAR qperc        AS C                         NO-UNDO.
DEF VAR InPerc       AS DE                        NO-UNDO.
DEF VAR DelDay       AS I                         NO-UNDO.
DEF VAR xDontUpd     AS LOGIC                     NO-UNDO.
DEF VAR lOrigin      AS C                         NO-UNDO.
DEF VAR lDefAcc      AS LOGIC                     NO-UNDO. 
DEF VAR lApVatAmt    AS DEC                       NO-UNDO. 

DEF VAR ldCustDP     AS DEC                       NO-UNDO.
DEF VAR ldCustAP     AS DEC                       NO-UNDO.
DEF VAR ldCustOP     AS DEC                       NO-UNDO. 
DEF VAR lOldPaym     AS DEC                       NO-UNDO.
DEF VAR lOldDepo     AS DEC                       NO-UNDO.
DEF VAR lOldOver     AS DEC                       NO-UNDO.
DEF VAR lOldAdv      AS DEC                       NO-UNDO.
DEF VAR lOldDisc     AS DEC                       NO-UNDO. 
DEF VAR lOldCLoss    AS DEC                       NO-UNDO. 
DEF VAR ldChkAr      AS DEC                       NO-UNDO. 
DEF VAR llPLock      AS LOG                       NO-UNDO. 
DEF VAR lcDueLabel   AS CHAR                      NO-UNDO. 
DEF VAR lcTotLabel   AS CHAR                      NO-UNDO. 
DEF VAR ldCredLoss   AS DEC                       NO-UNDO. 
DEF VAR new_custint  AS LOG                       NO-UNDO.
DEF VAR ldVatAmt     AS DEC                       NO-UNDO. 
DEF VAR ldCLossAmt   AS DEC                       NO-UNDO. 
DEF VAR ldOldVat     AS DEC                       NO-UNDO.
DEF VAR ldAPAmt      AS DEC                       NO-UNDO. 
DEF VAR lcPref       AS CHAR                      NO-UNDO.

/* Interests */
DEF NEW SHARED VAR intpro  AS DE EXTENT 10 NO-UNDO.
DEF NEW SHARED VAR intdays AS I  EXTENT 10 NO-UNDO.
DEF NEW SHARED VAR intsumm AS DE EXTENT 10 NO-UNDO.

IF llDoEvent THEN 
DO FOR Payment:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhPayment AS HANDLE NO-UNDO.
   lhPayment = BUFFER Payment:HANDLE.
   RUN StarEventInitialize(lhPayment).

   DEFINE VARIABLE lhCustIntEvent AS HANDLE NO-UNDO.
   lhCustIntEvent = BUFFER CustIntEvent:HANDLE.
   RUN StarEventInitialize(lhCustIntEvent).

END.

form
 Invoice.InvNum      label "InvNo" AT 2
 HELP "INVOICE NBR (+ENTER)   OR   VOUCHER NBR (+HOME)"

 Invoice.CustNum     label "Cust"
 Invoice.CustName    no-label format "x(26)"
 pVouch             label "Voucher"
 help "Voucher #, F9: Previous vouchers" SKIP
with title color value(ctc) " " + ynimi + " PAYMENTS TO INVOICES "
 + string(pvm,"99-99-99") + " " COLOR value(cfc) ROW 1 col 1
  width 80 side-labels FRAME INV-NO.

form
 Invoice.InvAmt     label "Billed Amount .."  at  2 format "zzzzzz9.99-"
 Invoice.InvDate    label "Invoice Date"      AT 32 SKIP
 ldPaidAmt          label "Already Paid ..."  AT  2
 visupvm label "Date of Paym"                   AT 32 SKIP
 "-----------"   at 20 
 Invoice.CashDisc     
    label "Cash Discnt." AT 32 SKIP
 
 lcDueLabel      AT 2 NO-LABEL FORMAT "X(17)"   
    ldOldDue     NO-LABEL
 Invoice.CashDiscDate       label "Dueday of CD"      AT 32 skip(1)

 ldPayment label "Cash Payment ..." 
    help "Amount paid as money"     
    AT 2 
 Invoice.DueDate       
    label "Dueday ....."  AT 32 SKIP

 aprop AT 2   
    label "From Adv.Payment"  
    help "Use customer's Advance Payment Balance as payment"
 suopvm 
    LABEL "Payment Day "
    help "Date when payment was made"
    SKIP

 oprop AT 2   
    label "From Overpayment"  
    help "Use customer's Overpayment Balance as payment"
 kirjpvm 
    LABEL "Booking Day " 
    help "Booking day (accounting day), day when payment was registered"
    SKIP

 dprop AT 2
    label "From Deposit ..."  
    help "Use customer's Deposit Balance as payment"
 lOrigin AT 32 
    LABEL "Origin ....."
    format "x(7)" 
    help "Source of payment, start with 'MP' if created here"    
    SKIP

 ldCLossAmt AT 2
    LABEL "Credit Loss ...."
    HELP  "Amount booked to credit loss"
    FORMAT "zzzzzz9.99-"

 lDisc           label "Discounts ......" AT 2
    help "Amount as discount (not money) " 
 "-----------"                            AT 20       
    myoh         label "Late Days " AT 32 
    SKIP
 lcTotLabel NO-LABEL AT 2 FORMAT "X(17)"    
    ysuoyht      NO-LABEL
    ykorko       label "Overt. Int" AT 32        
    SKIP(1)
with title color value(ctc) " PAYMENT "
 COLOR value(cfc) ROW 4 col 1 OVERLAY side-labels FRAME payment.

/* acctietoja varten */
form
 kaccno[ 1] AT 2 NO-LABEL space(2) kaccmk[ 1] NO-LABEL SKIP
 kaccno[ 2] AT 2 NO-LABEL space(2) kaccmk[ 2] NO-LABEL SKIP
 kaccno[ 3] AT 2 NO-LABEL space(2) kaccmk[ 3] NO-LABEL SKIP
 kaccno[ 4] AT 2 NO-LABEL space(2) kaccmk[ 4] NO-LABEL SKIP
 kaccno[ 5] AT 2 NO-LABEL space(2) kaccmk[ 5] NO-LABEL SKIP
 kaccno[ 6] AT 2 NO-LABEL space(2) kaccmk[ 6] NO-LABEL SKIP
 kaccno[ 7] AT 2 NO-LABEL space(2) kaccmk[ 7] NO-LABEL SKIP
 kaccno[ 8] AT 2 NO-LABEL space(2) kaccmk[ 8] NO-LABEL SKIP
 kaccno[ 9] AT 2 NO-LABEL space(2) kaccmk[ 9] NO-LABEL SKIP
 kaccno[10] AT 2 NO-LABEL space(2) kaccmk[10] NO-LABEL SKIP
 skip(1) "----------------------"                        SKIP
 deb at 2 label "Debet "                                 SKIP
 kre at 2 label "Credit"                                 SKIP
WITH
 title color value(ctc) " ACCOUNT       AMOUNT "
 COLOR value(cfc) ROW 4 col 57 side-labels OVERLAY FRAME acct.

FORM 
SKIP(1)
"   This customer has an unused Balance which"       SKIP
"   can be used as a payment to this invoice."       SKIP
"   Accept or change the suggested sum below:"       SKIP(1)

"   Unpaid Balance of invoice ..:" 
   ldOldDue    FORMAT "->,>>>,>>9.99" SKIP(1)

"   Advance payment Balance:" 
   ldCustAP    FORMAT "->,>>>,>>9.99" 
   "Use as payment: " 
      aprop FORMAT "->,>>>,>>9.99"
   SKIP
"   Overpayment Balance ...:" 
   ldCustOP FORMAT "->,>>>,>>9.99" 
   "Use as payment: " 
      oprop FORMAT "->,>>>,>>9.99"
   SKIP
"   Deposit Balance .......:"   
   ldCustDP FORMAT "->,>>>,>>9.99" 
   "Use as payment: " 
      dprop FORMAT "->,>>>,>>9.99"
   SKIP(1)

WITH title color value(ctc) " USE CUSTOMER'S UNBOOKED Balance ? "
           COLOR value(cfc) OVERLAY ROW 3 centered NO-LABELS FRAME overp.


FUNCTION fDefaultAcc RETURNS LOGICAL
    (iAmount AS DEC,
     iAccount AS INT).

     IF iAmount NE 0 THEN DO:
        ASSIGN 
        kaccmk[ii] = iAmount
        kaccno[ii] = iAccount
        ii          = ii + 1.
        RETURN TRUE.
     END. 

     ELSE RETURN FALSE. 

END FUNCTION. 

FUNCTION fCreateOptrans RETURNS LOGICAL
    (iType AS INT,
     iAmount AS DEC).

   CREATE OPLog.
   ASSIGN OPLog.CreStamp    = fMakeTS()
          OPLog.CustNum     = Invoice.CustNum
          OPLog.EventDate   = kirjpvm
          OPLog.UserCode    = katun
          OPLog.EventType   = iType
          OPLog.InvNum      = Invoice.InvNum
          OPLog.Voucher     = ConseNo
          OPLog.Amt         = iAmount.

END FUNCTION.

FUNCTION fCurrLabels RETURNS LOGICAL
   (iCurrency AS CHAR).

   ASSIGN lcDueLabel =  "Balance Due " +
                       (IF iCurrency = ""
                        THEN "...."
                        ELSE iCurrency + " ") +
                       ":"
          lcTotLabel = "TOTAL " +
                       (IF iCurrency = ""
                        THEN ".........."
                        ELSE iCurrency + " ......") +
                       ":".

   RETURN TRUE.                        

END FUNCTION.


DO:
    {Func/cparam.i OverPayAcc     return}. OverPayAcc  = TMSParam.IntVal.
    {Func/cparam.i AdvPaymAcc     return}. AdvPaymAcc  = TMSParam.IntVal.
    {Func/cparam.i ResDepositsAcc return}. DepositAcc  = TMSParam.IntVal.
    {Func/cparam.i CreditLossAcc  return}. liCLossAcc  = TMSParam.IntVal.
    {Func/cparam.i BankAcc        return}. BankAcc     = TMSParam.IntVal.
    {Func/cparam.i OTIntAcc       return}. OTIntAcc    = TMSParam.IntVal.
    {Func/cparam.i OverTimeInt    return}. OverTimeInt = TMSParam.DecVal.
    {Func/cparam.i MinIntPay      return}. MinIntPay   = TMSParam.DecVal.
    {Func/cparam.i ReceivAcc      return}. ReceivAcc   = TMSParam.IntVal.
    {Func/cparam.i DiscAcc        return}. DiscAcc     = TMSParam.IntVal.
    {Func/cparam.i DateLimit      return}. DateLimit   = TMSParam.DateVal.
    {Func/cparam.i IntCalcMet     return}. IntCalcMet  = TMSParam.IntVal.
END.


ASSIGN muispvm    = pvm.

fCurrLabels(""). 

assign cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME INV-NO.
view FRAME payment.
view FRAME acct.

LASKU:
repeat FOR Payment TRANSACTION ON ENDKEY UNDO LASKU, LEAVE LASKU:

   ehto = 9. RUN Syst/ufkey.
   PAUSE 0.
   CLEAR FRAME payment.
   CLEAR FRAME acct.
   CLEAR FRAME INV-NO.

   DISPLAY lcDueLabel lcTotLabel WITH FRAME payment.

   opaym = FALSE.

   IF lastvou ne 0 THEN message "Last voucher # used is" lastvou.

   /* INV.NO (enter) OR VOUCHER  NO: (home) */
   PROMPT-FOR Invoice.InvNum WITH FRAME INV-NO EDITING:
      READKEY.
      if LOOKUP(keylabel(lastkey),"HOME,H") > 0 THEN LEAVE.
      ELSE APPLY LASTKEY.
   END.


   IF input Invoice.InvNum = "" or input Invoice.InvNum = "0"
   THEN LEAVE LASKU.

   IF LOOKUP(KEYLABEL(Lastkey),"Home,H") > 0 THEN DO:
      FIND Payment WHERE Payment.Voucher = INPUT FRAME INV-NO Invoice.InvNum
      NO-LOCK NO-ERROR.
      IF NOT AVAIL Payment THEN DO:
         BELL.
         MESSAGE 
         "Voucher No." INPUT FRAME INV-NO Invoice.InvNum SKIP
         "does NOT exist on any payment !"
         VIEW-AS ALERT-BOX
         TITLE " INVALID VOUCHER NO. ".
         NEXT lasku.
      END.
      /* payment was found */
      DISP
      Payment.InvNum @ Invoice.InvNum WITH FRAME INV-NO.
      pVouch = STRING(Payment.Voucher).
   END.
   ELSE pvouch = "NEW".

   FIND Invoice where Invoice.InvNum = INPUT FRAME INV-NO Invoice.InvNum
        exclusive-lock no-error.

   IF NOT AVAILABLE Invoice OR 
      Invoice.Brand NE gcBrand 
   THEN DO:
      bell. message "Unknown Invoice Nbr !". pause 1 no-message.
      CLEAR FRAME payment no-pause. NEXT LASKU.
   END.

   /* involved in an ongoing payment plan */
   FOR EACH PPInv OF Invoice NO-LOCK,
      FIRST PaymPlan OF PPInv NO-LOCK WHERE
            PaymPlan.PPStatus < 4:
      ok = FALSE.      
      MESSAGE "Invoice is a part of an payment plan." SKIP
              "Continue with registering payment ?" 
      VIEW-AS ALERT-BOX
      QUESTION
      BUTTONS YES-NO
      SET ok.
      IF NOT ok THEN NEXT lasku.
   END.
    
   /* Lasku was found */
   ASSIGN 
      si-recid   = recid(Invoice)
      ldPayment  = 0 
      ldPaidAmt  = 0 
      suopvm     = muispvm 
      lDisc      = 0
      lOrigin    = ""
      visupvm    = ? 
      ykorko     = 0 
      myoh       = 0
      dprop      = 0
      oprop      = 0
      aprop      = 0
      old_dprop  = 0
      old_oprop  = 0
      old_aprop  = 0
      ldCLossAmt = 0
      ldOldVat   = 0.

   /* kerAtAAn aiemmat suoritukset jos on */
   visupvm = ?.

   ldOldDue = fCalcInvBal(BUFFER Invoice,
                          TODAY + 1000,
                          FALSE).  /* disregard epayment */

   FOR EACH Payment of Invoice no-lock:
       IF visupvm = ? THEN visupvm = Payment.AccDate.
       visupvm = maximum(visupvm,Payment.AccDate).
   END.

   FIND Customer where Customer.CustNum = Invoice.CustNum no-lock.

   ASSIGN ldPaidAmt = Invoice.InvAmt - ldOldDue.

   fCurrLabels(Invoice.Currency). 

   DISPLAY
      Invoice.CustNum
      Invoice.CustName
   WITH FRAME INV-NO.
   DISPLAY
      Invoice.InvAmt
      Invoice.InvDate
      ldPaidAmt
      visupvm
      Invoice.CashDisc
      lcDueLabel
      ldOldDue
      Invoice.CashDiscDate
      Invoice.DueDate
      lcTotLabel 
   WITH FRAME payment.

   ASSIGN ok = TRUE.
   PAUSE 0 no-message.
   message "Is this the invoice You mean (Y/N)?" UPDATE ok.

   IF NOT ok THEN UNDO LASKU, NEXT LASKU.

   IF Invoice.PaymState = 5 THEN DO:
       MESSAGE "There is a pending ePayment on invoice;" SKIP
               STRING(Invoice.ePaymDate,"99-99-9999") SKIP
               Invoice.ePaymAmt Invoice.Currency
       VIEW-AS ALERT-BOX
       TITLE " ePaid ".
   END.
    
   /* KysytAAn tositenumero  */
   UPDATE pVouch WITH FRAME INV-NO EDITING:
      READKEY. nap = keylabel(LASTKEY).
      /* HELP tositenumerolle */
      IF lookup(nap,"f9,!") > 0  THEN DO:
         RUN Ar/nnsuse(INPUT  Invoice.InvNum, OUTPUT i).
         IF i > 0 THEN disp i format "zzzzzzz9" @ pVouch WITH FRAME INV-NO.
         NEXT.
      END.
      ELSE APPLY LASTKEY.
   END.

   HIDE MESSAGE no-pause. 

   if pVouch = "" THEN UNDO LASKU, LEAVE LASKU.

   IF pVouch ne "NEW" THEN DO: /* VANHAN suorituksen change */
      new_paym = FALSE.

      FIND FIRST Payment where
                 Payment.Voucher = integer(pVouch)  AND
                 Payment.InvNum  = Invoice.InvNum
      exclusive-lock no-error.

      /* Set default sums AND values */
      ldPayment = 0.
      IF AVAILABLE Payment THEN DO:

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPayment).

         /* check period */
         llPLock = fPeriodLocked(Payment.AccDate,TRUE). 

         ASSIGN suopvm    = Payment.PaymDate
                lDisc     = Payment.Discount ysuoyht = Payment.TotAmt
                kirjpvm   = Payment.AccDate  deb = 0 kre = 0
                memory    = recid(Payment)
                lOrigin   = Payment.PaymSrc.

         DO ii = 1 TO 10:
            ASSIGN 
            kaccmk[ii]  = Payment.Posting[ii]
            kaccno[ii]  = Payment.AccNum[ii]
            kactype[ii] = Payment.AccType[ii].

            CASE kactype[ii]:
            /* overpayment */
            WHEN 6  THEN ASSIGN 
               old_oprop = old_oprop + kaccmk[ii].
            /* Deposit */
            WHEN 7  THEN ASSIGN
               old_dprop = old_dprop + kaccmk[ii].
            /* adv.payment */
            WHEN 19 THEN ASSIGN 
               old_aprop = old_aprop + kaccmk[ii]. 
            /* credit loss */
            WHEN 18 THEN ldCLossAmt = ldCLossAmt + kaccmk[ii].    
            /* vat */
            WHEN 5 THEN ldOldVat = ldOldVat + kaccmk[ii].
            END CASE.    

         END.

         /* credit loss includes vat */
         IF ldCLossAmt NE 0 THEN ldCLossAmt = ldCLossAmt + ldOldVat.
         
         ldPayment = Payment.PaymAmt - old_oprop - old_dprop - old_aprop -
                     ldCLossAmt.
                     
          /* lasketaan laskun aiemmat suoritukset uusiksi */
         ASSIGN visupvm  = ? ldPaidAmt = 0
                aprop    = old_aprop
                dprop    = old_dprop
                oprop    = old_oprop
                ldOldDue = Invoice.InvAmt. 

         FOR EACH Payment of Invoice where recid(Payment) NE memory
         no-lock:

            /* ONLY into A/R accounts */
            DO i = 1 TO 10:
               IF Payment.AccType[i] = 1 THEN 
               ldOldDue = ldOldDue + Payment.Posting[i].
            END.

            IF visupvm = ? THEN visupvm = Payment.PaymDate.
            visupvm = maximum(visupvm,Payment.PaymDate).
         END.

         ASSIGN ldPaidAmt = Invoice.InvAmt - ldOldDue.

         DISPLAY ldPaidAmt
                 visupvm
                 ldOldDue 
                 suopvm
                 kirjpvm
                 ldPayment
                 aprop
                 oprop
                 dprop 
                 ldCLossAmt
                 lDisc
                 lOrigin
                 ysuoyht
         WITH FRAME payment.

         FIND Payment where recid(Payment) = memory exclusive-lock.

         /* onko tAllA suorituksella myOs korkotapahtuma ? */
         FIND FIRST CustIntEvent where CustIntEvent.Voucher = Payment.Voucher
         exclusive-lock no-error.

         IF AVAIL CustIntEvent THEN DO:
            ASSIGN myoh       = CustIntEvent.LateDays
                   ykorko     = CustIntEvent.Amt.
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustIntEvent).
         END.
         ELSE ASSIGN myoh = 0 ykorko = 0.
         DISPLAY myoh ykorko WITH FRAME payment.

         /* lasketaan accOinnin debit ja kredit */
         DO i = 1 TO 10:
            IF Payment.Posting[i] < 0 THEN
               kre = kre + Payment.Posting[i].
            ELSE IF Payment.Posting[i] > 0 THEN
               deb = deb + Payment.Posting[i].
         END.
         kre = 0 - kre.
         DISPLAY kaccno kaccmk deb kre  WITH FRAME acct.

         IF CAN-FIND(FIRST memo WHERE
                           memo.Brand     = gcBrand   AND
                           memo.HostTable = "Payment" AND
                           memo.KeyValue  = STRING(Payment.Voucher) AND
                           memo.memotext NE "") THEN DO:
            MESSAGE 
            "There are hand-written notes on this payment record !"
            VIEW-AS ALERT-BOX.
         END.

         lDefAcc = FALSE. 

      END.  /* AVAILABLE Payment */

      ELSE DO:
         bell. message "No previous payments !". NEXT.
      END.
   END. /* VANHA Payment */

   ELSE DO:     /* UUSI Payment */
      ASSIGN new_paym  = TRUE
             lDefAcc   = TRUE
             llPLock   = FALSE
             lOrigin   = "MP".

      /* here we CREATE a NEW payment record AND set a temporary
         voucer no. that is made from the RECID */

      CREATE Payment. 
      ASSIGN Payment.Voucher  = fGetIntVoucher()
             Payment.Brand    = gcBrand 
             suopvm           = muispvm
             ldPayment        = ldOldDue
             kirjpvm          = muispvm.

      IF suopvm <= Invoice.CashDiscDate THEN DO: /* oikeus k-aleen */
         lDisc   = Invoice.CashDisc.
         IF lDisc < 0 THEN lDisc = 0.
         ldPayment = ldOldDue - Invoice.CashDisc.
         DISPLAY suopvm ldPayment lDisc WITH FRAME payment.
      END.

      ysuoyht = ldPayment + lDisc + dprop + oprop + aprop.
      DISP ysuoyht WITH FRAME payment.

   END.     /* NEW payment */

   /* Nyt ollaan valmiita kysymAAn suoritusta.  Jos valittiin aiempi
      Payment muutettavaksi, sen tiedot ovat nyt ruudulla.  */

   ASSIGN xDontUpd = NOT new_paym.

   /* only IF this is a NEW payment */
   IF new_paym = TRUE THEN DO:

      /* Does this customer have some unused balance */
      ASSIGN ldCustDP = fGetCustBal(Invoice.CustNum,"TOTAL","DP")
             ldCustOP = fGetCustBal(Invoice.CustNum,"TOTAL","OP")
             ldCustAP = fGetCustBal(Invoice.CustNum,"TOTAL","AP").

      IF ldCustOP NE 0 OR 
         ldCustDP NE 0 OR
         ldCustAP NE 0
      THEN DO:
         PAUSE 0.
         DISP  
         ldCustDP
         ldCustOP 
         ldCustAP 
         ldOldDue
         WITH FRAME overp.

         /* generate a proposed overpayment sum,
            Priority is adv.paym, overpayment, Deposit 
         */
         ASSIGN aprop      = MIN(ldCustAP,ldOldDue)
                ldeBalProp = ldOldDue - aprop
                oprop      = MIN(ldCustOP,ldeBalProp)
                ldeBalProp = ldeBalProp - dprop
                dprop      = MIN(ldCustDP,ldeBalProp). 

         repeat:

            UPDATE aprop oprop dprop
            WITH FRAME overp EDITING:

               READKEY. nap = keylabel(LASTKEY).

               IF lookup(nap,poisnap) > 0 THEN DO:
                  HIDE MESSAGE.
                  if frame-field = "aprop" THEN DO:
                     IF INPUT aprop > ldCustAP THEN DO:
                        message "Customer's advance payment Balance is less"
                                "than this intended payment sum."
                         VIEW-AS ALERT-BOX.
                         NEXT-PROMPT aprop.
                         NEXT. 
                     END. 
                  END. 
                  else if frame-field = "oprop" THEN DO:
                     IF INPUT oprop > ldCustOP THEN DO:
                         message "Customer's overpayment Balance is less"
                                 "than this intended payment sum."
                         VIEW-AS ALERT-BOX.
                         NEXT-PROMPT oprop.
                         NEXT. 
                     END. 
                  END. 
                  else if frame-field = "dprop" THEN DO:
                     IF INPUT dprop > ldCustDP THEN DO:
                         message "Customer's overpayment Balance is less"
                                 "than this intended payment sum."
                         VIEW-AS ALERT-BOX.
                         NEXT-PROMPT dprop.
                         NEXT. 
                     END. 
                  END. 

               END.
               APPLY LASTKEY.

            END. /* EDITING */

            IF dprop + oprop + aprop > ldOldDue THEN DO:
               message "Sum used from balances can not be bigger than"
                       "invoice's open Balance."
               VIEW-AS ALERT-BOX.
            END.

            ELSE LEAVE.

         END. /* repeat */

         /* adjust the default cash payment */
         ASSIGN ldPayment = ldOldDue - dprop - oprop - aprop.

         HIDE FRAME overp no-pause.
      END.

   END. /* new_paym = TRUE */

   ASSIGN opaym = (dprop + oprop + aprop <> 0).

   PaidAmt:
   repeat WITH FRAME payment:

      IF xDontUpd = FALSE THEN DO:
         ehto = 9. RUN Syst/ufkey.
         PAUSE 0.

         /* old values */
         ASSIGN lOldPaym  = ldPayment
                lOldDepo  = dprop
                lOldOver  = oprop
                lOldAdv   = aprop
                lOldDisc  = lDisc
                lOldCLoss = ldClossAmt.

         UPDATE   ldPayment 
                  aprop  when opaym
                  oprop  when opaym
                  dprop  WHEN opaym
                  ldCLossAmt
                  lDisc 
                  suopvm 
                  kirjpvm 
                  lOrigin
         WITH FRAME payment
         EDITING:
            READKEY. nap = keylabel(LASTKEY).

            IF lookup(nap,"F4") > 0 THEN DO:
               LEAVE PaidAmt.
            END. 

            IF lookup(nap,poisnap) > 0 THEN DO:
               HIDE MESSAGE.

               IF frame-field = "suopvm" THEN DO:

                  IF INPUT suopvm < Invoice.InvDate THEN DO:
                     BELL.
                     message "Note: Date is prior to invoice's Date !"
                     VIEW-AS ALERT-BOX.
                  END.

                  IF new_paym AND INPUT suopvm <= Invoice.CashDiscDate THEN DO:
                     lDisc    = Invoice.CashDisc.
                     IF lDisc < 0 THEN lDisc = 0.
                     ldPayment = ldOldDue - Invoice.CashDisc.
                     DISPLAY ldPayment lDisc WITH FRAME payment.
                  END.
               END.

               IF frame-field = "dprop" THEN DO:
                  IF INPUT FRAME payment dprop - old_dprop >
                     ldCustDP 
                  THEN DO:
                     MESSAGE
                     "   You can NOT book a bigger Deposit sum than " SKIP  
                     "   customer's total Deposit Balance !"
                     VIEW-AS ALERT-BOX error.
                     NEXT-PROMPT dprop. NEXT.
                  END. 
               END.    

               IF frame-field = "oprop" THEN DO:
                  IF INPUT FRAME payment oprop - old_oprop > 
                     ldCustOP
                  THEN DO:
                     MESSAGE
                     "   You can NOT book a bigger overpayment sum than " SKIP  
                     "   customer's total overpayment Balance !"
                     VIEW-AS ALERT-BOX error.
                     NEXT-PROMPT oprop. NEXT.
                  END. 
               END. 

               IF frame-field = "aprop" THEN DO:
                  IF INPUT FRAME payment aprop - old_aprop > 
                      ldCustAP 
                  THEN DO:
                     MESSAGE
                     "   You can NOT book a bigger advance payment sum than " 
                     SKIP  
                     "   customer's total advance payment Balance !"
                     VIEW-AS ALERT-BOX error.
                     NEXT-PROMPT aprop. NEXT.
                  END. 
               END. 

               ELSE IF frame-field = "Kirjpvm" THEN DO:
                  IF INPUT kirjpvm < Invoice.InvDate THEN DO:
                     BELL.
                     message "Note: Date is prior to invoice's Date !"
                     VIEW-AS ALERT-BOX.
                  END.
               END.

               ELSE IF FRAME-FIELD = "ldCLossAmt" THEN DO:
                  IF INPUT ldCLossAmt NE 0 AND 
                     INDEX(INPUT lOrigin,"CL") = 0 
                  THEN DO:
                     lOrigin = lOrigin + "CL".
                     DISPLAY lOrigin WITH FRAME payment.
                  END.
               END. 
               
               IF lookup(FRAME-FIELD,
                         "ldPayment,dprop,oprop,aprop,ldCLossAmt,lDisc") > 0
               THEN DO:

                  IF new_paym and FRAME-FIELD NE "lDisc" THEN DO:                                    lDisc = ldOldDue - 
                             INPUT FRAME payment ldPayment - 
                             INPUT FRAME payment dprop     - 
                             INPUT FRAME payment oprop     - 
                             INPUT FRAME payment aprop     - 
                             INPUT FRAME payment ldCLossAmt.
                     DISPLAY lDisc WITH FRAME payment.
                  END.

                  ysuoyht =
                  INPUT FRAME payment dprop +
                  INPUT FRAME payment oprop +
                  INPUT FRAME payment aprop +
                  INPUT FRAME payment ldCLossAmt + 
                  INPUT FRAME payment ldPayment +                 
                  INPUT FRAME payment lDisc .

                  DISP ysuoyht WITH FRAME payment.

                  IF ysuoyht = 0 THEN
                     MESSAGE
                     "  WARNING: Total payment = 0.00 ! "
                     VIEW-AS ALERT-BOX WARNING.
               END.
               
            END. /* poisnap INNER */
         
            APPLY LASTKEY.

         END.   /* EDITING INNER */

         /* myOhAstynyt Payment ? */
         IF INPUT suopvm - Invoice.DueDate > 0 AND
            ysuoyht NE 0 AND
            Invoice.InterestPerm = TRUE 
         THEN DO:
            RUN Ar/calcint(Invoice.DueDate,
                        input suopvm,
                        IntCalcMet,
                        ysuoyht,
                        Invoice.CustNum,
                        OUTPUT qdays,
                        output qperc,
                        output ykorko).

            ASSIGN 
            DelDay = INT(ENTRY(1,qdays))
            myoh   = DelDay
            InPerc = DECIMAL(ENTRY(1,qperc)) / 100.

            IF ykorko = ? THEN ykorko = 0.
            IF ykorko le MinIntPay THEN ykorko = 0.
         END.
         ELSE IF ysuoyht NE 0 THEN ASSIGN myoh = 0 ykorko = 0.

         IF ysuoyht NE 0 THEN 
         DISPLAY myoh ykorko ysuoyht WITH FRAME payment.

         /* amounts changed */
         IF lOldPaym  NE ldPayment OR
            lOldDepo  NE dprop     OR
            lOldOver  NE oprop     OR
            lOldAdv   NE aprop     OR
            lOldDisc  NE lDisc     OR
            lOldCLoss NE ldCLossAmt 
         THEN ASSIGN lDefAcc = TRUE. 

     END.  /* xdontupd = FALSE */

     assign muispvm  = suopvm. cfc = "sel".
     RUN Syst/ufcolor. ASSIGN ccc = cfc.

     ASSIGN lAPVatAmt = 0. 

     /* suggest postings */
     IF lDefAcc THEN DO:
        ASSIGN
        kaccno  = 0
        kaccmk  = 0
        kactype = 0
        ii      = 1
        lDefAcc = FALSE. 

        /* cash payment */
        fDefaultAcc(ldPayment,BankAcc).
        /* discount */
        fDefaultAcc(lDisc,DiscAcc).
        /* Deposit */
        fDefaultAcc(dprop,DepositAcc).
        /* overpayment */
        fDefaultAcc(oprop,OverPayAcc).
        /* adv.payment */
        fDefaultAcc(aprop,AdvPaymAcc). 

        ldAPAmt = aprop.
        
        /* is this an advance payment invoice */
        IF ldAPAmt = 0 THEN DO: 
           FIND Account WHERE 
                Account.Brand  = gcBrand AND
                Account.AccNum = Invoice.ARAccNum NO-LOCK NO-ERROR.
           IF AVAILABLE Account AND Account.AccType = 19 AND lAPVatAmt = 0
           THEN ldAPAmt = -1 * ysuoyht.
        END. 


        /* VAT from adv.payment */
        IF Invoice.VatUsage < 3 AND ldAPAmt NE 0 THEN DO:
           ASSIGN lAPVatAmt = -1 * fAPVatAmt(Customer.Region,ldAPAmt, kirjpvm). 
           IF lAPVatAmt NE 0 THEN DO:
              /* assisting AccNum FOR handling ap-vat */
              fDefaultAcc(lAPVatAmt,liAPVatAcc).
              /* actual VAT debt */
              fDefaultAcc(-1 * lAPVatAmt,liAPVatDebtAcc). 
           END.   
        END. 

        /* credit loss */
        IF ldCLossAmt NE 0 THEN DO:
   
           ldVatAmt = 0.
           /* VAT from credit loss */
           IF Invoice.VatUsage < 3 THEN 
           ldVatAmt = fCLVat(ldCLossAmt,
                             BUFFER Invoice,
                             OUTPUT AccNum).
 
           fDefaultAcc(ldCLossAmt - ldVatAmt,liCLossAcc).

           fDefaultAcc(ldVatAmt,AccNum). 
        END. 

        
        /* receive AccNum */
        fDefaultAcc(-1 * ysuoyht,Invoice.ARAccNum). 
        
     END.     

     acct1:  /* kysytAAn accOinti */
     repeat ON ENDKEY UNDO LASKU, NEXT LASKU:

       IF xDontUpd = FALSE THEN DO:

         /* KysellAAn lisAyksen acctietoja, kunnes oikein */
         ASSIGN deb = 0 kre = 0.
         DISPLAY deb kre WITH FRAME acct.
         UPDATE
            kaccno[1] kaccmk[1]
            kaccno[2] kaccmk[2]
            kaccno[3] kaccmk[3]
            kaccno[4] kaccmk[4]
            kaccno[5] kaccmk[5]
            kaccno[6] kaccmk[6]
            kaccno[7] kaccmk[7]
            kaccno[8] kaccmk[8]
            kaccno[9] kaccmk[9]
            kaccno[10] kaccmk[10]
         WITH FRAME acct EDITING:

            READKEY. nap = keylabel(LASTKEY).

            IF lookup(nap,poisnap) > 0 THEN DO: 
               PAUSE 0.
               HIDE MESSAGE.

               IF frame-field = "kaccno" THEN DO:
                  ac-ind = frame-index. 
                  AccNum = INPUT FRAME acct kaccno[ac-ind]. 
                  IF AccNum NE 0 THEN DO:
                     FIND Account where 
                         Account.Brand = gcBrand AND
                         Account.AccNum = AccNum no-lock no-error.
                     IF NOT AVAIL Account THEN DO:
                        bell. message "Unknown account !".
                        NEXT.
                     END.
                     message "AccNum: " AccNum Account.AccName.
                     kactype[ac-ind] = Account.AccType.
                  END.
                  ELSE DO:
                     IF INPUT kaccmk[frame-index] NE 0 THEN DO:
                        BELL.
                        message "Account Nbr MUST be given !".
                        NEXT.
                     END.
                  END.      
               END.

               ELSE IF frame-field = "kaccmk" THEN DO:
                  ASSIGN i = frame-index INPUT kaccno[i] INPUT kaccmk[i].

                  IF kaccno[i] = 0 AND kaccmk[i] = 0 THEN LEAVE.

                  IF (kaccmk[i] NE 0 AND kaccno[i] =  0) OR
                     (kaccmk[i]  = 0 AND kaccno[i] NE 0) THEN DO:
                     bell. message "Erroneus payment !".
                     NEXT-PROMPT kaccno[i].
                     NEXT.
                  END.
                  ASSIGN deb = 0 kre = 0.
                  DO ii = 1 TO 10:
                     IF kaccmk[ii] > 0 THEN ASSIGN deb = deb + kaccmk[ii].
                     ELSE                           kre = kre - kaccmk[ii].
                  END.
                  DISPLAY deb kre WITH FRAME acct.

               END. /* kaccmk */
            END.

            PAUSE 0.
            APPLY LASTKEY.

         END. /* EDITING */

       END.     /* xdontupd = FALSE */

       PAUSE 0.
       HIDE MESSAGE. 

       ASSIGN deb         = 0 
              kre         = 0
              ldChkAr     = 0
              ldCredLoss  = 0
              ldVatAmt    = 0
              kactype     = 0 
              ac-ind      = 0. 

       DO ii = 1 TO 10.
          IF kaccno[ii] NE 0 THEN DO:
             FIND Account where 
                Account.Brand  = gcBrand AND
                Account.AccNum = kaccno[ii]
                no-lock no-error.
             IF AVAILABLE Account THEN ASSIGN 
                kactype[ii] = Account.AccType.
          END.

          IF kaccmk[ii] > 0 THEN ASSIGN deb = deb + kaccmk[ii].
          ELSE                          kre = kre - kaccmk[ii].

          IF kactype[ii] = 1 THEN ASSIGN ldChkAr = ldChkAr - kaccmk[ii]. 

          /* credit loss */
          ELSE IF kactype[ii] = 18 THEN ASSIGN
             ldCredLoss = ldCredLoss + kaccmk[ii]
             ac-ind     = ii.
          /* possible vat amount */
          ELSE IF kactype[ii] = 5 THEN 
             ldVatAmt = ldVatAmt + kaccmk[ii].
       END.

       IF ldCredLoss NE 0 THEN ldCredLoss = ldCredLoss + ldVatAmt.
       
       /* vat from credit loss */
       IF ldCredLoss NE 0 AND xDontUpd = FALSE THEN DO:
              
          ldVatAmt = fCLVat(ldCredLoss,
                            BUFFER Invoice,
                            OUTPUT ii).
          
          IF ldVatAmt NE 0 THEN DO:
          
             /* is there an old vat line */
             do i = 1 to 10:
                if kaccno[i] = ii then do:
                   assign 
                   /* reduce credit loss posting with vat amount */
                   kaccmk[ac-ind] = kaccmk[ac-ind] + (kaccmk[i] - ldVatAmt)
                   kaccmk[i]      = ldVatAmt
                   ldVatAmt       = 0.
                   leave.
                end.
             end.

             /* if not then create a new vat line */
             if ldVatAmt NE 0 THEN DO:
                do i = 1 to 10:
                   if kaccno[i] = 0 AND kaccmk[i] = 0 then leave.
                end.
             
                ASSIGN 
                kaccno[i]      = ii
                kaccmk[i]      = ldVatAmt
                /* reduce credit loss posting with vat amount */
                kaccmk[ac-ind] = kaccmk[ac-ind] - kaccmk[i].
             end.
             
             DISPLAY  kaccmk[i] kaccno[i] kaccmk[ac-ind]
             WITH FRAME acct.
          END.                        
       END.

       DISP deb kre WITH FRAME acct.

       /* check that invoice is not overpaid */
       IF ABS(ldChkAr) > ABS(ldOldDue) THEN DO:
          ok = FALSE.
          MESSAGE "Invoice's open Balance is" ldOldDue 
                  "before saving this payment. With this payment postings"
                  "to a/r account exceed the open Balance."
                  "Ok to continue ?"
          VIEW-AS ALERT-BOX 
          QUESTION
          BUTTONS YES-NO
          SET ok.
          IF NOT ok THEN NEXT acct1. 
       END.

       ok = TRUE.
       IF ysuoyht > 0 AND ysuoyht + lAPVatAmt NE deb THEN ok = FALSE.
       IF ysuoyht < 0 AND ysuoyht - lAPVatAmt NE kre THEN ok = FALSE.


       IF NOT ok     OR
          deb <> kre
       THEN DO:

            COLOR DISPLAY messages deb kre WITH FRAME acct.
            COLOR DISPLAY messages ysuoyht WITH FRAME payment.
            BELL.
            MESSAGE 
            "Given sums by accounts don't match with invoice's total." 
            VIEW-AS ALERT-BOX
            warning. 
            COLOR DISPLAY normal deb kre WITH FRAME acct.
            COLOR DISPLAY normal ysuoyht WITH FRAME payment.
            IF deb NE kre THEN DO:
               NEXT acct1.
            END.
            ELSE LEAVE acct1.
       END.
       LEAVE acct1.

      END.   /* acct1 -repeat */

      ASSIGN xDontUpd = FALSE.

      toimi:
      repeat:

         extra-amt = 0.
         ftime = TRUE.

         IF new_paym THEN IF ysuoyht > ldOldDue  THEN DO:
            extra-amt = ldOldDue - ysuoyht. 
         END.

         ASSIGN  ufk = 0
         ufk[1] = 91 ufk[2] = 993 ufk[3] = 992 ufk[4] = 833
         ufk[5] = 832 ufk[6] = 12 ufk[7] = 4 ufk[8] = 8 ehto = 0.
         IF new_paym THEN ufk[7] = 0.
         IF ykorko = 0 OR Invoice.InvType = 3 OR Invoice.InvType = 4
         THEN ufk[4] = 0.

         /* period locked */
         IF llPLock THEN ASSIGN ufk[1] = 0
                                ufk[4] = 0
                                ufk[5] = 0
                                ufk[7] = 0.

         ehto = 0. RUN Syst/ufkey.p.

         IF toimi  = 1 THEN NEXT PaidAmt.              /* change */

         ELSE IF toimi = 2 THEN DO:                        /* print memo */
            RUN Mc/memo(INPUT Invoice.CustNum,
                     INPUT "Invoice",
                     INPUT STRING(INPUT Invoice.InvNum),
                     INPUT "Invoice").
            NEXT.
         END.

         ELSE IF toimi = 3 THEN DO:

            IF new_paym THEN DO:
               /* we have TO set some preliminary values TO following fields
                  because nnpmno.p uses AND displays them on screen */

               ASSIGN
               Payment.InvNum  = Invoice.InvNum
               Payment.CustNum = Invoice.CustNum
               Payment.AccDate = kirjpvm
               Payment.PaymDate = suopvm.
            END.

            RUN Mc/memo(INPUT Invoice.CustNum,
                     INPUT "Payment",
                     INPUT STRING(Payment.Voucher),
                     INPUT "Payment").
            NEXT.
         END.   

         ELSE IF toimi = 8 THEN UNDO LASKU, LEAVE LASKU.   /* Paluu */
         ELSE IF toimi = 6 THEN UNDO LASKU, NEXT  LASKU.   /* En talleta */

         ELSE IF toimi = 4 OR toimi = 5 THEN DO:

            /* check period */
            IF fPeriodLocked(kirjpvm,TRUE) THEN NEXT. 

            FIND InvGroup where 
               InvGroup.Brand    = gcBrand AND
               InvGroup.InvGroup = Customer.InvGroup 
               NO-LOCK.
            ASSIGN b-acc = InvGroup.UpdCustBal.

            /* latest payment date and payment behaviour */
            fPaymBehaviour(Invoice.CustNum,
                           "",
                           suopvm,
                           IF new_paym 
                           THEN Invoice.DueDate
                           ELSE ?). 

            IF new_paym THEN DO:
               /* Payment record is already created ... */
               /* Get the voucher no. */
               lastvou = Payment.Voucher.

               Payment.ImportStamp = fMakeTS().

               /* credit loss posted */
               IF ldCLossAmt > 0 THEN Payment.PaymType = 1.
               ELSE IF ldClossAmt < 0 THEN Payment.PaymType = 2.
               
                /* external voucher id */
               Payment.ExtVoucher = fGetAndUpdExtVoucher(Customer.InvGroup,
                                                         Payment.PaymType,
                                                         Payment.AccDate,
                                                         OUTPUT lcPref).
            END.

            /* ASSIGN the payment record (NEW OR old) */
            ASSIGN
            Payment.CustNum  = Invoice.CustNum
            Payment.CustName = Invoice.CustName
            Payment.InvNum   = Invoice.InvNum
            Payment.ExtInvId = Invoice.ExtInvID
            Payment.InvAmt   = Invoice.InvAmt
            Payment.PaymAmt  = ldPayment
            Payment.TotAmt   = ysuoyht
            Payment.Discount = lDisc
            Payment.InvDate  = Invoice.InvDate
            Payment.DueDate  = Invoice.DueDate
            Payment.PaymDate = suopvm
            Payment.AccDate  = kirjpvm.

            /* make sure that payments added here are marked properly */
            IF new_paym THEN DO:
               IF SUBSTRING(lOrigin,1,2) NE "MP"
               THEN lOrigin = "MP" + lOrigin.
            END.
            ELSE IF SUBSTRING(Payment.PaymSrc,1,2) = "MP" AND
               SUBSTRING(lOrigin,1,2) NE "MP"
            THEN lOrigin = "MP" + lOrigin.
            
            Payment.PaymSrc = lOrigin.
            
            /* IF overpayment Balance OR adv.payment Balance have been
               changed -> FIRST reduce the old amounts */
            IF old_oprop NE 0 THEN DO:
               /* RETURN of Overpayment */
               fCreateOptrans (7, old_oprop).
               fCustBal(Invoice.CustNum,
                        "",
                        "OP",
                        old_oprop). 
            END.
            IF old_aprop NE 0 THEN DO:
               /* RETURN of adv.payment */
               fCreateOptrans (12, old_aprop).
               fCustBal(Invoice.CustNum,
                        "",
                        "AP",
                        old_aprop). 
            END.
            IF old_dprop NE 0 THEN DO:
                /* Deposit */
               fCreateOptrans (14, old_dprop).
               fCustBal(Invoice.CustNum,
                        "",
                        "DP",
                        old_dprop). 
            END. 

            DO ii = 1 TO 10:

               /* get the AccNum types */
               ASSIGN kactype[ii] = 0.
               IF kaccno[ii] NE 0 THEN DO:
                  FIND Account where 
                     Account.Brand  = gcBrand AND
                     Account.AccNum = kaccno[ii]
                     no-lock no-error.
                  IF AVAILABLE Account THEN ASSIGN 
                     kactype[ii] = Account.AccType.
               END.

               /* IF overpayment OR adv.payment THEN make an OPLog record */
               IF kactype[ii] = 6 /* overpayment */ THEN DO:

                  fCreateOptrans(IF kaccmk[ii] > 0
                                 THEN 5       /* Overpayment used AS payment */
                                 ELSE 1,      /* Partial overpayment */
                                 -1 * kaccmk[ii]). 

                  fCustBal(Invoice.CustNum,
                           "",
                           "OP",
                           -1 * kaccmk[ii]). 

               END. 

               ELSE IF kactype[ii] = 19 /* adv.payment */ THEN DO:

                  fCreateOptrans(12,
                                 -1 * kaccmk[ii]). 

                  fCustBal(Invoice.CustNum,
                           "",
                           "AP",
                           -1 * kaccmk[ii]). 
               END. 

               ELSE IF kactype[ii] = 7 /* Deposit */ THEN DO:

                  fCreateOptrans(14,
                                 -1 * kaccmk[ii]). 

                  fCustBal(Invoice.CustNum,
                           "",
                           "DP",
                           -1 * kaccmk[ii]). 
               END.

               ASSIGN
                  Payment.AccNum[ii]   = kaccno[ii]
                  Payment.Posting[ii]   = kaccmk[ii]
                  Payment.AccType[ii]  = kactype[ii].
            END.

            IF toimi = 4 /* korkotapahtuma */ THEN DO:
               FIND FIRST CustIntEvent where 
                          CustIntEvent.Voucher = Payment.Voucher
               exclusive-lock no-error.
               IF AVAIL CustIntEvent THEN DO:
                  ed-korko = CustIntEvent.Amt.
                  IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustIntEvent).
                  new_custint = FALSE.
               END.   
               ELSE DO:
                  new_custint = TRUE.
                  /* Luodaan korkotietue */
                  CREATE CustIntEvent.

                  ASSIGN
                  CustIntEvent.Brand   = Payment.Brand
                  CustIntEvent.Voucher = Payment.Voucher
                  CustIntEvent.InvNum  = Invoice.InvNum
                  CustIntEvent.CustNum = Invoice.CustNum
                  CustIntEvent.InvDate = Invoice.InvDate
                  CustIntEvent.DueDate = Invoice.DueDate
                  ed-korko     = 0.

               END.

               ASSIGN
               CustIntEvent.PaymDate = INPUT suopvm
               CustIntEvent.LateDays = DelDay
               CustIntEvent.Amt  = ykorko /* IntAmt */
               CustIntEvent.InvAmt  = Invoice.InvAmt
               CustIntEvent.PaidAmt  = ysuoyht
               CustIntEvent.Percent = InPerc.

               /* Interest arrays */

               DO i = 1 TO 10:
                  ASSIGN
                  CustIntEvent.IntPerc[i]   = intpro[i]
                  CustIntEvent.IntDays[i]   = intdays[i]
                  CustIntEvent.IntAmt[i] = intsumm[i].
               END.

               IF new_custint AND llDoEvent 
               THEN RUN StarEventMakeCreateEvent(lhCustIntEvent).

               IF NOT new_custint AND llDoEvent 
               THEN RUN StarEventMakeModifyEvent(lhCustIntEvent).

               /* interest balance */
               IF b-acc THEN 
               fCustBal(Invoice.CustNum,
                        "",
                        "INT",
                        (ykorko - ed-korko)).
            END.

            IF toimi = 5 THEN DO:
               /* ei haluttu korkoa, mutta onko vanha CustIntEvent olemassa
                  jos nyt muutettiin vanhaa suoritusta ? */
               FIND FIRST CustIntEvent WHERE
                          CustIntEvent.Voucher = Payment.Voucher
               exclusive-lock no-error.
               IF AVAIL CustIntEvent AND b-acc THEN DO:

                  fCustBal(Invoice.CustNum,
                           "",
                           "INT",
                           -1 * CustIntEvent.Amt). 
                  IF llDoEvent 
                  THEN RUN StarEventMakeDeleteEvent(lhCustIntEvent).
                  DELETE CustIntEvent.
               END.
            END.

            IF new_paym AND llDoEvent THEN
            RUN StarEventMakeCreateEvent(lhPayment).

            IF NOT new_paym AND llDoEvent THEN
            RUN StarEventMakeModifyEvent(lhPayment).

         END.  /* toimi = 4/5 */

         ELSE IF toimi = 7 THEN DO:   /* removal */

            /* check period */
            IF fPeriodLocked(kirjpvm,TRUE) THEN NEXT. 

            ok = FALSE.
            message "Do You really want to erase this payment (Y/N) ?" 
            UPDATE ok.
            IF NOT ok THEN NEXT TOIMI.    

            FIND InvGroup where 
               InvGroup.Brand = gcBrand AND
               InvGroup.InvGroup = Customer.InvGroup 
               NO-LOCK.
            ASSIGN b-acc = InvGroup.UpdCustBal.

            /* haetaan vanha Payment uudelleen memoryin siltA varalta
               ettA kjA olisi jo muuttanut sitA */
            FIND Payment where recid(Payment) = memory exclusive-lock.

            /* poistetaan myOs CustIntEvent */
            FIND FIRST CustIntEvent where
                       CustIntEvent.Voucher = Payment.Voucher
            exclusive-lock no-error.
            IF AVAIL CustIntEvent THEN DO:
               ed-korko = CustIntEvent.Amt.
               IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCustIntEvent).
               DELETE CustIntEvent.
            END.
            ELSE ed-korko = 0.

            /* overpayment */
            IF old_oprop NE 0 THEN DO:
                fCreateOptrans(8,old_oprop).

                fCustBal(Invoice.CustNum,
                         "",
                         "OP",
                         old_oprop). 
            END.   
            /* advance payment */
            IF old_aprop NE 0 THEN DO:
               fCreateOptrans(15,old_aprop).

               fCustBal(Invoice.CustNum,
                        "",
                        "AP",
                        old_aprop). 
            END.
            /* Deposit */
            IF old_dprop NE 0 THEN DO:
               fCreateOptrans(16,old_dprop). 

               fCustBal(Invoice.CustNum,
                        "",
                        "DP",
                        old_dprop). 
            END. 

            /* balances */
            IF b-acc THEN DO:

                /* interest debt */
                fCustBal(Invoice.CustNum,
                         "",
                         "INT",
                         -1 * ed-korko). 

            END.

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPayment).
            DELETE Payment.

         END.

         IF toimi = 4 OR toimi = 5 OR toimi = 7 THEN DO:
            /* pAivitetAAn laskutietue */

            /* Lasketaan suoritusten yhteismAArA */
            ASSIGN Invoice.PaymDate = ?.

            ldNewDue = fCalcInvBal(BUFFER Invoice,
                                   TODAY + 1000,
                                   FALSE).  /* disregard epayment */
                                   
            Invoice.PaidAmt = Invoice.InvAmt - ldNewDue. 

            IF b-acc THEN DO:
               fCustBal(Invoice.CustNum,
                        "",
                        "ARBAL",
                        -1 * (ldOldDue - ldNewDue)). 
            END.

            FOR EACH Payment of Invoice no-lock.
               IF Invoice.PaymDate = ? THEN Invoice.PaymDate = Payment.PaymDate.
               Invoice.PaymDate = maximum(Invoice.PaymDate,Payment.PaymDate).
            END.

            /* credit loss posted */
            ldCredLoss = fCredLossPaid(BUFFER Invoice, 
                                       TODAY,
                                       OUTPUT i).
                                       
            IF ldCredLoss NE 0 THEN Invoice.PaymState = 3.
            ELSE DO:
               IF Invoice.PaidAmt = Invoice.InvAmt THEN Invoice.PaymState = 2. 

               /* don't change payment state if invoice is in a paymplan */
               ELSE IF Invoice.PaymState NE 4 THEN DO:
                  IF Invoice.PaidAmt = 0 
                  THEN Invoice.PaymState = 0. 
                  ELSE Invoice.PaymState = 1.
               END.    
            END.

            /* update possible payment plan */
            fPaymPlanPaid(Invoice.CustNum,
                          Invoice.InvNum,
                          ldOldDue - ldNewDue).
 
            /* check if deposit invoice is created from order or from
               omsu request */
            IF Invoice.InvType = 3 OR Invoice.InvType = 4 THEN DO:
               fBalanceInvoicePaid(Invoice.InvNum,
                                   Invoice.PaymState).
            END.                       

            RUN show-op.

            NEXT LASKU.
         END.
      END. /* toimi */
   END. /* PaidAmt */
END. /* LASKU */


HIDE FRAME acct no-pause.
HIDE FRAME payment no-pause.
HIDE FRAME INV-NO no-pause.


PROCEDURE show-op:

   ASSIGN ldCustDP = fGetCustBal(Invoice.CustNum,"TOTAL","DP")
          ldCustOP = fGetCustBal(Invoice.CustNum,"TOTAL","OP")
          ldCustAP = fGetCustBal(Invoice.CustNum,"TOTAL","AP"). 

   IF dprop NE 0 OR
      oprop NE 0 OR
      aprop NE 0 
   THEN  MESSAGE 
   "After this booking this customer has an" SKIP
   "- Adv.payment Balance of " string(ldCustAP,"->,>>>,>>9.99") SKIP
   "- Overpayment Balance of " string(ldCustOP,"->,>>>,>>9.99") SKIP
   "- Deposit Balance of     " string(ldCustDP,"->,>>>,>>9.99") SKIP
   VIEW-AS ALERT-BOX INFORMATION
   TITLE " BALANCES ". 

END PROCEDURE.   

