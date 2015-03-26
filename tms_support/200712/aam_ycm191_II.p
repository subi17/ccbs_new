{testpaa.i}
{cparam.i2}
{timestamp.i}
{fvoucher.i}
{eventval.i}
{fcustbal.i}

DEF VAR liCount         AS INT  NO-UNDO.
DEF VAR liVouch         AS INT  NO-UNDO.
DEF VAR liBankAcc       AS INT  NO-UNDO.
DEF VAR liVatAcc        AS INT  NO-UNDO.
DEF VAR liTopUpAcc      AS INT  NO-UNDO.
DEF VAR liExtraAcc      AS INT  NO-UNDO.
DEF VAR liAdvAcc        AS INT  NO-UNDO. 
DEF VAR lcMemo          AS CHAR NO-UNDO. 
DEF VAR liAction        AS INT  NO-UNDO.  /* 1=add,2=reduce (cancel) */
DEF VAR lcPref          AS CHAR NO-UNDO. 
DEF VAR ldVatPerc       AS DEC  NO-UNDO. 
DEF VAR lcSource        AS CHAR NO-UNDO.
DEF VAR lcExtVoucher    AS CHAR NO-UNDO.
DEF VAR lrRecid         AS RECID NO-UNDO.
DEF VAR ldAmt           AS DEC  NO-UNDO.
DEF VAR lcName          AS CHAR NO-UNDO.

def var i as int no-undo.
def var j as int no-undo.

assign  
      liBankAcc   = 33331111
      liTopUpAcc  = 33332222.

for each order no-lock where
         order.brand = "1" and
         order.source = "script" and
         order.clitype = "tarj3",
   first prepaidrequest no-lock where
         prepaidrequest.brand = "1" and
         prepaidrequest.msseq = order.msseq and
         prepaidrequest.source = "web order":

   i = i + 1.
          
   if i mod 100 = 0 then do:
      pause 0.
      disp i j with 1 down.
   
      if i mod 5000 = 0 then do:
         pause 30.
      end.
   end.
      
   if can-find(first payment where
                     payment.brand = "1" and
                     payment.refnum = string(prepaidrequest.pprequest))
   then next.

   FIND Customer WHERE Customer.CustNum = order.CustNum NO-LOCK NO-ERROR.

   IF NOT AVAILABLE Customer THEN next.

   lcMemo = "Preactivated subscription: " + Order.CLI + 
            ", pp-request " + STRING(PrePaidRequest.PPRequest).

   /* Get the voucher no. */
   liVouch = fGetAndUpdVoucher(gcBrand,2).

   ldAmt = PrePaidRequest.TopUpAmt / 100.
   
   lcName = Customer.FirstName + " " + 
            Customer.CustName + " " + 
            Customer.SurName2.
   
   if lcName = "" then lcName = Customer.CompanyName.         

   
   CREATE Payment.
   ASSIGN 
       Payment.Brand    = gcBrand
       Payment.Voucher  = liVouch
       Payment.CustNum  = Customer.CustNum
       Payment.CustName = lcName
       Payment.InvNum   = 0
       Payment.InvAmt   = 0
       Payment.PaymAmt  = ldAmt
       Payment.TotAmt   = ldAmt
       Payment.Discount = 0
       Payment.InvDate  = ?
       Payment.DueDate  = ?
       Payment.PaymDate = 12/31/7
       Payment.AccDate  = 12/31/7
       Payment.PaymSrc  = "IT"
       Payment.PaymType = 9 
       Payment.RefNum   = STRING(PrePaidRequest.PPRequest)

       Payment.AccNum[1]  = liBankAcc
       Payment.Posting[1] = ldAmt
       Payment.AccType[1] = 13
    
       Payment.AccNum[2]  = liTopUpAcc
       Payment.Posting[2] = -1 * ldAmt
       Payment.AccType[2] = 20.
       
   /* time when posted */
   Payment.ImportStamp = fMakeTS().

   IF lcMemo > "" THEN DO:
   
      /* separate Memo */
      CREATE Memo.
      ASSIGN 
          Memo.Brand     = gcBrand
          Memo.HostTable = "payment"
          Memo.KeyValue  = STRING(Payment.Voucher)
          Memo.CustNum   = Payment.CustNum
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = katun 
          Memo.MemoTitle = "INITIAL TOPUP"
          Memo.MemoText  = lcMemo.
          Memo.CreStamp  = fMakeTS().
   
   END.

   lrRecid = RECID(Payment).

   REPEAT:
      /* external voucher id */
      Payment.ExtVoucher = fGetAndUpdExtVoucher(Customer.InvGroup,
                                                Payment.PaymType,
                                                Payment.AccDate,
                                                OUTPUT lcPref).

      FIND Payment WHERE RECID(Payment) = lrRecid EXCLUSIVE-LOCK.
   
      lcExtVoucher = Payment.ExtVoucher.
   
      IF NOT CAN-FIND(FIRST Payment WHERE
                            Payment.Brand      = gcBrand AND
                            Payment.ExtVoucher = lcExtVoucher AND
                            RECID(Payment) NE lrRecid)
      THEN LEAVE.
   END.
 
   j = j + 1. 
end.

disp i j.



