{testpaa.i}
{fvoucher.i}
{timestamp.i}

def var ldtaxperc  as dec  no-undo.
def var i          as int  no-undo.
def var j          as int  no-undo.
def var ldvat      as dec  no-undo.
def var lcpref     as char no-undo.
def var lcinvgroup as char no-undo.


def stream slog.
output stream slog to /apps/tms/Work/topup_paym_canc.log append.

put stream slog unformatted
   "Request"  chr(9)
   "Msisdn"   chr(9)
   "Type"     chr(9)
   "Response" chr(9)
   "Orig. Voucher"  chr(9)
   "Date"        chr(9)
   "Amount"      chr(9)
   "New Voucher" skip.

def buffer bpaym for payment.

for each payment no-lock use-index paymtype where
         payment.brand = "1" and
         payment.paymtype = 7 and
         payment.paymarc = "",
   first prepaidrequest no-lock use-index pprequest where
         prepaidrequest.brand = "1" and
         prepaidrequest.pprequest = integer(payment.refnum):

   if prepaidrequest.respcode = 0 then next.

   if can-find(first bpaym use-index paymarc where
                     bpaym.brand   = "1" and
                     bpaym.paymarc = payment.extvoucher and
                     bpaym.paymtype = payment.paymtype)
   then next. 

   
   find customer where customer.custnum = payment.custnum no-lock.

   disp payment.accdate
        payment.paymamt
        payment.extvoucher
        prepaidrequest.request
        prepaidrequest.topupamt 
        prepaidrequest.pprequest
        prepaidrequest.cli format "x(13)"
        skip
        prepaidrequest.vatamt 
        prepaidrequest.source
        prepaidrequest.respcode 
        skip
        payment.posting[1 for 3] skip
        payment.accnum[1 for 3] format ">>>>>>>9" 
        skip(1).
   next.

   ASSIGN lcInvGroup = Customer.InvGroup
          ldTaxPerc  = INTEGER(100 * prepaidrequest.vatamt / 
                                      prepaidrequest.topupamt).
        
   /* get correct invgroup through vatpercent */
   FOR FIRST VatCode NO-LOCK WHERE
             VatCode.TaxClass = "1" AND
             VatCode.VatPerc  = ldTaxPerc,
       FIRST InvGroup NO-LOCK WHERE
             InvGroup.Brand   = "1" AND
             InvGroup.TaxZone = VatCode.TaxZone:
      lcInvGroup = InvGroup.InvGroup.          
   END.
       
          
   create bpaym.
   bpaym.voucher = fGetAndUpdVoucher(gcbrand,2).

   buffer-copy payment except voucher extvoucher to bpaym.

   bpaym.accdate = today.
   bpaym.paymarc = payment.extvoucher.
   
   bPaym.ExtVoucher = fGetAndUpdExtVoucher(lcInvGroup,
                                           bPaym.PaymType,
                                           bpaym.accdate,
                                           OUTPUT lcPref).

   do j = 1 to 6:
      bpaym.posting[j] = bpaym.posting[j] * -1.
   end.

   put stream slog unformatted
      prepaidrequest.pprequest   chr(9)
      prepaidrequest.cli         chr(9)
      prepaidrequest.request     chr(9)
      prepaidrequest.respcode    chr(9)
      payment.Extvoucher         chr(9)
      payment.accdate            chr(9)
      payment.paymamt            chr(9)
      bpaym.extvoucher           skip.

   CREATE Memo.
   ASSIGN Memo.Brand     = gcBrand
          Memo.HostTable = "payment"
          Memo.KeyValue  = STRING(bPaym.Voucher)
          Memo.CustNum   = Payment.CustNum
          Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
          Memo.CreUser   = "snet" 
          Memo.MemoTitle = "TopUp Payment"
          Memo.MemoText  = "Cancellation for TopUp posting due to " +
                           "unsuccessful TopUp event." + chr(10) +
                           "Cancelled voucher: " + payment.extvoucher.
          Memo.CreStamp  = fMakeTS().
    
end.
   

 