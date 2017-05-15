{Syst/testpaa.i}
{Func/ftaxdata.i}
{Func/fvoucher.i}

def var ldtaxperc  as dec  no-undo.
def var i          as int  no-undo.
def var j          as int  no-undo.
def var ldvat      as dec  no-undo.
def var lcpref     as char no-undo.
def var lcinvgroup as char no-undo.

def buffer bpaym for payment.
def buffer breq  for prepaidrequest.

def stream slog.
output stream slog to /apps/tms/Work/topup_comp_tax.log append.

for each payment no-lock where
         payment.brand = "1" and
         payment.paymtype = 7,
   first prepaidrequest no-lock use-index pprequest where
         prepaidrequest.brand = "1" and
         prepaidrequest.pprequest = integer(payment.refnum) and
         prepaidrequest.source   = "cc" and
         prepaidrequest.topupamt > 0 and
         prepaidrequest.vatamt = 0 and
         prepaidrequest.tsrequest < 20070401:

   i = i + 1.

   FIND MobSub WHERE MobSub.CLI = prepaidrequest.CLI NO-LOCK NO-ERROR.
   IF AVAILABLE MobSub THEN 
      FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK NO-ERROR.
   ELSE DO:
      FIND FIRST MsOwner NO-LOCK WHERE
                 MsOwner.Brand = "1" AND
                 MsOwner.CLI   = prepaidrequest.CLI NO-ERROR.
      IF AVAILABLE MsOwner THEN 
         FIND Customer WHERE Customer.CustNum = MsOwner.InvCust 
             NO-LOCK NO-ERROR.
      ELSE do:
         message "ERROR:Unknown CLI" prepaidrequest.cli
                 "req." prepaidrequest.pprequest
         view-as alert-box.
         next.
      end.
   END.

   IF NOT AVAILABLE Customer THEN do:
      message "ERROR:Unknown customer, req." prepaidrequest.pprequest
      view-as alert-box.
      next.
   end.
   
   /* tax% through customer's region */
   ldTaxPerc = fRegionTaxPerc(Customer.Region,"1"). 

   ldVat = round((prepaidrequest.topupamt / 100) * ldTaxPerc / 100,2).

   if ldVat = 0 then next.

   disp payment.accdate
        payment.paymamt
        payment.extvoucher
        prepaidrequest.request
        prepaidrequest.topupamt 
        prepaidrequest.vatamt 
        prepaidrequest.respcode 
        prepaidrequest.tsrequest
        skip
        payment.posting[1 for 3] skip
        payment.accnum[1 for 3] format ">>>>>>>9" skip
        ldtaxperc format "->>>9.99"
        ldvat format "->>>>>>9.99" skip(1).
   next.   

   find breq where recid(breq) = recid(prepaidrequest) exclusive-lock.
   assign breq.vatamt = ldvat * 100
          /*
          breq.source = "CCX"
          */.

   find bpaym where recid(bpaym) = recid(payment) exclusive-lock.
   assign bpaym.posting[1] = bpaym.posting[1] + ldvat
          bpaym.posting[3] = -1 * ldvat.

   lcInvGroup = Customer.InvGroup.
   
   /* get correct invgroup through vatpercent */
   FOR FIRST VatCode NO-LOCK WHERE
             VatCode.TaxClass = "1" AND
             VatCode.VatPerc  = ldTaxPerc,
       FIRST InvGroup NO-LOCK WHERE
             InvGroup.Brand   = "1" AND
             InvGroup.TaxZone = VatCode.TaxZone:
      lcInvGroup = InvGroup.InvGroup.
   END.
                                               
   /* external voucher id */
   Payment.ExtVoucher = fGetAndUpdExtVoucher(lcInvGroup,
                                             Payment.PaymType,
                                             Payment.AccDate,
                                             OUTPUT lcPref).
                                               
   put stream slog unformatted
      prepaidrequest.pprequest chr(9)
      prepaidrequest.vatamt    chr(9)
      payment.voucher          skip.
   
   j = j + 1.
   pause 0.
   disp i j with 1 down.
   
end.
