DEFINE VARIABLE i AS INTEGER NO-UNDO. 
def stream sout.
output stream sout to check_sabadell_payterm.log.

DEFINE VARIABLE j AS INTEGER NO-UNDO. 
DEFINE VARIABLE k AS INTEGER NO-UNDO. 
DEFINE VARIABLE l AS INTEGER NO-UNDO. 

FOR EACH invoice NO-LOCK where
         invoice.brand = "1" and
         invoice.invtype = 1 and
         invoice.invdate = 7/1/2014:

   i = i + 1.
   if i mod 100 = 0 THEN DO:
      disp i k with frame a.
      pause 0.
   END.

   for each invrow NO-LOCK where
            invrow.invnum = invoice.invnum and
          lookup(invrow.billcode,"PAYTERMBS,PAYTERMENDBS,RVTERMBSF") > 0:
      
      FIND FIRST subinvoice NO-LOCK where
                 subinvoice.invnum = invoice.invnum and
                 subinvoice.subinvnum = invrow.subinvnum no-error.

      put stream sout unformatted subinvoice.invnum ";" invoice.custnum ";" 
         subinvoice.msseq ";" invrow.billcode skip.
      k = k + 1.
   end.

end.
