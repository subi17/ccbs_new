{date.i}
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 

def stream sout.
output stream sout to /apps/yoigo/tms_support/billing/log/check_fusion_invoices.txt.

DEFINE VARIABLE llFound AS LOGICAL NO-UNDO. 
DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 

ldeNow = fMakeTS().

FOR EACH invoice NO-LOCK where
         invoice.brand = "1" and
         invoice.invdate eq 12/1/2013 and
         invoice.invtype = 1:

   if invoice.chgstamp >= ldeNow then next.

   FOR EACH subinvoice of invoice NO-LOCK:
      
      llFound = false.
      FOR EACH invrow of invoice NO-LOCK where
               invrow.subinvnum = subinvoice.subinvnum:

         if invrow.billcode begins "contsf" or
            invrow.billcode begins "contff" then do:
            llFound = true.
            leave.
         end.
      end.
            
      if llFound and (invoice.deltype ne 3 and
         invoice.deltype ne 13) then  do:
            put stream sout unformatted 
               invoice.invnum "|" 
               invoice.extinvid "|" 
               invoice.custnum "|"
               invoice.deltype "|"
               subinvoice.msseq "|"
               subinvoice.cli "|"
               "ERROR: Fusion subscription invoice has a wrong delivery type" skip.
         j = j + 1.
      end.

      if (invoice.deltype eq 3 or
         invoice.deltype eq 13) and llfound eq false then do:
         put stream sout unformatted 
            invoice.invnum "|" 
            invoice.extinvid "|" 
            invoice.custnum "|"
            invoice.deltype "|"
            subinvoice.msseq "|"
            subinvoice.cli "|"
            "ERROR: Fusion invoice subinvoice does not contain fusion monthly fee" skip.
          j = j + 1.
      end.
   end.

   i = i + 1.
   if i mod 100 = 0 then do:
      disp i label "invoices" j label "errors" with frame a.
      pause 0.
   end.
end.

