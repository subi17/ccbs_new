{commali.i}
{finvbal.i}

def var ldbal as dec no-undo.
def var i     as int no-undo.
def var llok  as log no-undo.

def var ldtfrom as date no-undo.
def var ldtto   as date no-undo.

ldtfrom = date(month(today),1,year(today)).
if month(today) = 12
then ldtto = date(12,31,year(today)).
else ldtto = date(month(today) + 1,1,year(today)) - 1.

pause 0.
update ldtfrom format "99-99-99" label "From Date" skip
       ldtto   format "99-99-99" label "To Date ."
       with side-labels overlay row 8 centered 
            title " Unprinted Invoices " frame funprint.

llok = true.
message "Start listing unprinted invoices ?"            
view-as alert-box question
buttons yes-no
set llok.

if not llok then do:
   hide frame funprint no-pause.
   return.
end.   

def stream slog.
output stream slog to value("/tmp/TF_unprinted_inv" +
                            string(ldtto,"999999") + ".txt").

put stream slog unformatted
   "Unprinted invoice from " 
       string(ldtfrom,"99.99.99") "-"        
       string(ldtto,"99.99.99") skip
   "Customer"        chr(9)
   "Invoice"         chr(9)
   "InvDate"         chr(9)
   "Printing Denied" chr(9)
   "Amount"          chr(9)
   "Debt"            skip.

for each invoice no-lock use-index invdate where
         invoice.brand   = "1"   and
         invoice.invtype ne 3    and
         invoice.invtype ne 4    and
         invoice.printstate  = 0 and
         invoice.crinvnum    = 0 and
         invoice.invdate    >= ldtfrom and
         invoice.invdate    <= ldtto:

   ldbal = fCalcInvBal(buffer invoice,
                       today,
                       false).
            
   i = i + 1.

   put stream slog unformatted
      invoice.custnum   chr(9)
      invoice.invnum    chr(9)
      invoice.invdate   chr(9)
      invoice.invcfg[1] chr(9)
      invoice.invamt    chr(9)
      ldbal             skip.

   pause 0.
   disp i invoice.invdate with overlay row 14 centered
        1 down frame fstat .

                          
end.

hide frame fstat    no-pause.
hide frame funprint no-pause.

