def var i     as int no-undo.
def var ldamt as dec no-undo.

session:numeric-format = "european".

def stream slog.
output stream slog to /tmp/credit_mincons_october_calls.txt.

put stream slog unformatted
   "MSISDN"    chr(9)
   "Customer"  chr(9)
   "Invoice"   chr(9)
   "Credited Min.Consumption" chr(9)
   "Calls 1-31.10." skip.


for each invoice no-lock use-index invdate where
         invoice.brand   = "1"     and
         invoice.invdate = 12/10/7 and
         invoice.invtype = 1,
   first invrow of invoice no-lock where
         invrow.rowtype  = 4 and 
         invrow.billcode = "mccontr1" and
         invrow.amt < 0
by invoice.cli:
         
   ldamt = 0.
         
   for each mobcdr no-lock use-index invseq where
            mobcdr.invcust = invoice.custnum and
            mobcdr.invseq  = invoice.invseq  and
            mobcdr.datest >= 10/1/7          and
            mobcdr.datest <= 10/31/7:
            
      ldamt = ldamt + mobcdr.amount.      
   end.

   put stream slog unformatted
      invoice.cli      chr(9)
      invoice.custnum  chr(9)
      invoice.extinvid chr(9)
      invrow.amt       chr(9)
      round(ldamt,2)   skip.
      
   i = i + 1.
   pause 0.
   disp i with 1 down.
   
end.         
