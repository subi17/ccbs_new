def var i as int no-undo.

def stream slog.
output stream slog to /tmp/penalty_fees.txt.

put stream slog unformatted
   "MSISDN"   chr(9)
   "Invoice"  chr(9)
   "Amount"   chr(9)
   "From"     chr(9)
   "To"       skip.

for each invoice no-lock use-index invdate where
         brand = "1" and
         invdate = 7/1/8 and
         invtype = 1,
    each invrow of invoice no-lock where
         invrow.rowtype = 4 and
         invrow.billcode = "termperiod"
by invoice.cli:
         
   put stream slog unformatted
      invoice.cli  chr(9)
      invoice.extinvid chr(9)
      invrow.amt chr(9).

   for first dccli no-lock where
             dccli.msseq = invoice.msseq and
             dccli.dcevent = "term18":
      put stream slog unformatted
         string(dccli.validfrom,"99.99.9999") chr(9)
         string(dccli.validto,"99.99.9999").
   end.
      
   put stream slog skip.
         
   i = i + 1.
   
   if i mod 100 = 0 then do:
       pause 0.
       disp i with 1 down.
   end.
end.

disp i.


