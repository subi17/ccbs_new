def var i as int no-undo.

def buffer binv for invoice.

def stream slog.
output stream slog to /apps/snet/200807/deny_printing_100.log append.

for each invoice no-lock use-index invdate where
         invoice.brand = "1" and
         invoice.invdate = 7/1/8 and
         invoice.invtype = 1 and
         invoice.invcfg[1] = false,
   first invrow of invoice no-lock where
         invrow.rowtype = 4 and
         invrow.billcode = "termperiod" and
         invrow.amt = 100:

      i = i + 1.

      put stream slog unformatted
          invoice.invnum "|"
          invoice.cli    "|"
          invoice.custnum skip.
          
      find binv where recid(binv) = recid(invoice) exclusive-lock.
      binv.InvCfg[1] = true.

      if i mod 10 = 0 then do:
         pause 0.
         disp i with 1 down.
      end.
end.   
      
disp i.

