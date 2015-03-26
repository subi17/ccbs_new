def var i as int no-undo.

def buffer binv for invoice.

for each invoice no-lock use-index invdate where
         brand = "1" and
         invdate = 12/1/8 and
         invtype = 1 and
         ddstate = 1:
        
   find binv where recid(binv) = recid(invoice) exclusive-lock.
   assign binv.DDState = 0
          binv.DDFile  = "".

   i = i + 1.
   if i mod 1000 = 0 then do:
      pause 0.
      disp i with 1 down.
   end.


end.

disp i.