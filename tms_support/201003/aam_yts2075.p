def var i as int no-undo.
def var j as int no-undo.

def buffer binv for invoice.

FOR EACH Invoice NO-LOCK USE-INDEX Invdate WHERE
         Invoice.Brand   = "1"   and
         Invoice.InvDate <= 12/31/8 and
         Invoice.InvType = 1:
                  
   i = i + 1.
   
   if invoice.deliverystate = 0 then do:
      find first binv where recid(binv) = recid(invoice) exclusive-lock.
      binv.deliverystate = 2.
      j = j + 1.
   end.
   
   if i mod 1000 = 0 then do:
      pause 0.
      disp i j invoice.invdate with 1 down.
   end.
end.   
               
disp i j .
               
