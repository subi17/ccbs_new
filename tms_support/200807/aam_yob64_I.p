def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.

def buffer binv for invoice.

for each invoice no-lock use-index invdate where
         invoice.brand = "1" and
         invoice.invdate = 7/1/8 and
         invoice.invtype = 1:

   if invoice.printstate > 0 or invoice.ddstate > 0 then do:
    
      if invoice.printstate > 0 then j = j + 1.
      if invoice.ddstate > 0 then k = k + 1.
      
      find binv where recid(binv) = recid(invoice) exclusive-lock.
      binv.printstate = 0.
      binv.ddstate = 0.
   end.    
         
   i = i + 1.
   
   if i mod 1000 = 0 then do:
      pause 0.
      disp i j k with 1 down.
   end.
end.   
      
disp i j k.

