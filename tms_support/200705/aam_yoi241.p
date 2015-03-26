def buffer brow for invrow.

def var i     as int no-undo.
def var j     as int no-undo.
def var liacc as int no-undo.

for each invoice no-lock where
         brand = "1" and
         invdate >= 5/28/7,
    each invrow of invoice no-lock,
   first billitem no-lock where 
         billitem.brand    = "1" and
         billitem.billcode = invrow.billcode:
         

   i = i + 1.
         
   CASE invoice.VATUsage:
   WHEN 0 OR 
   WHEN 1 THEN liacc = BillItem.AccNum.
   WHEN 2 THEN liacc = BillItem.EUConAccNum.
   WHEN 3 THEN liacc = BillItem.EUAccNum.
   WHEN 4 THEN liacc = BillItem.FSAccNum.
   END CASE.


   if invrow.slsacc ne liacc then do:
         
      j = j + 1.
     
      /*       
      disp billitem.billcode
           liacc          format ">>>>>>>>9" 
           invrow.slsacc  format ">>>>>>>>9". 
      */
           
      find brow where recid(brow) = recid(invrow) exclusive-lock.
      brow.slsacc = liacc.
      
   end.
      
   pause 0.
   disp i j invoice.invdate with 1 down.
end.         


