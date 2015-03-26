def var i as int no-undo.
def var j as int no-undo.

def buffer brow for invrow.

for each invoice no-lock where
         invoice.brand = "1" and
         invoice.invdate >= 1/1/9,
    each invrow of invoice no-lock,
   first billitem no-lock where
         billitem.brand = "1" and
         billitem.billcode = invrow.billcode:

   i = i + 1.
         
   if BillItem.AccNum ne invrow.slsacc then do:
   
       j = j + 1.
       find brow where recid(brow) = recid(invrow) exclusive-lock.
       brow.slsacc = billitem.accnum.
       
       /*
       disp invrow.billcode 
          invrow.slsacc format ">>>>>>>9"
          billitem.accnum format ">>>>>>>9"
          invoice.invtype format ">9".
       */   
   end.
   
   if i mod 1000 = 0 then do:
      pause 0.
      disp i j invoice.invdate with 1 down.
   end.
end.
         
disp i j.
         
