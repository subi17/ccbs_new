def buffer bcust for customer.

def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.

for each customer no-lock:

   i = i + 1.
   
   if customer.creditlimit = 0 then do:
      find bcust where recid(bcust) = recid(customer) exclusive-lock
        no-wait no-error.
      
      if not locked bcust then assign 
         bcust.creditlimit = 100    
         j = j + 1.
      else k = k + 1.   
   end.
   
   pause 0.
   disp  i j k with 1 down.

end.

