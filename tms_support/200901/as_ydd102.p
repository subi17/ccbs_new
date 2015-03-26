def stream sout.
output stream sout to /apps/snet/200901/as_ydd102.txt.
DEFINE VARIABLE liOrderType AS INTEGER NO-UNDO. 
def buffer bufOrder FOR Order.

FOR EACH order where
   order.brand = "1" and
   order.orderid < 1216083 NO-LOCK:
   
   liOrderType = 0.
   
   if order.ordertype ne 0 then next.

   if order.mnpstatus > 0 then liOrderType = 1.
   else liOrderType = 0.
   
   if order.orderchannel begins "renewal" then liOrderType = 2.
   
   if order.ordertype ne liordertype then do:
      
      do trans:
         find bufOrder where rowid(bufOrder) = rowid(order) EXCLUSIVE-LOCK.
         
         put stream sout unformatted 
            bufOrder.orderid "|" 
            bufOrder.ordertype "|" 
            liordertype  skip.
         
         assign bufOrder.OrderType = liOrderType.
         release bufOrder.
      end.
   end.
ENd.
