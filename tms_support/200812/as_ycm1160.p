input from /apps/snet/200812/as_ycm1160.input.
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

def stream sbak.
output stream sbak to /apps/snet/200812/as_ycm1160.bak.

repeat:
   import unformatted lcLine.

   FIND FIRST orderdelivery where
      orderdelivery.brand   = "1" and
      orderdelivery.orderid = int(entry(1,lcLine,"|")) and
      string(orderdelivery.lotimestamp) begins substring(entry(2,lcLine,"|"), 1, 18) NO-LOCK NO-ERROR.
  
   export stream sbak orderdelivery.
   find current orderdelivery EXCLUSIVE-LOCK.
   delete orderdelivery.
end.
