{testpaa.i}
katun = "anttis".

output to /apps/snet/200809/ycm897_hold_orders.txt.
DEFINE VARIABLE lcSep AS CHARACTER NO-UNDO INIT "|". 
{date.i}

FOR EACH order where
   order.brand = "1" and
   LOOKUP(order.statuscode,"6,7") = 0 AND
   order.orderchannel NE "POS" NO-LOCK:
   
   put unformatted 
      order.cli lcSep 
      order.msseq lcSep
      order.orderid lcSep
      fTS2HMS(order.crstamp) SKIP.
END.
output close.
