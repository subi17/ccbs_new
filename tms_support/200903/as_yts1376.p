DEFINE VARIABLE cFile AS CHARACTER NO-UNDO. 
cFile = "/apps/snet/200903/as_yts1376_2.txt".
define stream sOrders.
OUTPUT STREAM sOrders TO VALUE (cFile).
for each order where order.brand = "1" and 
    order.crstamp > 20090212  NO-lock use-index Stamp:
   find orderaccessory where 
      orderaccessory.brand = "1" and
      orderaccessory.orderid = order.orderid no-lock no-error.
   if avail orderaccessory then
   do:
      if orderaccessory.ProductCode EQ "P021Z10P2" THEN DO:
         find sim where sim.icc = order.icc NO-LOCK NO-ERROR.
         PUT STREAM sOrders UNFORMATTED Order.OrderId " "
         order.crstamp " "
         order.statuscode " "
         order.ordertype " "
         order.mnpstatus " "
         order.invnum " "
         (if avail sim then string(sim.simstat) else "NO SIM") SKIP.
      END.   
   end.
end.

OUTPUT STREAM sOrders CLOSE.
