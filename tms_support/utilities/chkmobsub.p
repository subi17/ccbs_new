DEFINE STREAM sCorrData.
DEFINE STREAM sOKData.

OUTPUT STREAM sOKData TO VALUE("/home/harrim/okdata_20080908_v4.txt").
OUTPUT STREAM sCorrData TO VALUE("/home/harrim/corrodata_20080908_v4.txt").

for each mobsub no-lock:
   for each order where order.brand = "1" and order.msseq = mobsub.msseq no-lock:
       find ordercustomer where OrderCustomer.Brand = "1" and 
                                Ordercustomer.orderid = order.orderid and
                                OrderCustomer.RowType = 1                      
                                no-lock no-error.
       IF NOT AVAIL OrderCustomer THEN
       DO:
           PUT STREAM sCorrData UNFORMATTED "MobSub.msSeq = " MobSub.MsSeq 
           ", Order.OrderId = " Order.Orderid SKIP.
       END.
       ELSE
       DO:
           FIND Customer WHERE Customer.Brand = "1" AND Customer.CustNum = MobSub.CustNum NO-LOCK NO-ERROR.
           IF AVAIL Customer THEN
              PUT STREAM sOKData UNFORMATTED "Subscription Id = " MobSub.MsSeq 
              ", OrgId = " Customer.OrgId  " CLI = " Order.CLI SKIP.
           ELSE
              PUT STREAM sCorrData UNFORMATTED 
                  "Customer lacking from mobsub.msseq = " MobSub.MsSeq 
                  " with custnum = " MobSub.CustNum ", CLI = " Order.CLI SKIP.
       END.
   end.
END.

FOR EACH order NO-LOCK WHERE order.brand = "1":
   
       find ordercustomer where OrderCustomer.Brand = "1" and 
                                Ordercustomer.orderid = order.orderid and
                                OrderCustomer.RowType = 1                      
                                no-lock no-error.
       IF NOT AVAIL OrderCustomer THEN
       DO:
           PUT STREAM sCorrData UNFORMATTED "MobSub.msSeq = 0, Order.OrderId = " Order.Orderid SKIP.
       END.

END.




OUTPUT STREAM sCorrData CLOSE.
OUTPUT STREAM sOKData CLOSE.
