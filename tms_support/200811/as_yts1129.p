{Syst/commpaa.i}
katun  = "anttis".
gcBrand = "1".
DEFINE VARIABLE lcCharValue AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldAmount AS DECIMAL NO-UNDO. 
DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 

DEFINE VARIABLE orders AS CHAR NO-UNDO. 

orders = "1344435 1345737 1346391 1347239 1347378 1348302 1348347 1348385 1348553 1349673 1349699 1349730 1350115 1350441 1351554".

orders = orders + " 1359366 1359285 1358898 1358879 1358165 1358112 1357986 1356699 1356284 1356059 1355828 1347192 1345224 1343973 1343849".

def stream slog.
output stream slog to /apps/snet/200811/as_yts1129.log.

def stream sfee.
output stream sfee to /apps/snet/200811/as_yts1129_singlefee.d.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
do i = 1 to num-entries(orders, " ")with frame a:
   
   FIND FIRST order where order.brand = "1" and
      order.orderid = int(entry(i,orders," ")) NO-LOCK NO-ERROR.
   
   FIND FIRST orderaccessory of order NO-LOCK.
/*   orderaccessory.discount = 89.
   FIND CURRENT orderaccessory NO-LOCK. */

   FIND FIRST mobsub where mobsub.msseq = order.msseq NO-LOCK NO-ERROR.
   IF NOT AVAIL mobsub and order.mnpstatus = 0 then next.
   
   FOR EACH SingleFee EXCLUSIVE-LOCK WHERE 
      SingleFee.Brand     = gcBrand AND
      SingleFee.HostTable = "Order" AND
      SingleFee.KeyValue  = STRING(Order.OrderId) AND
      SingleFee.CalcObj   = "CASHFEE":
      export stream sfee singlefee.
      delete singlefee.
   END.
   
   /* add initial fees and additional cost (delivery charge) and cash invoice */
   RUN Mc/cashfee.p (Order.OrderID,
             1,                     /* action 1=create fees */
             OUTPUT lcCharValue,
             OUTPUT ldAmount,
             OUTPUT lcError).

   /* write possible error to an order memo */
   IF lcError BEGINS "Error" THEN DO:
      DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                    "Order",
                    STRING(Order.OrderID),
                    (if avail mobsub then MobSub.CustNum else 0),

                    "CASH INVOICE FAILED",
                    lcError).
   END.
   
   
   FIND FIRST sim where sim.icc = order.icc NO-LOCK NO-ERROR.
   put stream slog unformatted 
      order.orderid "|"
      order.orderchannel "|"
      order.mnpstatus "|"
      orderaccessory.discount "|"
      orderaccessory.productcode "|"
      sim.simstat "|"
      order.invnum "|"
      lcError skip.
ENd.
