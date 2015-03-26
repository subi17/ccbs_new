def buffer bOrder for order.
def stream sout.
output stream sout to as_yts_2166.log.

DEFINE TEMP-TABLE ttOrder
FIELD i AS INT
INDEX i IS PRIMARY UNIQUE i. 

FOR EACH order where
   order.brand = "1" and
   order.orderid > 2500000 NO-LOCK:

   find first bOrder where
      bOrder.Brand = "1" and
      bOrder.ContractId = order.ContractId and
      bOrder.Crstamp = order.crStamp and
      bOrder.orderid > order.orderid and
      rowid(bOrder) ne rowid(ORDER) NO-LOCK no-error.

   if not avail bOrder then next.

   disp order.orderid bOrder.orderid order.crstamp bOrder.crstamp.

   put stream sout unformatted order.orderid "|" bOrder.orderid "|" order.crstamp "|" order.statuscode "|" bOrder.Statuscode skip.
end.
