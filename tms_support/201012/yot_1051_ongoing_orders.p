{Syst/commpaa.i}
katun = "Qvantel".
gcBrand = "1".

def stream sout.
output stream sout to yot_1051_ongoing_orders.txt.

put stream sout unformatted "MSISDN|SUBSCR.ID" skip.

LOOPPI:
FOR EACH order where
   order.brand = "1" and
   order.crstamp >= 20101201 AND
   order.crstamp < 20110106 AND
   order.statuscode ne "6" NO-LOCK:

   if lookup(order.statuscode,"7,8") > 0 then next.
   if order.ordertype = 2 then next.
   
   IF CAN-FIND(FIRST OfferItem WHERE
                     OfferItem.Brand = gcBrand AND
                     OfferItem.Offer = Order.Offer AND
                     OfferItem.ItemType = "BundleItem" AND
                     OfferItem.ItemKey = "MDUB" AND
                     OfferItem.EndStamp >= Order.CrStamp AND
                     OfferItem.BeginStamp <= Order.CrStamp) OR
      CAN-FIND(FIRST OrderAction WHERE
                     OrderAction.Brand = gcBrand AND
                     OrderAction.OrderId = Order.OrderID AND
                     OrderAction.ItemType = "BundleItem" AND
                     OrderAction.ItemKey = "MDUB") THEN DO:
   
      put stream sout unformatted
         order.cli "|" 
         order.msseq skip.
      
   end.

end.
