input from /apps/snet/200906/as_yts1537_cashfee.input.
def stream slog.
output stream slog to /apps/snet/200906/as_yts1537_cashfee_chg_offer.log.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcNewOffer AS CHARACTER NO-UNDO format "x(21)". 

repeat:
   import unformatted lcLine.

   find order where
      order.brand = "1" and
      order.orderid = int(lcLine) EXCLUSIVE-LOCK.

   find invoice where
      invoice.invnum = order.invnum NO-LOCK NO-ERROR.
   
   if not avail invoice
      or invoice.invtype ne 7 then next.

   if order.offer = "1" THEN NEXT.
  
   lcNewOffer = "".
   IF Order.Offer = "P040G45N20CDN1" THEN
      lcNewOffer = "P040G45N20CDN0".
   ELSE IF
      ORder.Offer = "P075MF6B20CDN1" THEN
       lcNewOffer = "P075MF6B20CDN0".

   put stream slog unformatted  
      order.orderid "|"
      /*
      invoice.extinvid when avail invoice
      invoice.invtype when avail invoice
      invoice.chargetype when avail invoice
     */

      order.offer "|"
      lcNewOffer skip.

   assign order.offer = lcNewOffer.
end.
