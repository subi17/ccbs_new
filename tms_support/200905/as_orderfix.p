def frame b with 15 down.
def stream slog.
output stream slog to /apps/snet/200905/as_order_fix.txt.
FOR EACH order wherE
   order.brand = "1" and
   order.orderid > 2000000 and
   crstamp > 20090505 and 
   offer ne "" and
   order.orderchannel ne "pos" and
   order.invnum > 0 NO-LOCK:

   if order.crstamp >= 20090507.28799 then next.

   find offer where
      offer.brand = "1" and
      offer.offer = order.offer NO-LOCK NO-ERROR.
      
   if avail offer then do:
      for each offeritem of offer where
         offeritem.endstamp >= order.crstamp  and
         offeritem.beginstamp <= order.crstamp NO-LOCK:
         if lookup(itemkey,"TS0000012") > 0 then do:
           FOR EACH invrow where
               invnum = order.invnum and
               lookup(invrow.billcode,"TS0000012") > 0 EXCLUSIVE-LOCK:
            /*   disp invrow except SlsAccNum with 1 col.*/
               find sim where
                  sim.icc = order.icc EXCLUSIVE-LOCK NO-eRROR.
               put stream slog unformatted order.orderid 
               "|" order.crstamp 
               "|" itemkey 
               "|" offeritem.amount 
               "|" offeritem.endstamp 
               "|" invrow.invnum 
               "|" invrow.billcode 
               "|" recid(invrow) 
               "|" sim.simstat skip.
               assign 
                  invrow.billcode = "TS0000010"
                  sim.simstat = 20.
               leave.
            END.
         end.
      end.
   end.
END.
