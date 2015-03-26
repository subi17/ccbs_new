FOR EACH order where
   order.brand = "1" and
   order.orderid = 2145348 /* 2151682 */ NO-LOCK:
   FOR EACH invoice where
      invoice.invnum = order.invnum NO-LOCK:
      FOR FIRST invrow where
         invrow.invnum = invoice.invnum and
         invrow.billcode = "TS0000002" EXCLUSIVE-LOCK :
         invrow.billcode = "TS0000001". 
      END.
   END.
   find sim where
      sim.icc = order.icc EXCLUSIVE-LOCK.
  
   if sim.simstat = 21 then
    sim.simstat  = 20.

   disp sim.simstat.
ENd.
