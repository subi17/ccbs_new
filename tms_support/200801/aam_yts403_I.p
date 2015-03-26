   def var i as int no-undo.
   
   
   for each order no-lock where
            order.brand = "1" and
            order.crstamp > 20071101 and
            lookup(order.statuscode,"1,3,7") = 0 and
            order.clitype begins "cont" and
            order.mnpstatus > 0,
      first orderaccessory of order no-lock where
            orderaccessory.productcode = "P013K53P2",
      first sim no-lock where
            sim.icc = order.icc and
            sim.simstat = 21,
      first invoice no-lock where
            invoice.invnum = order.invnum and
            invoice.chgstamp > 20080101 and
            invoice.chgstamp < 20080111.50400,
      first invrow of invoice no-lock where
            invrow.billcode = "TS0000004":
            
      i = i + 1.
            
      pause 0.
      disp i with 1 down.
   end.
 