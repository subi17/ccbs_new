{commpaa.i}
katun = "anttis".
gcBrand = "1".

{orderfunc.i}


DEFINE VARIABLE lcMNPs AS CHARACTER NO-UNDO. 

lcMNPs = "00500111100708113507498 00500111100708130910704 00500111100707081100259".


DEFINE VARIABLE lcMNP AS CHARACTER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 


do i = 1 to num-entries(lcMNPs, " "):
   
   lcMNP = entry(i, lcMNPs, " ").
   
   find mnpprocess where
        mnpprocess.portrequest = lcMNP and
        mnpprocess.mnptype = 1 EXCLUSIVE-LOCK.

   find order where
        order.brand = "1" and
        order.orderid = mnpprocess.orderid EXCLUSIVE-LOCK.

   assign
      mnpprocess.statuscode = 7
      mnpprocess.statusreason = "CANC_ABONA"
      mnpprocess.updatets = fMakeTS()
      order.mnpstatus = 8.

   fSetOrderStatus(order.orderid, "7").  

END.
