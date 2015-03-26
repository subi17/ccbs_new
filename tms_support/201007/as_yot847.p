{commpaa.i}
katun = "anttis".
gcBrand = "1".

{orderfunc.i}

find mnpprocess where
     mnpprocess.portrequest = "00500111100706115509279" EXCLUSIVE-LOCK.

find order where
     order.brand = "1" and
     order.orderid = mnpprocess.orderid EXCLUSIVE-LOCK.


disp order.statuscode order.mnpstatus mnpprocess.statuscode.

assign
   mnpprocess.statuscode = 7
   mnpprocess.statusreason = "CANC_TECNI"
   mnpprocess.updatets = fMakeTS()
   order.mnpstatus = 8.

fSetOrderStatus(order.orderid, "7").  
