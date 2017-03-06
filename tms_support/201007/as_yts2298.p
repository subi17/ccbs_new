{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".

{Func/orderfunc.i}

find mnpprocess where
     mnpprocess.portrequest = "00500111100511192923310" EXCLUSIVE-LOCK.

find order where
     order.brand = "1" and
     order.orderid = mnpprocess.orderid EXCLUSIVE-LOCK.

assign
   mnpprocess.statuscode = 7
   mnpprocess.statusreason = "CANC_TECNI"
   mnpprocess.updatets = fMakeTS()
   order.mnpstatus = 8.

fSetOrderStatus(order.orderid, "7"). 
