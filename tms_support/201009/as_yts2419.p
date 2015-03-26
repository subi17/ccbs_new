find mnpprocess where
     mnpprocess.portrequest = "00500111100510082201389" and
     mnpprocess.statuscode = 2 EXCLUSIVE-LOCK .

find order where
     order.brand = "1" and
     order.orderid = mnpprocess.orderid and
     order.mnpstatus = 3 EXCLUSIVE-LOCK.

assign
   mnpprocess.statuscode = 7
   order.mnpstatus = 8.
