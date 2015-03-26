/*
FIND FIRST Order WHERE
   Brand = "1" and
   Orderid = 1332557 and
   statuscode = "73" EXCLUSIVE-LOCK NO-ERROR.

order.statuscode = "6".
disp order.statuscode.
*/

FIND FIRST Order WHERE
   Brand = "1" and
   Orderid = 1389398 and
   statuscode = "73" EXCLUSIVE-LOCK NO-ERROR.

order.statuscode = "12". 
disp order.statuscode.
