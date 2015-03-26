/*
FIND FIRST order where
   orderid = 792578 NO-LOCK no-error.

FIND LAST MNPSub WHERE
   MNPSub.MsSeq = Order.MsSeq
   EXCLUSIVE-LOCK NO-ERROR.

MNPSub.PortingTime = 20080208.07200.
*/

FIND FIRST order where
   orderid = 799887 NO-LOCK no-error.

FIND LAST MNPSub WHERE
   MNPSub.MsSeq = Order.MsSeq
   EXCLUSIVE-LOCK NO-ERROR.

MNPSub.PortingTime = 20080207.10800.

