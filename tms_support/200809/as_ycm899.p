{testpaa.i}
katun = "anttis".

output to /apps/snet/200809/ycm897_all_orders.txt.
DEFINE VARIABLE lcSep AS CHARACTER NO-UNDO INIT "|". 
{date.i}

FOR EACH order where
   order.brand = "1" and
   order.statuscode EQ "6" AND
   lookup(order.orderchannel,"pos,pre-act") = 0 NO-LOCK:
   /*lookup(order.orderchannel,"self,cc,telesales,renewal-telesales" NO-LOCK:*/
   
   FIND FIRST mobsub WHERE
      mobsub.msseq = order.msseq NO-LOCK NO-ERROR.

   IF NOT AVAIL mobsub then 
      FIND FIRST termmobsub where termmobsub.msseq = order.msseq 
         NO-LOCK NO-ERROR.

   if avail mobsub or avail termmobsub then
   put unformatted 
      order.cli lcSep 
      order.msseq lcSep
      order.orderid lcSep
      fTS2HMS(order.crstamp) SKIP.
END.
output close.

LEAVE.

DEFINE VARIABLE lcMNPTime AS CHARACTER NO-UNDO. 

OUTPUT TO /apps/snet/200809/ycm897_dextra.txt.

FOR EACH SIM   NO-LOCK WHERE
         SIM.Brand   = gcBrand AND
         SIM.SimStat = 20,
   FIRST Order NO-LOCK WHERE
         Order.MsSeq = SIM.MsSeq:

  /* skip those in control */
   IF Order.StatusCode = "4" THEN NEXT.

   IF LOOKUP(STRING(Order.MNPStatus),"5,8") > 0 THEN NEXT.

   lcMNPTime = "".
   FIND FIRST MNPProcess WHERE
      MNPProcess.OrderID = Order.OrderId AND
      (MNPProcess.StatusCode EQ 5 OR
      MNPProcess.StatusCode EQ 6) NO-LOCK NO-ERROR.
   IF AVAIL MNPProcess THEN DO:
      /* Is MNP Transfer time known ? */
      FIND FIRST MNPSub WHERE
         MNPSub.MNPSeq = MNPProcess.MNPSeq AND
         MNPSub.MsSeq  = Order.MsSeq NO-LOCK NO-ERROR.

      IF NOT AVAIL MNPSub THEN DO:
        NEXT.
      END.
   END.

   FIND FIRST Invoice WHERE
              Invoice.InvNum = Order.InvNum
   NO-LOCK NO-ERROR.

   IF NOT AVAILABLE(Invoice) THEN NEXT.
   
   put unformatted 
      order.cli lcSep 
      order.msseq lcSep
      order.orderid lcSep
      fTS2HMS(order.crstamp) SKIP.

END.

FOR EACH Order NO-LOCK WHERE
         Order.Brand = gcBrand AND
         Order.StatusCode = "12" AND
         Order.OrderType = 2:
    FIND MobSub WHERE MobSub.MsSeq = Order.MsSeq NO-LOCK NO-ERROR.
    IF AVAIL MobSub THEN
    DO:
       /* Do handling only after successful after sales request */
       FIND FIRST MsRequest WHERE
         MsRequest.Brand = gcBrand AND
         MsRequest.ReqType = 46 AND
         MsRequest.CLI = Order.CLI AND
         MsRequest.ReqIParam1 = Order.OrderId AND
         MsRequest.ReqStatus = 2 NO-LOCK NO-ERROR.
       IF NOT AVAIL MsRequest THEN NEXT.

       FIND SIM WHERE SIM.Brand = gcBrand AND SIM.ICC = MobSub.ICC NO-LOCK NO-ERROR.
       IF NOT AVAILABLE SIM THEN NEXT.

  /* skip those in control */
   IF Order.StatusCode = "4" THEN NEXT.

   IF LOOKUP(STRING(Order.MNPStatus),"5,8") > 0 THEN NEXT.

   lcMNPTime = "".
   FIND FIRST MNPProcess WHERE
      MNPProcess.OrderID = Order.OrderId AND
      (MNPProcess.StatusCode EQ 5 OR
      MNPProcess.StatusCode EQ 6) NO-LOCK NO-ERROR.
   IF AVAIL MNPProcess THEN DO:
      /* Is MNP Transfer time known ? */
      FIND FIRST MNPSub WHERE
         MNPSub.MNPSeq = MNPProcess.MNPSeq AND
         MNPSub.MsSeq  = Order.MsSeq NO-LOCK NO-ERROR.

      IF AVAIL MNPSub THEN DO:
         IF MNPSub.PortingTime NE 0 THEN DO:
         END.
         ELSE NEXT.
      END.
   END.

   FIND FIRST Invoice WHERE
              Invoice.InvNum = Order.InvNum
   NO-LOCK NO-ERROR.

   IF NOT AVAILABLE(Invoice) THEN NEXT.
   
   put unformatted 
      order.cli lcSep 
      order.msseq lcSep
      order.orderid lcSep
      fTS2HMS(order.crstamp) SKIP.
  /* skip those in control */
   
   IF Order.StatusCode = "4" THEN NEXT.

   IF LOOKUP(STRING(Order.MNPStatus),"5,8") > 0 THEN NEXT.

   lcMNPTime = "".
   FIND FIRST MNPProcess WHERE
      MNPProcess.OrderID = Order.OrderId AND
      (MNPProcess.StatusCode EQ 5 OR
      MNPProcess.StatusCode EQ 6) NO-LOCK NO-ERROR.
   IF AVAIL MNPProcess THEN DO:
      /* Is MNP Transfer time known ? */
      FIND FIRST MNPSub WHERE
         MNPSub.MNPSeq = MNPProcess.MNPSeq AND
         MNPSub.MsSeq  = Order.MsSeq NO-LOCK NO-ERROR.

      IF AVAIL MNPSub THEN DO:
         IF MNPSub.PortingTime NE 0 THEN DO:
         END.
         ELSE NEXT.
      END.
   END.

   FIND FIRST Invoice WHERE
              Invoice.InvNum = Order.InvNum
   NO-LOCK NO-ERROR.

   IF NOT AVAILABLE(Invoice) THEN NEXT.
   
   put unformatted 
      order.cli lcSep 
      order.msseq lcSep
      order.orderid lcSep
      fTS2HMS(order.crstamp) SKIP.
   END.
END.

