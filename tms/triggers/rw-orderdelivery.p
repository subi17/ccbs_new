TRIGGER PROCEDURE FOR REPLICATION-WRITE OF OrderDelivery OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(OrderDelivery) THEN DO:
   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RecordId  = RECID(OrderDelivery)
      Ordercanal.RepLog.TableName = "OrderDelivery"
      Ordercanal.RepLog.EventType = "CREATE"
      Ordercanal.RepLog.KeyValue  = STRING(OrderDelivery.OrderId) + CHR(255)  +
                                    STRING(OrderDelivery.LOTimeStamp)
      Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(OrderDelivery) THEN DO: */
ELSE DO:
   BUFFER-COMPARE OrderDelivery USING OrderId LOStatusId CourierId LOId
   LOTimeStamp CourierShippingId TO Oldbuf SAVE RESULT IN llResult.
   
   IF NOT llResult THEN DO:
      CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.RecordId  = RECID(OrderDelivery)
         Ordercanal.RepLog.TableName = "OrderDelivery"
         Ordercanal.RepLog.EventType = "MODIFY"
         Ordercanal.RepLog.KeyValue  = STRING(Oldbuf.OrderId) + CHR(255) +
                                       STRING(Oldbuf.LOTimeStamp)
         Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO:  */
END. /* ELSE DO: */
