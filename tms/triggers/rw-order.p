TRIGGER PROCEDURE FOR REPLICATION-WRITE OF Order OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(Order) THEN DO:
   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RecordId  = RECID(Order)
      Ordercanal.RepLog.TableName = "Order"
      Ordercanal.RepLog.EventType = "CREATE"
      Ordercanal.RepLog.KeyValue  = STRING(Order.OrderId)
      OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(Order) THEN DO: */
ELSE DO:
   BUFFER-COMPARE Order USING OrderID CLI MsSeq OrderType OrderChannel 
   StatusCode ContractID Salesman Campaign CrStamp Referee
   ICC PayType CurrOper OldIcc OldPayType CLIType Offer
   DeliveryType DeliverySecure MNPStatus PortingDate 
   TO Oldbuf SAVE RESULT IN llResult.

   IF NOT llResult THEN DO:
      CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.RecordId  = RECID(Order)
         Ordercanal.RepLog.TableName = "Order"
         Ordercanal.RepLog.EventType = "MODIFY"
         Ordercanal.RepLog.KeyValue  = STRING(Oldbuf.OrderId)
         OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO: */
END. /* ELSE DO: */
