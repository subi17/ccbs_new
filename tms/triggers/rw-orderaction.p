TRIGGER PROCEDURE FOR REPLICATION-WRITE OF OrderAction OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(OrderAction) THEN DO:
   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RecordId  = RECID(OrderAction)
      Ordercanal.RepLog.TableName = "OrderAction"
      Ordercanal.RepLog.EventType = "CREATE"
      Ordercanal.RepLog.KeyValue  = STRING(OrderAction.OrderId) + CHR(255) +
                                    OrderAction.ItemType        + CHR(255) +
                                    OrderAction.ItemKey
      OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(OrderAction) THEN DO: */
ELSE DO:
   BUFFER-COMPARE OrderAction USING OrderId ItemType ItemKey 
   TO Oldbuf SAVE RESULT IN llResult.
   
   IF NOT llResult THEN DO:
      CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.RecordId  = RECID(OrderAction)
         Ordercanal.RepLog.TableName = "OrderAction"
         Ordercanal.RepLog.EventType = "MODIFY"
         Ordercanal.RepLog.KeyValue  = STRING(Oldbuf.OrderId) + CHR(255) +
                                       Oldbuf.ItemType        + CHR(255) +
                                       Oldbuf.ItemKey
         OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO:  */
END. /* ELSE DO: */
