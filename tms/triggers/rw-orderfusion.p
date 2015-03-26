TRIGGER PROCEDURE FOR REPLICATION-WRITE OF OrderFusion OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(OrderFusion) THEN DO:
   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RecordId  = RECID(OrderFusion)
      Ordercanal.RepLog.TableName = "OrderFusion"
      Ordercanal.RepLog.EventType = "CREATE"
      Ordercanal.RepLog.KeyValue  = STRING(OrderFusion.OrderId)
      Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(OrderFusion) THEN DO: */
ELSE DO:
   BUFFER-COMPARE OrderFusion USING OrderId FusionStatus FixedNumber
   FixedNumberType FixedStatus TO Oldbuf SAVE RESULT IN llResult.
   
   IF NOT llResult THEN DO:
      CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.RecordId  = RECID(OrderFusion)
         Ordercanal.RepLog.TableName = "OrderFusion"
         Ordercanal.RepLog.EventType = "MODIFY"
         Ordercanal.RepLog.KeyValue  = STRING(Oldbuf.OrderId)
         Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO:  */
END. /* ELSE DO: */
