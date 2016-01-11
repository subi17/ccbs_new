TRIGGER PROCEDURE FOR REPLICATION-WRITE OF OrderAccessory OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(OrderAccessory) THEN RETURN.
IF OrderAccessory.TerminalType NE 1 THEN RETURN.
   
BUFFER-COMPARE OrderAccessory 
   USING OrderID
         TerminalType
         IMEI 
         ProductCode 
         Amount
         Discount TO Oldbuf SAVE RESULT IN llResult.
IF llResult EQ TRUE THEN RETURN.

FIND FIRST Order NO-LOCK WHERE
           Order.brand = "1" AND
           Order.OrderID = OrderAccessory.OrderID NO-ERROR.
IF NOT AVAIL Order THEN RETURN.

CREATE OrderCanal.RepLog.
ASSIGN
   OrderCanal.RepLog.RecordId  = RECID(Order)
   OrderCanal.RepLog.TableName = "Order"
   OrderCanal.RepLog.EventType = "MODIFY"
   OrderCanal.RepLog.KeyValue  = STRING(Order.OrderId) 
   OrderCanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
