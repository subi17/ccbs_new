TRIGGER PROCEDURE FOR REPLICATION-DELETE OF OrderAccessory.

{HPD/HPDConst.i}

&IF {&ORDERACCESSORY_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW OrderAccessory
THEN RETURN.

/* Only require for phone terminaltype */
IF OrderAccessory.TerminalType NE 1
THEN RETURN.

CREATE Ordercanal.RepLog.
ASSIGN
   Ordercanal.RepLog.TableName = "OrderAccessory"
   Ordercanal.RepLog.EventType = "DELETE"
   Ordercanal.RepLog.EventTime = NOW
   Ordercanal.RepLog.KeyValue  = STRING(OrderAccessory.OrderId)
   .

&ENDIF