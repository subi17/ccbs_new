TRIGGER PROCEDURE FOR REPLICATION-WRITE OF OrderAccessory OLD BUFFER oldOrderAccessory.

{HPD/HPDConst.i}

&IF {&ORDERACCESSORY_WRITE_TRIGGER_ACTIVE} &THEN

/* If this is a new OrderAccessory and terminaltype is not phone,
   we won't send the information */ 
IF NEW(OrderAccessory) AND OrderAccessory.TerminalType NE 1
THEN RETURN.

CREATE Ordercanal.RepLog.
ASSIGN
   Ordercanal.RepLog.TableName = "OrderAccessory"
   Ordercanal.RepLog.EventType = (IF NEW(OrderAccessory)
                                  THEN "CREATE"
                                  ELSE IF OrderAccessory.TerminalType NE 1
                                  THEN "DELETE"
                                  ELSE "MODIFY")
   Ordercanal.RepLog.EventTime = NOW
   .

IF Ordercanal.RepLog.EventType = "DELETE" 
THEN Ordercanal.RepLog.KeyValue = STRING(OrderAccessory.OrderId).
ELSE Ordercanal.RepLog.RowID    = STRING(ROWID(OrderAccessory)).

IF NOT NEW(OrderAccessory) AND OrderAccessory.TerminalType = 1
THEN DO: 
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE OrderAccessory USING
      OrderId
   TO oldOrderAccessory SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:   
      CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.TableName = "OrderAccessory"
         Ordercanal.RepLog.EventType = "DELETE"
         Ordercanal.RepLog.EventTime = NOW
         Ordercanal.RepLog.KeyValue  = STRING(oldOrderAccessory.OrderId)
         .
   END.
END.

&ENDIF