TRIGGER PROCEDURE FOR REPLICATION-WRITE OF OrderPayment OLD BUFFER oldOrderPayment.

{HPD/HPDConst.i}

&IF {&ORDERPAYMENT_WRITE_TRIGGER_ACTIVE} &THEN

/* The OrderPayment information is sent to HPD along with the
   Order information => We need to do replog record for order */

/* If this is a new OrderPayment => no need to do replog for Order */ 
IF NEW(OrderPayment)
THEN RETURN.

DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

BUFFER-COMPARE OrderPayment USING
   OrderID
   Method
TO oldOrderPayment SAVE RESULT IN llSameValues.

IF llSameValues
THEN RETURN.

IF oldOrderPayment.OrderID <> OrderPayment.OrderID
THEN
FOR Order FIELDS (Brand OrderID) NO-LOCK WHERE
   Order.Brand   = "1"                         AND
   Order.OrderID = oldOrderPayment.OrderID:

   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RowID     = STRING(ROWID(Order)).
      Ordercanal.RepLog.TableName = "Order"
      Ordercanal.RepLog.EventType = "MODIFY"
      Ordercanal.RepLog.EventTime = NOW
      .
END.

FOR Order FIELDS (Brand OrderID) NO-LOCK WHERE
   Order.Brand   = "1"                         AND
   Order.OrderID = OrderPayment.OrderID:

   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RowID     = STRING(ROWID(Order)).
      Ordercanal.RepLog.TableName = "Order"
      Ordercanal.RepLog.EventType = "MODIFY"
      Ordercanal.RepLog.EventTime = NOW
      .
END.

&ENDIF