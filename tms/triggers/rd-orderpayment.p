TRIGGER PROCEDURE FOR REPLICATION-DELETE OF OrderPayment.

{HPD/HPDConst.i}

&IF {&ORDERPAYMENT_DELETE_TRIGGER_ACTIVE} &THEN

/* The OrderPayment information is sent to HPD along with the
   Order information => We need to do replog record for order */

IF NEW OrderPayment
THEN RETURN.

FOR Order FIELDS (Brand OrderID) NO-LOCK WHERE
   Order.Brand   = "1"                         AND
   Order.OrderID = OrderPayment.OrderID:

   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RowID     = STRING(ROWID(Order))
      Ordercanal.RepLog.TableName = "Order"
      Ordercanal.RepLog.EventType = "MODIFY"
      Ordercanal.RepLog.EventTime = NOW
      .
END.

&ENDIF