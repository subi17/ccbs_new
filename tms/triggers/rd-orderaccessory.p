TRIGGER PROCEDURE FOR REPLICATION-DELETE OF OrderAccessory.

{HPD/HPDConst.i}
{Syst/tmsconst.i}

&IF {&ORDERACCESSORY_DELETE_TRIGGER_ACTIVE} &THEN

/* The OrderAccessory information is sent to HPD along with the
   Order information => We need to do replog record for order */

IF NEW OrderAccessory
THEN RETURN.

/* Only require for phone terminaltype */
IF OrderAccessory.TerminalType NE {&TERMINAL_TYPE_PHONE}
THEN RETURN.

FOR Order FIELDS (Brand OrderID) NO-LOCK WHERE
   Order.Brand   = "1"                         AND
   Order.OrderID = OrderAccessory.OrderID:

   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RowID     = STRING(ROWID(Order)).
      Ordercanal.RepLog.TableName = "Order"
      Ordercanal.RepLog.EventType = "MODIFY"
      Ordercanal.RepLog.EventTime = NOW
      .
END.

&ENDIF