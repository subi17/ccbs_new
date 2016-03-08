TRIGGER PROCEDURE FOR REPLICATION-DELETE OF MNPProcess.

{HPD/HPDConst.i}
{Syst/tmsconst.i}


&IF {&MNPPROCESS_DELETE_TRIGGER_ACTIVE} &THEN

/* The MNPProcess information is sent to HPD along with the
   Order information => We need to do replog record for order */

IF NEW MNPProcess
THEN RETURN.

/* Only require for type in mnptype */
IF MNPProcess.MNPType NE {&MNP_TYPE_IN}
THEN RETURN.

FOR Order FIELDS (Brand OrderID) NO-LOCK WHERE
   Order.Brand   = "1"                         AND
   Order.OrderID = MNPProcess.OrderID:

   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RowID     = STRING(ROWID(Order))
      Ordercanal.RepLog.TableName = "Order"
      Ordercanal.RepLog.EventType = "MODIFY"
      Ordercanal.RepLog.EventTime = NOW
      .
END.

&ENDIF