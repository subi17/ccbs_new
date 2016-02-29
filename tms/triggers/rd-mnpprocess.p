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

   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RowID     = STRING(ROWID(Order)).
      Common.RepLog.TableName = "Order"
      Common.RepLog.EventType = "MODIFY"
      Common.RepLog.EventTime = NOW
      .
END.

&ENDIF