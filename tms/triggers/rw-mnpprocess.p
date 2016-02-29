TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MNPProcess OLD BUFFER oldMNPProcess.

{HPD/HPDConst.i}
{Syst/tmsconst.i}

&IF {&MNPPROCESS_WRITE_TRIGGER_ACTIVE} &THEN

/* The MNPProcess information is sent to HPD along with the
   Order information => We need to do replog record for order */

/* If MNPType was not type in and it still is not
   => no need to do replog for Order */
IF oldMNPProcess.MNPType NE {&MNP_TYPE_IN} AND MNPProcess.MNPType NE {&MNP_TYPE_IN} 
THEN RETURN. 

DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

BUFFER-COMPARE MNPProcess USING
   OrderID
   MNPType
   FormRequest 
   PortRequest 
   PortingTime
   UpdateTS
   StatusCode
   CreatedTS
   StatusReason
TO oldMNPProcess SAVE RESULT IN llSameValues.

IF llSameValues
THEN RETURN.

IF oldMNPProcess.OrderID <> MNPProcess.OrderID
THEN
FOR Order FIELDS (Brand OrderID) NO-LOCK WHERE
   Order.Brand   = "1"                         AND
   Order.OrderID = oldMNPProcess.OrderID:

   CREATE Common.RepLog.
   ASSIGN
      Common.RepLog.RowID     = STRING(ROWID(Order)).
      Common.RepLog.TableName = "Order"
      Common.RepLog.EventType = "MODIFY"
      Common.RepLog.EventTime = NOW
      .
END.

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