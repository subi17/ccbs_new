TRIGGER PROCEDURE FOR REPLICATION-WRITE OF OrderAccessory OLD BUFFER oldOrderAccessory.

{HPD/HPDConst.i}
{Syst/tmsconst.i}

&IF {&ORDERACCESSORY_WRITE_TRIGGER_ACTIVE} &THEN

/* The OrderAccessory information is sent to HPD along with the
   Order information => We need to do replog record for order */

/* If this is a new OrderAccessory => no need to do replog for Order */ 
IF NEW(OrderAccessory)
THEN RETURN.

/* If terminaltype was not phone and it still is not phone
   => no need to do replog for Order */
IF oldOrderAccessory.TerminalType NE {&TERMINAL_TYPE_PHONE} AND OrderAccessory.TerminalType NE {&TERMINAL_TYPE_PHONE} 
THEN RETURN. 

DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

BUFFER-COMPARE OrderAccessory USING
   OrderID
   TerminalType
   IMEI 
   ProductCode 
   Amount
   Discount
TO oldOrderAccessory SAVE RESULT IN llSameValues.

IF llSameValues
THEN RETURN.

{triggers/replog_tenantname.i}

IF oldOrderAccessory.OrderID <> OrderAccessory.OrderID
THEN
FOR Order FIELDS (Brand OrderID) NO-LOCK WHERE
   Order.Brand   = "1"                         AND
   Order.OrderID = oldOrderAccessory.OrderID:

   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RowID     = STRING(ROWID(Order))
      Ordercanal.RepLog.TableName = "Order"
      Ordercanal.RepLog.EventType = "MODIFY"
      Ordercanal.RepLog.EventTime = NOW
      Ordercanal.RepLog.TenantName = fRepLogTenantName(BUFFER Order:HANDLE)
      .
END.

FOR Order FIELDS (Brand OrderID) NO-LOCK WHERE
   Order.Brand   = "1"                         AND
   Order.OrderID = OrderAccessory.OrderID:

   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RowID     = STRING(ROWID(Order))
      Ordercanal.RepLog.TableName = "Order"
      Ordercanal.RepLog.EventType = "MODIFY"
      Ordercanal.RepLog.EventTime = NOW
      Ordercanal.RepLog.TenantName = fRepLogTenantName(BUFFER Order:HANDLE)
      .
END.

&ENDIF