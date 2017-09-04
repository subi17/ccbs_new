TRIGGER PROCEDURE FOR REPLICATION-DELETE OF OrderCustomer.

{HPD/HPDConst.i}

&IF {&ORDERCUSTOMER_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW OrderCustomer
THEN RETURN.

{triggers/replog_tenantname.i}

CREATE Ordercanal.RepLog.
ASSIGN
   Ordercanal.RepLog.TableName  = "OrderCustomer"
   Ordercanal.RepLog.EventType  = "DELETE"
   Ordercanal.RepLog.EventTime  = NOW
   Ordercanal.RepLog.TenantName = fRepLogTenantName(BUFFER OrderCustomer:HANDLE)
   Ordercanal.RepLog.KeyValue   = {HPD/keyvalue.i OrderCustomer . {&HPDKeyDelimiter} OrderId RowType}
   .

/* Only require for rowtype 1 */
IF OrderCustomer.RowType NE 1
THEN RETURN.

FOR Order FIELDS (Brand OrderID) NO-LOCK WHERE
   Order.Brand   = "1"                         AND
   Order.OrderID = OrderCustomer.OrderID:

   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RowID      = STRING(ROWID(Order))
      Ordercanal.RepLog.TableName  = "Order"
      Ordercanal.RepLog.EventType  = "MODIFY"
      Ordercanal.RepLog.EventTime  = NOW
      Ordercanal.RepLog.TenantName = fRepLogTenantName(BUFFER Order:HANDLE)
      .
END.

&ENDIF