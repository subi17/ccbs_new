TRIGGER PROCEDURE FOR REPLICATION-WRITE OF OrderCustomer OLD BUFFER oldOrderCustomer.

{HPD/HPDConst.i}

&IF {&ORDERCUSTOMER_WRITE_TRIGGER_ACTIVE} &THEN

CREATE Ordercanal.RepLog.
ASSIGN
   Ordercanal.RepLog.TableName = "OrderCustomer"
   Ordercanal.RepLog.EventType = (IF NEW(OrderCustomer)
                                  THEN "CREATE"
                                  ELSE "MODIFY")
   Ordercanal.RepLog.EventTime = NOW
   .

IF NOT NEW(OrderCustomer)
THEN DO:
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE OrderCustomer USING
      OrderId
      RowType
   TO oldOrderCustomer SAVE RESULT IN llSameValues.

   IF NOT llSameValues
   THEN DO:
      CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.TableName = "OrderCustomer"
         Ordercanal.RepLog.EventType = "DELETE"
         Ordercanal.RepLog.EventTime = NOW
         Ordercanal.RepLog.KeyValue  = {HPD/keyvalue.i oldOrderCustomer . {&HPDKeyDelimiter} OrderId RowType}
         .
   END.

   /* If rowtype was not 1 and it still is not 1
      => no need to do replog for Order */
   IF oldOrderCustomer.RowType NE 1 AND OrderCustomer.RowType NE 1
   THEN RETURN.

   BUFFER-COMPARE OrderCustomer USING
      OrderId
      CustId
      CustIdType
   TO oldOrderCustomer SAVE RESULT IN llSameValues.

   IF llSameValues
   THEN RETURN.

   IF oldOrderAccessory.OrderID <> OrderAccessory.OrderID
   THEN
   FOR Order FIELDS (Brand OrderID) NO-LOCK WHERE
      Order.Brand   = "1"                         AND
      Order.OrderID = oldOrderCustomer.OrderID:

      CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.RowID     = STRING(ROWID(Order))
         Ordercanal.RepLog.TableName = "Order"
         Ordercanal.RepLog.EventType = "MODIFY"
         Ordercanal.RepLog.EventTime = NOW
         .
   END.

   FOR Order FIELDS (Brand OrderID) NO-LOCK WHERE
      Order.Brand   = "1"                         AND
      Order.OrderID = OrderCustomer.OrderID:

      CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.RowID     = STRING(ROWID(Order))
         Ordercanal.RepLog.TableName = "Order"
         Ordercanal.RepLog.EventType = "MODIFY"
         Ordercanal.RepLog.EventTime = NOW
         .
   END.
END.

&ENDIF