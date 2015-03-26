TRIGGER PROCEDURE FOR REPLICATION-WRITE OF OrderCustomer OLD BUFFER Oldbuf.

DEF VAR llResult AS LOG NO-UNDO.

IF NEW(OrderCustomer) THEN DO:
   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RecordId  = RECID(OrderCustomer)
      Ordercanal.RepLog.TableName = "OrderCustomer"
      Ordercanal.RepLog.EventType = "CREATE"
      Ordercanal.RepLog.KeyValue  = STRING(OrderCustomer.OrderId) + CHR(255) +
                                    STRING(OrderCustomer.RowType)
      Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
END. /* IF NEW(OrderCustomer) THEN DO: */
ELSE DO:
   BUFFER-COMPARE OrderCustomer USING OrderId RowType CustTitle FirstName
   SurName1 SurName2 ZipCode Region Address PostOffice Company DelType
   KialaCode CustId CustIdType TO Oldbuf SAVE RESULT IN llResult.
   
   IF NOT llResult THEN DO:
      CREATE Ordercanal.RepLog.
      ASSIGN
         Ordercanal.RepLog.RecordId  = RECID(OrderCustomer)
         Ordercanal.RepLog.TableName = "OrderCustomer"
         Ordercanal.RepLog.EventType = "MODIFY"
         Ordercanal.RepLog.KeyValue  = STRING(Oldbuf.OrderId) + CHR(255) +
                                       STRING(Oldbuf.RowType)
         Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).
   END. /* IF NOT llResult THEN DO:  */
END. /* ELSE DO: */
