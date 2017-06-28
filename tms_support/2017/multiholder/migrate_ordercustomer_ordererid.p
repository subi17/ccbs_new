DEFINE VARIABLE liOrderId  AS INTEGER INITIAL -1   NO-UNDO.
DEFINE VARIABLE liCount    AS INTEGER              NO-UNDO.
DEFINE VARIABLE llContinue AS LOGICAL INITIAL TRUE NO-UNDO.

DO WHILE llContinue:

   llContinue = FALSE.
   
   TRANSACTION_LOOP:
   DO TRANSACTION:
   
      FOR EACH  Order EXCLUSIVE-LOCK WHERE
                Order.Brand   = "1" AND
                Order.OrderID > liOrderId USE-INDEX OrderId,
          FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
                OrderCustomer.Brand   = "1" AND
                OrderCustomer.OrderId = Order.OrderId AND
                OrderCustomer.RowType = 1:
   
         ASSIGN
            OrderCustomer.AuthCustId     = Order.OrdererID
            OrderCustomer.AuthCustIdType = Order.OrdererIDType
            Order.OrdererID              = ""
            Order.OrdererIDType          = ""
            liCount                      = liCount + 1.
   
         IF liCount > 50000
         THEN DO:
            ASSIGN
               liOrderId  = Order.OrderId
               liCount    = 0
               llContinue = TRUE.
            LEAVE TRANSACTION_LOOP.
         END.
      END.
      
   END.
END.