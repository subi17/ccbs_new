DISABLE TRIGGERS FOR LOAD OF Order.
DISABLE TRIGGERS FOR LOAD OF OrderCustomer.

DEFINE VARIABLE liOrderId    AS INTEGER INITIAL -1   NO-UNDO.
DEFINE VARIABLE liCount      AS INTEGER              NO-UNDO.
DEFINE VARIABLE llContinue   AS LOGICAL INITIAL TRUE NO-UNDO.
DEFINE VARIABLE liTotalCount AS INTEGER              NO-UNDO.

DO WHILE llContinue:

   llContinue = FALSE.

   TRANSACTION_LOOP:
   DO TRANSACTION:

      FOR EACH  Order EXCLUSIVE-LOCK WHERE
                Order.Brand   = "1"       AND
                Order.OrderID > liOrderId AND
                Order.OrdererID > "" USE-INDEX OrderId,
          FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
                OrderCustomer.Brand   = "1" AND
                OrderCustomer.OrderId = Order.OrderId AND
                OrderCustomer.RowType = 1:

         /* There are old cases where Order have
            rowtype 5 (ORDERCUSTOMER_ROWTYPE_CIF_CONTACT) OrderCustomer
            which has company information (it shouldn't have).
            In these cases we incorrectly have contact person
            information stored to order.ordererid and not the rowtype 1
            person id which would be the correct one.
            => TMS doesn't have rowtype 1 person information in these cases
            => To get the same behaviour we store the rowtype 5 person information
               to the rowtype 1 authcustid
         */

         ASSIGN
            OrderCustomer.AuthCustId     = Order.OrdererID
            OrderCustomer.AuthCustIdType = Order.OrdererIDType
            Order.OrdererID              = ""
            Order.OrdererIDType          = ""
            liCount                      = liCount + 1
            liTotalCount                 = liTotalCount + 1.

         IF liTotalCount MOD 1000 = 0 THEN DO: DISP liTotalCount. PAUSE 0. END.
         IF liCount > 5000
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
