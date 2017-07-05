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
                Order.OrdererID > "" USE-INDEX OrderId:

         /* If there is a rowtype 5 (ORDERCUSTOMER_ROWTYPE_CIF_CONTACT)
            OrderCustomer for the order then we only have contact person
            information in TMS and not the normal personid which comes
            with rowtype 1 but was overwritten by rowtype 5!!! */
         FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
                    OrderCustomer.Brand   = "1" AND
                    OrderCustomer.OrderId = Order.OrderId AND
                    OrderCustomer.RowType = 5 NO-ERROR.

         /* Lets verify still that the rowtype 5 contains the
            company information otherwise the rowtype 1
            personid information was stored to the order */
         IF NOT AVAILABLE OrderCustomer OR OrderCustomer.CustIDType NE "CIF"
         THEN FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
                         OrderCustomer.Brand   = "1" AND
                         OrderCustomer.OrderId = Order.OrderId AND
                         OrderCustomer.RowType = 1 NO-ERROR.

         /* NOTE: OrderCustomer with rowtype 1 won't have
                  authcustid data when it was taken from rowtype 5
                  after the program has been run!!! */
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
