{Syst/commpaa.i}
gcBrand = "1".

DEF STREAM sOL.
DEF STREAM sOO.

DEF VAR liOrderID   AS INT  NO-UNDO.
DEF VAR liCustomer  AS INT  NO-UNDO.
DEF VAR llCorporate AS LOG  NO-UNDO.
DEF VAR lcError     AS CHAR NO-UNDO.

INPUT STREAM sOL FROM "orderlist.txt".
OUTPUT STREAM sOO TO "YDR_2327_Customer_Creation.log".
PUT STREAM sOO UNFORMATTED
    "OrderID;CustNum;New/Existing;ErrorText"
    SKIP.
REPEAT TRANSACTION:
   ASSIGN liOrderID   = 0
          liCustomer  = 0
          lcError     = ""
          llCorporate = FALSE.

   IMPORT STREAM sOL UNFORMATTED liOrderID.
   
   IF liOrderID = 0 THEN NEXT.

   PUT STREAM sOO UNFORMATTED liOrderID ";".
   FIND FIRST Order NO-LOCK WHERE
              Order.Brand   = gcBrand   AND
              Order.OrderID = liOrderID NO-ERROR.
   IF AVAILABLE Order AND Order.CustNum = 0 THEN
   DO:
      FIND FIRST OrderCustomer NO-LOCK WHERE
                 OrderCustomer.Brand   = gcBrand       AND
                 OrderCustomer.OrderID = Order.OrderID AND
                 OrderCustomer.RowType = 1 NO-ERROR.
      IF AVAILABLE OrderCustomer THEN DO:
         FIND FIRST Customer NO-LOCK WHERE
                    Customer.Brand      = gcBrand                  AND
                    Customer.OrgID      = OrderCustomer.CustID     AND
                    Customer.CustIDType = OrderCustomer.CustIDType NO-ERROR.
         IF AVAILABLE Customer THEN DO:
            FIND CURRENT Order EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN Order.CustNum = Customer.CustNum.
            PUT STREAM sOO UNFORMATTED
                Order.CustNum ";"
                "Existing;Customer already exist in TMS so assigning it"
                SKIP.
            RELEASE Order.
            NEXT.
         END.
      END.

      RUN Mm/createcustomer.p(INPUT Order.OrderID,1,FALSE,TRUE,OUTPUT liCustomer).
      
      PUT STREAM sOO UNFORMATTED
          liCustomer ";"
          "New;Customer Created in TMS".
      llCorporate = CAN-FIND(OrderCustomer WHERE
                             OrderCustomer.Brand      = gcBrand       AND
                             OrderCustomer.OrderID    = Order.OrderID AND
                             OrderCustomer.RowType    = 1             AND
                             OrderCustomer.CustIdType = "CIF").

      FOR EACH OrderCustomer NO-LOCK WHERE
               OrderCustomer.Brand   = gcBrand AND
               OrderCustomer.OrderID = Order.OrderID:
         IF llCorporate AND (OrderCustomer.RowType = 1 OR OrderCustomer.RowType = 5) THEN DO:
            RUN Mm/createcustcontact.p(OrderCustomer.OrderID,
                                    liCustomer,
                                    OrderCustomer.RowType,
                                    OUTPUT lcError).
            IF lcError > "" THEN DO:
               DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                                "Order",
                                STRING(OrderCustomer.OrderID),
                                liCustomer,
                                "CUSTOMER CONTACT CREATION FAILED",
                                lcError).
            END.
         END.
         ELSE IF OrderCustomer.RowType = 1 AND NOT Order.PayType THEN
         DO:
            FIND FIRST Customer EXCLUSIVE-LOCK WHERE
                       Customer.CustNum = liCustomer NO-ERROR.
            IF AVAILABLE Customer THEN DO:
               ASSIGN Customer.AuthCustID     = Order.OrdererID
                      Customer.AuthCustIDType = Order.OrdererIDType.
               RELEASE Customer.
            END.
         END.
      END.
      IF lcError > "" THEN
         PUT STREAM sOO UNFORMATTED
             lcError.
   END.
   ELSE
      PUT STREAM sOO UNFORMATTED
          ";;Invalid OrderID".
   PUT STREAM sOO UNFORMATTED SKIP.
END.

INPUT CLOSE.
