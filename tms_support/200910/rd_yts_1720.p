
/* update ordercustomer and order data from a  list */

DEFINE INPUT PARAMETER pcInputFile AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcOutputFile AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER llSimulated AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOut AS CHARACTER NO-UNDO. 


DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOrdererIdType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOrdererId AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCustomerIdType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCustomerId AS CHARACTER NO-UNDO. 


DEFINE STREAM sin.
DEFINE STREAM sout.

INPUT STREAM sin FROM VALUE(pcInputFile).
OUTPUT STREAM sout TO VALUE(pcOutputFile).

REPEAT:
   IMPORT STREAM sin UNFORMATTED cLine.
   lcCLI  = ENTRY(3,cLine," ").
   lcCustomerIdType = "CIF".
   lcCustomerId = ENTRY(8,cLine," ").

   FIND FIRST Order WHERE
        Order.CLI = lcCLI NO-LOCK NO-ERROR.
   IF NOT AVAIL Order THEN DO:
       lcOut = "ERROR: Order for CLI " + lcCLI + " not found." .
       PUT STREAM sout UNFORMATTED lcOut SKIP.
       NEXT.
   END.

   FIND OrderCustomer WHERE
        OrderCustomer.Brand = "1" AND
        OrderCustomer.OrderId = Order.OrderId and 
        OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.

   IF NOT AVAIL Order THEN DO:
       lcOut = "ERROR: OrderCustomer for CLI " + lcCLI + " not found." .
       PUT STREAM sout UNFORMATTED lcOut SKIP.
       NEXT.
   END.

   /* copy ordercustomer info to order */
   ASSIGN 
   lcOrdererId = OrderCustomer.CustId
   lcOrdererIdType = OrderCustomer.CustIdType .
   
   IF llSimulated THEN DO:
     lcOut = "OrdererIdType/ID : " +  lcOrdererIdType + lcOrdererId +
             " CustomerIdType/ID : " + lcCustomerIdType + lcCustomerId.
     PUT STREAM sout UNFORMATTED lcOut SKIP.
   END.
   ELSE DO:
        FIND CURRENT Order EXCLUSIVE-LOCK NO-ERROR.
          ASSIGN Order.OrdererId = lcOrdererId
                 Order.OrdererIdType = lcOrdererIdType.

        RELEASE Order.

        FIND CURRENT OrderCustomer EXCLUSIVE-LOCK NO-ERROR.
          ASSIGN OrderCustomer.CustId = lcCustomerId
                 OrderCustomer.CustIdType = lcCustomerIdType.
        RELEASE OrderCustomer.
        
        lcOut = "Done".
       PUT STREAM sout UNFORMATTED lcOut SKIP.
   END.

END.

OUTPUT STREAM sout CLOSE.
INPUT STREAM sin CLOSE.
