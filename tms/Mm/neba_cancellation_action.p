/* ---------------------------------------------------------------------------
  MODULE .......: cancel_neba_order
  FUNCTION .....: Handle NEBA permamency handling when order is cancelled
                  before fixed line installation.
                  Actions:
                  -Create customer for billing if needed
                  -Create NEBA fee for the cancelled order
  APPLICATION ..: TMS
  CREATED ......: 20.4.2018 / ilsavola
  MODIFIED .....: 
  VERSION ......: yoigo
  -------------------------------------------------------------------------- */



{Syst/tmsconst.i}
{Syst/eventval.i}

DEF INPUT  PARAMETER iiOrderId  AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocError AS CHAR NO-UNDO.

DEF VAR liCashCust AS INT NO-UNDO.
DEF VAR lcSelectedFee AS CHAR NO-UNDO.
DEF VAR ldNebaAmt AS DECIMAL NO-UNDO.


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   ocError = "Error:Neba fee not found for order " +
                                         STRING(iiOrderID).
   lhSingleFee = BUFFER SingleFee:HANDLE.

END.

/*Function contails logig for selecting correct fee in NEBA case cancellation*/
FUNCTION fSelectNebaFee RETURNS CHAR
   (INPUT iiOrderID  AS INT,
    OUTPUT odValue   AS DEC,
    OUTPUT ocFeeName AS CHAR):
   /*TODO: select correct permanency - finding method unclear...offers?*/

   odValue = 110.0.
   ocFeeName = "NEBTERMPERIOD".
   RETURN "". /*Error handling: add error message to ret valua.*/

END.   

FUNCTION fCreateSingleFee RETURNS LOGICAL
   (icBillCode  AS CHAR,
    idAmount    AS DEC,
    ilVatIncl   AS LOG,
    icFeeModel  AS CHAR,
    iiCustNum   AS INT,
    iiOrderId   AS INT):

   DO TRANS:
      CREATE SingleFee.

      ASSIGN
      SingleFee.Brand       = Syst.Var:gcBrand
      SingleFee.FMItemId    = NEXT-VALUE(bi-seq)
      SingleFee.CustNum     = iiCustnum
      SingleFee.BillTarget  = 1
      SingleFee.CalcObj     = "NBTERM"
      SingleFee.BillCode    = icBillCode
      SingleFee.BillPeriod  = YEAR(TODAY) * 100 + MONTH(TODAY)
      SingleFee.Concerns[1] = (YEAR(TODAY) * 100 + MONTH(TODAY)) * 100 + 
                               DAY(TODAY)
      SingleFee.Amt         = idAmount         /* Payment          */
      SingleFee.Memo[1]     = ""
      SingleFee.Memo[2]     = ""
      SingleFee.HostTable   = "Order"
      SingleFee.KeyValue    = STRING(iiOrderID)
      SingleFee.BillType    = "SF"
      SingleFee.Contract    = ""
      SingleFee.Active      = TRUE
      SingleFee.FeeModel    = icFeeModel
      /*SingleFee.VATIncl     = ilVatIncl*/
      . 

      IF llDoEvent THEN
         RUN StarEventMakeCreateEventWithMemo(lhSingleFee,
                                              Syst.Var:katun,
                                              "NebaFeeCreation").

      RELEASE SingleFee.
   END.

END FUNCTION.

/*Actual logic*/

FIND FIRST Order NO-LOCK WHERE 
           Order.Brand eq Syst.Var:gcBrand AND
           Order.OrderID EQ iiOrderId NO-ERROR.
IF NOT AVAILABLE OrderCustomer THEN DO:
   ocError = "Error:Order not available".
   RETURN.
END.


FIND FIRST OrderCustomer WHERE
           OrderCustomer.Brand   = Syst.Var:gcBrand       AND
           OrderCustomer.OrderID = Order.OrderID AND
           OrderCustomer.RowType = 1 NO-LOCK NO-ERROR.
IF NOT AVAILABLE OrderCustomer THEN DO:
   ocError = "Error:Customer data not available".
   RETURN.
END.

FIND Customer WHERE Customer.CustNum = OrderCustomer.CustNum
      NO-LOCK NO-ERROR.
/*If customer does not exist yet, we need to create entry for billing*/
IF NOT AVAIL Customer THEN DO:
   RUN Mm/createcustomer.p(Order.OrderId,
                           1,
                           FALSE,
                           FALSE, /* do not update existing customer */
                           OUTPUT liCashCust).
   IF liCashCust = ? OR liCashCust = 0 THEN DO:
      ocError = "Error:Customer has not been created".
      RETURN.
   END.
END.

/*Select NEBA permanency for the customer. */
lcSelectedFee =  fSelectNebaFee(iiOrderId,
                                OUTPUT ldNebaAmt,
                                OUTPUT lcSelectedFee).
IF ocError NE "" THEN RETURN. ocError = "Error:Neba fee not found for order " +
                                      STRING(iiOrderID).


/*create fee.*/
fCreateSingleFee(lcSelectedFee,
                 ldNebaAmt,
                 FALSE,
                 "",
                 liCashCust,
                 iiOrderId).

FINALLY :

fCleanEventObjects().

END.



