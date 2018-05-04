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
   lhSingleFee = BUFFER SingleFee:HANDLE.

END.
FUNCTION fSelectNebaFee RETURNS CHAR
   (INPUT iiOrderID  AS INT,
    OUTPUT odValue   AS DEC,
    OUTPUT ocFeeName AS CHAR):

   odValue = 0.0.
   ocFeeName = "".

   FIND FIRST OrderAction NO-LOCK WHERE
            OrderAction.brand eq "1" and
            OrderAction.orderid eq 70190910 and
            OrderAction.itemkey begins "nebterm" NO-ERROR.
   IF NOT AVAIL OrderAction then RETURN "No NEBA orderaction".
   FIND FIRST DayCampaign NO-LOCK WHERE
               DayCampaign.brand eq "1" and
               DayCampaign.dcevent eq OrderAction.ItemKey NO-ERROR.
   IF NOT AVAIL daycampaign THEN RETURN "No NEBA dayycampaign".
   FIND FIRST FMItem NO-LOCK WHERE
               FMItem.Brand  EQ DayCampaign.Brand AND
               FMItem.FeeModel EQ DayCampaign.TermFeeModel NO-ERROR.
   IF AVAIL FMItem THEN DO:
            disp fmitem.feemodel FORMAT "X(30)".
            disp fmitem.amount.
            odValue = FMItem.Amount.
            ocFeeName = FMItem.FeeModel.
            RETURN "".
   END.
   RETURN "No NEBA fmitem".
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
      SingleFee.CalcObj     = "NEBTERM"
      SingleFee.BillCode    = icBillCode
      SingleFee.BillPeriod  = YEAR(TODAY) * 100 + MONTH(TODAY)
      SingleFee.Concerns[1] = (YEAR(TODAY) * 100 + MONTH(TODAY)) * 100 + 
                               DAY(TODAY)
      SingleFee.Amt         = idAmount         /* Payment          */
      SingleFee.Memo[1]     = ""
      SingleFee.Memo[2]     = ""
      SingleFee.HostTable   = "Order"
      SingleFee.KeyValue    = STRING(iiOrderId)
      SingleFee.BillType    = "SF"
      SingleFee.Contract    = ""
      SingleFee.Active      = TRUE
      SingleFee.FeeModel    = icFeeModel
      SingleFee.OrderID     = iiOrderID
      Singlefee.Memo        = "Neba order tremination"
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
IF NOT AVAILABLE Order THEN DO:
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
ELSE DO:
   /*Customer already exists*/
   liCashCust = Customer.Custnum.
END.

/*Select NEBA permanency for the customer. */
ocError =  fSelectNebaFee(iiOrderId,
                                OUTPUT ldNebaAmt,
                                OUTPUT lcSelectedFee).
IF ocError NE "" THEN RETURN. 

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



