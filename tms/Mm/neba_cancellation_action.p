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
              OrderAction.Brand EQ Syst.Var:gcBrand AND
              OrderAction.Orderid EQ iiOrderId AND
              OrderAction.itemkey BEGINS "nebterm" NO-ERROR.
   IF NOT AVAIL OrderAction THEN RETURN "No NEBA orderaction".

   FIND FIRST DayCampaign NO-LOCK WHERE
              DayCampaign.brand EQ Syst.Var:gcBrand AND
              DayCampaign.dcevent eq OrderAction.ItemKey NO-ERROR.
   IF NOT AVAIL DayCampaign THEN RETURN "No NEBA dayycampaign".

   FIND FIRST FMItem NO-LOCK WHERE
              FMItem.Brand  EQ DayCampaign.Brand AND
              FMItem.FeeModel EQ DayCampaign.TermFeeModel NO-ERROR.
   IF NOT AVAIL FMItem THEN RETURN "No NEBA fmitem".

   odValue = FMItem.Amount.
   ocFeeName = FMItem.BillCode.
   RETURN "".
END.

FUNCTION fCreateSingleFee RETURNS LOGICAL
   (icBillCode  AS CHAR,
    idAmount    AS DEC,
    ilVatIncl   AS LOG,
    icFeeModel  AS CHAR,
    iiCustNum   AS INT,
    iiOrderId   AS INT):
   DEF VAR liOrdreID AS INT NO-UNDO. 

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
      SingleFee.VATIncl     = ilVatIncl
      . 

      IF llDoEvent THEN
         RUN StarEventMakeCreateEventWithMemo(lhSingleFee,
                                              Syst.Var:katun,
                                              "NebaFeeCreation").

      RELEASE SingleFee.
   END.

END FUNCTION.

/* MAIN starts */

FIND FIRST Order NO-LOCK WHERE 
           Order.Brand eq Syst.Var:gcBrand AND
           Order.OrderID EQ iiOrderId NO-ERROR.
IF NOT AVAILABLE Order THEN DO:
   ocError = "Error:Order not available".
   RETURN.
END.


FIND FIRST OrderCustomer WHERE
           OrderCustomer.Brand   EQ Syst.Var:gcBrand       AND
           OrderCustomer.OrderID EQ Order.OrderID AND
           OrderCustomer.RowType EQ 1 NO-LOCK NO-ERROR.
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
IF ocError NE "" THEN DO:
   ocError = "Error:" + ocError.
   RETURN.
END.  

FIND FIRST Customer WHERE Customer.CustNum EQ liCashCust NO-LOCK NO-ERROR.

IF NOT AVAIL Customer THEN DO:
   ocError = "Error:Customer creation failed".
   RETURN.
END.

/*create fee.*/
fCreateSingleFee(lcSelectedFee,
                 ldNebaAmt,
                 Customer.VATIncl,
                 "",
                 liCashCust,
                 iiOrderId).

FINALLY :

fCleanEventObjects().

END.



