/* CDS-6 */
FUNCTION fCreateCustomerAccount RETURNS LOGICAL
  (INPUT iiOrder   AS INT,
   INPUT iiTarget  AS INT):

DEFINE VARIABLE ldaOrderDate       AS DATE      NO-UNDO. 
DEFINE VARIABLE liTime             AS INTEGER   NO-UNDO.

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand   = Syst.Var:gcBrand AND
              Order.OrderId = iiOrder
              NO-ERROR.
   IF NOT AVAILABLE Order THEN RETURN FALSE. 

   Func.Common:mSplitTS(Order.CrStamp, OUTPUT ldaOrderDate, OUTPUT liTime).

   FIND FIRST OrderCustomer EXCLUSIVE-LOCK WHERE
              OrderCustomer.Brand   = Syst.Var:gcBrand AND
              OrderCustomer.OrderID = iiOrder AND
              OrderCustomer.RowType = iiTarget NO-ERROR.
   IF NOT AVAILABLE OrderCustomer THEN RETURN FALSE. 

   CREATE CustomerAccount.
   ASSIGN
      CustomerAccount.AccountID = NEXT-VALUE(AccountID)
      CustomerAccount.CustNum = OrderCustomer.CustNum 
      CustomerAccount.DefaultAcc = TRUE
      CustomerAccount.BillCycle = 1
      CustomerAccount.InvInterval = 1
      CustomerAccount.DelType = OrderCustomer.DelType
      CustomerAccount.FromDate = TODAY 
      CustomerAccount.ToDate = 12/31/2049.
/*
      CustomerAccount.AccountName = 
      CustomerAccount.ShippingAddressID =       
      CustomerAccount.DueDateOffset = 
*/
   FIND FIRST Customer NO-LOCK WHERE Customer.CustNum = CustomerAccount.CustNum NO-ERROR.
   IF AVAIL Customer THEN DO:
      FIND FIRST InvGroup OF Customer NO-LOCK.
      CustomerAccount.InvoiceGroup = InvGroup.InvGroup.
   END.

   RETURN TRUE.
END.
/* CDS-6 */

