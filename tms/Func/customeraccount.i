
/* customeraccount.i    23.03.18/ker 
*/

/* CDS-6 CDS-12 */
FUNCTION fCreateCustomerAccount RETURNS LOGICAL
  (INPUT iiCustNum   AS INT):

   CREATE CustomerAccount.
   ASSIGN
      CustomerAccount.AccountID = NEXT-VALUE(AccountID)
      CustomerAccount.CustNum = iiCustNum 
      CustomerAccount.DefaultAcc = TRUE
      CustomerAccount.BillCycle = 1
      CustomerAccount.InvInterval = 1
      CustomerAccount.FromDate = TODAY 
      CustomerAccount.ToDate = 12/31/2049.
/*
      CustomerAccount.AccountName = 
      CustomerAccount.ShippingAddressID =       
      CustomerAccount.DueDateOffset = 
*/
   FIND FIRST Customer NO-LOCK WHERE Customer.CustNum = CustomerAccount.CustNum NO-ERROR.
   IF AVAIL Customer THEN DO:
      CustomerAccount.DelType = Customer.DelType.
      FIND FIRST InvGroup OF Customer NO-LOCK.
      CustomerAccount.InvoiceGroup = InvGroup.InvGroup.
   END.

   RETURN TRUE.
END.
/* CDS-6 CDS-12 */

