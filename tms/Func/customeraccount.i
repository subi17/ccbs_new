&IF "{&CUSTOMERACCOUNT_I}" NE "YES"
&THEN
&GLOBAL-DEFINE CUSTOMERACCOUNT_I YES
/* customeraccount.i    23.03.18/ker 
*/

/* CDS-6 CDS-12 */
FUNCTION fCreateCustomerAccount RETURNS LOGICAL
  (INPUT iiCustNum   AS INT):

   DEF BUFFER Customer FOR Customer.

   FIND Customer NO-LOCK WHERE 
        Customer.CustNum = CustomerAccount.CustNum NO-ERROR.
   IF NOT AVAIL Customer THEN RETURN FALSE.

   CREATE CustomerAccount.
   ASSIGN
      CustomerAccount.AccountID = NEXT-VALUE(AccountID)
      CustomerAccount.CustNum = iiCustNum 
      CustomerAccount.DefaultAcc = TRUE
      CustomerAccount.BillCycle = 1
      CustomerAccount.InvInterval = 1
      CustomerAccount.FromDate = TODAY 
      CustomerAccount.ToDate = 12/31/2049
      CustomerAccount.DelType = Customer.DelType
      CustomerAccount.InvoiceGroup = Customer.InvGroup.
/*
      CustomerAccount.AccountName = 
      CustomerAccount.ShippingAddressID =       
      CustomerAccount.DueDateOffset = 
*/
   RETURN TRUE.
END.
/* CDS-6 CDS-12 */


/* CDS-12 CDS-13*/
FUNCTION fCloseCustomerAccount RETURNS LOGICAL
  (iiCustNum   AS INT):

   IF NOT CAN-FIND(FIRST MobSub NO-LOCK WHERE
               Mobsub.Brand    = Syst.Var:gcBrand AND
               MobSub.CustNum  = iiCustNum) THEN
      FIND FIRST CustomerAccount NO-LOCK WHERE CustomerAccount.Custnum EQ MobSub.CustNum NO-ERROR.
      IF AVAIL CustomerAccount THEN 
         CustomerAccount.ToDate = TODAY.     

   RETURN TRUE.
END.
/* CDS-12 CDS-13 */

&ENDIF
