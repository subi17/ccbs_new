&IF "{&CUSTOMERACCOUNT_I}" NE "YES"
&THEN
&GLOBAL-DEFINE CUSTOMERACCOUNT_I YES
/* customeraccount.i    23.03.18/ker 
*/

/* CDS-6 CDS-12 */
/* ************************  Function Implementations ***************** */
FUNCTION fCreateCustomerAccount RETURNS LOGICAL
  (INPUT iiCustNum   AS INT):

   DEF BUFFER Customer FOR Customer.

   FIND Customer NO-LOCK WHERE 
        Customer.CustNum = iiCustNum NO-ERROR.
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
  (iiAccountID   AS INT):
     
   DEF VAR liMobSubCount AS INT NO-UNDO.     

   DEF BUFFER bMobSub   FOR MobSub.

   FIND FIRST CustomerAccount EXCLUSIVE-LOCK WHERE CustomerAccount.AccountID EQ iiAccountID NO-ERROR.
   IF AVAIL CustomerAccount THEN DO: 
      /* Check if this is the last subscription, if yes, close CustomerAccount. */
      FOR EACH bMobSub WHERE bMobSub.Brand   = Syst.Var:gcBrand   AND
                            bMobSub.CustNum = CustomerAccount.CustNum NO-LOCK:
         liMobSubCount = liMobSubCount + 1.
      END.      
      
      IF liMobSubCount = 1 THEN CustomerAccount.ToDate = TODAY.
   END.
   
   RETURN TRUE.
END.
/* CDS-12 CDS-13 */



FUNCTION fUpdateCustomerAccountDelType RETURNS LOGICAL
   (INPUT iiCustNum AS INT,
    INPUT iiDelType AS INT): 
 
   FIND FIRST CustomerAccount EXCLUSIVE-LOCK WHERE 
      CustomerAccount.Custnum EQ iiCustNum NO-ERROR.
   IF AVAIL CustomerAccount THEN   
      CustomerAccount.DelType = iiDelType.

   RETURN TRUE.

END FUNCTION.



FUNCTION fReopenCustomerAccount RETURNS LOGICAL
   (INPUT iiAccountID AS INT):

   FIND FIRST CustomerAccount EXCLUSIVE-LOCK WHERE CustomerAccount.AccountID EQ iiAccountID NO-ERROR.
   IF AVAIL CustomerAccount AND CustomerAccount.ToDate = TODAY THEN 
      /* ToDate = TODAY means that there are no other subscriptions yet active */
      CustomerAccount.ToDate = 12/31/2049. 

END FUNCTION.      


&ENDIF
