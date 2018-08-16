&IF "{&CUSTOMERACCOUNT_I}" NE "YES"
&THEN
&GLOBAL-DEFINE CUSTOMERACCOUNT_I YES
/* customeraccount.i    23.03.18/ker 
*/

/* CDS-6 CDS-12 */
/* ************************  Function Implementations ***************** */

FUNCTION fGetDefaultCustomerAccount RETURNS INTEGER
  (INPUT iiCustNum AS INT):
   
   DEF BUFFER CustomerAccount FOR CustomerAccount.
   
   FIND FIRST CustomerAccount NO-LOCK WHERE 
              CustomerAccount.CustNum EQ iiCustNum AND
              CustomerAccount.DefaultAcc = TRUE AND
              CustomerAccount.Todate > TODAY NO-ERROR.
   IF AVAIL CustomerAccount THEN RETURN CustomerAccount.AccountID.

   RETURN 0.
END.


FUNCTION fCreateDefaultCustomerAccount RETURNS INTEGER
  (INPUT iiCustNum AS INT):

   DEF VAR liAccountID AS INT NO-UNDO. 

   DEF BUFFER CustomerAccount FOR CustomerAccount.

   liAccountID = fGetDefaultCustomerAccount(iiCustNum).
   IF liAccountID > 0 THEN RETURN liAccountID.

   CREATE CustomerAccount.
   ASSIGN
      CustomerAccount.AccountID = NEXT-VALUE(AccountID)
      CustomerAccount.CustNum = iiCustNum 
      CustomerAccount.DefaultAcc = TRUE
      CustomerAccount.FromDate = TODAY 
      CustomerAccount.ToDate = 12/31/2049.

   RETURN CustomerAccount.AccountID.
END.
/* CDS-6 CDS-12 */


/* CDS-12 CDS-13*/
FUNCTION fCloseCustomerAccount RETURNS LOGICAL
  (iiAccountID   AS INT):
     
   DEF BUFFER MobSub          FOR MobSub.
   DEF BUFFER CustomerAccount FOR CustomerAccount.

   FIND FIRST CustomerAccount NO-LOCK WHERE CustomerAccount.AccountID EQ iiAccountID NO-ERROR.
   IF NOT AVAIL CustomerAccount THEN RETURN FALSE.

   IF CustomerAccount.DefaultAcc EQ TRUE THEN RETURN FALSE.

   /* Check if this is the last subscription, if yes, close CustomerAccount. */
   IF CAN-FIND(FIRST MobSub NO-LOCK WHERE 
                     MobSub.Brand   = Syst.Var:gcBrand   AND
                     MobSub.CustNum = CustomerAccount.CustNum AND
                     MobSub.AccountID = CustomerAccount.AccountID) THEN RETURN FALSE.
      
   FIND CURRENT CustomerAccount EXCLUSIVE-LOCK.
   ASSIGN CustomerAccount.ToDate = TODAY.
   RELEASE CustomerAccount.

   RETURN TRUE.

END.
/* CDS-12 CDS-13 */

FUNCTION fReopenCustomerAccount RETURNS LOGICAL
   (INPUT iiAccountID AS INT):

   DEF BUFFER CustomerAccount FOR CustomerAccount.

   FIND FIRST CustomerAccount EXCLUSIVE-LOCK WHERE 
              CustomerAccount.AccountID EQ iiAccountID AND
              CustomerAccount.Todate <= TODAY NO-ERROR.

   IF NOT AVAIL CustomerAccount THEN RETURN FALSE.

   CustomerAccount.ToDate = 12/31/2049. 

   RELEASE CustomerAccount.

   RETURN TRUE.

END FUNCTION.      

&ENDIF
