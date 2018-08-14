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
     
   DEF VAR liMobSubCount AS INT NO-UNDO.     

   DEF BUFFER bMobSub   FOR MobSub.

   FIND FIRST CustomerAccount EXCLUSIVE-LOCK WHERE CustomerAccount.AccountID EQ iiAccountID NO-ERROR.
   IF NOT AVAIL CustomerAccount THEN DO:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
             ErrorLog.ActionID  = "CloseCustomerAccount"
             ErrorLog.TableName = "CustomerAccount"
             ErrorLog.KeyValue  = STRING(iiAccountID) 
             ErrorLog.ErrorMsg  = "Customer Account not found"
             ErrorLog.UserCode  = Syst.Var:katun
             ErrorLog.ActionTS  = Func.Common:mMakeTS().
      RETURN FALSE.
   END.

   /* Check if this is the last subscription, if yes, close CustomerAccount. */
   FOR EACH bMobSub WHERE bMobSub.Brand   = Syst.Var:gcBrand   AND
                          bMobSub.CustNum = CustomerAccount.CustNum NO-LOCK:
      liMobSubCount = liMobSubCount + 1.
   END.      
      
   IF liMobSubCount = 1 THEN CustomerAccount.ToDate = TODAY.
   RETURN TRUE.

END.
/* CDS-12 CDS-13 */



FUNCTION fUpdateCustomerAccountDelType RETURNS LOGICAL
   (INPUT iiCustNum AS INT,
    INPUT iiDelType AS INT): 
 
   FIND FIRST CustomerAccount EXCLUSIVE-LOCK WHERE 
      CustomerAccount.Custnum EQ iiCustNum NO-ERROR.
   IF NOT AVAIL CustomerAccount THEN DO:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
             ErrorLog.ActionID  = "UpdateCustomerAccount"
             ErrorLog.TableName = "CustomerAccount"
             ErrorLog.KeyValue  = STRING(iiCustNum) 
             ErrorLog.ErrorMsg  = "Customer Account not found"
             ErrorLog.UserCode  = Syst.Var:katun
             ErrorLog.ActionTS  = Func.Common:mMakeTS().
      RETURN FALSE.
   END.   
   
   RETURN TRUE.

END FUNCTION.



FUNCTION fReopenCustomerAccount RETURNS LOGICAL
   (INPUT iiAccountID AS INT):

   FIND FIRST CustomerAccount EXCLUSIVE-LOCK WHERE CustomerAccount.AccountID EQ iiAccountID NO-ERROR.
   IF AVAIL CustomerAccount AND CustomerAccount.ToDate = TODAY THEN 
      /* ToDate = TODAY means that there are no other subscriptions yet active */
      CustomerAccount.ToDate = 12/31/2049. 

   RETURN TRUE.

END FUNCTION.      



FUNCTION fUpdateAccountID RETURNS LOGICAL
  (INPUT iiCustNum   AS INT):  

   DEF VAR liAccountID           AS INT NO-UNDO.
    
   DEF BUFFER bMobSub            FOR MobSub.
   DEF BUFFER bCustomerAccount   FOR CustomerAccount.

   FIND FIRST bCustomerAccount NO-LOCK WHERE bCustomerAccount.Custnum EQ iiCustNum NO-ERROR.
   IF AVAIL bCustomerAccount THEN DO:
      liAccountID = bCustomerAccount.AccountID.
      FIND FIRST bMobSub EXCLUSIVE-LOCK WHERE bMobSub.Custnum EQ iiCustNum NO-ERROR.
      IF AVAIL bMobSub THEN 
         bMobSub.AccountID = liAccountID.
   END.
   
   FOR EACH MsOwner EXCLUSIVE-LOCK WHERE MsOwner.Brand EQ Syst.Var:gcBrand AND 
                                           MsOwner.Custnum EQ iiCustNum.
   IF AVAIL MsOwner THEN
      ASSIGN MsOwner.AccountID = liAccountID.
   END.   
   
   RETURN TRUE.

END FUNCTION. 


&ENDIF
