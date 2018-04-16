&IF "{&ADDRESS_I}" NE "YES"
&THEN
&GLOBAL-DEFINE ADDRESS_I YES
/* address.i    12.04.18/ker 
*/

FUNCTION fUpdateAddress RETURNS LOGICAL
   (INPUT iiCustNum AS INT,
    INPUT icAddress AS CHAR,
    INPUT icPostOffice AS CHAR,
    INPUT icZipCode AS CHAR,
    INPUT icRegion AS CHAR,
    INPUT icCountry AS CHAR): 


   FIND FIRST Address WHERE Address.Keyvalue = STRING(iiCustNum) EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL Address THEN DO:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
             ErrorLog.ActionID  = "UpdateAddress"
             ErrorLog.TableName = "Address"
             ErrorLog.KeyValue  = STRING(iiCustNum) 
             ErrorLog.ErrorMsg  = "InvoiceTargetGroup not found"
             ErrorLog.UserCode  = Syst.Var:katun
             ErrorLog.ActionTS  = Func.Common:mMakeTS().
      RETURN FALSE.
   END.
   IF Address.AddressType = "Billing" THEN DO:     
      ASSIGN
         Address.Address = icAddress
         Address.City = icPostOffice
         Address.ZipCode = icZipCode
         Address.Region = icRegion
         Address.Country = icCountry.

      FIND FIRST CustomerReport WHERE CustomerReport.Custnum = iiCustNum
      NO-LOCK NO-ERROR.

      IF AVAIL CustomerReport THEN
         ASSIGN
            Address.StreetCode = CustomerReport.StreetCode
            Address.CityCode = CustomerReport.CityCode
            Address.TownCode = CustomerReport.TownCode.
   END.            
   
   RETURN TRUE.

END FUNCTION.


FUNCTION fUpdateInvTargetGrpBankAccnt RETURNS LOGICAL
   (INPUT iiCustNum AS INT,
    INPUT icBankAcct AS CHAR):

   FIND FIRST InvoiceTargetGroup EXCLUSIVE-LOCK USE-INDEX Custnum WHERE
              InvoiceTargetGroup.Custnum = iiCustNum NO-ERROR.
   IF NOT AVAIL InvoiceTargetGroup THEN DO:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
             ErrorLog.ActionID  = "UpdateInvoiceTargetGroupBankAccount"
             ErrorLog.TableName = "InvoiceTargetGroup"
             ErrorLog.KeyValue  = STRING(InvoiceTargetGroup.Custnum) 
             ErrorLog.ErrorMsg  = "InvoiceTargetGroup not found"
             ErrorLog.UserCode  = Syst.Var:katun
             ErrorLog.ActionTS  = Func.Common:mMakeTS().
      RETURN FALSE.
   END.       
      
   InvoiceTargetGroup.BankAccount = icBankAcct.

   RETURN TRUE.

END FUNCTION.



&ENDIF
