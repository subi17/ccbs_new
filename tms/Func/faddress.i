/* faddress.i       13.04.04/aam 

   get address for a letter 
   correct customer (and msowner) should be in buffer when this is called 
   
   changes:         14.12.05/aam username from customer, not msowner
                    18.01.06/aam use fPrintCustName()
                    25.01.06/aam type 5 = invoice customer
                    27.01.06/aam lastname and firstname 
                    01.12.06/aam custidtype, custid, bankacc, email
*/

DEF BUFFER bOwner FOR Customer.
      
FUNCTION fTargetAddress RETURNS LOGICAL
   (iiAddress AS INT).

   IF (iiAddress = 4 AND Customer.AgrCust NE Customer.CustNum) OR
      (iiAddress = 5 AND Customer.InvCust NE Customer.CustNum) 
   THEN DO:
      IF iiAddress = 4 THEN  
         FIND bOwner WHERE 
              bOwner.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
      ELSE      
         FIND bOwner WHERE 
              bOwner.CustNum = Customer.InvCust NO-LOCK NO-ERROR.
              
      IF AVAILABLE bOwner THEN 
      ASSIGN lcEPLRName    = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                              BUFFER bOwner)
             lcEPLRLast    = IF bOwner.FirstName > ""
                             THEN bOwner.CustName + 
                                  (IF bOwner.SurName2 > ""
                                   THEN " " + bOwner.SurName2
                                   ELSE "")
                             ELSE bOwner.CompanyName
             lcEPLRFirst   = bOwner.FirstName
             lcEPLRCoName  = bOwner.CoName
             lcEPLRAddr    = bOwner.Address
             lcEPLRZipCode = bOwner.ZipCode
             lcEPLRPost    = bOwner.ZipCode + " " + bOwner.PostOffice
             lcEPLRCountry = bOwner.Country.
             
      &IF "{&SetLastFirstName}" = "YES"
      &THEN ASSIGN lcTagLastName   = lcEPLRLast
                   lcTagFirstName  = lcEPLRFirst
                   lcTagCustIDType = bOwner.CustIDType
                   lcTagCustID     = bOwner.OrgID
                   lcTagBankAcc    = bOwner.BankAcc
                   lcCustEMail     = bOwner.EMail.
      &ENDIF             
   END.
   
   ELSE IF iiAddress = 2 THEN DO:
      ASSIGN lcEPLRName    = Customer.IDelName
             lcEPLRLast    = Customer.IDelName
             lcEPLRFirst   = ""
             lcEPLRCoName  = Customer.IDelCoName
             lcEPLRAddr    = Customer.IDelAddr
             lcEPLRZipCode = Customer.IDelZipCode
             lcEPLRPost    = Customer.IDelZipCode + " " + 
                             Customer.IDelPost
             lcEPLRCountry = Customer.IDelCountry.
      
      &IF "{&SetLastFirstName}" = "YES"
      &THEN ASSIGN lcTagLastName   = Customer.IDelName
                   lcTagFirstName  = ""
                   lcTagCustIDType = ""
                   lcTagCustID     = ""
                   lcTagBankAcc    = ""
                   lcCustEMail     = "".
      &ENDIF             
   END.
   
   ELSE DO:
      ASSIGN lcEPLRName    = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                              BUFFER Customer)
             lcEPLRLast    = IF Customer.FirstName > ""
                             THEN Customer.CustName + 
                                  (IF Customer.SurName2 > ""
                                   THEN " " + Customer.SurName2
                                   ELSE "")
                             ELSE Customer.CompanyName
             lcEPLRFirst   = Customer.FirstName
             lcEPLRCoName  = Customer.CoName
             lcEPLRAddr    = Customer.Address
             lcEPLRZipCode = Customer.ZipCode
             lcEPLRPost    = Customer.ZipCode + " " + Customer.PostOffice
             lcEPLRCountry = Customer.Country.
             
      &IF "{&SetLastFirstName}" = "YES"
      &THEN ASSIGN lcTagLastName   = lcEPLRLast
                   lcTagFirstName  = lcEPLRFirst
                   lcTagCustIDType = Customer.CustIDType
                   lcTagCustID     = Customer.OrgID
                   lcTagBankAcc    = Customer.BankAcc
                   lcCustEMail     = Customer.EMail.
      &ENDIF             
             
   END.

   /* take only one firstname */
   &IF "{&SetLastFirstName}" = "YES"
   &THEN IF INDEX(lcTagFirstName," ") > 1
         THEN lcTagFirstName = SUBSTRING(lcTagFirstName,1,
                                         INDEX(lcTagFirstName," ") - 1).
   &ENDIF             

END FUNCTION.

