/* customer_address.i    11.03.09/aam 
*/
{Syst/commali.i}

DEF VAR lcCustName    AS CHAR NO-UNDO.
DEF VAR lcFirstName   AS CHAR NO-UNDO.
DEF VAR lcContPhone   AS CHAR NO-UNDO.
DEF VAR lcAddress     AS CHAR NO-UNDO.
DEF VAR lcZipCode     AS CHAR NO-UNDO.
DEF VAR lcRegion      AS CHAR NO-UNDO. 
DEF VAR lcPost        AS CHAR NO-UNDO.
DEF VAR lcCountry     AS CHAR NO-UNDO.
DEF VAR lcCountryName AS CHAR NO-UNDO.
DEF VAR lcBankOffice  AS CHAR NO-UNDO.
DEF VAR lcBankAcc     AS CHAR NO-UNDO.
DEF VAR liLanguage    AS INT  NO-UNDO.

FUNCTION fCheckAddress RETURNS CHARACTER
   (icName            AS CHAR,
    icZipCode         AS CHAR,
    icCountry         AS CHAR,
    OUTPUT ocNCountry AS CHAR):

   DEF VAR lcErrMess AS CHAR NO-UNDO.
   DEF VAR liCnt     AS INT  NO-UNDO.
   
   ASSIGN lcErrMess  = ""        
          ocNCountry = "".
          
   icCountry = RIGHT-TRIM(icCountry).
   
   /* check that name is valid */
   IF icName = "" THEN lcErrMess = "Name empty". 

   /* foreign country */
   ELSE IF LOOKUP(icCountry,"ES,SPAIN,") = 0 THEN DO:
      IF LENGTH(icCountry) = 2 
      THEN FIND Country WHERE Country.Country = icCountry
           NO-LOCK NO-ERROR.
      ELSE FIND FIRST Country WHERE Country.CoName = icCountry 
           NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Country OR 
         LENGTH(Country.Country) NE 2
      THEN lcErrMess = "Invalid country " + icCountry.
      ELSE ocNCountry = Country.Country.
   END.        
       
   /* spanish postcode */
   ELSE DO:
       liCnt = INTEGER(icZipCode) NO-ERROR.

       IF ERROR-STATUS:ERROR OR liCnt = 0 OR 
          LENGTH(TRIM(icZipCode)) NE 5 
       THEN lcErrMess = "Invalid zip code " + icZipCode. 

       ocNCountry = "ES".
   END.

   RETURN lcErrMess. 

END FUNCTION.

FUNCTION fSetCustData RETURNS LOGICAL:

   ASSIGN 
      lcCustName   = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                           BUFFER Customer)
      lcFirstName  = IF Customer.CustIDType NE "CIF" AND 
                        Customer.FirstName > ""
                     THEN Customer.FirstName 
                     ELSE lcCustName
      lcAddress    = Customer.Address
      lcZipCode    = Customer.ZipCode 
      lcPost       = Customer.PostOffice
      lcCountry    = Customer.Country
      lcContPhone  = ""
      lcBankOffice = ""
      lcBankAcc    = ""
      liLanguage   = IF Customer.Language = 4
                     THEN 1
                     ELSE Customer.Language.

   FIND Region WHERE Region.Region = Customer.Region NO-LOCK NO-ERROR.
   IF AVAILABLE Region 
   THEN lcRegion = Region.RgName.
   ELSE lcRegion = Customer.Region.
   
   /* all name and address data in capital letters */
   ASSIGN lcCustName   = CAPS(lcCustName)
          lcFirstName  = CAPS(lcFirstName)
          lcAddress    = CAPS(lcAddress)
          lcPost       = CAPS(lcPost)
          lcCountry    = CAPS(lcCountry)
          lcRegion     = CAPS(lcRegion).
          
   /* bank */
   IF Customer.BankAcc > "" THEN DO:
   
      lcBankAcc = Customer.BankAcc.
      
      /* hide last digits, add spaces */
      IF LENGTH(lcBankAcc) > 4 THEN 
         SUBSTRING(lcBankAcc,LENGTH(lcBankAcc) - 3) = FILL("*",4).
         
      IF LENGTH(lcBankAcc) > 10 THEN 
         lcBankAcc = SUBSTRING(lcBankAcc,1,4) + " " + 
                     SUBSTRING(lcBankAcc,5,4) + " " +
                     SUBSTRING(lcBankAcc,9,4) + " " +
                     SUBSTRING(lcBankAcc,13,2) + " " +
                     SUBSTRING(lcBankAcc,15). 
      
      FIND FIRST Bank WHERE
                 Bank.Brand      = gcBrand AND
                 Bank.BankID     = SUBSTRING(Customer.BankAcc,5,4) AND
                 Bank.BankOffice = SUBSTRING(Customer.BankAcc,9,4) 
      NO-LOCK NO-ERROR.
      IF AVAILABLE Bank THEN lcBankOffice = Bank.Name.
   END.
   
   IF Customer.SMSNumber > "" THEN lcContPhone = Customer.SMSNumber.
   ELSE lcContPhone = Customer.Phone.
   
END FUNCTION.

