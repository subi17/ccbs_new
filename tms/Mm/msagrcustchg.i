/* msagrcustchg.i   29.04.09/aam 
*/
&IF "{&MSAGRCUSTCHG_I}" NE "YES"
&THEN
&GLOBAL-DEFINE MSAGRCUSTCHG_I YES

{Syst/tmsconst.i}
{Func/fcustdata.i}

DEF TEMP-TABLE ttCustomer NO-UNDO LIKE Customer
   FIELD cBirthDay AS CHAR
   FIELD CityCode LIKE CustomerReport.CityCode
   FIELD StreetCode LIKE CustomerReport.StreetCode
   FIELD TownCode LIKE CustomerReport.TownCode.

FUNCTION fCreateAccDataParam RETURNS CHAR
   (INPUT ihCustomer AS HANDLE,
    INPUT icSalesMan AS CHAR,
    INPUT icAgrCustIdType AS CHAR,
    INPUT icAgrCustId AS CHAR,
    INPUT icStreetCode AS CHAR,
    INPUT icCityCode AS CHAR,
    INPUT icTownCode AS CHAR,
    INPUT icMandateId AS CHAR,
    OUTPUT ocError AS CHAR):

   DEF VAR lcCode AS CHAR NO-UNDO. 
   
   lcCode = 
      trim(ihCustomer::Custname) + ";" + 
      trim(ihCustomer::FirstName) + ";" +
      trim(ihCustomer::Surname2) + ";" +
      TRIM(ihCustomer::COName) + ";" + 
      TRIM(ihCustomer::Companyname) + ";" +
      TRIM(ihCustomer::Address) + ";" + 
      TRIM(ihCustomer::ZipCode) + ";" +
      TRIM(ihCustomer::PostOffice) + ";" +
      TRIM(ihCustomer::Country) + ";" +
      TRIM(ihCustomer::EMail) + ";" +
      icSalesMan + ";" +
      TRIM(ihCustomer::CustIdType) + ";" +
      TRIM(ihCustomer::OrgId) + ";" +
      (IF ihCustomer::BirthDay NE ? THEN
       STRING(ihCustomer::BirthDay, "99-99-9999") ELSE "") + ";" +
      STRING(ihCustomer::Language) + ";" + 
      TRIM(ihCustomer::HonTitle) + ";" +
      TRIM(ihCustomer::Region) + ";" +
      TRIM(ihCustomer::BankAcct) + ";" +
      TRIM(ihCustomer::Nationality) + ";" +
      (IF ihCustomer::FoundationDate NE ? THEN
         STRING(ihCustomer::FoundationDate,"99-99-9999") ELSE "") + ";" +
      TRIM(ihCustomer::smsnumber) + ";" +
      TRIM(ihCustomer::phone) + ";" +
      STRING(ihCustomer::DirMarkSMS) + ";" +
      STRING(ihCustomer::DirMarkEmail) + ";" +
      STRING(ihCustomer::DirMarkPost) + ";" +
      STRING(ihCustomer::OutMarkSMS) + ";" +
      STRING(ihCustomer::OutMarkEmail) + ";" +
      STRING(ihCustomer::OutMarkPost) + ";" +
      icStreetCode + ";" +
      icCityCode + ";" +
      ";" + /* deltype is removed */
      TRIM(icAgrCustIdType) + ";" +
      TRIM(icAgrCustId) + ";" +
      icTownCode + ";" +
      (IF icMandateId > "" THEN icMandateId ELSE "").

   IF NUM-ENTRIES(lcCode,";") > 35 THEN 
      ocError = "Check data, it cannot contain semicolons (;)".

   RETURN lcCode.  

END.
            
FUNCTION fParseAccDataParam RETURNS LOGICAL
  (INPUT icDataField AS CHARACTER,
   OUTPUT TABLE FOR ttCustomer):

   CREATE ttCustomer.
   ASSIGN
      ttCustomer.CustName        = ENTRY(1,icDataField,";")
      ttCustomer.FirstName       = ENTRY(2,icDataField,";")
      ttCustomer.Surname2        = ENTRY(3,icDataField,";")
      ttCustomer.COName          = ENTRY(4,icDataField,";")
      ttCustomer.Companyname     = ENTRY(5,icDataField,";")
      ttCustomer.Address         = ENTRY(6,icDataField,";")
      ttCustomer.ZipCode         = ENTRY(7,icDataField,";")
      ttCustomer.PostOffice      = ENTRY(8,icDataField,";")
      ttCustomer.Country         = ENTRY(9,icDataField,";")
      ttCustomer.Email           = ENTRY(10,icDataField,";")
      ttCustomer.SalesMan        = ENTRY(11,icDataField,";")
      ttCustomer.CustIdType      = ENTRY(12,icDataField,";")
      ttCustomer.OrgId           = ENTRY(13,icDataField,";")
      ttCustomer.BirthDay        = DATE(ENTRY(14,icDataField,";"))
      ttCustomer.Language        = INT(ENTRY(15,icDataField,";"))
      ttCustomer.HonTitle        = ENTRY(16,icDataField,";")
      ttCustomer.Region          = ENTRY(17,icDataField,";")
      ttCustomer.BankAcc         = ENTRY(18,icDataField,";")
      ttCustomer.Nationality     = ENTRY(19,icDataField,";")
      ttCustomer.FoundationDate  = DATE(ENTRY(20,icDataField,";"))
      ttCustomer.smsnumber       = ENTRY(21,icDataField,";")
      ttCustomer.phone           = ENTRY(22,icDataField,";")
      ttCustomer.DirMarkSMS      = LOGICAL(ENTRY(23,icDataField,";"))
      ttCustomer.DirMarkEmail    = LOGICAL(ENTRY(24,icDataField,";"))
      ttCustomer.DirMarkPost     = LOGICAL(ENTRY(25,icDataField,";"))
      ttCustomer.OutMarkSMS      = LOGICAL(ENTRY(26,icDataField,";"))
      ttCustomer.OutMarkEmail    = LOGICAL(ENTRY(27,icDataField,";"))
      ttCustomer.OutMarkPost     = LOGICAL(ENTRY(28,icDataField,";"))
      ttCustomer.StreetCode      = ENTRY(29,icDataField,";")
      ttCustomer.CityCode        = ENTRY(30,icDataField,";") 
      ttCustomer.AuthCustIdType  = ENTRY(32,icDataField,";")
      ttCustomer.AuthCustId      = ENTRY(33,icDataField,";")
      ttCustomer.TownCode        = ENTRY(34,icDataField,";")
      ttCustomer.SearchName      = SUBSTRING(ttCustomer.CustName + " " + 
                                           ttCustomer.FirstName,1,8) 
      NO-ERROR.

   IF ERROR-STATUS:ERROR THEN RETURN FALSE.
   RETURN TRUE.

END.

FUNCTION fParseAccOrderCustomer RETURNS LOGICAL
   (iiOrderID AS INT,
    OUTPUT TABLE FOR ttCustomer):

   DEF BUFFER OrderCustomer FOR OrderCustomer.
   DEF BUFFER Order FOR Order.   

   FIND Order NO-LOCK WHERE
        Order.Brand = Syst.Var:gcBrand AND
        Order.OrderID = iiOrderID NO-ERROR.
   IF NOT AVAIL Order THEN RETURN FALSE.

   FIND OrderCustomer NO-LOCK WHERE
        OrderCustomer.Brand = Syst.Var:gcBrand AND
        OrderCustomer.OrderID = Order.OrderID AND
        OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_ACC} NO-ERROR.
   IF NOT AVAIL OrderCustomer THEN RETURN FALSE.

   CREATE ttCustomer.
   ASSIGN
      ttCustomer.CustName        = OrderCustomer.SurName1
      ttCustomer.FirstName       = OrderCustomer.FirstName
      ttCustomer.Surname2        = OrderCustomer.SurName2
      ttCustomer.COName          = "" /* not used with ACC orders */
      ttCustomer.Companyname     = OrderCustomer.Company
      ttCustomer.Address         = OrderCustomer.Address
      ttCustomer.ZipCode         = OrderCustomer.ZipCode
      ttCustomer.PostOffice      = OrderCustomer.PostOffice
      ttCustomer.Country         = OrderCustomer.Country
      ttCustomer.Email           = OrderCustomer.Email
      ttCustomer.SalesMan        = Order.SalesMan
      ttCustomer.CustIdType      = OrderCustomer.CustIDType
      ttCustomer.OrgId           = OrderCustomer.CustID
      ttCustomer.BirthDay        = OrderCustomer.BirthDay
      ttCustomer.Nationality     = OrderCustomer.Nationality
      ttCustomer.Language        = INTEGER(OrderCustomer.Language)
      ttCustomer.HonTitle        = OrderCustomer.CustTitle
      ttCustomer.Region          = OrderCustomer.Region
      ttCustomer.BankAcc         = OrderCustomer.BankCode
      ttCustomer.FoundationDate  = OrderCustomer.FoundationDate
      ttCustomer.smsnumber       = OrderCustomer.MobileNumber
      ttCustomer.phone           = OrderCustomer.FixedNumber
      ttCustomer.DirMarkSMS      = OrderCustomer.OperSMSMarketing
      ttCustomer.DirMarkEmail    = OrderCustomer.OperEMailMarketing
      ttCustomer.DirMarkPost     = OrderCustomer.OperPostMarketing
      ttCustomer.OutMarkSMS      = OrderCustomer.OutSMSMarketing
      ttCustomer.OutMarkEmail    = OrderCustomer.OutEMailMarketing
      ttCustomer.OutMarkPost     = OrderCustomer.OutPostMarketing
      ttCustomer.StreetCode      = OrderCustomer.AddressCodC
      ttCustomer.CityCode        = OrderCustomer.AddressCodP
      ttCustomer.AuthCustIdType  = OrderCustomer.AuthCustIdType
      ttCustomer.AuthCustId      = OrderCustomer.AuthCustID
      ttCustomer.TownCode        = OrderCustomer.AddressCodM
      ttCustomer.SearchName      = SUBSTRING(OrderCustomer.Surname1 + " " + 
                                             OrderCustomer.FirstName,1,8)
      NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN RETURN FALSE.
   RETURN TRUE.

END.
&ENDIF
