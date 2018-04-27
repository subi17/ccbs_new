 
 /*------------------------------------------------------------------------
    File        : Order
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Apr 23 12:30:10 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS bss.cls.OrderCustomer IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY CustNum AS INTEGER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Title AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY FirstName AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY SurName1 AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY SurName2 AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Nationality AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Language AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY CustIdType AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY CustId AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY AuthCustIdType AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY AuthCustId AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY BirthDay AS DATE NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Street AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY PostOffice AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Region AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY ZipCode AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Country AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Email AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY MobileNumber AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY FixedNumber AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY BankCode AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoMandate AS CLASS bss.cls.OrderAction NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY OutBankMarketing AS LOGICAL NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY DontSharePersData AS LOGICAL NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY OperEMailMarketing AS LOGICAL NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY OutEMailMarketing AS LOGICAL NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY OperAllMarketing AS LOGICAL NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY OperPostMarketing AS LOGICAL NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY OutPostMarketing AS LOGICAL NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY OperSMSMarketing AS LOGICAL NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY OutSMSMarketing AS LOGICAL NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY AddressCodC AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY AddressCodP AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY AddressCodM AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Category AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Company AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY FoundationDate AS DATE NO-UNDO
      GET.
      SET.

   DEFINE PROTECTED VARIABLE aoOrder AS CLASS bss.cls.Order NO-UNDO.

   CONSTRUCTOR PUBLIC OrderCustomer
     ( ioOrder AS CLASS bss.cls.Order ):
      aoOrder = ioOrder.   
   END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderCustomer():
     IF VALID-OBJECT(aoMandate)
     THEN DELETE OBJECT aoMandate.
     aoOrder = ?.
	END DESTRUCTOR.

   METHOD PUBLIC VOID mCreateData
     (iiCustNum AS INTEGER):

      DEFINE BUFFER Customer FOR Customer.

      FIND Customer NO-LOCK WHERE Customer.CustNum = iiCustNum NO-ERROR.
      IF NOT AVAILABLE Customer
      THEN UNDO, THROW NEW InternalError
               (SUBSTITUTE("CustNum '&1' doesn't exists", iiCustNum)).

      ASSIGN
         THIS-OBJECT:CustNum            = iiCustNum
         THIS-OBJECT:FirstName          = Customer.FirstName
         THIS-OBJECT:SurName1           = Customer.CustName
         THIS-OBJECT:SurName2           = Customer.SurName2
         THIS-OBJECT:Nationality        = Customer.Nationality
         THIS-OBJECT:Language           = STRING(Customer.Language)
         THIS-OBJECT:CustIdType         = Customer.CustIdType
         THIS-OBJECT:CustId             = Customer.OrgId
         THIS-OBJECT:AuthCustIdType     = Customer.AuthCustIdType
         THIS-OBJECT:AuthCustId         = Customer.AuthCustId
         THIS-OBJECT:BirthDay           = Customer.Birthday
         THIS-OBJECT:Street             = Customer.Address
         THIS-OBJECT:PostOffice         = Customer.PostOffice
         THIS-OBJECT:Region             = Customer.Region
         THIS-OBJECT:ZipCode            = Customer.ZipCode
         THIS-OBJECT:Country            = Customer.Country
         THIS-OBJECT:Email              = Customer.Email
         THIS-OBJECT:MobileNumber       = Customer.SMSNumber
         THIS-OBJECT:FixedNumber        = Customer.Phone
         THIS-OBJECT:BankCode           = Customer.BankAcc
         THIS-OBJECT:OutBankMarketing   = Customer.OutMarkBank
         THIS-OBJECT:DontSharePersData  = Customer.DontSharePersData
         THIS-OBJECT:OperEMailMarketing = Customer.DirMarkEmail
         THIS-OBJECT:OutEMailMarketing  = Customer.OutMarkEmail
         THIS-OBJECT:OperAllMarketing   = Customer.DirMarkEmail OR
                                          Customer.OutMarkEmail OR
                                          Customer.DirMarkPost  OR
                                          Customer.OutMarkPost  OR
                                          Customer.DirMarkSMS   OR
                                          Customer.OutMarkSMS
         THIS-OBJECT:OperPostMarketing  = Customer.DirMarkPost
         THIS-OBJECT:OutPostMarketing   = Customer.OutMarkPost
         THIS-OBJECT:OperSMSMarketing   = Customer.DirMarkSMS
         THIS-OBJECT:OutSMSMarketing    = Customer.OutMarkSMS
         THIS-OBJECT:Category           = Customer.Category
         THIS-OBJECT:Company            = Customer.CompanyName
         THIS-OBJECT:FoundationDate     = Customer.FoundationDate.

      FIND CustomerReport NO-LOCK WHERE
           CustomerReport.Custnum = Customer.Custnum NO-ERROR.
           
      IF AVAILABLE CustomerReport
      THEN ASSIGN
              AddressCodC = CustomerReport.StreetCode
              AddressCodP = CustomerReport.CityCode
              AddressCodM = CustomerReport.TownCode.

   END METHOD.
	
	METHOD PUBLIC VOID mCreateData
	  (ioJsonObject AS CLASS JsonObject):

      IF ioJsonObject:Has("company_id")
      THEN ASSIGN
              THIS-OBJECT:CustIdType     = "CIF"
              THIS-OBJECT:CustId         = ioJsonObject:GetCharacter("company_id")
              THIS-OBJECT:Company        = ioJsonObject:GetCharacter("company_name")
              THIS-OBJECT:FoundationDate = ioJsonObject:GetCharacter("company_foundation_date")
              THIS-OBJECT:AuthCustIdType = ioJsonObject:GetCharacter("id-type")
              THIS-OBJECT:AuthCustId     = ioJsonObject:GetCharacter("person-id").
      ELSE ASSIGN
              THIS-OBJECT:CustIdType     = ioJsonObject:GetCharacter("id-type")
              THIS-OBJECT:CustId         = ioJsonObject:GetCharacter("person-id").

      ASSIGN
         THIS-OBJECT:FirstName          = ioJsonObject:GetCharacter("fname")
         THIS-OBJECT:SurName1           = ioJsonObject:GetCharacter("lname")
         THIS-OBJECT:SurName2           = ioJsonObject:GetCharacter("lname2")
         THIS-OBJECT:Nationality        = ioJsonObject:GetCharacter("nationality")
         THIS-OBJECT:Language           = STRING(LOOKUP(ioJsonObject:GetCharacter("language"),
                                                 "es_ES,es_CA,es_EU,es_GA,en"))
         THIS-OBJECT:CustIdType         = ioJsonObject:GetCharacter("id-type")
         THIS-OBJECT:CustId             = ioJsonObject:GetCharacter("person-id")
         THIS-OBJECT:BirthDay           = ioJsonObject:GetDate("birthday")
         THIS-OBJECT:Street             = ioJsonObject:GetCharacter("street")
         THIS-OBJECT:PostOffice         = ioJsonObject:GetCharacter("city")
         THIS-OBJECT:Region             = ioJsonObject:GetCharacter("region")
         THIS-OBJECT:ZipCode            = ioJsonObject:GetCharacter("zip")
         THIS-OBJECT:Country            = ioJsonObject:GetCharacter("country")
         THIS-OBJECT:Email              = ioJsonObject:GetCharacter("email")
         THIS-OBJECT:MobileNumber       = ioJsonObject:GetCharacter("sms_number")
         THIS-OBJECT:FixedNumber        = ioJsonObject:GetCharacter("phone_number")
         THIS-OBJECT:BankCode           = ioJsonObject:GetCharacter("bankaccount")
         THIS-OBJECT:OutBankMarketing   = ioJsonObject:GetLogical("mark-bank-3rd")
         THIS-OBJECT:DontSharePersData  = ioJsonObject:GetLogical("mark-dont-share-personal-data")
         THIS-OBJECT:OperEMailMarketing = ioJsonObject:GetLogical("mark-email")
         THIS-OBJECT:OutEMailMarketing  = ioJsonObject:GetLogical("mark-email-3rd")
         THIS-OBJECT:OperPostMarketing  = ioJsonObject:GetLogical("mark-post")
         THIS-OBJECT:OutPostMarketing   = ioJsonObject:GetLogical("mark-post-3rd")
         THIS-OBJECT:OperSMSMarketing   = ioJsonObject:GetLogical("mark-sms")
         THIS-OBJECT:OutSMSMarketing    = ioJsonObject:GetLogical("mark-sms-3rd")
         THIS-OBJECT:OperAllMarketing   = THIS-OBJECT:OperEMailMarketing OR
                                          THIS-OBJECT:OutEMailMarketing  OR
                                          THIS-OBJECT:OperPostMarketing  OR
                                          THIS-OBJECT:OutPostMarketing   OR
                                          THIS-OBJECT:OperSMSMarketing   OR
                                          THIS-OBJECT:OutSMSMarketing
         THIS-OBJECT:AddressCodC        = ioJsonObject:GetCharacter("city_code")
         THIS-OBJECT:AddressCodP        = ioJsonObject:GetCharacter("street_code")
         THIS-OBJECT:AddressCodM        = ioJsonObject:GetCharacter("municipality_code").

      IF ioJsonObject:Has("mandate")
      THEN DO:
         aoMandate = NEW bss.cls.OrderAction(aoOrder).
         aoMandate:mCreateData("Mandate", ioJsonObject:GetCharacter("mandate"), "").
      END.

	END METHOD.
	
   METHOD PUBLIC VOID mStoreData():

      aoMandate:mStoreData().

      CREATE OrderCustomer.
      ASSIGN
         OrderCustomer.Brand              = Syst.Var:gcBrand
         OrderCustomer.OrderId            = aoOrder:OrderId
         OrderCustomer.CustNum            = THIS-OBJECT:CustNum
         OrderCustomer.FirstName          = THIS-OBJECT:FirstName
         OrderCustomer.SurName1           = THIS-OBJECT:SurName1
         OrderCustomer.SurName2           = THIS-OBJECT:SurName2
         OrderCustomer.Nationality        = THIS-OBJECT:Nationality
         OrderCustomer.Language           = THIS-OBJECT:Language
         OrderCustomer.CustIdType         = THIS-OBJECT:CustIdType
         OrderCustomer.CustId             = THIS-OBJECT:CustId
         OrderCustomer.AuthCustIdType     = THIS-OBJECT:AuthCustIdType
         OrderCustomer.AuthCustId         = THIS-OBJECT:AuthCustId
         OrderCustomer.BirthDay           = THIS-OBJECT:BirthDay
         OrderCustomer.Street             = THIS-OBJECT:Street
         OrderCustomer.PostOffice         = THIS-OBJECT:PostOffice
         OrderCustomer.Region             = THIS-OBJECT:Region
         OrderCustomer.ZipCode            = THIS-OBJECT:ZipCode
         OrderCustomer.Country            = THIS-OBJECT:Country
         OrderCustomer.Email              = THIS-OBJECT:Email
         OrderCustomer.MobileNumber       = THIS-OBJECT:MobileNumber
         OrderCustomer.FixedNumber        = THIS-OBJECT:FixedNumber
         OrderCustomer.BankCode           = THIS-OBJECT:BankCode
         OrderCustomer.OutBankMarketing   = THIS-OBJECT:OutBankMarketing
         OrderCustomer.DontSharePersData  = THIS-OBJECT:DontSharePersData
         OrderCustomer.OperEMailMarketing = THIS-OBJECT:OperEMailMarketing
         OrderCustomer.OutEMailMarketing  = THIS-OBJECT:OutEMailMarketing
         OrderCustomer.OperAllMarketing   = THIS-OBJECT:OperAllMarketing
         OrderCustomer.OperPostMarketing  = THIS-OBJECT:OperPostMarketing
         OrderCustomer.OutPostMarketing   = THIS-OBJECT:OutPostMarketing
         OrderCustomer.OperSMSMarketing   = THIS-OBJECT:OperSMSMarketing
         OrderCustomer.OutSMSMarketing    = THIS-OBJECT:OutSMSMarketing
         OrderCustomer.AddressCodC        = THIS-OBJECT:AddressCodC
         OrderCustomer.AddressCodP        = THIS-OBJECT:AddressCodP
         OrderCustomer.AddressCodM        = THIS-OBJECT:AddressCodM
         .

   END METHOD.

END CLASS.