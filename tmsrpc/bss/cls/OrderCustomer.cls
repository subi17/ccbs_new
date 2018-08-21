 
 /*------------------------------------------------------------------------
    File        : OrderCustomer
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : tlamminmaki
    Created     : Mon Apr 23 12:30:10 EEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS bss.cls.OrderCustomer IMPLEMENTS bss.cls.IObjectStorage:
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY RowType AS INTEGER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY CustNum AS INTEGER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY CustTitle AS CHARACTER NO-UNDO
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

   DEFINE PUBLIC PROPERTY CustIdType AS CHARACTER INITIAL ? NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY CustId AS CHARACTER INITIAL ? NO-UNDO
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
      aoOrder = ?.
	END DESTRUCTOR.

   METHOD PUBLIC VOID mCreateData
     (iiCustNum AS INTEGER):

      DEFINE BUFFER Customer FOR Customer.

      FIND Customer NO-LOCK WHERE Customer.CustNum = iiCustNum NO-ERROR.
      IF NOT AVAILABLE Customer
      THEN UNDO, THROW NEW ParamError
               (SUBSTITUTE("customer-not-found|&1", iiCustNum)).

      ASSIGN
         THIS-OBJECT:CustNum            = iiCustNum
         THIS-OBJECT:CustTitle          = Customer.HonTitle
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

      DEFINE VARIABLE lcSupportedTitles AS CHARACTER INITIAL
         "Sr.,Mr.,Sr,Mr,Mrs.,Sra.,Mrs,Sra" NO-UNDO.

      IF ioJsonObject:Has("title")
      THEN DO:
         IF LOOKUP(ioJsonObject:GetCharacter("title"), lcSupportedTitles) EQ 0
         THEN UNDO, THROW NEW ParamError(SUBSTITUTE("unsupported-person-title|&1",
                                                    ioJsonObject:GetCharacter("title"))).
         THIS-OBJECT:CustTitle = ioJsonObject:GetCharacter("title").
      END.


      DEFINE VARIABLE lcErrorPrefix AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:CustNum > 0
      THEN lcErrorPrefix = "target-customer".

      IF ioJsonObject:Has("company_id")
      THEN ASSIGN
              THIS-OBJECT:CustIdType     = "CIF"
              THIS-OBJECT:CustId         = ioJsonObject:GetCharacter("company_id")
              THIS-OBJECT:Company        = ioJsonObject:GetCharacter("company_name")
              THIS-OBJECT:FoundationDate = ioJsonObject:GetDate("company_foundation_date")
              THIS-OBJECT:AuthCustIdType = ioJsonObject:GetCharacter("id_type")
              THIS-OBJECT:AuthCustId     = ioJsonObject:GetCharacter("person_id").
      ELSE ASSIGN
              THIS-OBJECT:CustIdType     = ioJsonObject:GetCharacter("id_type") WHEN ioJsonObject:Has("id_type")
              THIS-OBJECT:CustId         = ioJsonObject:GetCharacter("person_id") WHEN ioJsonObject:Has("person_id").

      IF NOT THIS-OBJECT:CustId > ""
      THEN UNDO, THROW NEW ParamError(SUBSTITUTE("&1-company-or-person-id-required|&2",
                                                 lcErrorPrefix,
                                                 THIS-OBJECT:CustNum)).

      IF NOT THIS-OBJECT:CustIdType > ""
      THEN UNDO, THROW NEW ParamError(SUBSTITUTE("&1-id-type-required|&2",
                                                 lcErrorPrefix,
                                                 THIS-OBJECT:CustNum)).

      /* New customer, throw error if any of the following is missing */
      IF THIS-OBJECT:CustNum EQ 0
      THEN ASSIGN
         THIS-OBJECT:FirstName          = ioJsonObject:GetCharacter("fname")
         THIS-OBJECT:SurName1           = ioJsonObject:GetCharacter("lname")
         THIS-OBJECT:SurName2           = ioJsonObject:GetCharacter("lname2")
         THIS-OBJECT:Language           = mGetLanguage(ioJsonObject:GetCharacter("language"))
         THIS-OBJECT:Nationality        = ioJsonObject:GetCharacter("nationality")
         THIS-OBJECT:BirthDay           = ioJsonObject:GetDate("birthday")
         THIS-OBJECT:Street             = ioJsonObject:GetCharacter("street")
         THIS-OBJECT:PostOffice         = ioJsonObject:GetCharacter("city")
         THIS-OBJECT:Region             = ioJsonObject:GetCharacter("region")
         THIS-OBJECT:ZipCode            = ioJsonObject:GetCharacter("zip")
         THIS-OBJECT:Country            = mGetCountry(ioJsonObject:GetCharacter("country"))
         THIS-OBJECT:Email              = ioJsonObject:GetCharacter("email")
         THIS-OBJECT:MobileNumber       = ioJsonObject:GetCharacter("sms_number")
         THIS-OBJECT:FixedNumber        = ioJsonObject:GetCharacter("phone_number")
         THIS-OBJECT:DontSharePersData  = ioJsonObject:GetLogical("mark_dont_share_personal_data")
         THIS-OBJECT:OperEMailMarketing = ioJsonObject:GetLogical("mark_email")
         THIS-OBJECT:OutEMailMarketing  = ioJsonObject:GetLogical("mark_email_3rd")
         THIS-OBJECT:OperPostMarketing  = ioJsonObject:GetLogical("mark_post")
         THIS-OBJECT:OutPostMarketing   = ioJsonObject:GetLogical("mark_post_3rd")
         THIS-OBJECT:OperSMSMarketing   = ioJsonObject:GetLogical("mark_sms")
         THIS-OBJECT:OutSMSMarketing    = ioJsonObject:GetLogical("mark_sms_3rd")
         THIS-OBJECT:AddressCodC        = ioJsonObject:GetCharacter("city_code")
         THIS-OBJECT:AddressCodP        = ioJsonObject:GetCharacter("street_code")
         THIS-OBJECT:AddressCodM        = ioJsonObject:GetCharacter("municipality_code").

      /* Existing customer, just update the information */
      ELSE
      ASSIGN
         THIS-OBJECT:FirstName          = ioJsonObject:GetCharacter("fname") WHEN ioJsonObject:Has("fname")
         THIS-OBJECT:SurName1           = ioJsonObject:GetCharacter("lname") WHEN ioJsonObject:Has("lname")
         THIS-OBJECT:SurName2           = ioJsonObject:GetCharacter("lname2") WHEN ioJsonObject:Has("lname2")
         THIS-OBJECT:Language           = mGetLanguage(ioJsonObject:GetCharacter("language")) WHEN ioJsonObject:Has("language")
         THIS-OBJECT:Nationality        = ioJsonObject:GetCharacter("nationality") WHEN ioJsonObject:Has("nationality")
         THIS-OBJECT:BirthDay           = ioJsonObject:GetDate("birthday") WHEN ioJsonObject:Has("birthday")
         THIS-OBJECT:Street             = ioJsonObject:GetCharacter("street") WHEN ioJsonObject:Has("street")
         THIS-OBJECT:PostOffice         = ioJsonObject:GetCharacter("city") WHEN ioJsonObject:Has("city")
         THIS-OBJECT:Region             = ioJsonObject:GetCharacter("region") WHEN ioJsonObject:Has("region")
         THIS-OBJECT:ZipCode            = ioJsonObject:GetCharacter("zip") WHEN ioJsonObject:Has("zip")
         THIS-OBJECT:Country            = mGetCountry(ioJsonObject:GetCharacter("country")) WHEN ioJsonObject:Has("country")
         THIS-OBJECT:Email              = ioJsonObject:GetCharacter("email") WHEN ioJsonObject:Has("email")
         THIS-OBJECT:MobileNumber       = ioJsonObject:GetCharacter("sms_number") WHEN ioJsonObject:Has("sms_number")
         THIS-OBJECT:FixedNumber        = ioJsonObject:GetCharacter("phone_number") WHEN ioJsonObject:Has("phone_number")
         THIS-OBJECT:DontSharePersData  = ioJsonObject:GetLogical("mark_dont_share_personal_data") WHEN ioJsonObject:Has("mark_dont_share_personal_data")
         THIS-OBJECT:OperEMailMarketing = ioJsonObject:GetLogical("mark_email") WHEN ioJsonObject:Has("mark_email")
         THIS-OBJECT:OutEMailMarketing  = ioJsonObject:GetLogical("mark_email_3rd") WHEN ioJsonObject:Has("mark_email_3rd")
         THIS-OBJECT:OperPostMarketing  = ioJsonObject:GetLogical("mark_post") WHEN ioJsonObject:Has("mark_post")
         THIS-OBJECT:OutPostMarketing   = ioJsonObject:GetLogical("mark_post_3rd") WHEN ioJsonObject:Has("mark_post_3rd")
         THIS-OBJECT:OperSMSMarketing   = ioJsonObject:GetLogical("mark_sms") WHEN ioJsonObject:Has("mark_sms")
         THIS-OBJECT:OutSMSMarketing    = ioJsonObject:GetLogical("mark_sms_3rd") WHEN ioJsonObject:Has("mark_sms_3rd")
         THIS-OBJECT:AddressCodC        = ioJsonObject:GetCharacter("city_code") WHEN ioJsonObject:Has("city_code")
         THIS-OBJECT:AddressCodP        = ioJsonObject:GetCharacter("street_code") WHEN ioJsonObject:Has("street_code")
         THIS-OBJECT:AddressCodM        = ioJsonObject:GetCharacter("municipality_code") WHEN ioJsonObject:Has("municipality_code").

      /* BankCode is not mandatory when the customer is prepaid customer
         so we need to check the value somewhere else.
         OutBankMarketing - lets populate only if available */
      ASSIGN
         THIS-OBJECT:BankCode           = REPLACE(ioJsonObject:GetCharacter("bankaccount")," ","") WHEN ioJsonObject:Has("bankaccount")
         THIS-OBJECT:OutBankMarketing   = ioJsonObject:GetLogical("mark_bank_3rd") WHEN ioJsonObject:Has("mark_bank_3rd")
         THIS-OBJECT:OperAllMarketing   = THIS-OBJECT:OperEMailMarketing OR
                                          THIS-OBJECT:OutEMailMarketing  OR
                                          THIS-OBJECT:OperPostMarketing  OR
                                          THIS-OBJECT:OutPostMarketing   OR
                                          THIS-OBJECT:OperSMSMarketing   OR
                                          THIS-OBJECT:OutSMSMarketing.

      IF THIS-OBJECT:BankCode > "" AND
         NOT Func.BankAccount:mValid(THIS-OBJECT:BankCode)
      THEN UNDO, THROW NEW ParamError(SUBSTITUTE("bank-account-invalid|&1",
                                                 THIS-OBJECT:BankCode)).
	END METHOD.

   METHOD PUBLIC VOID mStoreData():

      CREATE OrderCustomer.
      ASSIGN
         OrderCustomer.Brand              = Syst.Var:gcBrand
         OrderCustomer.OrderId            = aoOrder:OrderId
         OrderCustomer.RowType            = THIS-OBJECT:RowType
         OrderCustomer.CustNum            = THIS-OBJECT:CustNum
         OrderCustomer.CustTitle          = THIS-OBJECT:CustTitle
         OrderCustomer.FirstName          = THIS-OBJECT:FirstName
         OrderCustomer.SurName1           = THIS-OBJECT:SurName1
         OrderCustomer.SurName2           = THIS-OBJECT:SurName2
         OrderCustomer.Company            = THIS-OBJECT:Company
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
         OrderCustomer.Address            = THIS-OBJECT:Street
         OrderCustomer.AddressCodC        = THIS-OBJECT:AddressCodC
         OrderCustomer.AddressCodP        = THIS-OBJECT:AddressCodP
         OrderCustomer.AddressCodM        = THIS-OBJECT:AddressCodM
         .

   END METHOD.

   METHOD PRIVATE CHARACTER mGetCountry
      (icCountry AS CHARACTER):

      FOR Country NO-LOCK WHERE
          Country.Country = icCountry:
         RETURN Country.Country.
      END.

      UNDO, THROW NEW ParamError(SUBSTITUTE("country-unknown|&1", icCountry)).

   END METHOD.

   METHOD PRIVATE CHARACTER mGetLanguage
      (icLanguage AS CHARACTER):

      FOR FIRST Language NO-LOCK WHERE
          Language.LanguageCode = icLanguage:
         RETURN STRING(Language.Language).
      END.

      UNDO, THROW NEW ParamError(SUBSTITUTE("language-unknown|&1", icLanguage)).

   END METHOD.


END CLASS.
