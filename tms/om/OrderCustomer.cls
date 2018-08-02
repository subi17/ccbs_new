 
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

CLASS om.OrderCustomer IMPLEMENTS bss.cls.IObjectStorage:
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
      
   DEFINE PUBLIC PROPERTY aoDefaultCustomer AS cm.Customer NO-UNDO 
       GET.
       SET.
       
   DEFINE PUBLIC PROPERTY AddressID AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY Profession AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY BuildingNum AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY AddressCompl AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY Latitude AS CHARACTER NO-UNDO
      GET.
      SET.
   
   DEFINE PUBLIC PROPERTY Longitude AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY Gescal AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY FloorNum AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY StreetType AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY BisDuplicate AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY BlockNum AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY DoorNum AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY Letter AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Stair AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY Hand AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY KM AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY CoverageToken AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PROTECTED VARIABLE aoOrder AS CLASS om.Order NO-UNDO.

   CONSTRUCTOR PUBLIC OrderCustomer
     ( ioOrder AS CLASS om.Order ):
      aoOrder = ioOrder.
   END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderCustomer():
	    
      ASSIGN aoOrder = ?
             aoDefaultCustomer = ?.
             
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
         THIS-OBJECT:AddressCodC        = ioJsonObject:GetCharacter("city_code") WHEN ioJsonObject:Has("city_code")
         THIS-OBJECT:AddressCodP        = ioJsonObject:GetCharacter("street_code") WHEN ioJsonObject:Has("street_code")
         THIS-OBJECT:AddressCodM        = ioJsonObject:GetCharacter("municipality_code") WHEN ioJsonObject:Has("municipality_code").

      /* BankCode is not mandatory when the customer is prepaid customer
         so we need to check the value somewhere else.*/
      ASSIGN
         THIS-OBJECT:BankCode           = REPLACE(ioJsonObject:GetCharacter("bankaccount")," ","") WHEN ioJsonObject:Has("bankaccount")
         .

      IF THIS-OBJECT:BankCode > "" AND
         NOT Func.BankAccount:mValid(THIS-OBJECT:BankCode)
      THEN UNDO, THROW NEW ParamError(SUBSTITUTE("bank-account-invalid|&1",
                                                 THIS-OBJECT:BankCode)).
	END METHOD.
	
	METHOD PUBLIC VOID mCreateRowCustomerData(INPUT ioOrderCustAddrObj  AS CLASS JsonObject) :
	    
	    DEFINE VARIABLE ioValidationData AS CLASS JsonObject NO-UNDO.
	    
        aoDefaultCustomer = NEW cm.Customer().
        ASSIGN aoDefaultCustomer = aoOrder:aoCustomer.
        
        IF ioOrderCustAddrObj:Has("validation_data")
        THEN DO:
            
            ASSIGN 
                ioValidationData  = NEW JsonObject() 
                ioValidationData  = ioOrderCustAddrObj:GetJsonObject("validation_data").
                
            IF VALID-OBJECT(ioValidationData) THEN                 
            DO:
                ASSIGN                    
                    THIS-OBJECT:AddressCodC = ioValidationData:GetCharacter("street_code")
                    THIS-OBJECT:AddressCodP = ioValidationData:GetCharacter("city_code") 
                    THIS-OBJECT:AddressCodM = ioValidationData:GetCharacter("municipality_code")
                    .
            END.
        END.
        
        
        ASSIGN 
            THIS-OBJECT:CustNum            = aoDefaultCustomer:CustNum
            THIS-OBJECT:CustTitle          = aoDefaultCustomer:CustTitle
            THIS-OBJECT:FirstName          = aoDefaultCustomer:FirstName
            THIS-OBJECT:SurName1           = aoDefaultCustomer:SurName1
            THIS-OBJECT:SurName2           = aoDefaultCustomer:SurName2
            THIS-OBJECT:Company            = aoDefaultCustomer:CompanyName
            THIS-OBJECT:Nationality        = aoDefaultCustomer:Nationality
            THIS-OBJECT:Language           = aoDefaultCustomer:Language
            THIS-OBJECT:CustIdType         = aoDefaultCustomer:CustIdType
            THIS-OBJECT:CustId             = aoDefaultCustomer:CustId
            THIS-OBJECT:AuthCustIdType     = aoDefaultCustomer:AuthCustIdType
            THIS-OBJECT:AuthCustId         = aoDefaultCustomer:AuthCustId
            THIS-OBJECT:BirthDay           = aoDefaultCustomer:BirthDay
            THIS-OBJECT:Profession         = aoDefaultCustomer:Profession
            THIS-OBJECT:Street             = ioOrderCustAddrObj:GetCharacter("street")
            THIS-OBJECT:Region             = ioOrderCustAddrObj:GetCharacter("region")
            THIS-OBJECT:ZipCode            = ioOrderCustAddrObj:GetCharacter("zip")
            THIS-OBJECT:Country            = ioOrderCustAddrObj:GetCharacter("country")
            THIS-OBJECT:AddressID          = ioOrderCustAddrObj:GetCharacter("address_id")
            THIS-OBJECT:BuildingNum        = ioOrderCustAddrObj:GetCharacter("building_number")
            THIS-OBJECT:AddressCompl       = ioOrderCustAddrObj:GetCharacter("additional_address")
            THIS-OBJECT:Latitude           = ioOrderCustAddrObj:GetCharacter("latitude")
            THIS-OBJECT:Longitude          = ioOrderCustAddrObj:GetCharacter("longitude")
            THIS-OBJECT:Gescal             = ioOrderCustAddrObj:GetCharacter("gescal") WHEN ioOrderCustAddrObj:Has("gescal")
            THIS-OBJECT:FloorNum           = ioOrderCustAddrObj:GetCharacter("floor")
            THIS-OBJECT:StreetType         = ioOrderCustAddrObj:GetCharacter("street_type")
            THIS-OBJECT:BisDuplicate       = ioOrderCustAddrObj:GetCharacter("bis_duplicate")
            THIS-OBJECT:BlockNum           = ioOrderCustAddrObj:GetCharacter("block")
            THIS-OBJECT:DoorNum            = ioOrderCustAddrObj:GetCharacter("door")
            THIS-OBJECT:Letter             = ioOrderCustAddrObj:GetCharacter("letter")
            THIS-OBJECT:Stair              = ioOrderCustAddrObj:GetCharacter("stair")
            THIS-OBJECT:Hand               = ioOrderCustAddrObj:GetCharacter("hand")
            THIS-OBJECT:KM                 = ioOrderCustAddrObj:GetCharacter("km")
            THIS-OBJECT:CoverageToken      = ioOrderCustAddrObj:GetCharacter("coveragetoken") WHEN ioOrderCustAddrObj:Has("coveragetoken")
            THIS-OBJECT:PostOffice         = ioOrderCustAddrObj:GetCharacter("city")
            THIS-OBJECT:Email              = aoDefaultCustomer:Email
            THIS-OBJECT:MobileNumber       = aoDefaultCustomer:MobileNumber
            THIS-OBJECT:FixedNumber        = aoDefaultCustomer:FixedNumber
            THIS-OBJECT:BankCode           = (IF aoOrder:aoCustomer:isNewCustomer THEN mGetBankAcctNum() ELSE aoOrder:aoCustomer:OrderCustBankAcct) 
            THIS-OBJECT:FoundationDate     = aoDefaultCustomer:FoundationDate
             .
                      	          
	END.

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
         OrderCustomer.Profession         = THIS-OBJECT:Profession
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
         OrderCustomer.BuildingNum        = THIS-OBJECT:BuildingNum
         OrderCustomer.AddressCompl       = THIS-OBJECT:AddressCompl
         OrderCustomer.Latitude           = THIS-OBJECT:Latitude
         OrderCustomer.Longitude          = THIS-OBJECT:Longitude
         OrderCustomer.Gescal             = THIS-OBJECT:Gescal
         OrderCustomer.Floor              = THIS-OBJECT:FloorNum
         OrderCustomer.StreetType         = THIS-OBJECT:StreetType
         OrderCustomer.BisDuplicate       = THIS-OBJECT:BisDuplicate
         OrderCustomer.Block              = THIS-OBJECT:BlockNum
         OrderCustomer.Door               = THIS-OBJECT:DoorNum
         OrderCustomer.Letter             = THIS-OBJECT:Letter
         OrderCustomer.Stair              = THIS-OBJECT:Stair
         OrderCustomer.Hand               = THIS-OBJECT:Hand
         OrderCustomer.KM                 = THIS-OBJECT:KM
         OrderCustomer.FoundationDate     = THIS-OBJECT:FoundationDate
         OrderCustomer.Email              = THIS-OBJECT:Email
         OrderCustomer.MobileNumber       = THIS-OBJECT:MobileNumber
         OrderCustomer.FixedNumber        = THIS-OBJECT:FixedNumber
         OrderCustomer.BankCode           = THIS-OBJECT:BankCode
         OrderCustomer.Address            = THIS-OBJECT:Street
         OrderCustomer.AddressCodC        = THIS-OBJECT:AddressCodC
         OrderCustomer.AddressCodP        = THIS-OBJECT:AddressCodP
         OrderCustomer.AddressCodM        = THIS-OBJECT:AddressCodM
         .
         
       IF OrderCustomer.BuildingNum NE ""  
       THEN ASSIGN OrderCustomer.Address = OrderCustomer.Address + " " + OrderCustomer.BuildingNum.   
       
       IF OrderCustomer.AddressCompl NE ""  
       THEN OrderCustomer.Address = OrderCustomer.Address + " " + OrderCustomer.AddressCompl. 

   END METHOD.

   METHOD PUBLIC CHARACTER mGetBankAcctNum():
           
       IF aoOrder:isPrePaidOrder
       THEN RETURN "".
       
       RETURN aoOrder:aoCustomer:bankCode.
         
   END.

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
   
   METHOD PUBLIC CHARACTER mCreateHolderData(INPUT ioHolderObj  AS CLASS JsonObject):
       
     /*  ASSIGN THIS-OBJECT:AuthCustIdType = ioHolderObj:GetCharacter("id_type").
       
       IF THIS-OBJECT:AuthCustIdType = "CIF"
       THEN ASSIGN THIS-OBJECT:AuthCustId  = ioHolderObj:GetCharacter("company_id").
       ELSE THIS-OBJECT:AuthCustId   =  ioHolderObj:GetCharacter("person_id").
    */   
   END METHOD.


END CLASS.