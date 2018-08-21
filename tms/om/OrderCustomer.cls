 
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

   DEFINE PUBLIC PROPERTY rowType AS INTEGER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY custNum AS INTEGER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY custTitle AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY firstName AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY surName1 AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY surName2 AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY nationality AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY language AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY custIdType AS CHARACTER INITIAL ? NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY custId AS CHARACTER INITIAL ? NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY authCustIdType AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY authCustId AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY birthDay AS DATE NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY street AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY postOffice AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY region AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY zipCode AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY country AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY email AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY mobileNumber AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY fixedNumber AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY bankCode AS CHARACTER NO-UNDO
      GET.
      SET.
   
   DEFINE PUBLIC PROPERTY addressCodC AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY addressCodP AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY addressCodM AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY category AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY company AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY foundationDate AS DATE NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY objDefaultCustomer AS cm.Customer NO-UNDO 
       GET.
       SET.
       
   DEFINE PUBLIC PROPERTY addressID AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY profession AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY buildingNum AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY addressCompl AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY latitude AS CHARACTER NO-UNDO
      GET.
      SET.
   
   DEFINE PUBLIC PROPERTY longitude AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY gescal AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY floorNum AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY streetType AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY bisDuplicate AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY blockNum AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY doorNum AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY letter AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY stair AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY hand AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY km AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY coverageToken AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY pro AS LOGICAL NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY selfEmployed AS LOGICAL NO-UNDO
      GET.
      SET.

   DEFINE PROTECTED VARIABLE objOrder AS CLASS om.Order NO-UNDO.

   CONSTRUCTOR PUBLIC OrderCustomer ( iObjOrder AS CLASS om.Order ):
       
      ASSIGN THIS-OBJECT:objOrder = iObjOrder.
      
   END CONSTRUCTOR.

   METHOD PUBLIC VOID mCreateData(iiCustNum AS INTEGER):

      DEFINE BUFFER bfCustomer FOR Customer.

      FIND bfCustomer NO-LOCK WHERE bfCustomer.CustNum = iiCustNum NO-ERROR.
      IF NOT AVAILABLE bfCustomer THEN
          UNDO, THROW NEW ParamError(SUBSTITUTE("customer-not-found|&1", iiCustNum)).

      ASSIGN
         THIS-OBJECT:custNum            = iiCustNum
         THIS-OBJECT:custTitle          = bfCustomer.HonTitle
         THIS-OBJECT:firstName          = bfCustomer.FirstName
         THIS-OBJECT:surName1           = bfCustomer.CustName
         THIS-OBJECT:surName2           = bfCustomer.SurName2
         THIS-OBJECT:nationality        = bfCustomer.Nationality
         THIS-OBJECT:language           = STRING(bfCustomer.Language)
         THIS-OBJECT:custIdType         = bfCustomer.CustIdType
         THIS-OBJECT:custId             = bfCustomer.OrgId
         THIS-OBJECT:authCustIdType     = bfCustomer.AuthCustIdType
         THIS-OBJECT:authCustId         = bfCustomer.AuthCustId
         THIS-OBJECT:birthDay           = bfCustomer.Birthday
         THIS-OBJECT:street             = bfCustomer.Address
         THIS-OBJECT:postOffice         = bfCustomer.PostOffice
         THIS-OBJECT:region             = bfCustomer.Region
         THIS-OBJECT:zipCode            = bfCustomer.ZipCode
         THIS-OBJECT:country            = bfCustomer.Country
         THIS-OBJECT:email              = bfCustomer.Email
         THIS-OBJECT:mobileNumber       = bfCustomer.SMSNumber
         THIS-OBJECT:fixedNumber        = bfCustomer.Phone
         THIS-OBJECT:bankCode           = bfCustomer.BankAcc
         THIS-OBJECT:category           = bfCustomer.Category
         THIS-OBJECT:company            = bfCustomer.CompanyName
         THIS-OBJECT:foundationDate     = bfCustomer.FoundationDate
         THIS-OBJECT:pro                = ( INDEX(objOrder:orderChannel,"PRO") > 0 ) 
         THIS-OBJECT:selfEmployed       = Func.ValidateACC:mIsSelfEmpl(bfCustomer.category)
         .

      FIND CustomerReport NO-LOCK WHERE
           CustomerReport.Custnum = bfCustomer.Custnum NO-ERROR.

       IF AVAILABLE CustomerReport THEN
           ASSIGN
               THIS-OBJECT:addressCodC = CustomerReport.StreetCode
               THIS-OBJECT:addressCodP = CustomerReport.CityCode
               THIS-OBJECT:addressCodM = CustomerReport.TownCode.

   END METHOD.

	METHOD PUBLIC VOID mCreateData(ioJsonObject AS CLASS JsonObject):

      DEFINE VARIABLE lcSupportedTitles AS CHARACTER INITIAL "Sr.,Mr.,Sr,Mr,Mrs.,Sra.,Mrs,Sra" NO-UNDO.

      IF ioJsonObject:Has("title") THEN
      DO:
          
         IF LOOKUP(ioJsonObject:GetCharacter("title"), lcSupportedTitles) EQ 0 THEN
             UNDO, THROW NEW ParamError(SUBSTITUTE("unsupported-person-title|&1",ioJsonObject:GetCharacter("title"))).
             
         THIS-OBJECT:custTitle = ioJsonObject:GetCharacter("title").
         
      END.


      DEFINE VARIABLE lcErrorPrefix AS CHARACTER NO-UNDO.
      IF THIS-OBJECT:custNum > 0 THEN
          lcErrorPrefix = "target-customer".

      IF ioJsonObject:Has("company_id") THEN
           ASSIGN
              THIS-OBJECT:custIdType     = "CIF"
              THIS-OBJECT:custId         = ioJsonObject:GetCharacter("company_id")
              THIS-OBJECT:company        = ioJsonObject:GetCharacter("company_name")
              THIS-OBJECT:foundationDate = ioJsonObject:GetDate("company_foundation_date")
              THIS-OBJECT:authCustIdType = ioJsonObject:GetCharacter("id_type")
              THIS-OBJECT:authCustId     = ioJsonObject:GetCharacter("person_id").
      ELSE ASSIGN
              THIS-OBJECT:custIdType     = ioJsonObject:GetCharacter("id_type") WHEN ioJsonObject:Has("id_type")
              THIS-OBJECT:custId         = ioJsonObject:GetCharacter("person_id") WHEN ioJsonObject:Has("person_id").

      IF NOT THIS-OBJECT:custId > "" THEN
          UNDO, THROW NEW ParamError(SUBSTITUTE("&1-company-or-person-id-required|&2",lcErrorPrefix,THIS-OBJECT:custNum)).

      IF NOT THIS-OBJECT:custIdType > "" THEN 
          UNDO, THROW NEW ParamError(SUBSTITUTE("&1-id-type-required|&2",lcErrorPrefix,THIS-OBJECT:custNum)).

          /* New customer, throw error if any of the following is missing */
      IF THIS-OBJECT:custNum EQ 0 THEN
          ASSIGN
              THIS-OBJECT:firstName    = ioJsonObject:GetCharacter("fname")
              THIS-OBJECT:surName1     = ioJsonObject:GetCharacter("lname")
              THIS-OBJECT:surName2     = ioJsonObject:GetCharacter("lname2")
              THIS-OBJECT:language     = mGetLanguage(ioJsonObject:GetCharacter("language"))
              THIS-OBJECT:nationality  = ioJsonObject:GetCharacter("nationality")
              THIS-OBJECT:birthDay     = ioJsonObject:GetDate("birthday")
              THIS-OBJECT:street       = ioJsonObject:GetCharacter("street")
              THIS-OBJECT:postOffice   = ioJsonObject:GetCharacter("city")
              THIS-OBJECT:region       = ioJsonObject:GetCharacter("region")
              THIS-OBJECT:zipCode      = ioJsonObject:GetCharacter("zip")
              THIS-OBJECT:country      = mGetCountry(ioJsonObject:GetCharacter("country"))
              THIS-OBJECT:email        = ioJsonObject:GetCharacter("email")
              THIS-OBJECT:mobileNumber = ioJsonObject:GetCharacter("sms_number")
              THIS-OBJECT:fixedNumber  = ioJsonObject:GetCharacter("phone_number")
              THIS-OBJECT:addressCodC  = ioJsonObject:GetCharacter("city_code")
              THIS-OBJECT:addressCodP  = ioJsonObject:GetCharacter("street_code")
              THIS-OBJECT:addressCodM  = ioJsonObject:GetCharacter("municipality_code").

      /* Existing customer, just update the information */
      ELSE
      ASSIGN
         THIS-OBJECT:firstName          = ioJsonObject:GetCharacter("fname") WHEN ioJsonObject:Has("fname")
         THIS-OBJECT:surName1           = ioJsonObject:GetCharacter("lname") WHEN ioJsonObject:Has("lname")
         THIS-OBJECT:surName2           = ioJsonObject:GetCharacter("lname2") WHEN ioJsonObject:Has("lname2")
         THIS-OBJECT:language           = mGetLanguage(ioJsonObject:GetCharacter("language")) WHEN ioJsonObject:Has("language")
         THIS-OBJECT:nationality        = ioJsonObject:GetCharacter("nationality") WHEN ioJsonObject:Has("nationality")
         THIS-OBJECT:birthDay           = ioJsonObject:GetDate("birthday") WHEN ioJsonObject:Has("birthday")
         THIS-OBJECT:street             = ioJsonObject:GetCharacter("street") WHEN ioJsonObject:Has("street")
         THIS-OBJECT:postOffice         = ioJsonObject:GetCharacter("city") WHEN ioJsonObject:Has("city")
         THIS-OBJECT:region             = ioJsonObject:GetCharacter("region") WHEN ioJsonObject:Has("region")
         THIS-OBJECT:zipCode            = ioJsonObject:GetCharacter("zip") WHEN ioJsonObject:Has("zip")
         THIS-OBJECT:country            = mGetCountry(ioJsonObject:GetCharacter("country")) WHEN ioJsonObject:Has("country")
         THIS-OBJECT:email              = ioJsonObject:GetCharacter("email") WHEN ioJsonObject:Has("email")
         THIS-OBJECT:mobileNumber       = ioJsonObject:GetCharacter("sms_number") WHEN ioJsonObject:Has("sms_number")
         THIS-OBJECT:fixedNumber        = ioJsonObject:GetCharacter("phone_number") WHEN ioJsonObject:Has("phone_number")
         THIS-OBJECT:addressCodC        = ioJsonObject:GetCharacter("city_code") WHEN ioJsonObject:Has("city_code")
         THIS-OBJECT:addressCodP        = ioJsonObject:GetCharacter("street_code") WHEN ioJsonObject:Has("street_code")
         THIS-OBJECT:addressCodM        = ioJsonObject:GetCharacter("municipality_code") WHEN ioJsonObject:Has("municipality_code").

      /* BankCode is not mandatory when the customer is prepaid customer
         so we need to check the value somewhere else.*/
      ASSIGN
         THIS-OBJECT:bankCode           = REPLACE(ioJsonObject:GetCharacter("bankaccount")," ","") WHEN ioJsonObject:Has("bankaccount")
         .

      IF THIS-OBJECT:bankCode > "" AND NOT Func.BankAccount:mValid(THIS-OBJECT:bankCode) THEN
          UNDO, THROW NEW ParamError(SUBSTITUTE("bank-account-invalid|&1",THIS-OBJECT:bankCode)).
          
	END METHOD.
	
	METHOD PUBLIC VOID mCreateRowCustomerData(INPUT objOrderCustomerAddress  AS CLASS JsonObject) :
	    
	    DEFINE VARIABLE ioValidationData AS CLASS JsonObject NO-UNDO.
	    
        objDefaultCustomer = NEW cm.Customer().
        ASSIGN objDefaultCustomer = objOrder:objCustomer.
        
        IF objOrderCustomerAddress:Has("validation_data") THEN
        DO:            
            ASSIGN 
                ioValidationData  = NEW JsonObject() 
                ioValidationData  = objOrderCustomerAddress:GetJsonObject("validation_data").
                
            IF VALID-OBJECT(ioValidationData) THEN                 
            DO:
                ASSIGN                    
                    THIS-OBJECT:addressCodC = ioValidationData:GetCharacter("street_code")
                    THIS-OBJECT:addressCodP = ioValidationData:GetCharacter("city_code") 
                    THIS-OBJECT:addressCodM = ioValidationData:GetCharacter("municipality_code")
                    .
            END.
        END.        
        
        ASSIGN 
            THIS-OBJECT:custNum            = objDefaultCustomer:CustNum
            THIS-OBJECT:custTitle          = objDefaultCustomer:CustTitle
            THIS-OBJECT:firstName          = objDefaultCustomer:FirstName
            THIS-OBJECT:surName1           = objDefaultCustomer:SurName1
            THIS-OBJECT:surName2           = objDefaultCustomer:SurName2
            THIS-OBJECT:company            = objDefaultCustomer:CompanyName
            THIS-OBJECT:nationality        = objDefaultCustomer:Nationality
            THIS-OBJECT:language           = objDefaultCustomer:Language
            THIS-OBJECT:custIdType         = objDefaultCustomer:CustIdType
            THIS-OBJECT:custId             = objDefaultCustomer:CustId
            THIS-OBJECT:authCustIdType     = objDefaultCustomer:AuthCustIdType
            THIS-OBJECT:authCustId         = objDefaultCustomer:AuthCustId
            THIS-OBJECT:birthDay           = objDefaultCustomer:BirthDay
            THIS-OBJECT:profession         = objDefaultCustomer:Profession
            THIS-OBJECT:category           = objDefaultCustomer:category
            THIS-OBJECT:street             = objOrderCustomerAddress:GetCharacter("street")
            THIS-OBJECT:region             = objOrderCustomerAddress:GetCharacter("region")
            THIS-OBJECT:zipCode            = objOrderCustomerAddress:GetCharacter("zip")
            THIS-OBJECT:country            = objOrderCustomerAddress:GetCharacter("country")
            THIS-OBJECT:addressID          = objOrderCustomerAddress:GetCharacter("address_id")
            THIS-OBJECT:buildingNum        = objOrderCustomerAddress:GetCharacter("building_number")
            THIS-OBJECT:addressCompl       = objOrderCustomerAddress:GetCharacter("additional_address")
            THIS-OBJECT:latitude           = objOrderCustomerAddress:GetCharacter("latitude")
            THIS-OBJECT:longitude          = objOrderCustomerAddress:GetCharacter("longitude")
            THIS-OBJECT:gescal             = objOrderCustomerAddress:GetCharacter("gescal") WHEN objOrderCustomerAddress:Has("gescal")
            THIS-OBJECT:floorNum           = objOrderCustomerAddress:GetCharacter("floor")
            THIS-OBJECT:streetType         = objOrderCustomerAddress:GetCharacter("street_type")
            THIS-OBJECT:bisDuplicate       = objOrderCustomerAddress:GetCharacter("bis_duplicate")
            THIS-OBJECT:blockNum           = objOrderCustomerAddress:GetCharacter("block")
            THIS-OBJECT:doorNum            = objOrderCustomerAddress:GetCharacter("door")
            THIS-OBJECT:letter             = objOrderCustomerAddress:GetCharacter("letter")
            THIS-OBJECT:stair              = objOrderCustomerAddress:GetCharacter("stair")
            THIS-OBJECT:hand               = objOrderCustomerAddress:GetCharacter("hand")
            THIS-OBJECT:km                 = objOrderCustomerAddress:GetCharacter("km")
            THIS-OBJECT:coverageToken      = objOrderCustomerAddress:GetCharacter("coveragetoken") WHEN objOrderCustomerAddress:Has("coveragetoken")
            THIS-OBJECT:postOffice         = objOrderCustomerAddress:GetCharacter("city")
            THIS-OBJECT:email              = objDefaultCustomer:Email
            THIS-OBJECT:mobileNumber       = objDefaultCustomer:MobileNumber
            THIS-OBJECT:fixedNumber        = objDefaultCustomer:FixedNumber
            THIS-OBJECT:bankCode           = (IF objOrder:objCustomer:isNewCustomer THEN mGetBankAcctNum() ELSE objOrder:objCustomer:OrderCustBankAcct) 
            THIS-OBJECT:foundationDate     = objDefaultCustomer:FoundationDate
            THIS-OBJECT:pro                = ( INDEX(objOrder:orderChannel,"PRO") > 0 )
            THIS-OBJECT:selfEmployed       = objDefaultCustomer:selfEmployed
             .
                      	          
	END.

   METHOD PUBLIC VOID mStoreData():

      CREATE OrderCustomer.
      ASSIGN
         OrderCustomer.Brand              = Syst.Var:gcBrand
         OrderCustomer.OrderId            = objOrder:OrderId
         OrderCustomer.RowType            = THIS-OBJECT:rowType
         OrderCustomer.CustNum            = THIS-OBJECT:custNum
         OrderCustomer.CustTitle          = THIS-OBJECT:custTitle
         OrderCustomer.FirstName          = THIS-OBJECT:firstName
         OrderCustomer.SurName1           = THIS-OBJECT:surName1
         OrderCustomer.SurName2           = THIS-OBJECT:surName2
         OrderCustomer.Company            = THIS-OBJECT:company
         OrderCustomer.Nationality        = THIS-OBJECT:nationality
         OrderCustomer.Profession         = THIS-OBJECT:profession
         OrderCustomer.Language           = THIS-OBJECT:language
         OrderCustomer.CustIdType         = THIS-OBJECT:custIdType
         OrderCustomer.CustId             = THIS-OBJECT:custId
         OrderCustomer.AuthCustIdType     = THIS-OBJECT:authCustIdType
         OrderCustomer.AuthCustId         = THIS-OBJECT:authCustId
         OrderCustomer.BirthDay           = THIS-OBJECT:birthDay 
         OrderCustomer.Street             = THIS-OBJECT:street
         OrderCustomer.PostOffice         = THIS-OBJECT:postOffice
         OrderCustomer.Region             = THIS-OBJECT:region
         OrderCustomer.ZipCode            = THIS-OBJECT:zipCode
         OrderCustomer.Country            = THIS-OBJECT:country
         OrderCustomer.BuildingNum        = THIS-OBJECT:buildingNum
         OrderCustomer.AddressCompl       = THIS-OBJECT:addressCompl
         OrderCustomer.Latitude           = THIS-OBJECT:latitude
         OrderCustomer.Longitude          = THIS-OBJECT:longitude
         OrderCustomer.Gescal             = THIS-OBJECT:gescal
         OrderCustomer.Floor              = THIS-OBJECT:floorNum
         OrderCustomer.StreetType         = THIS-OBJECT:streetType
         OrderCustomer.BisDuplicate       = THIS-OBJECT:bisDuplicate
         OrderCustomer.Block              = THIS-OBJECT:blockNum
         OrderCustomer.Door               = THIS-OBJECT:doorNum
         OrderCustomer.Letter             = THIS-OBJECT:letter
         OrderCustomer.Stair              = THIS-OBJECT:stair
         OrderCustomer.Hand               = THIS-OBJECT:hand
         OrderCustomer.KM                 = THIS-OBJECT:km
         OrderCustomer.FoundationDate     = THIS-OBJECT:foundationDate
         OrderCustomer.Email              = THIS-OBJECT:email
         OrderCustomer.MobileNumber       = THIS-OBJECT:mobileNumber
         OrderCustomer.FixedNumber        = THIS-OBJECT:fixedNumber
         OrderCustomer.BankCode           = THIS-OBJECT:bankCode
         OrderCustomer.Address            = THIS-OBJECT:street
         OrderCustomer.AddressCodC        = THIS-OBJECT:addressCodC
         OrderCustomer.AddressCodP        = THIS-OBJECT:addressCodP
         OrderCustomer.AddressCodM        = THIS-OBJECT:addressCodM
         OrderCustomer.pro                = THIS-OBJECT:pro
         OrderCustomer.SelfEmployed       = THIS-OBJECT:selfEmployed
         OrderCustomer.Category           = THIS-OBJECT:category
         .
         
       IF OrderCustomer.BuildingNum NE ""  THEN 
           ASSIGN OrderCustomer.Address = OrderCustomer.Address + " " + OrderCustomer.BuildingNum.   
       
       IF OrderCustomer.AddressCompl NE "" THEN 
           ASSIGN OrderCustomer.Address = OrderCustomer.Address + " " + OrderCustomer.AddressCompl. 

   END METHOD.

   METHOD PUBLIC CHARACTER mGetBankAcctNum():
           
       IF objOrder:isPrePaidOrder THEN
           RETURN "".
       
       RETURN objOrder:objCustomer:bankCode.
         
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
       
     /*  ASSIGN THIS-OBJECT:authCustIdType = ioHolderObj:GetCharacter("id_type").
       
       IF THIS-OBJECT:authCustIdType = "CIF" THEN 
            ASSIGN THIS-OBJECT:authCustId  = ioHolderObj:GetCharacter("company_id").
       ELSE THIS-OBJECT:authCustId   =  ioHolderObj:GetCharacter("person_id").
    */   
   END METHOD.
   
    DESTRUCTOR PUBLIC OrderCustomer():
        
        IF VALID-OBJECT (objOrder) THEN 
            DELETE OBJECT objOrder.
            
        IF VALID-OBJECT (objDefaultCustomer) THEN 
            DELETE OBJECT objDefaultCustomer.
    
             
    END DESTRUCTOR.

END CLASS.