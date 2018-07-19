 
 /*------------------------------------------------------------------------
    File        : Customer
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
USING Progress.Json.ObjectModel.JsonArray.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS bss.cls.Customer IMPLEMENTS bss.cls.IObjectStorage:
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY CustNum AS INTEGER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Category AS CHARACTER NO-UNDO
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
    
    DEFINE PUBLIC PROPERTY Profession AS CHARACTER NO-UNDO
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
        
    DEFINE PUBLIC PROPERTY SelfEmployed AS LOGICAL NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY BirthDay AS DATE NO-UNDO
        GET.
        SET.
    
    DEFINE PUBLIC PROPERTY FoundationDate AS DATE NO-UNDO
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
        
    DEFINE PUBLIC PROPERTY OperEMailMarketing AS LOGICAL NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY OutEMailMarketing AS LOGICAL NO-UNDO
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
    
    DEFINE PUBLIC PROPERTY OutBankMarketing AS LOGICAL NO-UNDO
        GET.
        SET.
    
    DEFINE PUBLIC PROPERTY DontSharePersData AS LOGICAL NO-UNDO
        GET.
        SET.
    
    DEFINE PUBLIC PROPERTY OperAllMarketing AS LOGICAL NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY AuthCustIdType AS CHARACTER NO-UNDO
        GET.
        SET(INPUT icCustIDType AS CHARACTER):
            
            ASSIGN THIS-OBJECT:AuthCustIdType  = icCustIDType.
            
        END SET.
    
    DEFINE PUBLIC PROPERTY AuthCustId AS CHARACTER NO-UNDO
        GET.
        SET(INPUT icCustID AS CHARACTER):
            
            ASSIGN THIS-OBJECT:AuthCustId = icCustID.
            
        END SET.
    
    DEFINE PUBLIC PROPERTY AddressIndex AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY AddrID AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY AddressDataIndex AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY aOCustAcct AS CLASS bss.cls.CustomerAccount NO-UNDO 
        GET.
        SET.   
        
    DEFINE PUBLIC PROPERTY isNewCustomer AS LOGICAL NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY aoDefaultCustAddress AS bss.cls.OrderAddress NO-UNDO 
        GET.
        SET.
    
    DEFINE PUBLIC PROPERTY DefaultAddrID AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY CompanyName AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY CustAcctData AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY DefaultAddressObj AS CLASS JsonObject NO-UNDO 
        GET.
        SET.   
        
    DEFINE PUBLIC PROPERTY OrderCustBankAcct AS CHARACTER NO-UNDO
       GET.
       SET(INPUT iiBankAcct AS CHARACTER):
           
           ASSIGN THIS-OBJECT:OrderCustBankAcct  =  iiBankAcct.
           
       END SET.
       
    DEFINE PUBLIC PROPERTY isProCustomer AS LOGICAL NO-UNDO
       GET.
       SET(INPUT ilispro AS LOGICAL) :
           
           ASSIGN THIS-OBJECT:isProCustomer   =  ilispro .
           
       END SET.
       
    DEFINE PUBLIC PROPERTY OrderChannel AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY DelType AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE TEMP-TABLE ttCustAccts NO-UNDO 
        FIELDS custacctobj  AS CLASS Progress.Lang.Object
        FIELDS custacctnum  AS INTEGER
        INDEX custacctnum IS PRIMARY UNIQUE custacctnum. 

	CONSTRUCTOR PUBLIC Customer():
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC Customer():
	    ASSIGN aOCustAcct            = ?
               aoDefaultCustAddress  = ?
               DefaultAddressObj     = ?.
	           
	END DESTRUCTOR.

	METHOD PUBLIC VOID mFindData
	  (iiCustNum AS INTEGER):

      DEFINE BUFFER bfCustomer FOR Customer.
      
      FIND FIRST bfCustomer NO-LOCK WHERE bfCustomer.CustNum = iiCustNum NO-ERROR.

      IF NOT AVAILABLE bfCustomer
      THEN UNDO, THROW NEW ParamError
               (SUBSTITUTE("customer-not-found|&1", iiCustNum)).

        ASSIGN
            THIS-OBJECT:CustNum            = bfCustomer.CustNum
            THIS-OBJECT:Category           = bfCustomer.Category
            THIS-OBJECT:CustID             = bfCustomer.orgID
            THIS-OBJECT:CustIdType         = bfCustomer.CustIdType            
            THIS-OBJECT:CustTitle          = bfCustomer.HonTitle
            THIS-OBJECT:FirstName          = bfCustomer.FirstName
            THIS-OBJECT:SurName1           = bfCustomer.CustName
            THIS-OBJECT:SurName2           = bfCustomer.SurName2
            THIS-OBJECT:CompanyName        = bfCustomer.CompanyName
            THIS-OBJECT:Nationality        = bfCustomer.Nationality   
            THIS-OBJECT:Language           = STRING(bfCustomer.Language)
            THIS-OBJECT:AuthCustIdType     = bfCustomer.AuthCustIdType
            THIS-OBJECT:AuthCustId         = bfCustomer.AuthCustId
            THIS-OBJECT:BirthDay           = bfCustomer.BirthDay
            THIS-OBJECT:Profession         = bfCustomer.Profession
            THIS-OBJECT:Email              = bfCustomer.Email
            THIS-OBJECT:MobileNumber       = bfCustomer.SMSNumber
            THIS-OBJECT:FixedNumber        = bfCustomer.Phone
            THIS-OBJECT:OutBankMarketing   = bfCustomer.OutMarkBank
            THIS-OBJECT:DontSharePersData  = bfCustomer.DontSharePersData
            THIS-OBJECT:OperEMailMarketing = bfCustomer.DirMarkEmail
            THIS-OBJECT:OutEMailMarketing  = bfCustomer.OutMarkEmail 
            THIS-OBJECT:OperPostMarketing  = bfCustomer.DirMarkPost
            THIS-OBJECT:OutPostMarketing   = bfCustomer.OutMarkPost
            THIS-OBJECT:OperSMSMarketing   = bfCustomer.DirMarkSMS
            THIS-OBJECT:OutSMSMarketing    = bfCustomer.OutMarkSMS 
            THIS-OBJECT:FoundationDate     = bfCustomer.FoundationDate
            .
     
	END METHOD.


   METHOD PUBLIC VOID mFindCustomer (INPUT iiCustAcct AS INTEGER) :
       
       
       FIND FIRST CustomerAccount 
            WHERE CustomerAccount.AccountID  = iiCustAcct NO-LOCK NO-ERROR .
            
       IF NOT AVAILABLE CustomerAccount
       THEN  UNDO, THROW NEW ParamError
               (SUBSTITUTE("Account_not_found|&1", iiCustAcct)).
               
       IF THIS-OBJECT:CustNum <> CustomerAccount.CustNum
       THEN UNDO, THROW NEW ParamError ("Invalid Custnum and AccountID combination.").
       
   END METHOD.
   
   METHOD PUBLIC VOID mCreateData
     (ioJsonObject AS CLASS JsonObject):

       DEFINE VARIABLE liCustnum AS INTEGER NO-UNDO.
       DEFINE VARIABLE CustAccounts AS CLASS JsonArray NO-UNDO.
       DEFINE VARIABLE CustAcctObj  AS CLASS JsonObject NO-UNDO.
       DEFINE VARIABLE liCustAcctCnt AS INTEGER NO-UNDO.
       DEFINE VARIABLE lcSupportedTitles AS CHARACTER INITIAL
           "Sr.,Mr.,Sr,Mr,Mrs.,Sra.,Mrs,Sra" NO-UNDO.
           
       DEFINE VARIABLE MarketingData AS CLASS JsonObject NO-UNDO .
       DEFINE VARIABLE ioCustIDObject AS CLASS JsonObject NO-UNDO.
       
       IF ioJsonObject:Has("title")
       THEN DO:
           IF LOOKUP(ioJsonObject:GetCharacter("title"), lcSupportedTitles) EQ 0
               THEN UNDO, THROW NEW ParamError(SUBSTITUTE("unsupported_person_title|&1",
                   ioJsonObject:GetCharacter("title"))).
           THIS-OBJECT:CustTitle = ioJsonObject:GetCharacter("title").
       END.
       
       FIND LAST customer USE-INDEX custnum NO-LOCK NO-ERROR.
       IF NOT AVAILABLE customer 
       THEN ASSIGN liCustNum = 1.
       ELSE liCustnum = Customer.CustNum + 1 .
       
       ASSIGN 
       THIS-OBJECT:isNewCustomer  = TRUE
       THIS-OBJECT:FirstName      = ioJsonObject:GetCharacter("fname")
       THIS-OBJECT:SurName1       = ioJsonObject:GetCharacter("lname")
       THIS-OBJECT:SurName2       = ioJsonObject:GetCharacter("lname2")
       THIS-OBJECT:Profession     = ioJsonObject:GetCharacter("profession")
       THIS-OBJECT:Nationality    = ioJsonObject:GetCharacter("nationality")
       THIS-OBJECT:Language       = mGetLanguage(ioJsonObject:GetCharacter("language"))
       THIS-OBJECT:SelfEmployed   = ioJsonObject:GetLogical("self_employed")
       THIS-OBJECT:BirthDay       = Func.Common:mGetDate(ioJsonObject:GetCharacter("birthday"))
       THIS-OBJECT:Email          = ioJsonObject:GetCharacter("email")
       THIS-OBJECT:MobileNumber   = ioJsonObject:GetCharacter("sms_number")
       THIS-OBJECT:FixedNumber    = ioJsonObject:GetCharacter("phone_number")
       THIS-OBJECT:CustNum        = liCustnum
       .
       
       IF ioJsonObject:Has("residential")
       THEN DO:
           ioCustIDObject  = NEW JsonObject().
           ioCustIDObject = ioJsonObject:GetJsonObject("residential").
           ASSIGN 
           THIS-OBJECT:CustIdType     = ioCustIDObject:GetCharacter("id_type")  
           THIS-OBJECT:CustId         = ioCustIDObject:GetCharacter("person_id")
           .
       END.
       ELSE IF ioJsonObject:Has("company")
       THEN DO:
           ASSIGN 
           ioCustIDObject  = NEW JsonObject()
           ioCustIDObject  = ioJsonObject:GetJsonObject("company")
           THIS-OBJECT:CustId          =  ioCustIDObject:GetCharacter("company_id")
           THIS-OBJECT:CustIdType      =  ioCustIDObject:GetCharacter("id_type")
           THIS-OBJECT:FoundationDate  =  Func.Common:mGetDate(ioCustIDObject:GetCharacter("foundation_date"))
           THIS-OBJECT:CompanyName     =  ioCustIDObject:GetCharacter("site_name")
           .          
       END.
       ELSE UNDO , THROW NEW ParamError ("Customer ID Data does not exist.").
       
       IF THIS-OBJECT:CustId = ""
       THEN UNDO , THROW NEW ParamError ("Expected either person_id or company_id").
       
       IF ioJsonObject:Has("marketing")
       THEN DO:
           ASSIGN MarketingData                  = ioJsonObject:GetJsonObject("marketing")
                  THIS-OBJECT:OperEMailMarketing = MarketingData:GetLogical("email")
                  THIS-OBJECT:OperPostMarketing  = MarketingData:GetLogical("post")
                  THIS-OBJECT:OperSMSMarketing   = MarketingData:GetLogical("sms")
                  THIS-OBJECT:OutBankMarketing   = MarketingData:GetLogical("bank_3rd")
                  THIS-OBJECT:OutEMailMarketing  = MarketingData:GetLogical("email_3rd")
                  THIS-OBJECT:OutPostMarketing   = MarketingData:GetLogical("post_3rd")
                  THIS-OBJECT:OutSMSMarketing    = MarketingData:GetLogical("sms_3rd")
                  THIS-OBJECT:DontSharePersData  = MarketingData:GetLogical("dont_share_personal_data")
                  THIS-OBJECT:OperAllMarketing   = THIS-OBJECT:OperEMailMarketing OR
                                                   THIS-OBJECT:OutEMailMarketing  OR
                                                   THIS-OBJECT:OperPostMarketing  OR
                                                   THIS-OBJECT:OutPostMarketing   OR
                                                   THIS-OBJECT:OperSMSMarketing   OR
                                                   THIS-OBJECT:OutSMSMarketing
                  .
       END.
       ELSE UNDO, THROW NEW ParamError ("Marketing data does not exist.").
       
       IF ioJsonObject:Has("default_billing_address")
       THEN DO:
           
           ASSIGN
               THIS-OBJECT:DefaultAddressObj = ioJsonObject:GetJsonObject("default_billing_address")
               aoDefaultCustAddress          = NEW bss.cls.OrderAddress()
               aoDefaultCustAddress:KeyValue = THIS-OBJECT:CustNum
               aoDefaultCustAddress:AddrType = {&ADDR_DEFAULT_BILLING}
               .
           aoDefaultCustAddress:mCreateData(THIS-OBJECT:DefaultAddressObj).
           
       END.
       
       IF ioJsonObject:Has("customer_accounts")
       THEN DO:
           
           ASSIGN CustAccounts  =  ioJsonObject:GetJsonArray("customer_accounts").
           
           DO liCustAcctCnt  = 1 TO CustAccounts:LENGTH :
               
               ASSIGN CustAcctObj = CustAccounts:GetJsonObject(liCustAcctCnt)
                      aOCustAcct = NEW bss.cls.CustomerAccount(THIS-OBJECT).
                      
               aOCustAcct:mCreateData(CustAcctObj).
               
               CREATE ttCustAccts.
               ASSIGN ttCustAccts.custacctobj = aOCustAcct
                      ttCustAccts.custacctnum = mGetNextCustAcctNum().
                      
               ASSIGN THIS-OBJECT:CustAcctData = THIS-OBJECT:CustAcctData + ":" + aOCustAcct:mGetCustomerAcctBankData().

           END.
       END.
       
       ASSIGN THIS-OBJECT:CustAcctData = TRIM(THIS-OBJECT:CustAcctData, ":").
       mAssignCategory().
     
   END METHOD.

   METHOD PUBLIC VOID mStoreData():

       CREATE Customer.
       ASSIGN 
           Customer.CustNum           = THIS-OBJECT:CustNum
           Customer.HonTitle          = THIS-OBJECT:CustTitle  
           Customer.FirstName         = THIS-OBJECT:FirstName
           Customer.CustName          = THIS-OBJECT:SurName1
           Customer.SurName2          = THIS-OBJECT:SurName2
           Customer.Nationality       = THIS-OBJECT:Nationality
           Customer.Language          = INTEGER(THIS-OBJECT:Language)
           Customer.CustIdType        = THIS-OBJECT:CustIdType
           Customer.OrgId             = THIS-OBJECT:CustId
           Customer.AuthCustIdType    = THIS-OBJECT:AuthCustIdType
           Customer.AuthCustId        = THIS-OBJECT:AuthCustId
           Customer.Birthday          = THIS-OBJECT:BirthDay
           Customer.Email             = THIS-OBJECT:Email
           Customer.SMSNumber         = THIS-OBJECT:MobileNumber
           Customer.Phone             = THIS-OBJECT:FixedNumber
           Customer.OutMarkBank       = THIS-OBJECT:OutBankMarketing
           Customer.DontSharePersData = THIS-OBJECT:DontSharePersData
           Customer.DirMarkEmail      = THIS-OBJECT:OperEMailMarketing
           Customer.OutMarkEmail      = THIS-OBJECT:OutEMailMarketing
           Customer.DirMarkPost       = THIS-OBJECT:OperPostMarketing
           Customer.OutMarkPost       = THIS-OBJECT:OutPostMarketing
           Customer.DirMarkSMS        = THIS-OBJECT:OperSMSMarketing
           Customer.OutMarkSMS        = THIS-OBJECT:OutSMSMarketing
           Customer.FoundationDate    = THIS-OBJECT:FoundationDate
           Customer.CompanyName       = THIS-OBJECT:CompanyName
           Customer.Profession        = THIS-OBJECT:Profession
           Customer.RateCust          = Customer.CustNum
           Customer.InvCust           = Customer.CustNum
           Customer.PaymCust          = Customer.CustNum
           Customer.RepCust           = Customer.CustNum
           Customer.AgrCust           = Customer.CustNum
           Customer.Brand             = Syst.Var:gcBrand
           Customer.CreUser           = Syst.Var:katun
           Customer.CreDate           = TODAY 
           Customer.Category          = THIS-OBJECT:Category
           .
           
       IF VALID-OBJECT(aoDefaultCustAddress)
       THEN DO:
           aoDefaultCustAddress:mStoreData().
           ASSIGN THIS-OBJECT:DefaultAddrID = aoDefaultCustAddress:AddrRegisterID.
           
       END.
           
       FOR EACH ttCustAccts :
           CAST(ttCustAccts.custacctobj,bss.cls.CustomerAccount):mStoreData().
       END.
      
   END METHOD.
   
    METHOD PRIVATE CHARACTER mGetLanguage
        (icLanguage AS CHARACTER):

        FOR FIRST Language NO-LOCK WHERE
            Language.LanguageCode = icLanguage:
            RETURN STRING(Language.Language).
        END.

        UNDO, THROW NEW ParamError(SUBSTITUTE("language_unknown|&1", icLanguage)).

    END METHOD.
    
        
    METHOD PUBLIC INTEGER mGetNextCustAcctNum():
        
        FIND LAST ttCustAccts USE-INDEX custacctnum NO-ERROR.
        IF NOT AVAILABLE ttCustAccts
        THEN RETURN 1.
        ELSE RETURN ( ttCustAccts.custacctnum + 1 ). 
         
    END METHOD.
    
    METHOD PUBLIC JsonObject mResult () :
        
        DEFINE VARIABLE ioCustAcctObject AS CLASS JsonObject NO-UNDO.
        DEFINE VARIABLE ioCustArray      AS CLASS JsonArray  NO-UNDO.
        DEFINE VARIABLE ioCustObj        AS CLASS JsonObject NO-UNDO.
        
        ASSIGN ioCustAcctObject = NEW JsonObject()
               ioCustObj        = NEW JsonObject()
               ioCustArray      = NEW JsonArray().
               
        FOR EACH ttCustAccts :
            ioCustAcctObject = CAST(ttCustAccts.custacctobj,bss.cls.CustomerAccount):mResult().
            ioCustArray:Add(ioCustAcctObject).
        END.      
        
        ioCustObj:Add("cust-num" , THIS-OBJECT:CustNum).
        ioCustObj:Add("default-billing-address-id" , THIS-OBJECT:DefaultAddrID).
        IF TEMP-TABLE ttCustAccts:HAS-RECORDS 
        THEN ioCustObj:Add("customer-accounts" , ioCustArray).
        
        RETURN ioCustObj.
        
    END METHOD.
    
    METHOD PUBLIC VOID mAssignCategory():
        
        DEFINE BUFFER bfCustCat FOR CustCat.
        DEFINE VARIABLE lcSegment AS CHARACTER NO-UNDO.

        IF THIS-OBJECT:isProCustomer THEN 
        DO:
            FIND FIRST bfCustCat NO-LOCK WHERE
                       bfCustCat.brand          =   Syst.Var:gcBrand         AND
                       bfCustCat.custidtype     =   THIS-OBJECT:CustIdType   AND
                       bfCustCat.selfemployed   =   THIS-OBJECT:SelfEmployed AND
                       bfCustCat.pro            =   THIS-OBJECT:isProCustomer NO-ERROR.
            IF AVAIL bfCustCat THEN 
            DO:
                THIS-OBJECT:Category = bfCustCat.category.
            END.
        END.
        ELSE 
        DO:
            IF THIS-OBJECT:CustIdType EQ {&CUSTID_COMPANY} THEN 
            DO:
                IF THIS-OBJECT:CustId BEGINS "V00" THEN
                    THIS-OBJECT:Category = "30".  
                ELSE
                    THIS-OBJECT:Category = "23".
            END.
            ELSE IF THIS-OBJECT:CustIdType EQ {&CUSTID_RESIDENTIAL_NIF} AND THIS-OBJECT:SelfEmployed  
            THEN ASSIGN THIS-OBJECT:Category = "44".         
            ELSE IF THIS-OBJECT:CustIdType EQ {&CUSTID_RESIDENTIAL_NIF} AND NOT THIS-OBJECT:SelfEmployed 
            THEN THIS-OBJECT:Category = "10". 
            ELSE IF THIS-OBJECT:CustIdType EQ {&CUSTID_RESIDENTIAL_NIE} AND THIS-OBJECT:SelfEmployed 
            THEN THIS-OBJECT:Category = "45".         
            ELSE IF THIS-OBJECT:CustIdType EQ {&CUSTID_RESIDENTIAL_NIE} AND NOT THIS-OBJECT:SelfEmployed 
            THEN THIS-OBJECT:Category = "11". 
            IF THIS-OBJECT:Category > "" THEN 
            DO:
                FIND FIRST bfCustCat NO-LOCK WHERE
                           bfCustCat.brand    EQ Syst.Var:gcBrand AND
                           bfCustCat.category EQ THIS-OBJECT:Category NO-ERROR.

                IF AVAIL bfCustCat 
                THEN lcSegment = bfCustCat.Segment.
            END.
            ELSE 
            DO:
                FIND FIRST bfCustCat NO-LOCK WHERE
                           bfCustCat.brand        EQ Syst.Var:gcBrand         AND
                           bfCustCat.custidtype   EQ THIS-OBJECT:CustIdType   AND
                           bfCustCat.selfemployed EQ THIS-OBJECT:SelfEmployed AND
                           bfCustCat.pro          EQ THIS-OBJECT:isProCustomer NO-ERROR.
                IF AVAIL CustCat THEN 
                DO:
                    ASSIGN 
                        lcSegment = bfCustCat.Segment
                        THIS-OBJECT:Category = bfCustCat.category.
                END.
            END.
        END.
        
    END METHOD.
    
    METHOD PUBLIC VOID mCheckSubscriptionLimit( INPUT lcNumberType AS CHARACTER ) :
        
        
        DEFINE VARIABLE liSubLimit     AS INTEGER NO-UNDO. 
        DEFINE VARIABLE lisubs         AS INTEGER NO-UNDO.
        DEFINE VARIABLE liActLimit     AS INTEGER NO-UNDO.
        DEFINE VARIABLE liActs         AS INTEGER NO-UNDO.
        DEFINE VARIABLE liCustSubLimit AS LOGICAL NO-UNDO.
        DEFINE VARIABLE liBypassRules  AS LOGICAL NO-UNDO.
        DEFINE VARIABLE liMultiSimType AS INTEGER NO-UNDO.
        
        DEFINE BUFFER bfpcCustomer FOR Customer.
        
        /*TO-DO PC:WHAT IS plBypassRules in the newton_add_Whole_order.p*/
      
        IF THIS-OBJECT:isNewCustomer = FALSE
            AND NOT THIS-OBJECT:OrderChannel BEGINS "migration"
            AND NOT liBypassRules        
            AND LOOKUP(lcNumberType,"new,mnp") > 0
            AND liMultiSimType       <> {&MULTISIMTYPE_SECONDARY}
        THEN DO:            
            
            liCustSubLimit =  Func.ValidateACC:mSubscriptionLimitCheck(INPUT THIS-OBJECT:CustId ,
                                                                       INPUT THIS-OBJECT:CustIdType,
                                                                       INPUT Func.ValidateACC:mIsSelfEmpl(THIS-OBJECT:Category),
                                                                       INPUT Func.ValidateACC:mIsPro(THIS-OBJECT:Category),
                                                                       1,
                                                                       OUTPUT liSubLimit,
                                                                       OUTPUT lisubs,
                                                                       OUTPUT liActLimit,
                                                                       OUTPUT liActs).
                                                                       
            IF liCustSubLimit = FALSE 
            THEN UNDO, THROW NEW ParamError("Subscription Limit Error to existing Customer."). 
            
            FOR FIRST bfpcCustomer WHERE
                      bfpcCustomer.Brand      =  Syst.Var:gcBrand       AND
                      bfpcCustomer.OrgId      =  THIS-OBJECT:CustId     AND
                      bfpcCustomer.CustIdType =  THIS-OBJECT:CustIdType AND
                      bfpcCustomer.Roles     <>  "inactive" NO-LOCK:
                          
                IF Func.ValidateOrder:mIsBarredSubscriptionExistsToCustomer(bfpcCustomer.Custnum) 
                THEN UNDO, THROW NEW ParamError("barring").                                                                   
        
            END.               
        
        END. /*  IF aoOrder:aoCustomer:isNewCustomer = FALSE */
        
    END METHOD.

END CLASS.