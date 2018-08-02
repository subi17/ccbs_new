 
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

CLASS cm.Customer IMPLEMENTS bss.cls.IObjectStorage:
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
       
    DEFINE PUBLIC PROPERTY aOCustAcct AS CLASS cm.CustomerAccount NO-UNDO 
        GET.
        SET.   
        
    DEFINE PUBLIC PROPERTY isNewCustomer AS LOGICAL NO-UNDO
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
       
    DEFINE PUBLIC PROPERTY BillAcctIDData AS CHARACTER NO-UNDO
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
       
    DEFINE PUBLIC PROPERTY gdpr_consent AS LOGICAL NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY CustAcctID AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY isCustomerUpdate AS LOGICAL NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY bankCode AS CHARACTER NO-UNDO
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
               DefaultAddressObj     = ?.
	           
	END DESTRUCTOR.

  METHOD PUBLIC CHARACTER mGetCustomerId
     (ioJsonObject AS CLASS JsonObject):

     DEFINE VARIABLE lobjCustomerType AS CLASS JsonObject NO-UNDO.

     IF ioJsonObject:Has("residential") THEN 
     DO:
        lobjCustomerType = NEW JsonObject().

        lobjCustomerType = ioJsonObject:GetJsonObject("residential").

        RETURN lobjCustomerType:GetCharacter("person_id").
     END.
     ELSE IF ioJsonObject:Has("company") THEN 
     DO:
           lobjCustomerType = NEW JsonObject().

           lobjCustomerType = ioJsonObject:GetJsonObject("company").

           RETURN lobjCustomerType:GetCharacter("company_id").
     END.    

     RETURN "".
      
  END METHOD.

  METHOD PUBLIC VOID mFillCustomerIDDetails
    (ioJsonObject AS CLASS JsonObject):

     DEFINE VARIABLE lobjCustomerType AS CLASS JsonObject NO-UNDO.

     IF ioJsonObject:Has("residential") THEN 
     DO:
        lobjCustomerType = NEW JsonObject().

        lobjCustomerType = ioJsonObject:GetJsonObject("residential").

        ASSIGN 
           THIS-OBJECT:CustIdType = lobjCustomerType:GetCharacter("id_type")  
           THIS-OBJECT:CustId     = lobjCustomerType:GetCharacter("person_id").
     END.
     ELSE IF ioJsonObject:Has("company") THEN 
     DO:
         lobjCustomerType = NEW JsonObject().

         lobjCustomerType = ioJsonObject:GetJsonObject("company").

         ASSIGN    
            THIS-OBJECT:CustId         =  lobjCustomerType:GetCharacter("company_id")
            THIS-OBJECT:CustIdType     =  lobjCustomerType:GetCharacter("id_type")
            THIS-OBJECT:FoundationDate =  Func.Common:mGetDate(lobjCustomerType:GetCharacter("foundation_date"))
            THIS-OBJECT:CompanyName    =  lobjCustomerType:GetCharacter("site_name").          
     END.

  END METHOD.  

	METHOD PUBLIC VOID mFindData
	  (iiCustNum AS INTEGER):

      DEFINE BUFFER bfCustomer FOR Customer.
      
      FIND FIRST bfCustomer NO-LOCK WHERE bfCustomer.CustNum = iiCustNum NO-ERROR.

      IF NOT AVAILABLE bfCustomer
      THEN UNDO, THROW NEW ParamError
               (SUBSTITUTE("customer-not-found|&1", iiCustNum)).

       /* this is useful for the rowtype_aggrement for existing customer who does not have any customer update information*/

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
            THIS-OBJECT:FoundationDate     = bfCustomer.FoundationDate
            .
     
	END METHOD.

   METHOD PUBLIC VOID mFindCustomerAcct (INPUT iiCustAcct AS INTEGER) :       
       
       FIND FIRST CustomerAccount 
            WHERE CustomerAccount.AccountID  = iiCustAcct NO-LOCK NO-ERROR .
            
       IF NOT AVAILABLE CustomerAccount
       THEN  UNDO, THROW NEW ParamError(SUBSTITUTE("Account_not_found|&1", iiCustAcct)).
               
       IF THIS-OBJECT:CustNum <> CustomerAccount.CustNum
       THEN UNDO, THROW NEW ParamError ("Invalid Custnum and AccountID combination.").
       
       ASSIGN THIS-OBJECT:CustAcctID  =  iiCustAcct.
       
   END METHOD.
   
  METHOD PUBLIC INTEGER mGetLatestCustNum():

      DEFINE VARIABLE liCustNum AS INTEGER NO-UNDO.

      FIND LAST Customer USE-INDEX Custnum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Customer THEN 
         ASSIGN liCustNum = 1.
      ELSE 
         liCustnum = Customer.CustNum + 1 . 

      RETURN liCustnum.

   END METHOD.

   METHOD PUBLIC VOID mValidate
     (ioJsonObject AS CLASS JsonObject):

      DEFINE VARIABLE liCustAcctCnt     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE llDefaultAcc      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lcCustomerID      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcSupportedTitles AS CHARACTER INITIAL
           "Sr.,Mr.,Sr,Mr,Mrs.,Sra.,Mrs,Sra" NO-UNDO.
      
      DEFINE VARIABLE lobjCustAccountArray AS CLASS JsonArray  NO-UNDO.
      DEFINE VARIABLE lobjCustAccount      AS CLASS JsonObject NO-UNDO.

      IF ioJsonObject:Has("title") THEN 
      DO:
           IF LOOKUP(ioJsonObject:GetCharacter("title"), lcSupportedTitles) EQ 0 THEN 
              UNDO, THROW NEW ParamError(SUBSTITUTE("unsupported_person_title|&1",ioJsonObject:GetCharacter("title"))).
      END.
      
      IF NOT (ioJsonObject:Has("residential") OR ioJsonObject:Has("company")) THEN 
         UNDO, THROW NEW ParamError("Customer ID Data does not exist.").
      ELSE
      DO:
          lcCustomerID = mGetCustomerId(ioJsonObject).

          IF lcCustomerID = "" THEN 
             UNDO, THROW NEW ParamError ("Expected either person_id or company_id").
      END.

      IF ioJsonObject:Has("customer_accounts") THEN 
      DO:
          ASSIGN lobjCustAccountArray =  ioJsonObject:GetJsonArray("customer_accounts").
           
          DO liCustAcctCnt = 1 TO lobjCustAccountArray:LENGTH:
               
             ASSIGN lobjCustAccount = lobjCustAccountArray:GetJsonObject(liCustAcctCnt).

             IF NOT llDefaultAcc THEN 
                ASSIGN llDefaultAcc = lobjCustAccount:GetLogical("default_account").
             ELSE 
                LEAVE.
          END.

          IF NOT llDefaultAcc THEN
             UNDO , THROW NEW ParamError ("Default customer account should be created when creating a new customer.").          
      END.            

   END METHOD.  

   METHOD PUBLIC VOID mCreateData
     (ioJsonObject AS CLASS JsonObject):

       DEFINE VARIABLE CustAccounts     AS CLASS JsonArray NO-UNDO.
       DEFINE VARIABLE CustAcctObj      AS CLASS JsonObject NO-UNDO.
      
       DEFINE VARIABLE liCustAcctCnt AS INTEGER NO-UNDO.
       DEFINE VARIABLE liCustnum     AS INTEGER NO-UNDO.

       mValidate(ioJsonObject).

       mFillCustomerIDDetails(ioJsonObject).

       ASSIGN 
           liCustnum                  = mGetLatestCustNum()
           THIS-OBJECT:isNewCustomer  = TRUE
           THIS-OBJECT:CustTitle      = ioJsonObject:GetCharacter("title")
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
           THIS-OBJECT:CustNum        = liCustnum.
       
       IF ioJsonObject:Has("customer_accounts") THEN 
       DO:
          ASSIGN CustAccounts =  ioJsonObject:GetJsonArray("customer_accounts").
           
          DO liCustAcctCnt = 1 TO CustAccounts:LENGTH:
               
               ASSIGN CustAcctObj = CustAccounts:GetJsonObject(liCustAcctCnt).

               ASSIGN aOCustAcct = NEW cm.CustomerAccount(THIS-OBJECT).
                      
               aOCustAcct:mCreateData(CustAcctObj).
               
               CREATE ttCustAccts.
               ASSIGN ttCustAccts.custacctobj = aOCustAcct
                      ttCustAccts.custacctnum = mGetNextCustAcctNum().
                      
               ASSIGN THIS-OBJECT:BillAcctIDData   = THIS-OBJECT:BillAcctIDData   + "|" + aOCustAcct:mGetBillAcctIDData().
               
                /*Example for the CustAcctData format
                 custacctid1;billacct1,billaccct2|custacct2;billacct3,billacct4|etc.. */
          END.

       END.
       
       ASSIGN THIS-OBJECT:BillAcctIDData   = TRIM(THIS-OBJECT:BillAcctIDData, "|").
              
       mAssignCategory().
     
   END METHOD.

   METHOD PUBLIC VOID mStoreCustomer():

      FIND FIRST Customer WHERE Customer.CustNum  =  THIS-OBJECT:CustNum EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF LOCKED(Customer) THEN 
         UNDO , THROW NEW ParamError ("Customer record is locked.").
       
      IF NOT AVAILABLE Customer THEN 
      DO:
         CREATE Customer.
      END.   

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
         Customer.Category          = THIS-OBJECT:Category.

   END METHOD.

   METHOD PUBLIC VOID mStoreCustomerAccount():

      FOR EACH ttCustAccts:
         CAST(ttCustAccts.custacctobj,cm.CustomerAccount):mStoreData().
      END.

   END METHOD.

   METHOD PUBLIC VOID mStoreData():

       mStoreCustomer().
       
       mStoreCustomerAccount().

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
            ioCustAcctObject = CAST(ttCustAccts.custacctobj,cm.CustomerAccount):mResult().
            ioCustArray:Add(ioCustAcctObject).
        END.      
        
        ioCustObj:Add("cust-num" , THIS-OBJECT:CustNum).
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
    
    METHOD PUBLIC INTEGER mGetBillingAcctID(INPUT liBillAcctIndex AS INTEGER) :
        
        IF THIS-OBJECT:BillAcctIDData = "" THEN 
            UNDO , THROW NEW ParamError("Billing Account data is not assigned.").
            
        /*custacctid1;billacct1,billaccct2|custacct2;billacct3,billacct4|etc.. */    
        
        DEFINE VARIABLE lcTotalBillAcctData AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcBillAcctList      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcBillAcct          AS CHARACTER NO-UNDO.
        
        ASSIGN lcTotalBillAcctData  =  ENTRY(1 , THIS-OBJECT:BillAcctIDData , "|")
               lcBillAcctList       =  ENTRY(2 , lcTotalBillAcctData        , ";").
               
        IF NUM-ENTRIES(lcBillAcctList) < liBillAcctIndex THEN 
            UNDO , THROW NEW ParamError("Invalid billing account index.").
        
        ASSIGN lcBillAcct  =  ENTRY(liBillAcctIndex , lcBillAcctList).
        
        INTEGER(lcBillAcct) NO-ERROR.
        
        IF ERROR-STATUS:ERROR THEN 
            UNDO , THROW NEW ParamError("Billing Account type cast error").
            
        RETURN INTEGER(lcBillAcct).
        
    END.
    
    METHOD PUBLIC VOID mAssignDefaultAddressObj (INPUT ioAddrObject AS JsonObject):
        
         ASSIGN THIS-OBJECT:DefaultAddressObj  = NEW JsonObject()
                THIS-OBJECT:DefaultAddressObj  = ioAddrObject.
        
    END METHOD.
    
    
    METHOD PUBLIC VOID mUpdateCustomerData (INPUT ioCustObj AS JsonObject) :
        
        DEFINE VARIABLE lcSupportedTitles AS CHARACTER INITIAL "Sr.,Mr.,Sr,Mr,Mrs.,Sra.,Mrs,Sra" NO-UNDO.
        DEFINE VARIABLE liCustnum         AS INTEGER   NO-UNDO.
        DEFINE VARIABLE CustAccounts      AS CLASS     JsonArray  NO-UNDO.
        DEFINE VARIABLE CustAcctObj       AS CLASS     JsonObject NO-UNDO.
        DEFINE VARIABLE liCustAcctCnt     AS INTEGER   NO-UNDO.
           
        DEFINE VARIABLE ioMarketingObj    AS CLASS     JsonObject NO-UNDO .
        DEFINE VARIABLE ioCustIDObject    AS CLASS     JsonObject NO-UNDO.
        
        DEFINE BUFFER bfcustomer FOR customer.
        
        mIsCustomerUpdate(INPUT ioCustObj).
        
        IF THIS-OBJECT:isCustomerUpdate = FALSE THEN RETURN.
        
        FIND FIRST bfCustomer WHERE bfcustomer.custnum = THIS-OBJECT:CustNum NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfcustomer THEN 
            UNDO, THROW NEW ParamError("Customer update failed.").
        
        IF ioCustObj:Has("title") THEN 
        DO:
            IF LOOKUP(ioCustObj:GetCharacter("title"), lcSupportedTitles) EQ 0
            THEN UNDO, THROW NEW ParamError(SUBSTITUTE("unsupported_person_title|&1",ioCustObj:GetCharacter("title"))).
            THIS-OBJECT:CustTitle = ioCustObj:GetCharacter("title").
        END.
        ELSE THIS-OBJECT:CustTitle  =  bfcustomer.HonTitle.        
        
        ASSIGN 
            THIS-OBJECT:FirstName     = (IF ioCustObj:Has("fname")  THEN ioCustObj:GetCharacter("fname")  ELSE bfCustomer.FirstName )
            THIS-OBJECT:SurName1      = (IF ioCustObj:Has("lname")  THEN ioCustObj:GetCharacter("lname")  ELSE bfCustomer.CustName  )   
            THIS-OBJECT:SurName2      = (IF ioCustObj:Has("lname2") THEN ioCustObj:GetCharacter("lname2") ELSE bfCustomer.SurName2  )
            THIS-OBJECT:Profession    = (IF ioCustObj:Has("profession") THEN ioCustObj:GetCharacter("profession") ELSE bfCustomer.Profession )
            THIS-OBJECT:Nationality   = (IF ioCustObj:Has("nationality") THEN ioCustObj:GetCharacter("nationality") ELSE  bfCustomer.Nationality)
            THIS-OBJECT:Language      = (IF ioCustObj:Has("language") THEN mGetLanguage(ioCustObj:GetCharacter("language")) ELSE STRING(bfCustomer.Language))
            THIS-OBJECT:SelfEmployed  = (IF ioCustObj:Has("self_employed") THEN ioCustObj:GetLogical("self_employed") ELSE NO )  
            THIS-OBJECT:BirthDay      = (IF ioCustObj:Has("birthday") THEN Func.Common:mGetDate(ioCustObj:GetCharacter("birthday")) ELSE bfCustomer.Birthday) 
            THIS-OBJECT:Email         = (IF ioCustObj:Has("email") THEN ioCustObj:GetCharacter("email") ELSE bfCustomer.Email)
            THIS-OBJECT:MobileNumber  = (IF ioCustObj:Has("sms_number") THEN ioCustObj:GetCharacter("sms_number") ELSE bfCustomer.SMSNumber) 
            THIS-OBJECT:FixedNumber   = (IF ioCustObj:Has("phone_number") THEN ioCustObj:GetCharacter("phone_number") ELSE bfCustomer.Phone)            
        .    
        
        IF ioCustObj:Has("residential")
        THEN DO:
            ioCustIDObject  = NEW JsonObject().
            ioCustIDObject  = ioCustObj:GetJsonObject("residential").
            ASSIGN 
                THIS-OBJECT:CustIdType = (IF ioCustIDObject:Has("id_type")   THEN ioCustIDObject:GetCharacter("id_type")   ELSE bfCustomer.CustIdType )
                THIS-OBJECT:CustId     = (IF ioCustIDObject:Has("person_id") THEN ioCustIDObject:GetCharacter("person_id") ELSE bfCustomer.OrgId ) 
                .
        END.
        ELSE IF ioCustObj:Has("company")
        THEN DO:
            ASSIGN 
                ioCustIDObject             = NEW JsonObject()
                ioCustIDObject             = ioCustObj:GetJsonObject("company")    
                THIS-OBJECT:CustId         = (IF ioCustIDObject:Has("company_id") THEN ioCustIDObject:GetCharacter("company_id") ELSE bfCustomer.OrgId)
                THIS-OBJECT:CustIdType     = (IF ioCustIDObject:Has("id_type")    THEN ioCustIDObject:GetCharacter("id_type")    ELSE bfCustomer.CustIdType )
                THIS-OBJECT:FoundationDate = (IF ioCustIDObject:Has("foundation_date") THEN Func.Common:mGetDate(ioCustIDObject:GetCharacter("foundation_date")) ELSE bfCustomer.FoundationDate) 
                THIS-OBJECT:CompanyName    = (IF ioCustIDObject:Has("site_name") THEN ioCustIDObject:GetCharacter("site_name")  ELSE bfCustomer.CompanyName )
                .          
        END.
        ELSE UNDO , THROW NEW ParamError ("Customer ID Data does not exist.").
        
        IF ioCustObj:Has("customer_accounts") THEN 
        DO:
            
            ASSIGN CustAccounts = ioCustObj:GetJsonArray("customer_accounts").
           
            DO liCustAcctCnt  = 1 TO CustAccounts:LENGTH :
               
                ASSIGN 
                    CustAcctObj = CustAccounts:GetJsonObject(liCustAcctCnt)
                    aOCustAcct  = NEW cm.CustomerAccount(THIS-OBJECT).
                      
                aOCustAcct:mUpdateCustAcctData(INPUT THIS-OBJECT:CustAcctID , INPUT CustAcctObj).
               
                CREATE ttCustAccts.
                ASSIGN 
                    ttCustAccts.custacctobj = aOCustAcct
                    ttCustAccts.custacctnum = mGetNextCustAcctNum().
                      
                ASSIGN THIS-OBJECT:BillAcctIDData = THIS-OBJECT:BillAcctIDData   + "|" + aOCustAcct:mGetBillAcctIDData().
               
            /*Example for the CustAcctData format
             CustacctID1;Bankacct1,Bankacct2|CustacctID2;Bankacct3,Bankacct4|etc.... 
             custacctid1;billacct1,billaccct2|custacct2;billacct3,billacct4|etc.. */

            END.          
            
        END.
        
        ASSIGN 
            THIS-OBJECT:CustAcctData   = TRIM(THIS-OBJECT:CustAcctData, "|")
            THIS-OBJECT:BillAcctIDData = TRIM(THIS-OBJECT:BillAcctIDData, "|").
              
        mAssignCategory().
        
     END METHOD. 
     
     
     METHOD PUBLIC VOID mIsCustomerUpdate(INPUT ioCustObj AS JsonObject ) :
         
         
         DEFINE VARIABLE liCustomerUpdate AS LOGICAL NO-UNDO.
         
         ASSIGN THIS-OBJECT:isCustomerUpdate  =  (ioCustObj:Has("title") OR 
                                                  ioCustObj:Has("fname") OR 
                                                  ioCustObj:Has("lname") OR
                                                  ioCustObj:Has("lname2") OR
                                                  ioCustObj:Has("profession") OR
                                                  ioCustObj:Has("nationality") OR
                                                  ioCustObj:Has("language") OR
                                                  ioCustObj:Has("self_employed") OR
                                                  ioCustObj:Has("birthday") OR
                                                  ioCustObj:Has("email") OR
                                                  ioCustObj:Has("sms_number") OR
                                                  ioCustObj:Has("phone_number") OR
                                                  ioCustObj:Has("company") OR
                                                  ioCustObj:Has("residential") OR
                                                  ioCustObj:Has("customer_accounts")                                                           
                                                  ).
         
     END METHOD.

END CLASS.