 
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

    DEFINE PUBLIC PROPERTY custNum AS INTEGER NO-UNDO
        GET.
        SET.

    DEFINE PUBLIC PROPERTY category AS CHARACTER NO-UNDO
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
    
    DEFINE PUBLIC PROPERTY profession AS CHARACTER NO-UNDO
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
        
    DEFINE PUBLIC PROPERTY selfEmployed AS LOGICAL NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY birthDay AS DATE NO-UNDO
        GET.
        SET.
    
    DEFINE PUBLIC PROPERTY foundationDate AS DATE NO-UNDO
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
        
    DEFINE PUBLIC PROPERTY authCustIdType AS CHARACTER NO-UNDO
        GET.
        SET.
    
    DEFINE PUBLIC PROPERTY authCustId AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY objCustAccount AS CLASS cm.CustomerAccount NO-UNDO 
        GET.
        SET.   
        
    DEFINE PUBLIC PROPERTY isNewCustomer AS LOGICAL NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY companyName AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY custAcctData AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY billAcctIdData AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY defaultAddressObj AS CLASS JsonObject NO-UNDO 
        GET.
        SET. 
        
    DEFINE PUBLIC PROPERTY orderCustBankAcct AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY isProCustomer AS LOGICAL NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY orderChannel AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY gdpr_consent AS LOGICAL NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY custAcctID AS INTEGER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY isCustomerUpdate AS LOGICAL NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY bankCode AS CHARACTER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY region AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY invoiceGroup AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE TEMP-TABLE ttCustAccts NO-UNDO 
        FIELDS custacctobj  AS CLASS Progress.Lang.Object
        FIELDS custacctnum  AS INTEGER
        INDEX custacctnum IS PRIMARY UNIQUE custacctnum. 

	CONSTRUCTOR PUBLIC Customer():
	    
	END CONSTRUCTOR.

  METHOD PUBLIC CHARACTER mGetCustomerId(ioJsonObject AS CLASS JsonObject):

     DEFINE VARIABLE lobjCustomerType AS CLASS JsonObject NO-UNDO.

     IF ioJsonObject:Has("residential") THEN 
     DO:
        lobjCustomerType = NEW JsonObject().

        lobjCustomerType = ioJsonObject:GetJsonObject("residential").

        RETURN lobjCustomerType:GetCharacter("id").
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
           THIS-OBJECT:custIdType = lobjCustomerType:GetCharacter("id_type")  
           THIS-OBJECT:custId     = lobjCustomerType:GetCharacter("id").  
     END.
     ELSE IF ioJsonObject:Has("company") THEN 
     DO:
         lobjCustomerType = NEW JsonObject().

         lobjCustomerType = ioJsonObject:GetJsonObject("company").

         ASSIGN    
            THIS-OBJECT:custId         =  lobjCustomerType:GetCharacter("company_id")
            THIS-OBJECT:custIdType     =  lobjCustomerType:GetCharacter("id_type")
            THIS-OBJECT:foundationDate =  Func.Common:mGetDate(lobjCustomerType:GetCharacter("foundation_date"))
            THIS-OBJECT:companyName    =  lobjCustomerType:GetCharacter("site_name").          
     END.

    END METHOD.  

    METHOD PUBLIC VOID mFindCustomer(iiCustNum AS INTEGER):

        DEFINE BUFFER bfCustomer FOR Customer.
      
        FIND FIRST bfCustomer NO-LOCK WHERE bfCustomer.CustNum = iiCustNum NO-ERROR.

        IF NOT AVAILABLE bfCustomer THEN
             UNDO, THROW NEW ParamError(SUBSTITUTE("customer_not_found|&1", iiCustNum)).

        /* this is useful for the rowtype_aggrement for existing customer who does not have any customer update information*/

        ASSIGN
            THIS-OBJECT:custNum        = bfCustomer.CustNum
            THIS-OBJECT:category       = bfCustomer.Category
            THIS-OBJECT:custId         = bfCustomer.orgID
            THIS-OBJECT:custIdType     = bfCustomer.CustIdType            
            THIS-OBJECT:custTitle      = bfCustomer.HonTitle
            THIS-OBJECT:firstName      = bfCustomer.FirstName
            THIS-OBJECT:surName1       = bfCustomer.CustName
            THIS-OBJECT:surName2       = bfCustomer.SurName2
            THIS-OBJECT:companyName    = bfCustomer.CompanyName
            THIS-OBJECT:nationality    = bfCustomer.Nationality   
            THIS-OBJECT:language       = STRING(bfCustomer.Language)
            THIS-OBJECT:authCustIdType = bfCustomer.AuthCustIdType
            THIS-OBJECT:authCustId     = bfCustomer.AuthCustId
            THIS-OBJECT:birthDay       = bfCustomer.BirthDay
            THIS-OBJECT:profession     = bfCustomer.Profession
            THIS-OBJECT:email          = bfCustomer.Email
            THIS-OBJECT:mobileNumber   = bfCustomer.SMSNumber
            THIS-OBJECT:fixedNumber    = bfCustomer.Phone
            THIS-OBJECT:foundationDate = bfCustomer.FoundationDate
            .
     
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
           ASSIGN 
               lobjCustAccount = ioJsonObject:GetJsonObject("customer_accounts").

           IF NOT llDefaultAcc THEN 
               ASSIGN llDefaultAcc = lobjCustAccount:GetLogical("default_account").
           ELSE 
               LEAVE.

           IF NOT llDefaultAcc THEN
               UNDO , THROW NEW ParamError ("Default customer account should be created when creating a new customer.").          
       END.            

   END METHOD.  

   METHOD PUBLIC VOID mCreateData
     (ioJsonObject AS CLASS JsonObject):

       DEFINE VARIABLE CustAccounts      AS CLASS JsonArray NO-UNDO.
       DEFINE VARIABLE objCustAcctStruct AS CLASS JsonObject NO-UNDO.
      
       DEFINE VARIABLE liCustAcctCnt AS INTEGER NO-UNDO.
       DEFINE VARIABLE liCustnum     AS INTEGER NO-UNDO.

       mValidate(ioJsonObject).

       mFillCustomerIDDetails(ioJsonObject).

       ASSIGN 
           THIS-OBJECT:isNewCustomer  = TRUE
           THIS-OBJECT:custTitle      = ioJsonObject:GetCharacter("title")
           THIS-OBJECT:firstName      = ioJsonObject:GetCharacter("fname")
           THIS-OBJECT:surName1       = ioJsonObject:GetCharacter("lname")
           THIS-OBJECT:surName2       = ioJsonObject:GetCharacter("lname2")
           THIS-OBJECT:profession     = ioJsonObject:GetCharacter("profession") WHEN ioJsonObject:Has("profession")
           THIS-OBJECT:nationality    = ioJsonObject:GetCharacter("nationality")
           THIS-OBJECT:language       = mGetLanguage(ioJsonObject:GetCharacter("language"))
           THIS-OBJECT:selfEmployed   = ioJsonObject:GetLogical("self_employed")
           THIS-OBJECT:birthDay       = Func.Common:mGetDate(ioJsonObject:GetCharacter("birthday"))
           THIS-OBJECT:email          = ioJsonObject:GetCharacter("email") WHEN ioJsonObject:Has("email")
           THIS-OBJECT:mobileNumber   = ioJsonObject:GetCharacter("sms_number")
           THIS-OBJECT:fixedNumber    = ioJsonObject:GetCharacter("phone_number") WHEN ioJsonObject:Has("phone_number")
           .
       
       IF ioJsonObject:Has("customer_accounts") THEN 
       DO:
           
           ASSIGN 
               objCustAcctStruct = ioJsonObject:GetJsonObject("customer_accounts").

           ASSIGN 
               objCustAccount = NEW cm.CustomerAccount(THIS-OBJECT).
                                 
           objCustAccount:mCreateData(objCustAcctStruct).
               
           CREATE ttCustAccts.
           ASSIGN 
               ttCustAccts.custacctobj = objCustAccount
               ttCustAccts.custacctnum = mGetNextCustAcctNum().
                      
           ASSIGN THIS-OBJECT:billAcctIdData = THIS-OBJECT:billAcctIdData   + "|" + objCustAccount:mGetBillAcctIDData().
               
       END.
       
       ASSIGN THIS-OBJECT:billAcctIdData   = TRIM(THIS-OBJECT:billAcctIdData, "|").
       
       /*Example for the CustAcctData format
        custacctid1;billacct1,billaccct2|custacct2;billacct3,billacct4|etc.. */
              
   END METHOD.

    METHOD PUBLIC VOID mStoreCustomer():

        DEFINE BUFFER bfNewCustomer FOR Customer.

        DEFINE VARIABLE liParamName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE liTempCust  AS INTEGER   NO-UNDO.

        mGetInvoiceGroup().
        
        ASSIGN 
            liParamName = "DefCust" + THIS-OBJECT:invoiceGroup.
      
        ASSIGN 
            THIS-OBJECT:custNum = INTEGER(Func.Common:mGetTMSParamValue("",liParamName,input {&INTEGER_PARAM})). 

        IF THIS-OBJECT:custNum = 0 OR THIS-OBJECT:custNum = ? THEN 
            ASSIGN 
                THIS-OBJECT:custNum = 300.
                
        ASSIGN liTempCust = THIS-OBJECT:custNum. /*cannot use properties as input-output params*/
                
        RUN Mm/copymobcu.p(INPUT-OUTPUT liTempCust, INPUT FALSE).
        
        ASSIGN THIS-OBJECT:custNum = liTempCust.
        
        FIND FIRST bfNewCustomer EXCLUSIVE-LOCK WHERE 
                   bfNewCustomer.CustNum = THIS-OBJECT:custNum
            NO-WAIT NO-ERROR.
                   
        IF LOCKED(bfNewCustomer) THEN 
            UNDO , THROW NEW ParamError ("Customer record is locked").
            
        IF NOT AVAILABLE bfNewCustomer THEN 
            UNDO , THROW NEW ParamError ("Customer record is not available to update").
        
        ASSIGN      
            THIS-OBJECT:custNum          = bfNewCustomer.CustNum
            bfNewCustomer.HonTitle       = THIS-OBJECT:custTitle  
            bfNewCustomer.FirstName      = THIS-OBJECT:firstName
            bfNewCustomer.CustName       = THIS-OBJECT:surName1
            bfNewCustomer.SurName2       = THIS-OBJECT:surName2
            bfNewCustomer.Nationality    = THIS-OBJECT:nationality
            bfNewCustomer.Language       = INTEGER(THIS-OBJECT:language)
            bfNewCustomer.CustIdType     = THIS-OBJECT:custIdType
            bfNewCustomer.OrgId          = THIS-OBJECT:custId
            bfNewCustomer.AuthCustIdType = THIS-OBJECT:authCustIdType
            bfNewCustomer.AuthCustId     = THIS-OBJECT:authCustId
            bfNewCustomer.Birthday       = THIS-OBJECT:birthDay
            bfNewCustomer.Email          = THIS-OBJECT:email
            bfNewCustomer.SMSNumber      = THIS-OBJECT:mobileNumber
            bfNewCustomer.Phone          = THIS-OBJECT:fixedNumber
            bfNewCustomer.FoundationDate = THIS-OBJECT:foundationDate
            bfNewCustomer.CompanyName    = THIS-OBJECT:companyName
            bfNewCustomer.Profession     = THIS-OBJECT:profession
            bfNewCustomer.Category       = THIS-OBJECT:category.

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
        IF NOT AVAILABLE ttCustAccts THEN 
            RETURN 1.
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
        END.      
        
        ioCustObj:Add("custnum" , THIS-OBJECT:custNum).
        IF TEMP-TABLE ttCustAccts:HAS-RECORDS THEN 
            ioCustObj:Add("customer-accounts" , ioCustAcctObject).
        
        RETURN ioCustObj.
        
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
            AND NOT THIS-OBJECT:orderChannel BEGINS "migration"
            AND NOT liBypassRules        
            AND LOOKUP(lcNumberType,"new,mnp") > 0
            AND liMultiSimType       <> {&MULTISIMTYPE_SECONDARY} THEN 
        DO:            
            
            liCustSubLimit =  Func.ValidateACC:mSubscriptionLimitCheck(INPUT THIS-OBJECT:custId ,
                                                                       INPUT THIS-OBJECT:custIdType,
                                                                       INPUT Func.ValidateACC:mIsSelfEmpl(THIS-OBJECT:category),
                                                                       INPUT Func.ValidateACC:mIsPro(THIS-OBJECT:category),
                                                                       1,
                                                                       OUTPUT liSubLimit,
                                                                       OUTPUT lisubs,
                                                                       OUTPUT liActLimit,
                                                                       OUTPUT liActs).
                                                                       
            IF liCustSubLimit = FALSE THEN 
                UNDO, THROW NEW ParamError("Subscription Limit Error to existing Customer."). 
            
            FOR FIRST bfpcCustomer WHERE
                      bfpcCustomer.Brand      =  Syst.Var:gcBrand       AND
                      bfpcCustomer.OrgId      =  THIS-OBJECT:custId     AND
                      bfpcCustomer.CustIdType =  THIS-OBJECT:custIdType AND
                      bfpcCustomer.Roles     <>  "inactive" NO-LOCK:
                          
                IF Func.ValidateOrder:mIsBarredSubscriptionExistsToCustomer(bfpcCustomer.Custnum) THEN 
                    UNDO, THROW NEW ParamError("barring").      
            END.                       
        END. /*  IF aoOrder:objCustomer:isNewCustomer = FALSE */
        
    END METHOD.
    
    METHOD PUBLIC INTEGER mGetBillingAcctID(INPUT liBillAcctIndex AS INTEGER) :
        
        IF THIS-OBJECT:billAcctIdData = "" THEN 
            UNDO , THROW NEW ParamError("Billing Account data is not assigned.").
            
        /*custacctid1;billacct1,billaccct2|custacct2;billacct3,billacct4|etc.. */    
        
        DEFINE VARIABLE lcTotalBillAcctData AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcBillAcctList      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcBillAcct          AS CHARACTER NO-UNDO.
       
        
        ASSIGN 
            lcTotalBillAcctData = ENTRY(1 , THIS-OBJECT:billAcctIdData , "|")
            lcBillAcctList      = ENTRY(2 , lcTotalBillAcctData        , ";").
            
        IF NUM-ENTRIES(lcBillAcctList) < liBillAcctIndex THEN 
            UNDO , THROW NEW ParamError("Invalid billing account index.").
        
        ASSIGN 
            lcBillAcct = ENTRY(liBillAcctIndex , lcBillAcctList).
        
        INTEGER(lcBillAcct) NO-ERROR.
        
        IF ERROR-STATUS:ERROR THEN 
            UNDO , THROW NEW ParamError("Billing Account type casting error").
            
        RETURN INTEGER(lcBillAcct).
        
    END.
    
    METHOD PUBLIC VOID mAssignBillAcctIdData(INPUT icBillAcctIdData AS CHARACTER):
        
        /*New billing account to the prepaid exising customer*/
        
        ASSIGN THIS-OBJECT:billAcctIdData = icBillAcctIdData.
        
    END METHOD.
    
    METHOD PUBLIC VOID mAssignDefaultAddressObj (INPUT ioAddrObject AS JsonObject):
        
        ASSIGN 
            THIS-OBJECT:defaultAddressObj = NEW JsonObject()
            THIS-OBJECT:defaultAddressObj = ioAddrObject.
        
    END METHOD.
    
    METHOD PUBLIC VOID mGetInvoiceGroup():
        
        IF THIS-OBJECT:region > "" THEN 
        DO:
            FIND Region WHERE Region.Region = THIS-OBJECT:region NO-LOCK NO-ERROR.
            IF AVAILABLE Region THEN 
            DO:            
                FOR FIRST InvGroup NO-LOCK WHERE
                          InvGroup.Brand   = Syst.Var:gcBrand AND
                          InvGroup.TaxZone = Region.TaxZone:                              
                    ASSIGN 
                        THIS-OBJECT:invoiceGroup = InvGroup.InvGroup.                               
                END.
            END.   
        END.

        /* if nothing was found based on region then get first with empty taxzone */
        FOR FIRST InvGroup NO-LOCK WHERE
                  InvGroup.Brand   = Syst.Var:gcBrand AND
                  InvGroup.TaxZone = "":                      
            ASSIGN 
                THIS-OBJECT:invoiceGroup = InvGroup.InvGroup.                     
        END.   

        /* if nothing was found then just get first */
        FOR FIRST InvGroup NO-LOCK WHERE
                  InvGroup.Brand   = Syst.Var:gcBrand:                      
            ASSIGN 
                THIS-OBJECT:invoiceGroup = InvGroup.InvGroup.                    
        END.   
        
    END METHOD.
    
    METHOD PUBLIC VOID mAssignRegion(INPUT lcRegion AS CHARACTER):

        ASSIGN THIS-OBJECT:region = lcRegion.
        
    END METHOD.
     
    DESTRUCTOR PUBLIC Customer():
        
        IF VALID-OBJECT(objCustAccount) THEN 
            DELETE OBJECT objCustAccount.
            
        IF VALID-OBJECT(DefaultAddressObj) THEN 
            DELETE OBJECT DefaultAddressObj.
    
    END DESTRUCTOR.

END CLASS.