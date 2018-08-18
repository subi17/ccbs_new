 
/*------------------------------------------------------------------------
   File        : CustomerAccount
   Purpose     : This will store the data of the Customer Accounts
   Syntax      : 
   Description : 
   Author(s)   : Koundinya Maddali
   Created     : Tue Jun 12 11:00:05 IST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS cm.CustomerAccount IMPLEMENTS bss.cls.IObjectStorage : 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
	DEFINE PUBLIC PROPERTY objCustomer AS CLASS cm.Customer NO-UNDO 
	    GET.
	    SET.
	    
	DEFINE PUBLIC PROPERTY defaultAcct AS LOGICAL NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY acctName AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY validFrom AS DATETIME-TZ NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY validTo AS DATETIME-TZ NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY accountId AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY custNum AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY statusCode  AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY billAddrID AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY objBillingAddress AS CLASS om.Address NO-UNDO 
        GET.
        SET.    
       
    DEFINE PUBLIC PROPERTY billingAcctIds AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE TEMP-TABLE ttBillingAcct NO-UNDO 
        FIELDS BillAcctObj  AS CLASS Progress.Lang.Object
        FIELDS BillAcctNum  AS INTEGER
        INDEX BillAcctNum IS PRIMARY UNIQUE BillAcctNum.
		
	CONSTRUCTOR PUBLIC CustomerAccount ( INPUT iobjCustomer AS CLASS cm.Customer ):
		
		ASSIGN objCustomer = iobjCustomer.
		
	END CONSTRUCTOR.
	
	METHOD PUBLIC VOID mCreateData(INPUT ioCustAcctObject AS CLASS JsonObject):
	   
	  DEFINE VARIABLE BillAcctArray     AS CLASS JsonArray NO-UNDO. 
	  DEFINE VARIABLE objBillAddrStruct AS CLASS JsonObject NO-UNDO.
	    
      ASSIGN 
          THIS-OBJECT:defaultAcct = ioCustAcctObject:GetLogical("default_account")
          THIS-OBJECT:acctName    = ioCustAcctObject:GetCharacter("account_name")
          THIS-OBJECT:validFrom   =  DATETIME-TZ(TODAY)
          THIS-OBJECT:validTo     =  DATETIME-TZ(DATE(12,31,2049))
          THIS-OBJECT:accountId   =  NEXT-VALUE (accountid)            
          THIS-OBJECT:statusCode  =  0
          objCustomer:custAcctID  =  THIS-OBJECT:accountId. /*0=pending, 1=active*/
          
      IF ioCustAcctObject:Has("billing_address") THEN 
      DO:    
          ASSIGN 
              objBillingAddress = NEW om.Address()
              objBillAddrStruct = NEW JsonObject()
              objBillAddrStruct = ioCustAcctObject:GetJsonObject("billing_address").

          objBillingAddress:mCreateData(objBillAddrStruct).

          ASSIGN objBillingAddress:AddrType  = {&BILLING_ADDRESS}. 
                 
          IF THIS-OBJECT:defaultAcct = TRUE THEN 
              objCustomer:mAssignDefaultAddressObj(objBillAddrStruct).
             
          objCustomer:mAssignRegion(objBillAddrStruct:GetCharacter("region")).    
      END.
      
      IF ioCustAcctObject:Has("billing_accounts") THEN 
      DO:
          ASSIGN 
              BillAcctArray = ioCustAcctObject:GetJsonArray("billing_accounts").
          mCreateBillingAcct(BillAcctArray).
      END.
	    
	END METHOD.
	
	METHOD PUBLIC VOID mCreateBillingAcct(INPUT iobjBillAcctArray AS CLASS JsonArray):
	    
        DEFINE VARIABLE liBillAcctCnt     AS INTEGER NO-UNDO.
        DEFINE VARIABLE objBillAcctStruct AS CLASS   JsonObject        NO-UNDO.
        DEFINE VARIABLE objBillAcct       AS CLASS   cm.BillingAccount NO-UNDO.
        
        DO liBillAcctCnt = 1 TO iobjBillAcctArray:LENGTH :
              
            ASSIGN 
                objBillAcctStruct = iobjBillAcctArray:GetJsonObject(liBillAcctCnt).

            IF liBillAcctCnt = 1 THEN
                objCustomer:bankCode = objBillAcctStruct:GetCharacter("bank_account").

            objBillAcct = NEW cm.BillingAccount(THIS-OBJECT).

            objBillAcct:mCreateData(objBillAcctStruct).
              
            CREATE ttBillingAcct.
            ASSIGN 
                ttBillingAcct.BillAcctObj = objBillAcct
                ttBillingAcct.BillAcctNum = mNextBillAcctNum().
                     
            ASSIGN 
                THIS-OBJECT:billingAcctIds = THIS-OBJECT:billingAcctIds  + "," + objBillAcct:mGetBillingAcctID().                    
        END.      
	    
        ASSIGN 
            THIS-OBJECT:billingAcctIds = TRIM(THIS-OBJECT:billingAcctIds , ",").
	    
	END METHOD.
	
	METHOD PUBLIC VOID mStoreData():
	                                        
    	CREATE CustomerAccount.
    	    
        ASSIGN 
            Customeraccount.AccountID   = THIS-OBJECT:accountId
            CustomerAccount.CustNum     = objCustomer:CustNum
            CustomerAccount.DefaultAcc  = THIS-OBJECT:defaultAcct
            CustomerAccount.AccountName = THIS-OBJECT:acctName
            CustomerAccount.FromDate    = THIS-OBJECT:validFrom
            CustomerAccount.ToDate      = THIS-OBJECT:validTo
            CustomerAccount.StatusCode  = THIS-OBJECT:statusCode
            THIS-OBJECT:custNum         = CustomerAccount.CustNum
            .
	   
	    IF VALID-OBJECT(objBillingAddress) THEN 
	    DO:
            ASSIGN 
                objBillingAddress:KeyValue  = THIS-OBJECT:custNum.
            objBillingAddress:mStoreData().
            ASSIGN THIS-OBJECT:billAddrId = objBillingAddress:AddrID.	         
	    END.
	     
        mStoreBillingAcct().   
	    
	END METHOD.
	
	METHOD PUBLIC VOID mStoreBillingAcct():
	    
	    FOR EACH ttBillingAcct :
            CAST(ttBillingAcct.BillAcctObj, cm.BillingAccount):mStoreData().
        END.
        
	END METHOD.
	
	METHOD PUBLIC CHARACTER mGetBillAcctIDData():
	    
	    RETURN (STRING(THIS-OBJECT:accountId) + ";" + THIS-OBJECT:billingAcctIds).
	    
	END METHOD.
	
	METHOD PUBLIC INTEGER mNextBillAcctNum() :
	    
	    FIND LAST ttBillingAcct USE-INDEX BillAcctNum NO-ERROR.
	    IF NOT AVAILABLE ttBillingAcct THEN RETURN 1.
	    ELSE RETURN ( ttBillingAcct.BillAcctNum + 1 ) . 
	
	END METHOD.
	
    METHOD PUBLIC JsonObject mResult():
        
        DEFINE VARIABLE loBillAcctArray AS CLASS JsonArray NO-UNDO.
        DEFINE VARIABLE loBillAcctobj   AS CLASS JsonObject NO-UNDO.
        DEFINE VARIABLE loCustAcctObj   AS CLASS JsonObject NO-UNDO.
        
        ASSIGN loBillAcctArray = NEW JsonArray()
               loBillAcctobj   = NEW JsonObject()
               loCustAcctObj   = NEW JsonObject(). 
               
        FOR EACH ttBillingAcct:
            
            loBillAcctobj = CAST(ttBillingAcct.BillAcctObj, cm.BillingAccount):mResult().
            loBillAcctArray:Add(loBillAcctobj).
            
        END.
        
        loCustAcctObj:Add("account-id" , THIS-OBJECT:accountId).
        IF TEMP-TABLE ttBillingAcct:HAS-RECORDS THEN 
            loCustAcctObj:Add("billing-accounts" , loBillAcctArray).
        
        RETURN loCustAcctObj.

    END METHOD.

	METHOD PUBLIC VOID mGetBillingAddressID() :
	    
	    DEFINE BUFFER bfAddress FOR Address.
	    
	    FIND FIRST bfAddress WHERE bfAddress.KeyValue     =  STRING(THIS-OBJECT:custNum) 
	                           AND bfAddress.HostTable    =  "CUSTOMER"
	                           AND bfAddress.AddressType  =  {&BILLING_ADDRESS} NO-LOCK NO-ERROR.
	                           
	    IF NOT AVAILABLE bfAddress THEN  
            UNDO, THROW NEW ParamError("Billing Address not found to update.").
            
         ASSIGN THIS-OBJECT:billAddrId  =  bfAddress.AddressID.	                                   
	    
	END METHOD.	
	
    METHOD PUBLIC VOID mFindCustomerAcct (INPUT iiCustAcct AS INTEGER) :       
       
        FIND FIRST CustomerAccount 
            WHERE CustomerAccount.AccountID  = iiCustAcct NO-LOCK NO-ERROR .
            
        IF NOT AVAILABLE CustomerAccount THEN  
            UNDO, THROW NEW ParamError(SUBSTITUTE("Account_not_found|&1", iiCustAcct)).
             
        ASSIGN 
            THIS-OBJECT:accountId   = CustomerAccount.AccountID
            THIS-OBJECT:custNum     = CustomerAccount.CustNum
            THIS-OBJECT:defaultAcct = CustomerAccount.DefaultAcc
            THIS-OBJECT:acctName    = CustomerAccount.AccountName
            THIS-OBJECT:validFrom   = CustomerAccount.FromDate
            THIS-OBJECT:validTo     = CustomerAccount.ToDate
            THIS-OBJECT:statusCode  = CustomerAccount.StatusCode
            objCustomer:custAcctID  = THIS-OBJECT:accountId.
                  
       
    END METHOD.
    
    METHOD PUBLIC INTEGER mGetCustomerAcctId() :
        
        RETURN THIS-OBJECT:accountId.
        
    END METHOD.
    
    METHOD PUBLIC INTEGER mGetCustNum():
        
        RETURN THIS-OBJECT:custNum.
        
    END METHOD.
	
    DESTRUCTOR PUBLIC CustomerAccount ( ):
        
        IF VALID-OBJECT(objCustomer) THEN 
            DELETE OBJECT objCustomer.
            
        IF VALID-OBJECT (objBillingAddress) THEN 
            DELETE OBJECT objBillingAddress.

    END DESTRUCTOR.
    
END CLASS.