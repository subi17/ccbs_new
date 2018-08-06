 
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
       
    DEFINE PUBLIC PROPERTY ValiTo AS DATETIME-TZ NO-UNDO
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
		
	CONSTRUCTOR PUBLIC CustomerAccount ( INPUT objCustomer AS CLASS cm.Customer ):
		
		ASSIGN objCustomer = objCustomer.
		
	END CONSTRUCTOR.


	
	METHOD PUBLIC VOID mCreateData(INPUT ioCustAcctObject AS CLASS JsonObject):
	    
	    DEFINE VARIABLE aoBillingAcct AS CLASS cm.BillingAccount NO-UNDO.
	    DEFINE VARIABLE BillAcctArray AS CLASS JsonArray NO-UNDO.
	    DEFINE VARIABLE BillAcctObj   AS CLASS JsonObject NO-UNDO.
	    DEFINE VARIABLE liBillAcctCnt AS INTEGER NO-UNDO.
	    
      ASSIGN 
          THIS-OBJECT:defaultAcct = ioCustAcctObject:GetLogical("default_account")
          THIS-OBJECT:acctName    = ioCustAcctObject:GetCharacter("account_name")
          THIS-OBJECT:validFrom   =  DATETIME-TZ(TODAY)
          THIS-OBJECT:ValiTo      =  DATETIME-TZ(DATE(12,31,2049))
          THIS-OBJECT:accountId   =  NEXT-VALUE (accountid)  
          THIS-OBJECT:custNum     =  objCustomer:CustNum
          THIS-OBJECT:statusCode  =  0. /*0=pending, 1=active*/
          
      IF ioCustAcctObject:Has("billing_address") THEN 
      DO:    
          objBillingAddress = NEW om.Address().

          objBillingAddress:mCreateData(ioCustAcctObject:GetJsonObject("billing_address")).

          ASSIGN objBillingAddress:KeyValue  = THIS-OBJECT:custNum
                 objBillingAddress:AddrType  = {&BILLING_ADDRESS}. 
                 
          IF THIS-OBJECT:defaultAcct = TRUE THEN 
              objCustomer:mAssignDefaultAddressObj(ioCustAcctObject:GetJsonObject("billing_address")).
      END.
      
      IF ioCustAcctObject:Has("billing_accounts") THEN 
      DO:
          ASSIGN BillAcctArray = ioCustAcctObject:GetJsonArray("billing_accounts").
          
          DO liBillAcctCnt = 1 TO BillAcctArray:LENGTH :
              
              ASSIGN BillAcctObj = BillAcctArray:GetJsonObject(liBillAcctCnt).

              IF liBillAcctCnt = 1 THEN
                 objCustomer:bankCode = BillAcctObj:GetCharacter("bank_account").

              aoBillingAcct = NEW cm.BillingAccount(THIS-OBJECT).

              aoBillingAcct:mCreateData(BillAcctObj).
              
              CREATE ttBillingAcct.
              ASSIGN ttBillingAcct.BillAcctObj = aoBillingAcct
                     ttBillingAcct.BillAcctNum = mNextBillAcctNum().
                     
              ASSIGN THIS-OBJECT:billingAcctIds  =  THIS-OBJECT:billingAcctIds  + "," + aoBillingAcct:mGetBillingAcctID().
          END.            
      END.

      ASSIGN THIS-OBJECT:billingAcctIds   = TRIM(THIS-OBJECT:billingAcctIds , ",").
	    
	END METHOD.
	
	METHOD PUBLIC VOID mStoreData():
	    
	    FIND FIRST CustomerAccount WHERE CustomerAccount.AccountID = THIS-OBJECT:accountId EXCLUSIVE-LOCK  NO-WAIT NO-ERROR.
	    
	    IF LOCKED CustomerAccount THEN 
            UNDO, THROW NEW ParamError("Customer Account record is locked.").
	    
	    IF NOT AVAILABLE CustomerAccount THEN                                     
    	    CREATE CustomerAccount.
    	    
	    ASSIGN Customeraccount.AccountID   = THIS-OBJECT:accountId
	           CustomerAccount.CustNum     = THIS-OBJECT:custNum
	           CustomerAccount.DefaultAcc  = THIS-OBJECT:defaultAcct
	           CustomerAccount.AccountName = THIS-OBJECT:acctName
	           CustomerAccount.FromDate    = THIS-OBJECT:validFrom
	           CustomerAccount.ToDate      = THIS-OBJECT:ValiTo
	           CustomerAccount.StatusCode  = THIS-OBJECT:statusCode
	     .
	   
	     IF VALID-OBJECT(objBillingAddress)
	     THEN DO:
             objBillingAddress:mStoreData().
             ASSIGN THIS-OBJECT:billAddrId = objBillingAddress:AddrID.	         
	     END.
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
        
        loCustAcctObj:Add("cust-acct-num" , THIS-OBJECT:accountId).
        loCustAcctObj:Add("billing-address-id" , THIS-OBJECT:billAddrId).
        IF TEMP-TABLE ttBillingAcct:HAS-RECORDS
        THEN loCustAcctObj:Add("billing-accounts" , loBillAcctArray).
        
        RETURN loCustAcctObj.

    END METHOD.
    
    METHOD PUBLIC VOID mUpdateCustAcctData( INPUT iiCustAcctNum AS INTEGER , INPUT ioCustAcctObj  AS JsonObject ) :
        
        DEFINE BUFFER bfCustAcct FOR CustomerAccount.
        
        DEFINE VARIABLE liBillAcctID AS INTEGER NO-UNDO.
        
        DEFINE VARIABLE aoBillingAcct AS CLASS cm.BillingAccount NO-UNDO.
        DEFINE VARIABLE BillAcctArray AS CLASS   JsonArray  NO-UNDO.
        DEFINE VARIABLE BillAcctObj   AS CLASS   JsonObject NO-UNDO.
        DEFINE VARIABLE liBillAcctCnt AS INTEGER NO-UNDO.
        
        FIND FIRST bfCustAcct WHERE bfCustAcct.AccountID  = iiCustAcctNum NO-LOCK NO-ERROR .
        
        IF NOT AVAILABLE bfCustAcct THEN 
            UNDO, THROW NEW ParamError(SUBSTITUTE("Account_not_found|&1", iiCustAcctNum)).
             
        ASSIGN 
            THIS-OBJECT:defaultAcct = (IF ioCustAcctObj:Has("default_account") THEN ioCustAcctObj:GetLogical("default_account")   ELSE bfCustAcct.DefaultAcc )   
            THIS-OBJECT:acctName    = (IF ioCustAcctObj:Has("account_name")    THEN ioCustAcctObj:GetCharacter("account_name") ELSE bfCustAcct.AccountName)
            THIS-OBJECT:validFrom   = bfCustAcct.FromDate
            THIS-OBJECT:ValiTo      = bfCustAcct.ToDate
            THIS-OBJECT:accountId   = bfCustAcct.AccountID  
            THIS-OBJECT:custNum     = bfCustAcct.CustNum
            THIS-OBJECT:statusCode  = bfCustAcct.StatusCode /*0=pending, 1=active*/
            .
            
        IF ioCustAcctObj:Has("billing_address")
        THEN DO:
                        
            mGetBillingAddressID().  /* We will get the Billing Address ID for updation*/
            
            objBillingAddress = NEW om.Address().            
            ASSIGN 
                objBillingAddress:KeyValue = THIS-OBJECT:custNum
                objBillingAddress:AddrType = {&BILLING_ADDRESS}
                .
                   
            IF THIS-OBJECT:defaultAcct = TRUE 
            THEN DO:                
                objCustomer:mAssignDefaultAddressObj(ioCustAcctObj:GetJsonObject("billing_address")).                
            END.                  
            
            objBillingAddress:mUpdateAddressData(INPUT THIS-OBJECT:billAddrId , INPUT ioCustAcctObj:GetJsonObject("billing_address")).           
                            
        END.        
        
        IF ioCustAcctObj:Has("billing_accounts")
        THEN DO:
            
            ASSIGN 
                BillAcctArray = ioCustAcctObj:GetJsonArray("billing_accounts").
            
            DO liBillAcctCnt = 1 TO BillAcctArray:LENGTH :
                                
                ASSIGN  
                    BillAcctObj   = BillAcctArray:GetJsonObject(liBillAcctCnt)
                    aoBillingAcct = NEW cm.BillingAccount(THIS-OBJECT).                    
                    
                ASSIGN liBillAcctID = mGetBillingAcctID(). /*We will get the Billing Account ID here.*/
                           
                aoBillingAcct:mUpdateBillAcctData( INPUT liBillAcctID , INPUT BillAcctObj).
                
                CREATE ttBillingAcct.
                ASSIGN 
                    ttBillingAcct.BillAcctObj = aoBillingAcct
                    ttBillingAcct.BillAcctNum = mNextBillAcctNum().
                       
                ASSIGN THIS-OBJECT:billingAcctIds = THIS-OBJECT:billingAcctIds  + "," + aoBillingAcct:mGetBillingAcctID().
                    
            END.            
        END.

        ASSIGN THIS-OBJECT:billingAcctIds = TRIM(THIS-OBJECT:billingAcctIds, ",").
        
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
	
    METHOD PUBLIC INTEGER mGetBillingAcctID():	    
	    
        FIND FIRST InvoiceTargetGroup WHERE InvoiceTargetGroup.Brand        =  Syst.Var:gcBrand
            AND InvoiceTargetGroup.AccountID    =  THIS-OBJECT:accountId
            AND InvoiceTargetGroup.CustAccName  =  THIS-OBJECT:acctName NO-LOCK NO-ERROR.
	                                    
        IF NOT AVAILABLE InvoiceTargetGroup THEN 
            UNDO, THROW NEW ParamError("Billing Account not found to update.").	    
            
        RETURN (InvoiceTargetGroup.ITGroupID).                                
	    
    END METHOD.
	
    DESTRUCTOR PUBLIC CustomerAccount ( ):
        
        IF VALID-OBJECT(objCustomer) THEN 
            DELETE OBJECT objCustomer.
            
        IF VALID-OBJECT (objBillingAddress) THEN 
            DELETE OBJECT objBillingAddress.

    END DESTRUCTOR.
    
END CLASS.