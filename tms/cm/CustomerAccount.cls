 
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
	
	DEFINE PUBLIC PROPERTY ioCustomerObj AS CLASS cm.Customer NO-UNDO 
	    GET.
	    SET.
	    
	DEFINE PUBLIC PROPERTY DefaultAcct AS LOGICAL NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY AcctName AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY ValidFrom AS DATETIME-TZ NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY ValiTo AS DATETIME-TZ NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY AccountID AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY CustNum AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY StatusCode  AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY BillAddrID AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY aoBillAddrObj AS CLASS om.OrderAddress NO-UNDO 
        GET.
        SET.    
       
    DEFINE PUBLIC PROPERTY BillingAcctIDs AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE TEMP-TABLE ttBillingAcct NO-UNDO 
        FIELDS BillAcctObj  AS CLASS Progress.Lang.Object
        FIELDS BillAcctNum  AS INTEGER
        INDEX BillAcctNum IS PRIMARY UNIQUE BillAcctNum.
		
	CONSTRUCTOR PUBLIC CustomerAccount ( INPUT aoCustomer AS CLASS cm.Customer ):
		
		ASSIGN ioCustomerObj = aoCustomer.
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC CustomerAccount ( ):

        ASSIGN ioCustomerObj = ?
               aoBillAddrObj = ?.

	END DESTRUCTOR.
	
	METHOD PUBLIC VOID mCreateData(INPUT ioCustAcctObject AS CLASS JsonObject):
	    
	    DEFINE VARIABLE aoBillingAcct AS CLASS cm.BillingAccount NO-UNDO.
	    DEFINE VARIABLE BillAcctArray AS CLASS JsonArray NO-UNDO.
	    DEFINE VARIABLE BillAcctObj   AS CLASS JsonObject NO-UNDO.
	    DEFINE VARIABLE liBillAcctCnt AS INTEGER NO-UNDO.
	    
      ASSIGN 
          THIS-OBJECT:DefaultAcct = ioCustAcctObject:GetLogical("default_account")
          THIS-OBJECT:AcctName    = ioCustAcctObject:GetCharacter("account_name")
          THIS-OBJECT:ValidFrom   =  DATETIME-TZ(TODAY)
          THIS-OBJECT:ValiTo      =  DATETIME-TZ(DATE(12,31,2049))
          THIS-OBJECT:AccountID   =  NEXT-VALUE (accountid)  
          THIS-OBJECT:CustNum     =  ioCustomerObj:CustNum
          THIS-OBJECT:StatusCode  =  0. /*0=pending, 1=active*/
          
      IF ioCustAcctObject:Has("billing_address") THEN 
      DO:    
          aoBillAddrObj = NEW om.OrderAddress().

          aoBillAddrObj:mCreateData(ioCustAcctObject:GetJsonObject("billing_address")).

          ASSIGN aoBillAddrObj:KeyValue  = THIS-OBJECT:CustNum
                 aoBillAddrObj:AddrType  = {&BILLING_ADDRESS}. 
                 
          IF THIS-OBJECT:DefaultAcct = TRUE THEN 
              ioCustomerObj:mAssignDefaultAddressObj(ioCustAcctObject:GetJsonObject("billing_address")).
      END.
      
      IF ioCustAcctObject:Has("billing_accounts") THEN 
      DO:
          ASSIGN BillAcctArray = ioCustAcctObject:GetJsonArray("billing_accounts").
          
          DO liBillAcctCnt = 1 TO BillAcctArray:LENGTH :
              
              ASSIGN BillAcctObj = BillAcctArray:GetJsonObject(liBillAcctCnt).

              IF liBillAcctCnt = 1 THEN
                 ioCustomerObj:bankCode = BillAcctObj:GetCharacter("bank_account").

              aoBillingAcct = NEW cm.BillingAccount(THIS-OBJECT).

              aoBillingAcct:mCreateData(BillAcctObj).
              
              CREATE ttBillingAcct.
              ASSIGN ttBillingAcct.BillAcctObj = aoBillingAcct
                     ttBillingAcct.BillAcctNum = mNextBillAcctNum().
                     
              ASSIGN THIS-OBJECT:BillingAcctIDs  =  THIS-OBJECT:BillingAcctIDs  + "," + aoBillingAcct:mGetBillingAcctID().
          END.            
      END.

      ASSIGN THIS-OBJECT:BillingAcctIDs   = TRIM(THIS-OBJECT:BillingAcctIDs , ",").
	    
	END METHOD.
	
	METHOD PUBLIC VOID mStoreData():
	    
	    FIND FIRST CustomerAccount WHERE CustomerAccount.AccountID = THIS-OBJECT:AccountID EXCLUSIVE-LOCK  NO-WAIT NO-ERROR.
	    
	    IF LOCKED CustomerAccount THEN 
            UNDO, THROW NEW ParamError("Customer Account record is locked.").
	    
	    IF NOT AVAILABLE CustomerAccount THEN                                     
    	    CREATE CustomerAccount.
    	    
	    ASSIGN Customeraccount.AccountID   = THIS-OBJECT:AccountID
	           CustomerAccount.CustNum     = THIS-OBJECT:CustNum
	           CustomerAccount.DefaultAcc  = THIS-OBJECT:DefaultAcct
	           CustomerAccount.AccountName = THIS-OBJECT:AcctName
	           CustomerAccount.FromDate    = THIS-OBJECT:ValidFrom
	           CustomerAccount.ToDate      = THIS-OBJECT:ValiTo
	           CustomerAccount.StatusCode  = THIS-OBJECT:StatusCode
	     .
	   
	     IF VALID-OBJECT(aoBillAddrObj)
	     THEN DO:
             aoBillAddrObj:mStoreData().
             ASSIGN THIS-OBJECT:BillAddrID = aoBillAddrObj:AddrID.	         
	     END.
	     FOR EACH ttBillingAcct :
             CAST(ttBillingAcct.BillAcctObj, cm.BillingAccount):mStoreData().
	     END.
	    
	END METHOD.
	
	METHOD PUBLIC CHARACTER mGetBillAcctIDData():
	    
	    RETURN (STRING(THIS-OBJECT:AccountID) + ";" + THIS-OBJECT:BillingAcctIDs).
	    
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
        
        loCustAcctObj:Add("cust-acct-num" , THIS-OBJECT:AccountID).
        loCustAcctObj:Add("billing-address-id" , THIS-OBJECT:BillAddrID).
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
            THIS-OBJECT:DefaultAcct = (IF ioCustAcctObj:Has("default_account") THEN ioCustAcctObj:GetLogical("default_account")   ELSE bfCustAcct.DefaultAcc )   
            THIS-OBJECT:AcctName    = (IF ioCustAcctObj:Has("account_name")    THEN ioCustAcctObj:GetCharacter("account_name") ELSE bfCustAcct.AccountName)
            THIS-OBJECT:ValidFrom   = bfCustAcct.FromDate
            THIS-OBJECT:ValiTo      = bfCustAcct.ToDate
            THIS-OBJECT:AccountID   = bfCustAcct.AccountID  
            THIS-OBJECT:CustNum     = bfCustAcct.CustNum
            THIS-OBJECT:StatusCode  = bfCustAcct.StatusCode /*0=pending, 1=active*/
            .
            
        IF ioCustAcctObj:Has("billing_address")
        THEN DO:
                        
            mGetBillingAddressID().  /* We will get the Billing Address ID for updation*/
            
            aoBillAddrObj = NEW om.OrderAddress().            
            ASSIGN 
                aoBillAddrObj:KeyValue = THIS-OBJECT:CustNum
                aoBillAddrObj:AddrType = {&BILLING_ADDRESS}
                .
                   
            IF THIS-OBJECT:DefaultAcct = TRUE 
            THEN DO:                
                ioCustomerObj:mAssignDefaultAddressObj(ioCustAcctObj:GetJsonObject("billing_address")).                
            END.                  
            
            aoBillAddrObj:mUpdateAddressData(INPUT THIS-OBJECT:BillAddrID , INPUT ioCustAcctObj:GetJsonObject("billing_address")).           
                            
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
                       
                ASSIGN THIS-OBJECT:BillingAcctIDs = THIS-OBJECT:BillingAcctIDs  + "," + aoBillingAcct:mGetBillingAcctID().
                    
            END.            
        END.

        ASSIGN THIS-OBJECT:BillingAcctIDs = TRIM(THIS-OBJECT:BillingAcctIDs, ",").
        
    END METHOD.	
	
	METHOD PUBLIC VOID mGetBillingAddressID() :
	    
	    DEFINE BUFFER bfAddress FOR Address.
	    
	    FIND FIRST bfAddress WHERE bfAddress.KeyValue     =  STRING(THIS-OBJECT:CustNum) 
	                           AND bfAddress.HostTable    =  "CUSTOMER"
	                           AND bfAddress.AddressType  =  {&BILLING_ADDRESS} NO-LOCK NO-ERROR.
	                           
	    IF NOT AVAILABLE bfAddress THEN  
            UNDO, THROW NEW ParamError("Billing Address not found to update.").
            
         ASSIGN THIS-OBJECT:BillAddrID  =  bfAddress.AddressID.	                                   
	    
	END METHOD.	
	
	METHOD PUBLIC INTEGER mGetBillingAcctID():	    
	    
	    FIND FIRST InvoiceTargetGroup WHERE InvoiceTargetGroup.Brand        =  Syst.Var:gcBrand
	                                    AND InvoiceTargetGroup.AccountID    =  THIS-OBJECT:AccountID
	                                    AND InvoiceTargetGroup.CustAccName  =  THIS-OBJECT:AcctName NO-LOCK NO-ERROR.
	                                    
        IF NOT AVAILABLE InvoiceTargetGroup THEN 
            UNDO, THROW NEW ParamError("Billing Account not found to update.").	    
            
        RETURN (InvoiceTargetGroup.ITGroupID).                                
	    
	END METHOD.
    
END CLASS.