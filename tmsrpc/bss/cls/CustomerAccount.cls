 
/*------------------------------------------------------------------------
   File        : CustomerAccount
   Purpose     : This will store the data of the Customer Accounts
   Syntax      : 
   Description : 
   Author(s)   : kmaddali
   Created     : Tue Jun 12 11:00:05 IST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS bss.cls.CustomerAccount IMPLEMENTS bss.cls.IObjectStorage : 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
	DEFINE PUBLIC PROPERTY ioCustomerObj AS CLASS bss.cls.Customer NO-UNDO 
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
       
    DEFINE PUBLIC PROPERTY BillingAddressIndex AS INTEGER NO-UNDO
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
       
    DEFINE TEMP-TABLE ttBillingAcct NO-UNDO 
        FIELDS BillAcctObj  AS CLASS Progress.Lang.Object
        FIELDS BillAcctNum  AS INTEGER
        INDEX BillAcctNum IS PRIMARY UNIQUE BillAcctNum.
		
	CONSTRUCTOR PUBLIC CustomerAccount ( INPUT aoCustomer AS CLASS bss.cls.Customer ):
		
		ASSIGN ioCustomerObj = aoCustomer.
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC CustomerAccount ( ):

        ASSIGN ioCustomerObj = ?.

	END DESTRUCTOR.
	
	METHOD PUBLIC VOID mCreateData(INPUT ioCustAcctObject AS CLASS JsonObject):
	    
	    DEFINE VARIABLE aoBillingAcct AS CLASS bss.cls.BillingAccount NO-UNDO.
	    DEFINE VARIABLE BillAcctArray AS CLASS JsonArray NO-UNDO.
	    DEFINE VARIABLE BillAcctObj   AS CLASS JsonObject NO-UNDO.
	    DEFINE VARIABLE liBillAcctCnt AS INTEGER NO-UNDO.
	    
        ASSIGN 
            THIS-OBJECT:DefaultAcct = ioCustAcctObject:GetLogical("default_account")
            THIS-OBJECT:AcctName    = ioCustAcctObject:GetCharacter("account_name")
            THIS-OBJECT:ValidFrom   =  DATETIME-TZ(Func.Common:mGetDate(ioCustAcctObject:GetCharacter("valid_from")))
            THIS-OBJECT:ValiTo      =  DATETIME-TZ(Func.Common:mGetDate(ioCustAcctObject:GetCharacter("valid_to")))
            THIS-OBJECT:AccountID   =  NEXT-VALUE (accountid)  
            THIS-OBJECT:CustNum     =  ioCustomerObj:CustNum
            THIS-OBJECT:StatusCode  =  1 /*0=pending, 1=active*/
            THIS-OBJECT:BillingAddressIndex = ioCustAcctObject:GetInteger("billing_address_data_index")
            .
        
        IF ioCustAcctObject:Has("billing_accounts")
        THEN DO:
            
            ASSIGN BillAcctArray = ioCustAcctObject:GetJsonArray("billing_accounts").
            
            DO liBillAcctCnt = 1 TO BillAcctArray:LENGTH :
                
                ASSIGN  
                    BillAcctObj   = BillAcctArray:GetJsonObject(liBillAcctCnt)
                    aoBillingAcct = NEW bss.cls.BillingAccount(THIS-OBJECT).
                           
                aoBillingAcct:mCreateData(BillAcctObj).
                
                CREATE ttBillingAcct.
                ASSIGN ttBillingAcct.BillAcctObj = aoBillingAcct
                       ttBillingAcct.BillAcctNum = mNextBillAcctNum().
                
            END.
            
        END.
        ELSE UNDO, THROW NEW ParamError("Billing Account Data is not available.").
        
	    
	END METHOD.
	
	METHOD PUBLIC VOID mStoreData():
	    
	    CREATE CustomerAccount.
	    ASSIGN Customeraccount.AccountID   = THIS-OBJECT:AccountID
	           CustomerAccount.CustNum     = THIS-OBJECT:CustNum
	           CustomerAccount.DefaultAcc  = THIS-OBJECT:DefaultAcct
	           CustomerAccount.AccountName = THIS-OBJECT:AcctName
	           CustomerAccount.FromDate    = THIS-OBJECT:ValidFrom
	           CustomerAccount.ToDate      = THIS-OBJECT:ValiTo
	           CustomerAccount.StatusCode  = THIS-OBJECT:StatusCode
	     .
	           
	     FOR EACH ttBillingAcct :
             CAST(ttBillingAcct.BillAcctObj, bss.cls.BillingAccount):mStoreData().
	     END.
	    
	END METHOD.
	
	METHOD PUBLIC INTEGER mNextBillAcctNum() :
	    
	    FIND LAST ttBillingAcct USE-INDEX BillAcctNum NO-ERROR.
	    IF NOT AVAILABLE ttBillingAcct THEN RETURN 1.
	    ELSE RETURN ( ttBillingAcct.BillAcctNum + 1 ) . 
	
	END METHOD.
	
    
END CLASS.