 
/*------------------------------------------------------------------------
   File        : BillingAccount
   Purpose     : Stores the Billing Account data for the Order creation
   Syntax      : 
   Description : 
   Author(s)   : kmaddali
   Created     : Mon Jun 11 14:08:20 IST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS bss.cls.BillingAccount IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
	DEFINE PUBLIC PROPERTY ITGroupID AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY AccountID AS INTEGER NO-UNDO
       GET.
       SET. 
	
    DEFINE PUBLIC PROPERTY aoCustAcct AS CLASS bss.cls.CustomerAccount NO-UNDO 
	   GET.
	   SET.
	    
	DEFINE PUBLIC PROPERTY CustAcctName AS CHARACTER NO-UNDO
       GET.
       SET.
        
    DEFINE PUBLIC PROPERTY Currency AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY BankAccount AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY BankName AS CHARACTER NO-UNDO
       GET.
       SET.
		
	DEFINE PUBLIC PROPERTY PayMethod AS INTEGER NO-UNDO
       GET.
       SET.	
       
    DEFINE PUBLIC PROPERTY MandateID AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY MandateDate AS DATE NO-UNDO
       GET.
       SET.   
       
    DEFINE PUBLIC PROPERTY CustNum AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY AgrCust AS INTEGER NO-UNDO
       GET.
       SET.   
       
    DEFINE PUBLIC PROPERTY DefaultGroup AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY BillCycle AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY InvInterval AS INTEGER NO-UNDO
       GET.
       SET.   
       
    DEFINE PUBLIC PROPERTY DelType AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY InvGroup AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY DurDateOffSet AS INTEGER NO-UNDO
       GET.
       SET.   
       
    DEFINE PUBLIC PROPERTY StatusCode AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY ToDate AS DATE NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY FromDate AS CHARACTER NO-UNDO
       GET.
       SET.
		
	CONSTRUCTOR PUBLIC BillingAccount (ioCustAcct AS CLASS bss.cls.CustomerAccount  ):
	    
		ASSIGN aoCustAcct = ioCustAcct.
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC BillingAccount ( ):
	    
	    ASSIGN aoCustAcct = ?.

	END DESTRUCTOR.
	
    METHOD PUBLIC VOID mCreateData (ioBillingAcct AS CLASS JsonObject) :
        
        ASSIGN 
            THIS-OBJECT:ITGroupID     = NEXT-VALUE(ITGroupID)
            THIS-OBJECT:AccountID     = aoCustAcct:AccountID
            THIS-OBJECT:CustAcctName  = aoCustAcct:AcctName
            THIS-OBJECT:Currency      = ioBillingAcct:GetCharacter("currency")
            THIS-OBJECT:BankAccount   = ioBillingAcct:GetCharacter("bank_account")
            THIS-OBJECT:BankName      = ioBillingAcct:GetCharacter("bank_name")
            THIS-OBJECT:PaymentMethod = ioBillingAcct:GetCharacter("invoice_charge_type")
            THIS-OBJECT:MandateID     = ioBillingAcct:GetCharacter("mandate_id")
            THIS-OBJECT:MandateDate   = Func.Common:mGetDate(ioBillingAcct:GetCharacter("mandate_date"))
            THIS-OBJECT:CustNum       = aoCustAcct:CustNum
            THIS-OBJECT:AgrCust       = THIS-OBJECT:CustNum /*TODO-JSON*/
            THIS-OBJECT:DefaultGroup  = TRUE /*TODO-JSON*/
            THIS-OBJECT:BillCycle     = ioBillingAcct:GetInteger("invoice_cycle")
            THIS-OBJECT:InvInterval   = ioBillingAcct:GetInteger("invoice_interval")
            THIS-OBJECT:DelType       = ioBillingAcct:GetCharacter("invoice_delivery_type")
            THIS-OBJECT:InvGroup      = ioBillingAcct:GetCharacter("invoice_group")
            THIS-OBJECT:DurDateOffSet = 0 /*TODO-JSON*/
            THIS-OBJECT:StatusCode    = 1
            THIS-OBJECT:FromDate      = Func.Common:mGetDate(ioBillingAcct:GetCharacter("valid_from"))
            THIS-OBJECT:ToDate        = Func.Common:mGetDate(ioBillingAcct:GetCharacter("valid_to"))
            .
        
    END METHOD.
    
    METHOD PUBLIC VOID mStoreData() :
        
        CREATE InvoiceTargetGroup.
        ASSIGN 
            InvoiceTargetGroup.ITGroupID     = THIS-OBJECT:ITGroupID
            InvoiceTargetGroup.AccountID     = THIS-OBJECT:AccountID
            InvoiceTargetGroup.CustAcctName  = THIS-OBJECT:CustAcctName
            InvoiceTargetGroup.Currency      = THIS-OBJECT:Currency
            InvoiceTargetGroup.BankAccount   = THIS-OBJECT:BankAccount
            InvoiceTargetGroup.BankName      = THIS-OBJECT:BankName
            InvoiceTargetGroup.PaymentMethod = THIS-OBJECT:PaymentMethod
            InvoiceTargetGroup.MandateID     = THIS-OBJECT:MandateID
            InvoiceTargetGroup.MandateDate   = THIS-OBJECT:MandateDate
            InvoiceTargetGroup.CustNum       = THIS-OBJECT:CustNum
            InvoiceTargetGroup.AgrCust       = THIS-OBJECT:AgrCust
            InvoiceTargetGroup.DefaultGroup  = THIS-OBJECT:DefaultGroup
            InvoiceTargetGroup.BillCycle     = THIS-OBJECT:BillCycle
            InvoiceTargetGroup.InvInterval   = THIS-OBJECT:InvInterval
            InvoiceTargetGroup.DelType       = THIS-OBJECT:DelType
            InvoiceTargetGroup.InvGroup      = THIS-OBJECT:InvGroup
            InvoiceTargetGroup.DurDateOffSet = THIS-OBJECT:DurDateOffSet
            InvoiceTargetGroup.StatusCode    = THIS-OBJECT:StatusCode
            InvoiceTargetGroup.FromDate      = THIS-OBJECT:FromDate
            InvoiceTargetGroup.ToDate        = THIS-OBJECT:ToDate
            .
        
    END METHOD.

END CLASS.