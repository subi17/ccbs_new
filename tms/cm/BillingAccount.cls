 
/*------------------------------------------------------------------------
   File        : BillingAccount
   Purpose     : Stores the Billing Account data for the Order creation
   Syntax      : 
   Description : 
   Author(s)   : Koundinya Maddali
   Created     : Mon Jun 11 14:08:20 IST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS cm.BillingAccount IMPLEMENTS bss.cls.IObjectStorage: 
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
	
    DEFINE PUBLIC PROPERTY aoCustAcct AS CLASS cm.CustomerAccount NO-UNDO 
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
		
	DEFINE PUBLIC PROPERTY PaymentMethod AS INTEGER NO-UNDO
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
       
    DEFINE PUBLIC PROPERTY DefaultGroup AS LOGICAL NO-UNDO
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
       
    DEFINE PUBLIC PROPERTY FromDate AS DATE NO-UNDO
       GET.
       SET.
		
	CONSTRUCTOR PUBLIC BillingAccount (ioCustAcct AS CLASS cm.CustomerAccount  ):
	    
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
            THIS-OBJECT:PaymentMethod = mGetTMSCodeValue("AccRec" , "ChargeType" , ioBillingAcct:GetCharacter("invoice_charge_type"))
            THIS-OBJECT:MandateID     = ioBillingAcct:GetCharacter("mandate_id")
            THIS-OBJECT:MandateDate   = Func.Common:mGetDate(ioBillingAcct:GetCharacter("mandate_date"))
            THIS-OBJECT:CustNum       = aoCustAcct:CustNum
            THIS-OBJECT:AgrCust       = THIS-OBJECT:CustNum /*TODO-JSON*/
            THIS-OBJECT:DefaultGroup  = TRUE /*TODO-JSON*/
            THIS-OBJECT:BillCycle     = ioBillingAcct:GetInteger("invoice_cycle")
            THIS-OBJECT:InvInterval   = ioBillingAcct:GetInteger("invoice_interval")
            THIS-OBJECT:InvGroup      = ioBillingAcct:GetCharacter("invoice_group")
            THIS-OBJECT:DurDateOffSet = 0 /*TODO-JSON*/
            THIS-OBJECT:StatusCode    = 0
            THIS-OBJECT:FromDate      = Func.Common:mGetDate(ioBillingAcct:GetCharacter("valid_from")) WHEN ioBillingAcct:Has("valid_from")
            THIS-OBJECT:ToDate        = Func.Common:mGetDate(ioBillingAcct:GetCharacter("valid_to"))   WHEN ioBillingAcct:Has("valid_to")
            .
            
        mValidateInvDeliveryType(ioBillingAcct:GetCharacter("invoice_delivery_type")).
            
        THIS-OBJECT:DelType       = mGetTMSCodeValue("Billing" , "DelType" , ioBillingAcct:GetCharacter("invoice_delivery_type")).
        
    END METHOD.
    
    METHOD PUBLIC VOID mStoreData() :
        
        FIND FIRST InvoiceTargetGroup WHERE 
                   InvoiceTargetGroup.Brand      =  Syst.Var:gcBrand  AND 
                   InvoiceTargetGroup.ITGroupID  =  THIS-OBJECT:ITGroupID EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                   
        IF LOCKED(InvoiceTargetGroup) THEN 
            UNDO, THROW NEW ParamError ("Billing Accout is locked.").
            
        IF NOT AVAILABLE InvoiceTargetGroup 
        THEN CREATE InvoiceTargetGroup.     
        
        ASSIGN 
            InvoiceTargetGroup.Brand         = Syst.Var:gcBrand
            InvoiceTargetGroup.ITGroupID     = THIS-OBJECT:ITGroupID
            InvoiceTargetGroup.AccountID     = THIS-OBJECT:AccountID
            InvoiceTargetGroup.CustAccName   = THIS-OBJECT:CustAcctName
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
            InvoiceTargetGroup.DueDateOffSet = THIS-OBJECT:DurDateOffSet
            InvoiceTargetGroup.StatusCode    = THIS-OBJECT:StatusCode
            InvoiceTargetGroup.FromDate      = THIS-OBJECT:FromDate
            InvoiceTargetGroup.ToDate        = THIS-OBJECT:ToDate
            .
            
    END METHOD.
    
    METHOD PUBLIC CHARACTER mGetBillingAcctID() :
        
        RETURN STRING(THIS-OBJECT:ITGroupID).
        
    END METHOD.

   METHOD PUBLIC JsonObject mResult ():

        DEFINE VARIABLE loBillObj AS CLASS JsonObject NO-UNDO.
        
        loBillObj = NEW JsonObject().
        loBillObj:Add("billing-acct-num" , THIS-OBJECT:ITGroupID ).
        
        RETURN loBillObj.
        
    END METHOD.

    METHOD PUBLIC INTEGER mGetTMSCodeValue( INPUT icCodeGroup AS CHARACTER ,
                                            INPUT icFieldName AS CHARACTER , 
                                            INPUT icCodeName AS CHARACTER ) :
                                                
       /*TO-DO PC : Please get the confirmation on this TMSCodes and investigate where it has been used in PC*/                                                
        
        FIND FIRST TMSCodes 
                WHERE TMSCodes.CodeGroup = icCodeGroup
                  AND TMSCodes.FieldName = icFieldName
                  AND TMSCodes.TableName = "Invoice"
                  AND TMSCodes.CodeName  = icCodeName  NO-LOCK NO-ERROR.
                  
        IF NOT AVAILABLE TMSCodes 
        THEN UNDO, THROW NEW ParamError ("TMSCodes record does not exist for the Billing Account.").
        
        RETURN INTEGER(TMSCodes.CodeValue).
        
    END METHOD .
    
    METHOD PUBLIC CHARACTER mValidateInvDeliveryType(INPUT liDeliveryMethod AS CHARACTER) :
        
        DEFINE VARIABLE liDelType AS INTEGER NO-UNDO.
        
        CASE liDeliveryMethod:
            
            WHEN "PAPER" THEN liDelType = {&INV_DEL_TYPE_PAPER}.
            WHEN "EMAIL" THEN liDelType = {&INV_DEL_TYPE_EMAIL}.
            WHEN "SMS"   THEN liDelType = {&INV_DEL_TYPE_ESI}.
            WHEN "No delivery" THEN liDelType = {&INV_DEL_TYPE_NO_DELIVERY}.
            WHEN "" THEN .
            OTHERWISE 
            UNDO, THROW NEW ParamError (SUBSTITUTE("Invalid Invoice Delivery Type &1 ." , liDeliveryMethod)). 
            
        END CASE.
                
    END METHOD.
    
    
    METHOD PUBLIC VOID mUpdateBillAcctData (INPUT iiBillAcctID AS INTEGER , INPUT ioBillingAcct AS JsonObject) :
        
        FIND FIRST InvoiceTargetGroup WHERE InvoiceTargetGroup.ITGroupID  = iiBillAcctID NO-LOCK NO-ERROR.
        
        IF NOT AVAILABLE  InvoiceTargetGroup THEN 
            UNDO, THROW NEW ParamError ("Billing account is unavailable to update.").
        
        ASSIGN 
            THIS-OBJECT:ITGroupID     = iiBillAcctID
            THIS-OBJECT:AccountID     = aoCustAcct:AccountID
            THIS-OBJECT:CustAcctName  = aoCustAcct:AcctName
            THIS-OBJECT:Currency      = (IF ioBillingAcct:Has("currency") THEN  ioBillingAcct:GetCharacter("currency") ELSE InvoiceTargetGroup.Currency )
            THIS-OBJECT:BankAccount   = (IF ioBillingAcct:Has("bank_account") THEN  ioBillingAcct:GetCharacter("bank_account") ELSE InvoiceTargetGroup.BankAccount )
            THIS-OBJECT:BankName      = (IF ioBillingAcct:Has("bank_name") THEN  ioBillingAcct:GetCharacter("bank_name") ELSE InvoiceTargetGroup.BankName )
            THIS-OBJECT:PaymentMethod = (IF ioBillingAcct:Has("invoice_charge_type") THEN mGetTMSCodeValue("AccRec" , "ChargeType" , ioBillingAcct:GetCharacter("invoice_charge_type"))
                                         ELSE InvoiceTargetGroup.PaymentMethod )    
            THIS-OBJECT:MandateID     = (IF ioBillingAcct:Has("mandate_id") THEN ioBillingAcct:GetCharacter("mandate_id") ELSE InvoiceTargetGroup.MandateID )
            THIS-OBJECT:MandateDate   = (IF ioBillingAcct:Has("mandate_date") THEN Func.Common:mGetDate(ioBillingAcct:GetCharacter("mandate_date")) ELSE InvoiceTargetGroup.MandateDate)
            THIS-OBJECT:CustNum       = aoCustAcct:CustNum
            THIS-OBJECT:AgrCust       = THIS-OBJECT:CustNum /*TODO-JSON*/
            THIS-OBJECT:DefaultGroup  = TRUE /*TODO-JSON*/
            THIS-OBJECT:BillCycle     = (IF ioBillingAcct:Has("invoice_cycle") THEN ioBillingAcct:GetInteger("invoice_cycle") ELSE InvoiceTargetGroup.BillCycle )
            THIS-OBJECT:InvInterval   = (IF ioBillingAcct:Has("invoice_interval") THEN ioBillingAcct:GetInteger("invoice_interval") ELSE InvoiceTargetGroup.InvInterval)
            THIS-OBJECT:InvGroup      = (IF ioBillingAcct:Has("invoice_group") THEN ioBillingAcct:GetCharacter("invoice_group") ELSE InvoiceTargetGroup.InvGroup)
            THIS-OBJECT:DurDateOffSet = 0 /*TODO-JSON*/
            THIS-OBJECT:StatusCode    = 0
            THIS-OBJECT:FromDate      = (IF ioBillingAcct:Has("valid_from") THEN Func.Common:mGetDate(ioBillingAcct:GetCharacter("valid_from")) ELSE InvoiceTargetGroup.FromDate) 
            THIS-OBJECT:ToDate        = (IF ioBillingAcct:Has("valid_to")  THEN Func.Common:mGetDate(ioBillingAcct:GetCharacter("valid_to")) ELSE InvoiceTargetGroup.ToDate)
            .
         
        IF ioBillingAcct:Has("invoice_delivery_type") THEN
        DO:
            mValidateInvDeliveryType(ioBillingAcct:GetCharacter("invoice_delivery_type")).
            THIS-OBJECT:DelType = mGetTMSCodeValue("Billing" , "DelType" , ioBillingAcct:GetCharacter("invoice_delivery_type")).
        END.     
        ELSE THIS-OBJECT:DelType = InvoiceTargetGroup.DelType.
        
    END METHOD.

END CLASS.