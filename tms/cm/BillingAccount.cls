 
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
	
	DEFINE PUBLIC PROPERTY itGroupId AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY accountId AS INTEGER NO-UNDO
       GET.
       SET. 
	
    DEFINE PUBLIC PROPERTY objCustAcct AS CLASS cm.CustomerAccount NO-UNDO 
	   GET.
	   SET.
	    
	DEFINE PUBLIC PROPERTY custAcctName AS CHARACTER NO-UNDO
       GET.
       SET.
        
    DEFINE PUBLIC PROPERTY currency AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY bankAccount AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY bankName AS CHARACTER NO-UNDO
       GET.
       SET.
		
	DEFINE PUBLIC PROPERTY paymentMethod AS INTEGER NO-UNDO
       GET.
       SET.	
       
    DEFINE PUBLIC PROPERTY mandateID AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY mandateDate AS DATE NO-UNDO
       GET.
       SET.   
       
    DEFINE PUBLIC PROPERTY custNum AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY agrCust AS INTEGER NO-UNDO
       GET.
       SET.   
       
    DEFINE PUBLIC PROPERTY defaultGroup AS LOGICAL NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY billCycle AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY invInterval AS INTEGER NO-UNDO
       GET.
       SET.   
       
    DEFINE PUBLIC PROPERTY delType AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY invGroup AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY durDateOffSet AS INTEGER NO-UNDO
       GET.
       SET.   
       
    DEFINE PUBLIC PROPERTY statusCode AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY toDate AS DATE NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY fromDate AS DATE NO-UNDO
       GET.
       SET.
		
	CONSTRUCTOR PUBLIC BillingAccount (ioCustAcct AS CLASS cm.CustomerAccount  ):
	    
		ASSIGN objCustAcct = ioCustAcct.
		
	END CONSTRUCTOR.
	
    METHOD PUBLIC VOID mCreateData (ioBillingAcct AS CLASS JsonObject) :
  
        ASSIGN 
            THIS-OBJECT:itGroupId     = NEXT-VALUE(ITGroupID)
            THIS-OBJECT:accountId     = objCustAcct:AccountID
            THIS-OBJECT:custAcctName  = objCustAcct:AcctName
            THIS-OBJECT:currency      = ioBillingAcct:GetCharacter("currency")
            THIS-OBJECT:bankAccount   = ioBillingAcct:GetCharacter("bank_account")
            THIS-OBJECT:bankName      = ioBillingAcct:GetCharacter("bank_name")
            THIS-OBJECT:paymentMethod = mGetTMSCodeValue("AccRec" , "ChargeType" , ioBillingAcct:GetCharacter("invoice_charge_type"))
            THIS-OBJECT:mandateID     = ioBillingAcct:GetCharacter("mandate_id")
            THIS-OBJECT:mandateDate   = Func.Common:mGetDate(ioBillingAcct:GetCharacter("mandate_date"))
            THIS-OBJECT:defaultGroup  = TRUE /*TODO-JSON*/
            THIS-OBJECT:billCycle     = ioBillingAcct:GetInteger("invoice_cycle")
            THIS-OBJECT:invInterval   = ioBillingAcct:GetInteger("invoice_interval")
            THIS-OBJECT:invGroup      = ioBillingAcct:GetCharacter("invoice_group")
            THIS-OBJECT:durDateOffSet = 0 /*TODO-JSON*/
            THIS-OBJECT:statusCode    = 0
            THIS-OBJECT:fromDate      = Func.Common:mGetDate(ioBillingAcct:GetCharacter("valid_from")) WHEN ioBillingAcct:Has("valid_from")
            THIS-OBJECT:toDate        = Func.Common:mGetDate(ioBillingAcct:GetCharacter("valid_to"))   WHEN ioBillingAcct:Has("valid_to")
            .
            
        mValidateInvDeliveryType(ioBillingAcct:GetCharacter("invoice_delivery_type")).
            
        THIS-OBJECT:delType  = mGetTMSCodeValue("Billing" , "DelType" , ioBillingAcct:GetCharacter("invoice_delivery_type")).
        
    END METHOD.
    
    METHOD PUBLIC VOID mStoreData() :
        
       CREATE InvoiceTargetGroup.     
        
        ASSIGN 
            InvoiceTargetGroup.Brand         = Syst.Var:gcBrand
            InvoiceTargetGroup.ITGroupID     = THIS-OBJECT:itGroupId
            InvoiceTargetGroup.AccountID     = THIS-OBJECT:accountId
            InvoiceTargetGroup.CustAccName   = THIS-OBJECT:custAcctName
            InvoiceTargetGroup.Currency      = THIS-OBJECT:currency
            InvoiceTargetGroup.BankAccount   = THIS-OBJECT:bankAccount
            InvoiceTargetGroup.BankName      = THIS-OBJECT:bankName
            InvoiceTargetGroup.PaymentMethod = THIS-OBJECT:paymentMethod
            InvoiceTargetGroup.MandateID     = THIS-OBJECT:mandateID
            InvoiceTargetGroup.MandateDate   = THIS-OBJECT:mandateDate
            InvoiceTargetGroup.CustNum       = objCustAcct:CustNum
            InvoiceTargetGroup.AgrCust       = InvoiceTargetGroup.CustNum
            InvoiceTargetGroup.DefaultGroup  = THIS-OBJECT:defaultGroup
            InvoiceTargetGroup.BillCycle     = THIS-OBJECT:billCycle
            InvoiceTargetGroup.InvInterval   = THIS-OBJECT:invInterval
            InvoiceTargetGroup.DelType       = THIS-OBJECT:delType
            InvoiceTargetGroup.InvGroup      = THIS-OBJECT:invGroup
            InvoiceTargetGroup.DueDateOffSet = THIS-OBJECT:durDateOffSet
            InvoiceTargetGroup.StatusCode    = THIS-OBJECT:statusCode
            InvoiceTargetGroup.FromDate      = THIS-OBJECT:fromDate
            InvoiceTargetGroup.ToDate        = THIS-OBJECT:toDate
            THIS-OBJECT:custNum              = InvoiceTargetGroup.CustNum
            .
            
    END METHOD.
    
    METHOD PUBLIC CHARACTER mGetBillingAcctID() :
        
        RETURN STRING(THIS-OBJECT:itGroupId).
        
    END METHOD.

   METHOD PUBLIC JsonObject mResult ():

        DEFINE VARIABLE loBillObj AS CLASS JsonObject NO-UNDO.
        
        loBillObj = NEW JsonObject().
        loBillObj:Add("account-id" , THIS-OBJECT:itGroupId ).
        
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
                  
        IF NOT AVAILABLE TMSCodes THEN 
            UNDO, THROW NEW ParamError ("TMSCodes record does not exist for the Billing Account.").
        
        RETURN INTEGER(TMSCodes.CodeValue).
        
    END METHOD .
    
    METHOD PUBLIC CHARACTER mValidateInvDeliveryType(INPUT liDeliveryMethod AS CHARACTER) :
        
        DEFINE VARIABLE liDelType AS INTEGER NO-UNDO.
        
        CASE liDeliveryMethod:
            
            WHEN "PAPER"       THEN liDelType = {&INV_DEL_TYPE_PAPER}.
            WHEN "EMAIL"       THEN liDelType = {&INV_DEL_TYPE_EMAIL}.
            WHEN "SMS"         THEN liDelType = {&INV_DEL_TYPE_ESI}.
            WHEN "No delivery" THEN liDelType = {&INV_DEL_TYPE_NO_DELIVERY}.
            WHEN "" THEN .
            OTHERWISE 
            UNDO, THROW NEW ParamError (SUBSTITUTE("Invalid Invoice Delivery Type &1 ." , liDeliveryMethod)). 
            
        END CASE.
                
    END METHOD.
    
    DESTRUCTOR PUBLIC BillingAccount ( ):
        
        IF VALID-OBJECT(objCustAcct) THEN 
            DELETE OBJECT objCustAcct.

    END DESTRUCTOR.

END CLASS.