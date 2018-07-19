 
/*------------------------------------------------------------------------
   File        : OrderProductPC
   Purpose     : Parses the Product data coming from BSS
   Syntax      : 
   Description : 
   Author(s)   : Koundinya Maddali
   Created     : Thu May 31 17:01:14 IST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS bss.cls.OrderProductPC INHERITS bss.cls.OrderProduct: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
	
    
    DEFINE PUBLIC PROPERTY aoOrderCustomer AS CLASS bss.cls.OrderCustomer NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY aoOrderPayment AS CLASS bss.cls.OrderPayment NO-UNDO
        GET.
        SET.    
        
    DEFINE PUBLIC PROPERTY aoOrderInspection AS CLASS bss.cls.OrderInspection NO-UNDO
        GET.
        SET.    
			
	CONSTRUCTOR PUBLIC OrderProductPC ( ioOrder AS CLASS bss.cls.Order ):
        SUPER(ioOrder).
    END CONSTRUCTOR.
		
	DESTRUCTOR PUBLIC OrderProductPC ( ):

        IF VALID-OBJECT(aoOrderCustomer)
        THEN DELETE OBJECT aoOrderCustomer.
        
        IF VALID-OBJECT(aoOrderPayment)
        THEN DELETE OBJECT aoOrderPayment.
        
        IF VALID-OBJECT(aoOrderInspection)
        THEN DELETE OBJECT aoOrderInspection.
            
    END DESTRUCTOR.
    
    
    METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( ioItemObject AS CLASS JsonObject ):
        
        DEFINE VARIABLE loCustAcct        AS CLASS     JsonObject NO-UNDO.
        DEFINE VARIABLE loPaymentData     AS CLASS     JsonObject NO-UNDO.
        DEFINE VARIABLE loInspectionData  AS CLASS     JsonObject NO-UNDO.
        DEFINE VARIABLE liCustAcctCustNum AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lcReturnValue     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE liSubLimit        AS INTEGER   NO-UNDO. 
        DEFINE VARIABLE lisubs            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE liActLimit        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE liActs            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE liCustSubLimit AS LOGICAL NO-UNDO.
        
        ASSIGN loCustAcct       =  aoOrder:loCustAcct 
               loPaymentData    =  aoOrder:OrderPaymentData
               loInspectionData =  aoOrder:OrderInspectionData.

        IF loCustAcct:Has("custnum")
        AND aoOrder:aoCustomer:CustNum = 0 
        THEN UNDO, THROW NEW ParamError("Customer entered in Customer Account does not exist.").
        
        ASSIGN
            aoOrderCustomer         = NEW bss.cls.OrderCustomer(aoOrder)
            aoOrderCustomer:RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
            .
        
        IF loCustAcct:Has("custnum")
        THEN DO:
            aoOrderCustomer:mCreateData(loCustAcct:GetInteger("custnum")).
            liCustSubLimit =  Func.ValidateACC:mSubscriptionLimitCheck(INPUT aoOrderCustomer:CustId ,
                                                                       INPUT aoOrderCustomer:CustIdType,
                                                                       INPUT Func.ValidateACC:mIsSelfEmpl(aoOrderCustomer:Category),
                                                                       INPUT Func.ValidateACC:mIsPro(aoOrderCustomer:Category),
                                                                       1,
                                                                       OUTPUT liSubLimit,
                                                                       OUTPUT lisubs,
                                                                       OUTPUT liActLimit,
                                                                       OUTPUT liActs).
                                                                       
            IF liCustSubLimit = FALSE 
            THEN UNDO, THROW NEW InternalError("Subscription Limit Error to Customer.").                                                                   
        END.
        ELSE DO:
            IF VALID-OBJECT(loCustAcct)
            THEN aoOrderCustomer:mCreateData(loCustAcct).
        END.
        
        IF VALID-OBJECT(loPaymentData)
        THEN DO:
            ASSIGN aoOrderPayment = NEW bss.cls.OrderPayment(aoOrder).
            aoOrderPayment:mCreateData(loPaymentData).
        END.
        
        IF VALID-OBJECT(loInspectionData)
        THEN DO:
            ASSIGN aoOrderInspection  = NEW bss.cls.OrderInspection(aoOrder).
            aoOrderInspection:mCreateData(loInspectionData).
        END. 
        
        
    END METHOD.
    
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        IF VALID-OBJECT(aoOrderCustomer)
        THEN aoOrderCustomer:mStoreData().
        
        IF VALID-OBJECT(aoOrderPayment)
        THEN aoOrderPayment:mStoreData().
        
        IF VALID-OBJECT(aoOrderInspection)
        THEN aoOrderInspection:mStoreData().
        
    END METHOD. 
    

END CLASS.