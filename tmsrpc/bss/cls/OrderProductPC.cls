 
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
        
	CONSTRUCTOR PUBLIC OrderProductPC ( ioOrder AS CLASS bss.cls.Order ):
        SUPER(ioOrder).
    END CONSTRUCTOR.
		
	DESTRUCTOR PUBLIC OrderProductPC ( ):

        IF VALID-OBJECT(aoOrderCustomer)
        THEN DELETE OBJECT aoOrderCustomer.
        
    END DESTRUCTOR.
    
    
    METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( ioItemObject AS CLASS JsonObject ):
        
    /*    DEFINE VARIABLE loCustAcct        AS CLASS     JsonObject NO-UNDO.
        DEFINE VARIABLE loPaymentData     AS CLASS     JsonObject NO-UNDO.
        DEFINE VARIABLE liCustAcctCustNum AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lcReturnValue     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE liSubLimit        AS INTEGER   NO-UNDO. 
        DEFINE VARIABLE lisubs            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE liActLimit        AS INTEGER   NO-UNDO.
        DEFINE VARIABLE liActs            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE liCustSubLimit    AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE liBypassRules     AS LOGICAL NO-UNDO.
        
        /*TO-DO PC:WHAT IS plBypassRules in the newton_add_Whole_order.p*/
      
        IF aoOrder:aoCustomer:isNewCustomer = FALSE
        AND NOT aoOrder:OrderChannel BEGINS "migration"
        AND NOT liBypassRules
        
        THEN DO:            
            
            liCustSubLimit =  Func.ValidateACC:mSubscriptionLimitCheck(INPUT aoOrder:aoCustomer:CustId ,
                                                                       INPUT aoOrder:aoCustomer:CustIdType,
                                                                       INPUT Func.ValidateACC:mIsSelfEmpl(aoOrder:aoCustomer:Category),
                                                                       INPUT Func.ValidateACC:mIsPro(aoOrder:aoCustomer:Category),
                                                                       1,
                                                                       OUTPUT liSubLimit,
                                                                       OUTPUT lisubs,
                                                                       OUTPUT liActLimit,
                                                                       OUTPUT liActs).
                                                                       
            IF liCustSubLimit = FALSE 
            THEN UNDO, THROW NEW InternalError("Subscription Limit Error to existing Customer."). 
            
            FOR FIRST Customer WHERE
                      Customer.Brand      = Syst.Var:gcBrand  AND
                      Customer.OrgId      = aoOrder:aoCustomer:CustId AND
                      Customer.CustIdType = aoOrder:aoCustomer:CustIdType AND
                      Customer.Roles NE "inactive" NO-LOCK:
                IF Func.ValidateOrder:mIsBarredSubscriptionExistsToCustomer(Customer.Custnum) 
                THEN lcFError = "barring".                                                                  
        
        END.   */            
        
    END METHOD.
    
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
   
    END METHOD. 
    

END CLASS.