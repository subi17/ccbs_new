 
/*------------------------------------------------------------------------
   File        : OrderProductSubscription
   Purpose     : This class file will store the Order Product data for the Mobile Subscription
   Syntax      : 
   Description : 
   Author(s)   : Koundinya Maddali
   Created     : Fri Jun 01 14:36:17 IST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.ParamError.
USING profcgi.RPC.JSON.InternalError.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS bss.cls.OrderProductSubscription INHERITS bss.cls.OrderProduct: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE PRIVATE TEMP-TABLE ttOrderProductParam NO-UNDO
        FIELD opobject AS CLASS Progress.Lang.Object
        FIELD paramnb  AS INTEGER
        INDEX paramnb IS PRIMARY UNIQUE paramnb
        .
	
    DEFINE PUBLIC PROPERTY aoSubscriptionParams AS CLASS bss.cls.OrderProductParam NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY aoBillingAcctIndex AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY AttrCLI AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY lcNumberType AS CHARACTER NO-UNDO
       GET.
       SET.
		
	CONSTRUCTOR PUBLIC OrderProductSubscription ( ioOrder AS CLASS bss.cls.Order ):
	    
        SUPER(ioOrder).
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProductSubscription ( ):
	    
	    ASSIGN aoSubscriptionParams = ?.

	END DESTRUCTOR.
	
	METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( ioItemObject AS CLASS JsonObject ):
            
        DEFINE VARIABLE loAttributes  AS CLASS JsonObject NO-UNDO.
        DEFINE VARIABLE lcReturnValue AS CHARACTER NO-UNDO.
        ASSIGN loAttributes                     = ioItemObject:GetJsonObject("attributes") WHEN ioItemObject:Has("attributes")
               THIS-OBJECT:aoBillingAcctIndex   = ioItemObject:GetInteger("billing-account-index") WHEN ioItemObject:Has("billing-account-index").
        
        IF VALID-OBJECT(loAttributes)
        THEN DO:
            
            ASSIGN
                lcNumberType = loAttributes:GetCharacter("number_type") 
                AttrCLI      = loAttributes:GetCharacter("number") 
                .
                
            IF LOOKUP(lcNumberType,"new,mnp,renewal,stc") = 0 
            THEN UNDO,THROW NEW ParamError (SUBSTITUTE("Unknown number_type: ", lcNumberType)). 
            
            IF lcNumberType = "mnp" AND 
                CAN-FIND(FIRST Order WHERE Order.CLI = AttrCLI AND 
                              (Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_PENDING} OR /*60*/
                               Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_ONGOING}))
            THEN UNDO, THROW NEW ParamError ("Requested number is in migration").
            
            lcReturnValue = Func.ValidateOrder:fCheckMSISDN(AttrCLI,lcNumberType). 
            
            IF lcReturnValue > ""
            THEN UNDO, THROW NEW InternalError(lcReturnValue). 
                                                      
            mCreateDataOrderProductParamData(loAttributes,"number_type","Char").
            mCreateDataOrderProductParamData(loAttributes,"number","Char").                                 
                                             
            IF loAttributes:Has("old-operator-code") 
            THEN mCreateDataOrderProductParamData(loAttributes,"old-operator-code","Char"). 
            
            ASSIGN aoOrder:lcCLI = AttrCLI.
            /*TODO PC:Please check the MULTIHOLDER INFROMATION WITH KIRAN*/
            
        END.
            
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        FOR EACH ttOrderProductParam:
            CAST(ttOrderProductParam.opobject, bss.cls.OrderProductParam):mStoreData().
        END.

    END METHOD.
    
    METHOD PUBLIC VOID mCreateDataOrderProductParamData  (INPUT loAttributes AS CLASS JsonObject ,
                                                          INPUT Attribute    AS CHARACTER,
                                                          INPUT AttrType     AS CHARACTER  ):                                                              
       
        aoSubscriptionParams = NEW bss.cls.OrderProductParam(THIS-OBJECT) .
        aoSubscriptionParams:mCreateData(loAttributes,
                                         Attribute,
                                         AttrType).
                                         
        CREATE ttOrderProductParam.
        ASSIGN 
            ttOrderProductParam.opobject = aoSubscriptionParams
            ttOrderProductParam.paramnb  = mNextParamNb().  
        
        
    END METHOD.
    
    METHOD PRIVATE INTEGER mNextParamNb():

        FIND LAST ttOrderProductParam USE-INDEX paramnb.
        IF NOT AVAILABLE ttOrderProductParam
            THEN RETURN 1.

        RETURN ( ttOrderProductParam.paramnb + 1 ).

    END METHOD.
    

END CLASS.