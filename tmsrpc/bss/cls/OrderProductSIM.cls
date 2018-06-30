 
/*------------------------------------------------------------------------
   File        : OrderProductSIM
   Purpose     : This Class file will parses the SIM data for the Subscription
   Syntax      : 
   Description : 
   Author(s)   : Koundinya Maddali
   Created     : Fri Jun 01 14:37:10 IST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.ParamError.
USING profcgi.RPC.JSON.InternalError.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS bss.cls.OrderProductSIM INHERITS bss.cls.OrderProduct: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
	
	DEFINE PRIVATE TEMP-TABLE ttOrderProductParam NO-UNDO
        FIELD opobject AS CLASS   Progress.Lang.Object
        FIELD paramnb  AS INTEGER
        INDEX paramnb IS PRIMARY UNIQUE paramnb
        .
    
	
    DEFINE PUBLIC PROPERTY aoSIMParams AS CLASS bss.cls.OrderProductParam NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY aoShippingCustomer AS CLASS bss.cls.OrderCustomer NO-UNDO
        GET.
        SET.
        
    CONSTRUCTOR PUBLIC OrderProductSIM ( ioOrder AS CLASS bss.cls.Order ):
		
        SUPER(ioOrder).
		
	END CONSTRUCTOR.
	
	DESTRUCTOR PUBLIC OrderProductSIM ( ):
	    
	    ASSIGN 
            aoSIMParams        = ?
            aoShippingCustomer = ? . 

	END DESTRUCTOR.
	
    METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( ioItemObject AS CLASS JsonObject ):
        
        DEFINE VARIABLE loAttributes  AS CLASS JsonObject NO-UNDO.
        DEFINE VARIABLE loShipAddress AS CLASS JsonObject NO-UNDO.
        
        ASSIGN 
            loAttributes = NEW JsonObject()
            loAttributes = ioItemObject:GetJsonObject("attributes") WHEN ioItemObject:Has("attributes").
        
        IF VALID-OBJECT(loAttributes)
        THEN DO:
            
            IF loAttributes:Has("ICC")
            THEN mCreateDataOrderProductParamData(loAttributes,"ICC","Char").
             
        END.
       
        IF loAttributes:Has("shipping_address")
        THEN DO:
            
            ASSIGN 
                aoShippingCustomer         = NEW bss.cls.OrderCustomer(aoOrder)
                aoShippingCustomer:RowType = {&ORDERCUSTOMER_ROWTYPE_DELIVERY}
                loShipAddress              = NEW JsonObject()
                loShipAddress              = loAttributes:GetJsonObject("shipping_address").
                
                aoShippingCustomer:mCreateRowCustomerData(loShipAddress).
        
        END.
        
    END METHOD.
    
    METHOD PUBLIC VOID mCreateDataOrderProductParamData  (INPUT loAttributes AS CLASS JsonObject ,
                                                          INPUT Attribute    AS CHARACTER,
                                                          INPUT AttrType     AS CHARACTER  ):  
                                                              
        DEFINE VARIABLE liNextParamNum AS INTEGER NO-UNDO.                                                            
       
        aoSIMParams = NEW bss.cls.OrderProductParam(THIS-OBJECT) .
        aoSIMParams:mCreateData(loAttributes,Attribute,AttrType).
       
        ASSIGN 
            liNextParamNum = mNextParamNb().
                                            
        CREATE ttOrderProductParam.
        ASSIGN 
            ttOrderProductParam.opobject = aoSIMParams
            ttOrderProductParam.paramnb  = liNextParamNum. 
                  
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        FOR EACH ttOrderProductParam:
            CAST(ttOrderProductParam.opobject, bss.cls.OrderProductParam):mStoreData().
        END.
        
        IF VALID-OBJECT(aoShippingCustomer)
        THEN aoShippingCustomer:mStoreData().
        
        
    END METHOD.
    
    METHOD PUBLIC INTEGER mNextParamNb():

        FIND LAST ttOrderProductParam USE-INDEX paramnb NO-ERROR.
               
        IF NOT AVAILABLE ttOrderProductParam THEN RETURN 1.
        
        RETURN ( ttOrderProductParam.paramnb + 1 ).

    END METHOD.

END CLASS.