 
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
            loAttributes = ioItemObject:GetJsonObject("attributes") WHEN ioItemObject:Has("attributes").
        
        IF VALID-OBJECT(loAttributes)
        AND loAttributes:Has("ICC") 
        THEN DO:
            
            aoSIMParams = NEW bss.cls.OrderProductParam(THIS-OBJECT).
            aoSIMParams:mCreateData(loAttributes,"ICC","Char").
            
        END.
        
        ASSIGN 
            loShipAddress = ioItemObject:GetJsonObject("shipping-address") WHEN ioItemObject:Has("shipping-address").
        
        IF VALID-OBJECT(loShipAddress) 
        THEN DO:
            
            /*aoShippingCustomer  = NEW bss.cls.OrderCustomer(aoOrder).
            ASSIGN 
                aoShippingCustomer:RowType          = {&ORDERCUSTOMER_ROWTYPE_DELIVERY}
                aoShippingCustomer:isShipCustomer   = YES
            .
            aoShippingCustomer:mCreateData(loShipAddress).
            */
        END.
        
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        IF VALID-OBJECT(aoSIMParams)
        THEN aoSIMParams:mStoreData().

        IF VALID-OBJECT(aoShippingCustomer)
        THEN aoShippingCustomer:mStoreData().
        
    END METHOD.

END CLASS.