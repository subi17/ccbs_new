 
/*------------------------------------------------------------------------
   File        : OrderProductAccessory
   Purpose     : 
   Syntax      : 
   Description : This class file will store the details of accessory that comes with the order. Ex:Router
   Author(s)   : Koundinya Maddali
   Created     : Thu Jun 21 18:53:13 IST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.ParamError.
USING profcgi.RPC.JSON.InternalError.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.JsonObject.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS bss.cls.OrderProductAccessory INHERITS bss.cls.OrderProduct: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
	
    DEFINE PRIVATE TEMP-TABLE ttOrderProductParam NO-UNDO
        FIELD opobject AS CLASS   Progress.Lang.Object
        FIELD paramnb  AS INTEGER
        INDEX paramnb IS PRIMARY UNIQUE paramnb
        .
	
	DEFINE PUBLIC PROPERTY aoShipAddress AS CLASS bss.cls.OrderCustomer NO-UNDO 
	    GET.
	    SET.
		
	CONSTRUCTOR PUBLIC OrderProductAccessory ( INPUT ioOrder AS CLASS bss.cls.Order ):
	    
		SUPER (ioorder).
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProductAccessory ( ):
	    
	    ASSIGN aoShipAddress = ?.

	END DESTRUCTOR.
	
    METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( ioItemObject AS CLASS JsonObject ):
        
        DEFINE VARIABLE loAttributes  AS CLASS JsonObject NO-UNDO.
        DEFINE VARIABLE loShipAddress AS CLASS JsonObject NO-UNDO.
        
        
        ASSIGN loAttributes             =    ioItemObject:GetJsonObject("attributes")
               loShipAddress            =    loAttributes:GetJsonObject("shipping_address")
               aoShipAddress            =    NEW bss.cls.OrderCustomer(aoOrder)
               aoShipAddress:RowType    =    {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}.
               
        aoShipAddress:mCreateRowCustomerData(loShipAddress).
        
        
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        IF VALID-OBJECT (aoShipAddress)
        THEN aoShipAddress:mStoreData().
        
    END METHOD.

END CLASS.