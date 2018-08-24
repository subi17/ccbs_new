 
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

CLASS pm.OrderProductAccessory INHERITS pm.OrderProduct: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
	
    DEFINE PRIVATE TEMP-TABLE ttOrderProductParam NO-UNDO
        FIELD opobject AS CLASS   Progress.Lang.Object
        FIELD paramnb  AS INTEGER
        INDEX paramnb IS PRIMARY UNIQUE paramnb
        .
	
	DEFINE PUBLIC PROPERTY objInstallationOrderCustomer AS CLASS om.OrderCustomer NO-UNDO 
	    GET.
	    SET.
		
	CONSTRUCTOR PUBLIC OrderProductAccessory ( INPUT iObjOrder AS CLASS om.Order ):
	    
		SUPER (iObjOrder).
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProductAccessory ( ):
	    
	    IF VALID-OBJECT(objInstallationOrderCustomer) THEN 
	        DELETE OBJECT objInstallationOrderCustomer. 

	END DESTRUCTOR.
	
    METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( ioAccessoryStruct AS CLASS JsonObject ):
        
        DEFINE VARIABLE loAttributes  AS CLASS JsonObject NO-UNDO.
        DEFINE VARIABLE loShipAddress AS CLASS JsonObject NO-UNDO.
        
        
        ASSIGN loAttributes                         = ioAccessoryStruct:GetJsonObject("attributes")
               loShipAddress                        = loAttributes:GetJsonObject("shipping_address")
               objInstallationOrderCustomer         = NEW om.OrderCustomer(objOrder)
               objInstallationOrderCustomer:RowType = {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}.
               
        objInstallationOrderCustomer:mCreateRowCustomerData(loShipAddress).
        
        
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        IF VALID-OBJECT (objInstallationOrderCustomer) THEN
        DO:
            objInstallationOrderCustomer:mAssignOrderProductID(THIS-OBJECT:orderProductID). 
            objInstallationOrderCustomer:mStoreData().
        END. 
            
        
    END METHOD.

END CLASS.