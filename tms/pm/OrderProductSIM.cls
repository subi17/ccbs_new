 
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

CLASS pm.OrderProductSIM INHERITS pm.OrderProduct: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
	
	DEFINE PRIVATE TEMP-TABLE ttOrderProductParam NO-UNDO
        FIELD opobject AS CLASS   Progress.Lang.Object
        FIELD paramnb  AS INTEGER
        INDEX paramnb IS PRIMARY UNIQUE paramnb
        .
	
    DEFINE PUBLIC PROPERTY objSimParams AS CLASS pm.OrderProductParam NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY objShipCustomer AS CLASS om.OrderCustomer NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY ICC AS CHARACTER NO-UNDO
       GET.
       SET.
        
    CONSTRUCTOR PUBLIC OrderProductSIM ( iObjOrder AS CLASS om.Order ):
		
        SUPER(iObjOrder).
		
	END CONSTRUCTOR.
	
    METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( objSimProductStruct AS CLASS JsonObject ):
        
        DEFINE VARIABLE loAttributes  AS CLASS JsonObject NO-UNDO.
        DEFINE VARIABLE loShipAddress AS CLASS JsonObject NO-UNDO.
        
        ASSIGN 
            loAttributes = NEW JsonObject()
            loAttributes = objSimProductStruct:GetJsonObject("attributes") WHEN objSimProductStruct:Has("attributes").
        
        IF VALID-OBJECT(loAttributes)
        THEN DO:
            
            /*ICC attribute will be in the json structure. But, some times icc will be assigned by dextra*/            
            IF loAttributes:Has("ICC")
            THEN DO:
                
                ASSIGN THIS-OBJECT:ICC  =  loAttributes:GetCharacter("ICC")
                       objOrder:lcICC    =  THIS-OBJECT:ICC.
                
                IF THIS-OBJECT:ICC > "" AND  
                   Func.ValidateOrder:misFreeICC(THIS-OBJECT:ICC) = FALSE 
                THEN UNDO, THROW NEW ParamError(SUBSTITUTE("SIM with ICC &1 not found or not free", THIS-OBJECT:ICC)).
                
                mCreateDataOrderProductParamData(loAttributes,"ICC","Char").    
            END. 
        END.
       
        IF loAttributes:Has("shipping_address") THEN 
        DO:
            
            ASSIGN 
                objShipCustomer         = NEW om.OrderCustomer(objOrder)
                objShipCustomer:RowType = {&ORDERCUSTOMER_ROWTYPE_DELIVERY}
                loShipAddress           = NEW JsonObject()
                loShipAddress           = loAttributes:GetJsonObject("shipping_address").
                
                objShipCustomer:mCreateRowCustomerData(loShipAddress).
        
        END.
        
    END METHOD.
    
    METHOD PUBLIC VOID mCreateDataOrderProductParamData  (INPUT loAttributes AS CLASS JsonObject ,
                                                          INPUT Attribute    AS CHARACTER,
                                                          INPUT AttrType     AS CHARACTER  ):  
                                                              
        DEFINE VARIABLE liNextParamNum AS INTEGER NO-UNDO.                                                            
       
        objSimParams = NEW pm.OrderProductParam(THIS-OBJECT) .
        objSimParams:mCreateData(loAttributes,Attribute,AttrType).
       
        ASSIGN 
            liNextParamNum = mNextParamNb().
                                            
        CREATE ttOrderProductParam.
        ASSIGN 
            ttOrderProductParam.opobject = objSimParams
            ttOrderProductParam.paramnb  = liNextParamNum. 
                  
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        FOR EACH ttOrderProductParam:
            CAST(ttOrderProductParam.opobject, pm.OrderProductParam):mStoreData().
        END.
        
        IF VALID-OBJECT(objShipCustomer) THEN 
            objShipCustomer:mStoreData().
        
        IF THIS-OBJECT:ICC > "" THEN 
            objOrder:mUpdateSimStatus(THIS-OBJECT:ICC).
        
    END METHOD.
    
    METHOD PUBLIC INTEGER mNextParamNb():

        FIND LAST ttOrderProductParam USE-INDEX paramnb NO-ERROR.
               
        IF NOT AVAILABLE ttOrderProductParam THEN RETURN 1.
        
        RETURN ( ttOrderProductParam.paramnb + 1 ).

    END METHOD.
    
    DESTRUCTOR PUBLIC OrderProductSIM ( ):
        
        IF VALID-OBJECT (objSimParams) THEN 
            DELETE OBJECT objSimParams.
            
        IF VALID-OBJECT(objShipCustomer) THEN 
            DELETE OBJECT objShipCustomer.

    END DESTRUCTOR.
    

END CLASS.