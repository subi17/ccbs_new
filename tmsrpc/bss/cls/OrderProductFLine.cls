 
/*------------------------------------------------------------------------
   File        : OrderProductFLine
   Purpose     : 
   Syntax      : 
   Description : This will store the product data for the Fixed Line for the Convergent order
   Author(s)   : kmaddali
   Created     : Thu Jun 21 12:36:59 IST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.ParamError.
USING profcgi.RPC.JSON.InternalError.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.JsonObject.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS bss.cls.OrderProductFLine INHERITS bss.cls.OrderProduct: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
	
    DEFINE PRIVATE TEMP-TABLE ttOrderProductParam NO-UNDO
        FIELD opobject AS CLASS   Progress.Lang.Object
        FIELD paramnb  AS INTEGER
        INDEX paramnb IS PRIMARY UNIQUE paramnb
        .
	
	DEFINE PUBLIC PROPERTY NumberType AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY Number AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY aoSubscriptionParams AS CLASS bss.cls.OrderProductParam NO-UNDO
        GET.
        SET.       
        
    DEFINE PUBLIC PROPERTY aoAggrementCustomer AS CLASS bss.cls.OrderCustomer NO-UNDO 
        GET.
        SET.
		
	CONSTRUCTOR PUBLIC OrderProductFLine ( INPUT ioOrder AS CLASS bss.cls.Order  ):
	    
		SUPER (ioOrder).
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProductFLine ( ):
	    
	    ASSIGN aoAggrementCustomer     =   ?
               aoSubscriptionParams    =   ?.

	END DESTRUCTOR.
	
    METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( ioItemObject AS CLASS JsonObject ):
        
        
        IF Func.ValidateOrder:mIsConvergentORFixedOnly(THIS-OBJECT:ProductID) = FALSE 
        THEN UNDO,THROW NEW ParamError (SUBSTITUTE("Product &1 is not convergent or fixed only.", THIS-OBJECT:ProductID)). 
        
        
        
        DEFINE VARIABLE loAttributes AS CLASS JsonObject NO-UNDO.
        
        ASSIGN loAttributes            =  ioItemObject:GetJsonObject("attributes")
               THIS-OBJECT:NumberType  =  loAttributes:GetCharacter("number_type")
               THIS-OBJECT:Number      =  loAttributes:GetCharacter("number")
               aoOrder:OrderType       =  {&ORDER_TYPE_NEW}.
               
        IF LOOKUP(THIS-OBJECT:NumberType,"new,mnp,renewal,stc") = 0 
        THEN UNDO,THROW NEW ParamError (SUBSTITUTE("Unknown number_type: ", THIS-OBJECT:NumberType)).   
        
        IF Func.ValidateOrder:misOrderProductExistsToFixedNumber(THIS-OBJECT:Number) 
        THEN UNDO,THROW NEW ParamError (SUBSTITUTE("Fixed number &1 already exists. ", THIS-OBJECT:Number)). 
               
        mCreateDataOrderProductParamData(loAttributes,"number_type","Char").
        mCreateDataOrderProductParamData(loAttributes,"number","Char").
        mCreateDataOrderProductParamData(loAttributes,"estimated_speed","Char").
        mCreateDataOrderProductParamData(loAttributes,"serial_number","Char").
        
        IF loAttributes:Has("old_operator_code")
        THEN mCreateDataOrderProductParamData(loAttributes,"old_operator_code","Char").
        
        IF loAttributes:Has("old_operator_name")
        THEN mCreateDataOrderProductParamData(loAttributes,"old_operator_name","Char").
        
        IF loAttributes:Has("mnptime")
        THEN mCreateDataOrderProductParamData(loAttributes,"mnptime","Char").
        
        ASSIGN aoAggrementCustomer           =  NEW bss.cls.OrderCustomer(aoOrder)
               aoAggrementCustomer:RowType   =  {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}.
               
        IF aoOrder:aoCustomer:isNewCustomer THEN 
        aoAggrementCustomer:mCreateRowCustomerData(aoOrder:aoCustomer:DefaultAddressObj).        
        
        
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        FOR EACH ttOrderProductParam:
            CAST(ttOrderProductParam.opobject, bss.cls.OrderProductParam):mStoreData().
        END.
        
        
        IF VALID-OBJECT(aoAggrementCustomer)
        THEN aoAggrementCustomer:mStoreData().
        
        
    END METHOD.
    
    METHOD PUBLIC VOID mCreateDataOrderProductParamData  (INPUT loAttributes AS CLASS JsonObject ,
                                                          INPUT Attribute    AS CHARACTER,
                                                          INPUT AttrType     AS CHARACTER  ):  
                                                              
        DEFINE VARIABLE liNextParamNum AS INTEGER NO-UNDO.                                                            
       
        aoSubscriptionParams = NEW bss.cls.OrderProductParam(THIS-OBJECT) .
        aoSubscriptionParams:mCreateData(loAttributes,Attribute,AttrType).
       
        ASSIGN liNextParamNum = mNextParamNb().
                                            
        CREATE ttOrderProductParam.
        ASSIGN 
            ttOrderProductParam.opobject     = aoSubscriptionParams
            ttOrderProductParam.paramnb      = liNextParamNum. 
                  
    END METHOD.
    
    METHOD PUBLIC INTEGER mNextParamNb():

        FIND LAST ttOrderProductParam USE-INDEX paramnb NO-ERROR.
               
        IF NOT AVAILABLE ttOrderProductParam THEN RETURN 1.
        
        RETURN ( ttOrderProductParam.paramnb + 1 ).

    END METHOD.
    

END CLASS.