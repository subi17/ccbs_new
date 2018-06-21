 
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
       
    DEFINE PUBLIC PROPERTY liBillAcctID AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY aoOrderAggrementCustomer AS CLASS bss.cls.OrderCustomer NO-UNDO 
        GET.
        SET. 
        
    DEFINE PUBLIC PROPERTY AddressObj AS CLASS JsonObject  NO-UNDO
        GET.
        SET.
       
   		
	CONSTRUCTOR PUBLIC OrderProductSubscription ( ioOrder AS CLASS bss.cls.Order ):
	    
        SUPER(ioOrder).
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProductSubscription ( ):
	    
	    ASSIGN aoSubscriptionParams      = ?
               aoOrderAggrementCustomer  = ?.

	END DESTRUCTOR.
	
	METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( ioItemObject AS CLASS JsonObject ):
            
        DEFINE VARIABLE loAttributes      AS CLASS     JsonObject NO-UNDO.
        DEFINE VARIABLE lcReturnValue     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcOldOperatorCode AS CHARACTER NO-UNDO.
        DEFINE VARIABLE loBillAddrObj     AS CLASS JsonObject NO-UNDO.
        
        ASSIGN loAttributes                     = ioItemObject:GetJsonObject("attributes") WHEN ioItemObject:Has("attributes")
               THIS-OBJECT:aoBillingAcctIndex   = ioItemObject:GetInteger("billing-account-index") WHEN ioItemObject:Has("billing-account-index").
        
        IF VALID-OBJECT(loAttributes)
        THEN DO:
            
            ASSIGN
                THIS-OBJECT:lcNumberType = loAttributes:GetCharacter("number_type") 
                THIS-OBJECT:AttrCLI      = loAttributes:GetCharacter("number")                
            .
                            
            IF LOOKUP(THIS-OBJECT:lcNumberType,"new,mnp,renewal,stc") = 0 
            THEN UNDO,THROW NEW ParamError (SUBSTITUTE("Unknown number_type: ", THIS-OBJECT:lcNumberType)). 
            
            IF THIS-OBJECT:lcNumberType = "mnp" AND 
                CAN-FIND(FIRST Order WHERE Order.CLI = THIS-OBJECT:AttrCLI AND 
                              (Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_PENDING} OR /*60*/
                               Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_ONGOING}))
            THEN UNDO, THROW NEW ParamError ("Requested number is in migration").
            
            IF Func.ValidateOrder:misCLITypeExists(THIS-OBJECT:ProductID) = NO 
            THEN UNDO , THROW NEW ParamError (SUBSTITUTE("Unknown Product ID : &1 ", THIS-OBJECT:ProductID)).
                     
            IF Func.ValidateOrder:misFilledCLI(THIS-OBJECT:AttrCLI) 
            THEN UNDO , THROW NEW ParamError (SUBSTITUTE("Number &1 is already assigned to other order. ", THIS-OBJECT:AttrCLI)).
            
            lcReturnValue = Func.ValidateOrder:fCheckMSISDN(THIS-OBJECT:AttrCLI,THIS-OBJECT:lcNumberType). 
            
            IF lcReturnValue > ""
            THEN UNDO, THROW NEW InternalError(lcReturnValue). 
            
            mCreateDataOrderProductParamData(loAttributes,"number_type","Char").
            mCreateDataOrderProductParamData(loAttributes,"number","Char").  
                                                 
            IF loAttributes:Has("old_operator_code") 
            THEN DO:
                mCreateDataOrderProductParamData(loAttributes,"old_operator_code","Char").
            END.
            
            IF THIS-OBJECT:lcNumberType = "MNP"
            THEN ASSIGN aoOrder:OrderType = {&ORDER_TYPE_MNP}.
            ELSE IF THIS-OBJECT:lcNumberType = "NEW" 
            THEN ASSIGN aoOrder:OrderType = {&ORDER_TYPE_NEW}.
            
            ASSIGN aoOrder:lcCLI            = THIS-OBJECT:AttrCLI
                   aoOrder:isPostPaidOrder  = Func.ValidateOrder:mIsPostpaidOrder(THIS-OBJECT:ProductID).
                   
            IF ioItemObject:Has("billing_account_id")
            THEN DO:
                /*This will be executed in case of existing customer.*/
                ASSIGN THIS-OBJECT:liBillAcctID  =  ioItemObject:GetInteger("billing_account_id").
                mIsBillingAccountExists(). 
                mGetBillingAddressObject(). 
            END.           
            
            IF aoOrder:isPostPaidOrder = NO 
            AND aoOrder:aoCustomer:isNewCustomer = FALSE
            THEN mGetBillingAddressObject(). /* This will prepare address object for the aggrement ordercustomer for prepaid order to existing customer */
            
            IF ioItemObject:Has("billing_account_index")
            THEN DO:
                
                /*WHAT WAS THE CODE THAT SHOULD BE WRITTEN HERE*/
                
            END.
                
            ASSIGN 
                aoOrderAggrementCustomer           =  NEW bss.cls.OrderCustomer(aoOrder)
                aoOrderAggrementCustomer:RowType   =  {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
               .
               
            IF aoOrder:aoCustomer:isNewCustomer THEN    
            aoOrderAggrementCustomer:mCreateRowCustomerData(aoOrder:aoCustomer:DefaultAddressObj).
            ELSE aoOrderAggrementCustomer:mCreateRowCustomerData(AddressObj).
                
            /*TODO PC:Please check the MULTIHOLDER INFROMATION WITH KIRAN*/
                
        END.
            
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        FOR EACH ttOrderProductParam:
            CAST(ttOrderProductParam.opobject, bss.cls.OrderProductParam):mStoreData().
        END.
        
        IF VALID-OBJECT (aoOrderAggrementCustomer)
        THEN DO:
            aoOrderAggrementCustomer:mStoreData().
        END.

    END METHOD.
    
    METHOD PUBLIC VOID mCreateDataOrderProductParamData  (INPUT loAttributes AS CLASS JsonObject ,
                                                          INPUT Attribute    AS CHARACTER,
                                                          INPUT AttrType     AS CHARACTER  ):  
                                                              
        DEFINE VARIABLE liNextParamNum AS INTEGER NO-UNDO.                                                            
       
        aoSubscriptionParams = NEW bss.cls.OrderProductParam(THIS-OBJECT) .
        aoSubscriptionParams:mCreateData(loAttributes,
                                         Attribute,
                                         AttrType).
       
        ASSIGN liNextParamNum =  mNextParamNb().
                                            
        CREATE ttOrderProductParam.
        ASSIGN 
            ttOrderProductParam.opobject     = aoSubscriptionParams.
            ttOrderProductParam.paramnb      = liNextParamNum. 
                  
    END METHOD.
    
    METHOD PUBLIC INTEGER mNextParamNb():

        FIND LAST ttOrderProductParam USE-INDEX paramnb NO-ERROR.
               
        IF NOT AVAILABLE ttOrderProductParam
        THEN RETURN 1.
        
        RETURN ( ttOrderProductParam.paramnb + 1 ).

    END METHOD.
    
    METHOD PUBLIC VOID mIsBillingAccountExists() :
        
        FIND FIRST InvoiceTargetGroup NO-LOCK WHERE 
                   InvoiceTargetGroup.ITGroupID    =   THIS-OBJECT:liBillAcctID 
                   NO-ERROR.
                   
        IF NOT AVAILABLE InvoiceTargetGroup
        THEN UNDO, THROW NEW ParamError(SUBSTITUTE("Billing Account &1 does not exist.", THIS-OBJECT:liBillAcctID)). 
        
        IF InvoiceTargetGroup.AccountID  <>  aoOrder:CustAcctNum 
        THEN UNDO , THROW NEW ParamError(SUBSTITUTE("Billing Account &1 does not associated to the entered customer account &2 .", 
                                                    THIS-OBJECT:liBillAcctID , aoOrder:CustAcctNum )). 
                                                    
        ASSIGN aoOrder:aoCustomer:OrderCustBankAcct  =  InvoiceTargetGroup.BankAccount.
                             
    END METHOD.
    
    METHOD PUBLIC VOID mGetBillingAddressObject() :
        
        DEFINE VARIABLE loValidationObj AS CLASS JsonObject NO-UNDO.
        
        FIND FIRST Address WHERE Address.HostTable    =  "Customer"
                             AND Address.KeyValue     =  STRING(aoOrder:aoCustomer:CustNum)
                             AND Address.AddressType  =  {&ADDR_DEFAULT_BILLING} NO-LOCK NO-ERROR.

        IF NOT AVAILABLE Address 
        THEN UNDO,THROW NEW ParamError("Default Billing Address Not Found.").
        
        ASSIGN            
            loValidationObj          = NEW JsonObject()
            THIS-OBJECT:AddressObj   = NEW JsonObject().
        
        AddressObj:ADD("address_id"  , Address.AddressRegisterID).
        AddressObj:ADD("street"      , Address.Address).
        AddressObj:Add("street_type" , Address.StreetType).
        AddressObj:Add("building_number" , Address.BuildingNum).
        AddressObj:Add("block" , Address.Block).
        AddressObj:Add("floor" , Address.Floor).
        AddressObj:Add("stair" , Address.Stair).
        AddressObj:Add("door" , Address.Door).
        AddressObj:Add("letter" , Address.Letter).
        AddressObj:Add("km" , Address.KM).
        AddressObj:Add("hand" , Address.Hand).
        AddressObj:Add("bis_duplicate" , Address.bisduplicate).
        AddressObj:Add("additional_address" , Address.addresscompl).
        /*AddressObj:Add("normalizedid" ,Address.normalizedid).*/
        AddressObj:Add("latitude" , Address.Latitude).
        AddressObj:Add("longitude" , Address.longitude).
        AddressObj:Add("city" , Address.City).
        AddressObj:Add("region" , Address.Region).
        AddressObj:Add("zip" , Address.zipCode).
        AddressObj:Add("country" , Address.Country).
        
        loValidationObj:Add("city_code" , Address.CityCode).
        loValidationObj:Add("street_code" , Address.StreetCode).
        loValidationObj:Add("municipality_code" , Address.towncode).
        
        AddressObj:Add("validation_data" , loValidationObj).
        
    END METHOD.

END CLASS.