 
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

CLASS pm.OrderProductSubscription INHERITS pm.OrderProduct: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE PRIVATE TEMP-TABLE ttOrderProductParam NO-UNDO
        FIELD opobject AS CLASS Progress.Lang.Object
        FIELD paramnb  AS INTEGER
        INDEX paramnb IS PRIMARY UNIQUE paramnb
        .
	
    DEFINE PUBLIC PROPERTY aoSubscriptionParams AS CLASS pm.OrderProductParam NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY AttrCLI AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY lcNumberType AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY aoOrderAggrementCustomer AS CLASS om.OrderCustomer NO-UNDO 
        GET.
        SET. 
        
    DEFINE PUBLIC PROPERTY AddressObj AS CLASS JsonObject  NO-UNDO
        GET.
        SET.
       
    CONSTRUCTOR PUBLIC OrderProductSubscription ( ioOrder AS CLASS om.Order ):
	    
        SUPER(ioOrder).
        
        IF aoOrder:isFixedOnly 
        THEN UNDO,THROW NEW ParamError ("Fixed Only Tariff should not have mobile subscription.").
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProductSubscription ( ):
	    
	    ASSIGN aoSubscriptionParams      = ?
               aoOrderAggrementCustomer  = ?.

	END DESTRUCTOR.
	
	METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( ioItemObject AS CLASS JsonObject ):
            
        DEFINE VARIABLE loAttributes      AS CLASS     JsonObject NO-UNDO.
        DEFINE VARIABLE lcReturnValue     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcOldOperatorCode AS CHARACTER NO-UNDO.
        DEFINE VARIABLE loBillAddrObj     AS CLASS     JsonObject NO-UNDO.
        DEFINE VARIABLE liCustNum         AS INTEGER   NO-UNDO.
        
        ASSIGN loAttributes                     = NEW Progress.Json.ObjectModel.JsonObject()
               loAttributes                     = ioItemObject:GetJsonObject("attributes") WHEN ioItemObject:Has("attributes")
               .
        
        IF VALID-OBJECT(loAttributes)
        THEN DO:
            
            ASSIGN
                THIS-OBJECT:lcNumberType = loAttributes:GetCharacter("mobile_number_type") 
                THIS-OBJECT:AttrCLI      = loAttributes:GetCharacter("mobile_number")                
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
                     
            lcReturnValue = Func.ValidateOrder:mCheckMSISDN(THIS-OBJECT:AttrCLI,THIS-OBJECT:lcNumberType). 
            
            IF lcReturnValue > ""
            THEN UNDO, THROW NEW ParamError(lcReturnValue). 
            
            ASSIGN 
                aoOrder:lcCLI          = THIS-OBJECT:AttrCLI
                aoOrder:isPrePaidOrder = Func.ValidateOrder:mIsPrepaidOrder(THIS-OBJECT:ProductID)
                aoOrder:CLIType        = THIS-OBJECT:ProductID.
                
            IF aoOrder:isPrePaidOrder AND INDEX(aoOrder:OrderChannel,"PRO") > 0
            THEN UNDO , THROW NEW ParamError("Prepaid subscriptions are not allowed for PRO customer(s)").
                  
            IF LOOKUP(THIS-OBJECT:lcNumberType,"renewal,stc") > 0
            THEN DO:
            
                IF Func.ValidateOrder:misSubscriptionExists(INPUT THIS-OBJECT:AttrCLI) = FALSE 
                    THEN UNDO , THROW NEW ParamError(SUBSTITUTE("Mobsub with msisdn &1 not found", THIS-OBJECT:AttrCLI)).
            
                ASSIGN 
                    liCustNum = Func.ValidateOrder:mGetMobSubCustNum(INPUT THIS-OBJECT:AttrCLI).
            
                IF liCustNum = 0
                THEN UNDO , THROW NEW ParamError("Customer not found for the renewal/stc order.").
            
                IF Func.ValidateOrder:misCustomerExists(INPUT aoOrder:aoCustomer:CustId , 
                                                        INPUT aoOrder:aoCustomer:CustIdType ,
                                                        INPUT liCustNum) = FALSE 
               THEN UNDO , THROW NEW ParamError("Customer not found with matching id and id-type.").
               
                ASSIGN 
                    aoOrder:isRenewalSTCOrder = TRUE
                    aoOrder:MsSeq             = Func.ValidateOrder:mGetMsSeq(THIS-OBJECT:AttrCLI).
            
            END.
            
            IF Func.ValidateOrder:misDuplicateContractExists(INPUT aoOrder:ContractId ,
                                                             INPUT THIS-OBJECT:AttrCLI )
            THEN UNDO , THROW NEW ParamError(SUBSTITUTE("Duplicate contract ID &1", aoOrder:ContractId)).   
            
            IF Func.ValidateOrder:mIsMigrationCLI(INPUT THIS-OBJECT:AttrCLI)
            THEN UNDO , THROW  NEW ParamError("Requested number is in migration").                                                         
            
            mCreateDataOrderProductParamData(loAttributes,"mobile_number_type","Char").
            mCreateDataOrderProductParamData(loAttributes,"mobile_number","Char").  
            
            ASSIGN aoOrder:MobileNumberType = THIS-OBJECT:lcNumberType.
                                                 
            IF loAttributes:Has("mobile_old_operator_code") 
            THEN DO:
                mCreateDataOrderProductParamData(loAttributes,"mobile_old_operator_code","Char").
            END.
            
            IF loAttributes:Has("mobile_old_operator_name") 
            THEN DO:
                mCreateDataOrderProductParamData(loAttributes,"mobile_old_operator_name","Char").
            END.
            
            IF THIS-OBJECT:lcNumberType = "MNP"
            THEN ASSIGN aoOrder:OrderType  = {&ORDER_TYPE_MNP}
                        aoOrder:StatusCode = {&ORDER_STATUS_MNP}.
            ELSE IF THIS-OBJECT:lcNumberType = "NEW" 
            THEN ASSIGN aoOrder:OrderType  = {&ORDER_TYPE_NEW}
                        aoOrder:StatusCode = {&ORDER_STATUS_NEW}.
                 
            IF ioItemObject:Has("billing_account_id")
            THEN DO:
                mGetBillingAddressObject(). 
            END.       
            
            IF aoOrder:isPrePaidOrder 
            AND aoOrder:aoCustomer:isNewCustomer = FALSE
            THEN mGetBillingAddressObject(). /* This will prepare address object for the aggrement ordercustomer for prepaid order to existing customer */
            
            ASSIGN 
                aoOrderAggrementCustomer           =  NEW om.OrderCustomer(aoOrder)
                aoOrderAggrementCustomer:RowType   =  {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
               .
               
            aoOrder:aoCustomer:mCheckSubscriptionLimit(INPUT THIS-OBJECT:lcNumberType).
               
            IF aoOrder:aoCustomer:isNewCustomer THEN    
            aoOrderAggrementCustomer:mCreateRowCustomerData(aoOrder:aoCustomer:DefaultAddressObj).
            ELSE aoOrderAggrementCustomer:mCreateRowCustomerData(AddressObj).
            
            IF loAttributes:Has("initial_topup_amount")
            THEN mCreateDataOrderProductParamData(loAttributes,"initial_topup_amount","Dec").
            
            IF loAttributes:Has("initial_topup_display_amount")
            THEN mCreateDataOrderProductParamData(loAttributes,"initial_topup_display_amount","Dec").
            
            /*TODO PC:Please check the MULTIHOLDER INFROMATION WITH KIRAN*/
                
        END.
            
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        FOR EACH ttOrderProductParam:
            CAST(ttOrderProductParam.opobject, pm.OrderProductParam):mStoreData().
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
       
        aoSubscriptionParams = NEW pm.OrderProductParam(THIS-OBJECT) .
        aoSubscriptionParams:mCreateData(loAttributes,Attribute,AttrType).
       
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
        Addressobj:Add("gescal" , Address.Gescal).
        Addressobj:Add("coveragetoken" , Address.CoverageToken).
        
        loValidationObj:Add("city_code" , Address.CityCode).
        loValidationObj:Add("street_code" , Address.StreetCode).
        loValidationObj:Add("municipality_code" , Address.towncode).
        
        AddressObj:Add("validation_data" , loValidationObj).
        
    END METHOD.
    
END CLASS.