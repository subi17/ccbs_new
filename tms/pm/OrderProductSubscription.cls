 
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
        
    DEFINE PUBLIC PROPERTY attrCli AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY mobileNumberType AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY ObjAggrementCustomer AS CLASS om.OrderCustomer NO-UNDO 
        GET.
        SET. 
        
    DEFINE PUBLIC PROPERTY objSubscriptionParams AS CLASS pm.OrderProductParam NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY AddressObj AS CLASS JsonObject  NO-UNDO
        GET.
        SET.
       
    CONSTRUCTOR PUBLIC OrderProductSubscription ( iObjOrder AS CLASS om.Order ):
	    
        SUPER(iObjOrder).
        
        IF objOrder:isFixedOnly THEN 
            UNDO,THROW NEW ParamError ("Fixed Only Tariff should not have mobile subscription.").
		
	END CONSTRUCTOR.

	
	
	METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( ioItemObject AS CLASS JsonObject ):
            
        DEFINE VARIABLE loAttributes      AS CLASS     JsonObject NO-UNDO.
        DEFINE VARIABLE lcReturnValue     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcOldOperatorCode AS CHARACTER NO-UNDO.
        DEFINE VARIABLE loBillAddrObj     AS CLASS     JsonObject NO-UNDO.
        DEFINE VARIABLE liCustNum         AS INTEGER   NO-UNDO.
        
        ASSIGN loAttributes = NEW Progress.Json.ObjectModel.JsonObject()
               loAttributes = ioItemObject:GetJsonObject("attributes") WHEN ioItemObject:Has("attributes")
               .
        
        IF VALID-OBJECT(loAttributes) THEN
        DO:
            
            ASSIGN
                THIS-OBJECT:mobileNumberType = loAttributes:GetCharacter("mobile_number_type") 
                THIS-OBJECT:attrCli      = loAttributes:GetCharacter("mobile_number")                
                .
                            
            IF LOOKUP(THIS-OBJECT:mobileNumberType,"new,mnp,renewal,stc") = 0 THEN
                UNDO,THROW NEW ParamError (SUBSTITUTE("Unknown number_type: ", THIS-OBJECT:mobileNumberType)). 
            
            IF THIS-OBJECT:mobileNumberType = "mnp" AND 
                CAN-FIND(FIRST Order WHERE Order.CLI = THIS-OBJECT:attrCli AND 
                              (Order.statusCode EQ {&ORDER_STATUS_MIGRATION_PENDING} OR /*60*/
                               Order.statusCode EQ {&ORDER_STATUS_MIGRATION_ONGOING})) THEN
                UNDO, THROW NEW ParamError ("Requested number is in migration").
            
            IF Func.ValidateOrder:misCLITypeExists(THIS-OBJECT:ProductID) = NO THEN
                UNDO , THROW NEW ParamError (SUBSTITUTE("Unknown Product ID : &1 ", THIS-OBJECT:ProductID)).
                     
            lcReturnValue = Func.ValidateOrder:mCheckMSISDN(THIS-OBJECT:attrCli,THIS-OBJECT:mobileNumberType). 
            
            IF lcReturnValue > "" THEN
                UNDO, THROW NEW ParamError(lcReturnValue). 
            
            ASSIGN 
                objOrder:lcCLI          = THIS-OBJECT:attrCli
                objOrder:isPrePaidOrder = Func.ValidateOrder:mIsPrepaidOrder(THIS-OBJECT:ProductID)
                objOrder:cliType        = THIS-OBJECT:ProductID.
                
            IF objOrder:isPrePaidOrder AND INDEX(objOrder:orderChannel,"PRO") > 0 THEN
                UNDO , THROW NEW ParamError("Prepaid subscriptions are not allowed for PRO customer(s)").
                  
            IF LOOKUP(THIS-OBJECT:mobileNumberType,"renewal,stc") > 0 THEN
            DO:
            
                IF Func.ValidateOrder:misSubscriptionExists(INPUT THIS-OBJECT:attrCli) = FALSE 
                    THEN UNDO , THROW NEW ParamError(SUBSTITUTE("Mobsub with msisdn &1 not found", THIS-OBJECT:attrCli)).
            
                ASSIGN 
                    liCustNum = Func.ValidateOrder:mGetMobSubCustNum(INPUT THIS-OBJECT:attrCli).
            
                IF liCustNum = 0 THEN
                    UNDO , THROW NEW ParamError("Customer not found for the renewal/stc order.").
            
                IF Func.ValidateOrder:misCustomerExists(INPUT objOrder:objCustomer:CustId , 
                                                        INPUT objOrder:objCustomer:CustIdType ,
                                                        INPUT liCustNum) = FALSE THEN
                    UNDO , THROW NEW ParamError("Customer not found with matching id and id-type.").
               
                ASSIGN 
                    objOrder:isRenewalSTCOrder = TRUE
                    objOrder:MsSeq             = Func.ValidateOrder:mGetMsSeq(THIS-OBJECT:attrCli).
            
            END.
            
            IF Func.ValidateOrder:misDuplicateContractExists(INPUT objOrder:ContractId ,
                                                             INPUT THIS-OBJECT:attrCli ) THEN
                UNDO , THROW NEW ParamError(SUBSTITUTE("Duplicate contract ID &1", objOrder:ContractId)).   
            
            IF Func.ValidateOrder:mIsMigrationCLI(INPUT THIS-OBJECT:attrCli) THEN
                UNDO , THROW  NEW ParamError("Requested number is in migration").                                                         
            
            mCreateDataOrderProductParamData(loAttributes,"mobile_number_type","Char").
            mCreateDataOrderProductParamData(loAttributes,"mobile_number","Char").  
            
            ASSIGN objOrder:mobileNumberType = THIS-OBJECT:mobileNumberType.
                                                 
            IF loAttributes:Has("mobile_old_operator_code") THEN
            DO:
                mCreateDataOrderProductParamData(loAttributes,"mobile_old_operator_code","Char").
            END.
            
            IF loAttributes:Has("mobile_old_operator_name") THEN
            DO:
                mCreateDataOrderProductParamData(loAttributes,"mobile_old_operator_name","Char").
            END.
            
            IF THIS-OBJECT:mobileNumberType = "MNP" THEN
                ASSIGN objOrder:orderType  = {&ORDER_TYPE_MNP}
                       objOrder:statusCode = {&ORDER_STATUS_MNP}.
            ELSE IF THIS-OBJECT:mobileNumberType = "NEW" THEN
                ASSIGN objOrder:orderType  = {&ORDER_TYPE_NEW}
                       objOrder:statusCode = {&ORDER_STATUS_NEW}.
                 
            IF ioItemObject:Has("billing_account_id") THEN
            DO:
                mGetBillingAddressObject(). 
            END.       
            
            IF objOrder:isPrePaidOrder 
            AND objOrder:objCustomer:isNewCustomer = FALSE THEN
                mGetBillingAddressObject(). /* This will prepare address object for the aggrement ordercustomer for prepaid order to existing customer */
            
            ASSIGN 
                ObjAggrementCustomer           =  NEW om.OrderCustomer(objOrder)
                ObjAggrementCustomer:RowType   =  {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
               .
               
            objOrder:objCustomer:mCheckSubscriptionLimit(INPUT THIS-OBJECT:mobileNumberType).
               
            IF objOrder:objCustomer:isNewCustomer THEN    
            ObjAggrementCustomer:mCreateRowCustomerData(objOrder:objCustomer:DefaultAddressObj).
            ELSE ObjAggrementCustomer:mCreateRowCustomerData(AddressObj).
            
            IF loAttributes:Has("initial_topup_amount") THEN
                mCreateDataOrderProductParamData(loAttributes,"initial_topup_amount","Dec").
            
            IF loAttributes:Has("initial_topup_display_amount") THEN
                mCreateDataOrderProductParamData(loAttributes,"initial_topup_display_amount","Dec").
            
            /*TODO PC:Please check the MULTIHOLDER INFROMATION WITH KIRAN*/
                
        END.
            
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        FOR EACH ttOrderProductParam:
            CAST(ttOrderProductParam.opobject, pm.OrderProductParam):mStoreData().
        END.
        
        IF VALID-OBJECT (ObjAggrementCustomer)
        THEN DO:
            ObjAggrementCustomer:mStoreData().
        END.

    END METHOD.
    
    METHOD PUBLIC VOID mCreateDataOrderProductParamData  (INPUT loAttributes AS CLASS JsonObject ,
                                                          INPUT Attribute    AS CHARACTER,
                                                          INPUT AttrType     AS CHARACTER  ):  
                                                              
        DEFINE VARIABLE liNextParamNum AS INTEGER NO-UNDO.                                                            
       
        objSubscriptionParams = NEW pm.OrderProductParam(THIS-OBJECT) .
        objSubscriptionParams:mCreateData(loAttributes,Attribute,AttrType).
       
        ASSIGN liNextParamNum =  mNextParamNb().
                                            
        CREATE ttOrderProductParam.
        ASSIGN 
            ttOrderProductParam.opobject     = objSubscriptionParams.
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
                             AND Address.KeyValue     =  STRING(objOrder:objCustomer:CustNum)
                             AND Address.AddressType  =  {&BILLING_ADDRESS} NO-LOCK NO-ERROR.

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
    
    DESTRUCTOR PUBLIC OrderProductSubscription ( ):
        
        IF VALID-OBJECT (objSubscriptionParams) THEN 
            DELETE OBJECT objSubscriptionParams.
            
        IF VALID-OBJECT (ObjAggrementCustomer) THEN 
            DELETE OBJECT ObjAggrementCustomer.

    END DESTRUCTOR.
    
END CLASS.