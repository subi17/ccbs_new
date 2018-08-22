 
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
    
    DEFINE PUBLIC PROPERTY ObjAggrementCustomer AS CLASS om.OrderCustomer NO-UNDO 
        GET.
        SET. 
        
    DEFINE PUBLIC PROPERTY objParamFactory AS CLASS pm.OrderProductParamFactory NO-UNDO
        GET.
        SET.   
        
    DEFINE PUBLIC PROPERTY objFixedPoCustomer AS CLASS om.OrderCustomer NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY objMobilePoCustomer AS CLASS om.OrderCustomer NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY AddressObj AS CLASS JsonObject  NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY objTopup  AS CLASS om.OrderTopup NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY objOrderMobile AS CLASS om.OrderMobile NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY objOrderFusion AS CLASS om.OrderFusion NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY fixNumberType AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY fixedNumber AS CHARACTER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY estimatedSpeed AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY serialNumber AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY fixedMNPTime AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY fixedCurrOper AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY fixedCurrOperCode AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY fixedLineCustomerType AS CHARACTER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY mobileNumber AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY mobileNumberType AS CHARACTER NO-UNDO
        GET.
        SET.
        
    DEFINE VARIABLE loAttributes AS CLASS JsonObject NO-UNDO.
       
    CONSTRUCTOR PUBLIC OrderProductSubscription ( iObjOrder AS CLASS om.Order ):
	    
        SUPER(iObjOrder).
        
	END CONSTRUCTOR.

	METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( iObjItemStruct AS CLASS JsonObject ):

        mGetProductTariffType().
        
        IF THIS-OBJECT:actionType = {&ORDER_PRODUCT_MOBILE} THEN 
            mCreateMobileProduct(iObjItemStruct).
        ELSE 
            mCreateFixedorCovergentProduct(iObjItemStruct).
                    
    END METHOD.
    
    METHOD PUBLIC VOID mGetProductTariffType () :
       
        IF Func.ValidateOrder:misCLITypeExists(THIS-OBJECT:productId) = FALSE THEN 
            UNDO, THROW NEW ParamError(SUBSTITUTE("invalid clitype : &1",THIS-OBJECT:productId)).
            
        IF Func.ValidateOrder:mIsConvergentORFixedOnly(THIS-OBJECT:productId) THEN 
            ASSIGN 
                THIS-OBJECT:actionType = {&ORDER_PRODUCT_FIXED_LINE}.
        ELSE 
            ASSIGN
                THIS-OBJECT:actionType = {&ORDER_PRODUCT_MOBILE}.       
       
    END METHOD.
    
    METHOD PUBLIC VOID mCreateMobileProduct(INPUT iObjItemStruct AS JsonObject):
        
        DEFINE VARIABLE lcReturnValue     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcOldOperatorCode AS CHARACTER NO-UNDO.
        DEFINE VARIABLE loBillAddrObj     AS CLASS     JsonObject NO-UNDO.
        DEFINE VARIABLE liCustNum         AS INTEGER   NO-UNDO.
        DEFINE VARIABLE objTopupStruct    AS CLASS     JsonObject NO-UNDO.
        
        ASSIGN 
            loAttributes = NEW Progress.Json.ObjectModel.JsonObject()
            loAttributes = iObjItemStruct:GetJsonObject("attributes") WHEN iObjItemStruct:Has("attributes")
            .
        
        IF VALID-OBJECT(loAttributes) THEN
        DO:                                                
            ASSIGN
                THIS-OBJECT:mobileNumberType = loAttributes:GetCharacter("mobile_number_type") 
                THIS-OBJECT:mobileNumber     = loAttributes:GetCharacter("mobile_number")                
                .
                            
            IF LOOKUP(THIS-OBJECT:mobileNumberType,"new,mnp") = 0 THEN
                UNDO,THROW NEW ParamError (SUBSTITUTE("Unknown number_type: ", THIS-OBJECT:mobileNumberType)). 
            
            IF THIS-OBJECT:mobileNumberType = "mnp" AND 
                CAN-FIND(FIRST Order WHERE Order.CLI = THIS-OBJECT:mobileNumber AND 
                (Order.statusCode EQ {&ORDER_STATUS_MIGRATION_PENDING} OR /*60*/
                 Order.statusCode EQ {&ORDER_STATUS_MIGRATION_ONGOING})) THEN
                UNDO, THROW NEW ParamError ("Requested number is in migration").
            
            ASSIGN 
                lcReturnValue = Func.ValidateOrder:mCheckMSISDN(THIS-OBJECT:mobileNumber,THIS-OBJECT:mobileNumberType). 
            
            IF lcReturnValue > "" THEN
                UNDO, THROW NEW ParamError(lcReturnValue). 
            
            ASSIGN 
                objOrder:lcCLI          = THIS-OBJECT:mobileNumber
                objOrder:isPrePaidOrder = Func.ValidateOrder:mIsPrepaidOrder(THIS-OBJECT:ProductID)
                objOrder:cliType        = THIS-OBJECT:ProductID.
                
            IF objOrder:isPrePaidOrder AND INDEX(objOrder:orderChannel,"PRO") > 0 THEN
                UNDO , THROW NEW ParamError("Prepaid subscriptions are not allowed for PRO customer(s)").
                  
            IF LOOKUP(THIS-OBJECT:mobileNumberType,"renewal,stc") > 0 THEN
            DO:
                            
                IF Func.ValidateOrder:misSubscriptionExists(INPUT THIS-OBJECT:mobileNumber) = FALSE THEN 
                    UNDO , THROW NEW ParamError(SUBSTITUTE("Mobsub with msisdn &1 not found", THIS-OBJECT:mobileNumber)).
            
                ASSIGN 
                    liCustNum = Func.ValidateOrder:mGetMobSubCustNum(INPUT THIS-OBJECT:mobileNumber).
            
                IF liCustNum = 0 THEN
                    UNDO , THROW NEW ParamError("Customer not found for the renewal/stc order.").
            
                IF Func.ValidateOrder:misCustomerExists(INPUT objOrder:objCustomer:CustId , 
                                                        INPUT objOrder:objCustomer:CustIdType ,
                                                        INPUT liCustNum) = FALSE THEN
                    UNDO , THROW NEW ParamError("Customer not found with matching id and id-type.").
               
                ASSIGN 
                    objOrder:isRenewalSTCOrder = TRUE
                    objOrder:MsSeq             = Func.ValidateOrder:mGetMsSeq(THIS-OBJECT:mobileNumber).
            
            END.
            
            IF Func.ValidateOrder:misDuplicateContractExists(INPUT objOrder:ContractId , INPUT THIS-OBJECT:mobileNumber ) THEN
                UNDO , THROW NEW ParamError(SUBSTITUTE("Duplicate contract ID &1", objOrder:ContractId)).   
            
            IF Func.ValidateOrder:mIsMigrationCLI(INPUT THIS-OBJECT:mobileNumber) THEN
                UNDO , THROW  NEW ParamError("Requested number is in migration").      /* TO-DO PC*/                                                   
            
            ASSIGN 
                objOrder:mobileNumberType = THIS-OBJECT:mobileNumberType.
                                                 
            IF THIS-OBJECT:mobileNumberType = "MNP" THEN
                ASSIGN objOrder:orderType  = {&ORDER_TYPE_MNP}
                    objOrder:statusCode = {&ORDER_STATUS_MNP}.
            ELSE IF THIS-OBJECT:mobileNumberType = "NEW" THEN
                ASSIGN objOrder:orderType  = {&ORDER_TYPE_NEW}
                       objOrder:statusCode = {&ORDER_STATUS_NEW}.
                 
            IF iObjItemStruct:Has("billing_account_id") OR 
                (objOrder:objCustomer:isNewCustomer = FALSE AND iObjItemStruct:Has("billing_account_index"))THEN
                 mGetBillingAddressObject(). 
            
            IF objOrder:isPrePaidOrder AND objOrder:objCustomer:isNewCustomer = FALSE THEN
                mGetBillingAddressObject(). /* This will prepare address object for the aggrement ordercustomer for prepaid order to existing customer */
            
            ASSIGN 
                ObjAggrementCustomer         = NEW om.OrderCustomer(objOrder)
                ObjAggrementCustomer:RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
                .
               
            objOrder:objCustomer:mCheckSubscriptionLimit(INPUT THIS-OBJECT:mobileNumberType).
               
            IF objOrder:objCustomer:isNewCustomer THEN    
                ObjAggrementCustomer:mCreateRowCustomerData(objOrder:objCustomer:DefaultAddressObj).
            ELSE 
                ObjAggrementCustomer:mCreateRowCustomerData(AddressObj).
                
            ObjAggrementCustomer:mAssignAuthorisedCustomer().
                
            objParamFactory = NEW pm.OrderProductParamFactory(THIS-OBJECT).
            objParamFactory:mCreateData(loAttributes).
            
            IF loAttributes:Has("initial_topup_amount") THEN 
            DO:                
                ASSIGN 
                    objTopup = NEW om.OrderTopup(objOrder).                    
                objTopup:mCreateData(loAttributes:GetDecimal("initial_topup_amount"),
                                     loAttributes:GetDecimal("initial_topup_display_amount"),
                                     {&INITIAL_TOPUP}).
            END.
            
            ASSIGN 
                objOrderMobile = NEW om.OrderMobile(THIS-OBJECT).
            objOrderMobile:mCreateData(loAttributes).
            
        /*TODO PC:Please check the MULTIHOLDER INFROMATION WITH KIRAN*/
        END. /* IF VALID-OBJECT(loAttributes) THEN */

    END METHOD.
    
    METHOD PUBLIC VOID mCreateFixedorCovergentProduct(INPUT iObjItemStruct AS JsonObject):

        DEFINE VARIABLE lcCLIError     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE liCustNum      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lcMobSubFixNum AS CHARACTER NO-UNDO.

        IF Func.ValidateOrder:mIsFixedOnlyTariff(THIS-OBJECT:productID) THEN
            ASSIGN objOrder:isFixedOnly = YES.
            
        ASSIGN 
            loAttributes              = NEW JsonObject()   
            loAttributes              = iObjItemStruct:GetJsonObject("attributes")
            THIS-OBJECT:fixNumberType = loAttributes:GetCharacter("fixedline_number_type")
            THIS-OBJECT:fixedNumber   = loAttributes:GetCharacter("fixedline_number").
            
        ASSIGN 
            THIS-OBJECT:mobileNumberType = loAttributes:GetCharacter("mobile_number_type") WHEN loAttributes:Has("mobile_number_type")
            THIS-OBJECT:mobileNumber     = loAttributes:GetCharacter("mobile_number")      WHEN loAttributes:Has("mobile_number")
            objOrder:mobileNumberType    = THIS-OBJECT:mobileNumberType
            objOrder:lcCLI               = THIS-OBJECT:mobileNumber
            objOrder:cliType             = THIS-OBJECT:productID.
            
        IF LOOKUP(THIS-OBJECT:fixNumberType,"new,mnp") = 0 THEN
            UNDO,THROW NEW ParamError (SUBSTITUTE("Unknown fixedline_number_type: &1 ", THIS-OBJECT:fixNumberType)).   
        
        IF Func.ValidateOrder:misFilledFixedNumber(THIS-OBJECT:fixedNumber) THEN
            UNDO,THROW NEW ParamError (SUBSTITUTE("Fixed number &1 already exists. ", THIS-OBJECT:fixedNumber)). 
            
        IF loAttributes:Has("mobile_number_type")
            AND LOOKUP(THIS-OBJECT:mobileNumberType,"new,mnp") = 0 THEN
            UNDO , THROW NEW ParamError (SUBSTITUTE("Unknown mobile_number_type: &1 ", THIS-OBJECT:mobileNumberType)).
        
        IF THIS-OBJECT:mobileNumber > "" THEN
        DO:
            ASSIGN 
                lcCLIError = Func.ValidateOrder:mCheckMSISDN(THIS-OBJECT:mobileNumber,THIS-OBJECT:mobileNumberType).
                
            IF lcCLIError > "" THEN   
                UNDO , THROW NEW ParamError(lcCLIError).
        END.
        
        IF THIS-OBJECT:mobileNumber > "" AND
            LOOKUP(THIS-OBJECT:mobileNumberType,"renewal,stc") > 0 THEN
        DO:
            
            IF Func.ValidateOrder:misSubscriptionExists(INPUT THIS-OBJECT:mobileNumber) = FALSE THEN
                UNDO , THROW NEW ParamError(SUBSTITUTE("Mobsub with msisdn &1 not found", THIS-OBJECT:mobileNumber)).
            
            ASSIGN 
                liCustNum = Func.ValidateOrder:mGetMobSubCustNum(INPUT THIS-OBJECT:mobileNumber).
            
            IF liCustNum = 0 THEN
                UNDO , THROW NEW ParamError("Customer not found for the renewal/stc order.").
            
            IF Func.ValidateOrder:misCustomerExists(INPUT objOrder:objCustomer:CustId , 
                                                    INPUT objOrder:objCustomer:CustIdType ,
                                                    INPUT liCustNum) = FALSE THEN
                UNDO , THROW NEW ParamError("Customer not found with matching id and id-type.").
            
            ASSIGN 
                objOrder:isRenewalSTCOrder = TRUE 
                objOrder:MsSeq             = Func.ValidateOrder:mGetMsSeq(THIS-OBJECT:mobileNumber).
            
        END.
        
        IF LOOKUP(THIS-OBJECT:fixNumberType,"renewal,stc") > 0 AND
            THIS-OBJECT:fixedNumber  > ""                      AND
            THIS-OBJECT:mobileNumber > ""  THEN                        /*This needs to be non-zero*/
        DO:
            /*Mobsub check is already done above misSubscriptionExists*/ 
            ASSIGN 
                lcMobSubFixNum = Func.ValidateOrder:mGetSubscriptionFixedNumber(INPUT THIS-OBJECT:mobileNumber).
                
            IF lcMobSubFixNum   EQ  "" OR 
               lcMobSubFixNum   EQ  ?  OR
               lcMobSubFixNum   NE  THIS-OBJECT:fixedNumber THEN
                UNDO , THROW NEW ParamError(SUBSTITUTE("Mobsub with Fixed Number &1 not found", THIS-OBJECT:fixedNumber)).    
            
        END.
        
        IF THIS-OBJECT:mobileNumber > "" AND  
            CAN-FIND(FIRST MobSub WHERE MobSub.Brand         =   Syst.Var:gcBrand AND 
                                        MobSub.FixedNumber   =   THIS-OBJECT:fixedNumber AND 
                                        MobSub.CLI          <>   THIS-OBJECT:mobileNumber) THEN
            UNDO , THROW NEW ParamError("Subscription already exists with Fixed Number " + THIS-OBJECT:fixedNumber).
        
        IF Func.ValidateOrder:mCheckOngoingFixedOrders(INPUT THIS-OBJECT:fixedNumber , INPUT THIS-OBJECT:fixNumberType) THEN
            UNDO , THROW NEW ParamError (SUBSTITUTE("Ongoing order for number &1 " , THIS-OBJECT:fixedNumber)). 
        
        IF NOT objOrder:orderChannel BEGINS "FUSION" THEN
            UNDO , THROW NEW ParamError (SUBST("Incorrect fusion order channel &1",objOrder:orderChannel)).  
        
        IF Func.ValidateOrder:misDuplicateContractExists(INPUT objOrder:ContractId , INPUT THIS-OBJECT:mobileNumber ) THEN
            UNDO , THROW NEW ParamError(SUBSTITUTE("Duplicate contract ID &1", objOrder:ContractId)). 
        
        IF THIS-OBJECT:fixNumberType = {&FUSION_FIXED_NUMBER_TYPE_MNP} AND THIS-OBJECT:fixedNumber  = "" THEN
            UNDO , THROW NEW ParamError("fixed_line_number is mandatory with fixed_line_number_type=MNP").
        
        IF THIS-OBJECT:fixNumberType = {&FUSION_FIXED_NUMBER_TYPE_MNP} AND loAttributes:GetCharacter("fixedline_old_operator_code") = "" THEN 
            UNDO , THROW NEW ParamError("fixed_line_mnp_old_operator_code is mandatory with fixed_line_number_type=MNP").
        
        IF THIS-OBJECT:mobileNumber > "" AND Func.ValidateOrder:mIsMigrationCLI(INPUT THIS-OBJECT:mobileNumber) THEN 
            UNDO , THROW  NEW ParamError("Requested number is in migration"). 
            
        IF THIS-OBJECT:mobileNumberType  =  {&TYPE_MNP} THEN
            ASSIGN objOrder:orderType = {&ORDER_TYPE_MNP}.
        ELSE IF THIS-OBJECT:mobileNumberType  =  {&TYPE_NEW} THEN
            ASSIGN objOrder:orderType = {&ORDER_TYPE_NEW}.  
            
        ASSIGN objParamFactory = NEW pm.OrderProductParamFactory(INPUT THIS-OBJECT).
        objParamFactory:mCreateData(loAttributes).
        
        ASSIGN 
            objOrderFusion = NEW om.OrderFusion(THIS-OBJECT).
        objOrderFusion:mCreateData(loAttributes).    
        
        IF Func.ValidateOrder:mIsConvergentTariff(THIS-OBJECT:productId) THEN 
        DO:            
            ASSIGN 
                objOrderMobile = NEW om.OrderMobile(THIS-OBJECT).
            objOrderMobile:mCreateData(loAttributes).
        END.
        
        ASSIGN 
            objAggrementCustomer         = NEW om.OrderCustomer(objOrder)
            objAggrementCustomer:RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}.
            
        objOrder:objCustomer:mCheckSubscriptionLimit(INPUT THIS-OBJECT:fixNumberType).  
        
        IF objOrder:objCustomer:isNewCustomer THEN 
            objAggrementCustomer:mCreateRowCustomerData(objOrder:objCustomer:DefaultAddressObj). 
            
        ObjAggrementCustomer:mAssignAuthorisedCustomer().
            
        IF loAttributes:Has("fixed_line_holder") THEN 
        DO:            
            ASSIGN 
                objFixedPoCustomer         = NEW om.OrderCustomer(objOrder)
                objFixedPoCustomer:RowType = {&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER}.
            
            IF objOrder:objCustomer:isNewCustomer  THEN
            DO:
                objFixedPoCustomer:mCreateRowCustomerData(objOrder:objCustomer:DefaultAddressObj).
            /*objFixedPoCustomer:mCreateHolderData(loAttributes:GetJsonObject("fixed_line_holder")).*/
            END.            
        END.
        
        IF loAttributes:Has("mobile_line_holder") THEN
        DO:            
            ASSIGN 
                objMobilePoCustomer         = NEW om.OrderCustomer(objOrder)
                objMobilePoCustomer:RowType = {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER}.
            
            IF objOrder:objCustomer:isNewCustomer  THEN 
            DO:
                objMobilePoCustomer:mCreateRowCustomerData(objOrder:objCustomer:DefaultAddressObj).
            /*aoMobilePOCustomer:mCreateHolderData(loAttributes:GetJsonObject("mobile_line_holder")).*/
            END.
        END.                        
        ASSIGN 
            objOrder:statusCode = {&ORDER_STATUS_PENDING_FIXED_LINE}.  
            
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        IF THIS-OBJECT:actionType = {&ORDER_PRODUCT_MOBILE} THEN 
            mStoreMobileProduct().
        ELSE 
            mStoreFixedorConvergentProduct().

    END METHOD.
    
    METHOD PUBLIC VOID mStoreMobileProduct():
 
        mStoreSubscriptionProduct().
                
        IF VALID-OBJECT(objTopup) THEN 
            objTopup:mStoreData(THIS-OBJECT:orderProductID).
            
    END METHOD.
    
    METHOD PUBLIC VOID mStoreFixedorConvergentProduct():
        
        mStoreSubscriptionProduct().
        
        IF VALID-OBJECT(objMobilePoCustomer) THEN 
            objMobilePoCustomer:mStoreData().
        
        IF VALID-OBJECT(objFixedPoCustomer) THEN 
            objFixedPoCustomer:mStoreData().
            
        IF VALID-OBJECT(objOrderFusion) THEN 
            objOrderFusion:mStoreData().
        
    END METHOD.
    
    METHOD PUBLIC VOID mStoreSubscriptionProduct():
        
        IF VALID-OBJECT(objAggrementCustomer) THEN 
            objAggrementCustomer:mStoreData().
        
        IF VALID-OBJECT(objParamFactory) THEN 
            objParamFactory:mStoreData().
            
        IF VALID-OBJECT(objOrderMobile) THEN 
            objOrderMobile:mStoreData().
        
    END METHOD.
    
    METHOD PUBLIC VOID mGetBillingAddressObject() :
        
        DEFINE VARIABLE loValidationObj AS CLASS JsonObject NO-UNDO.
        
        FIND FIRST Address WHERE Address.HostTable    =  "Customer" AND
                                 Address.KeyValue     =  STRING(objOrder:objCustomer:CustNum) AND
                                 Address.AddressType  =  {&BILLING_ADDRESS} NO-LOCK NO-ERROR.

        IF NOT AVAILABLE Address THEN 
            UNDO,THROW NEW ParamError("Default Billing Address Not Found.").
        
        ASSIGN            
            loValidationObj        = NEW JsonObject()
            THIS-OBJECT:AddressObj = NEW JsonObject().
        
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
        
        IF VALID-OBJECT(ObjAggrementCustomer) THEN 
            DELETE OBJECT ObjAggrementCustomer.
            
        IF VALID-OBJECT(objTopup) THEN 
            DELETE OBJECT objTopup.    
            
        IF VALID-OBJECT(objParamFactory) THEN 
            DELETE OBJECT objParamFactory.
            
        IF VALID-OBJECT(objOrderMobile) THEN 
            DELETE OBJECT objOrderMobile.
            
        IF VALID-OBJECT(objOrderFusion) THEN 
            DELETE OBJECT objOrderFusion.
            
        IF VALID-OBJECT (objFixedPoCustomer) THEN 
            DELETE OBJECT objFixedPoCustomer.
         
        IF VALID-OBJECT (objMobilePoCustomer) THEN 
            DELETE OBJECT objMobilePoCustomer.

    END DESTRUCTOR.
        
END CLASS.