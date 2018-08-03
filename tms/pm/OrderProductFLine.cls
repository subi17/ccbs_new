 
/*------------------------------------------------------------------------
   File        : OrderProductFLine
   Purpose     : 
   Syntax      : 
   Description : This will store the product data for the Fixed Line 
   Author(s)   : Koundinya Maddali
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

CLASS pm.OrderProductFLine INHERITS pm.OrderProduct: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
	
    DEFINE PRIVATE TEMP-TABLE ttOrderProductParam NO-UNDO
        FIELD opobject AS CLASS   Progress.Lang.Object
        FIELD paramnb  AS INTEGER
        INDEX paramnb IS PRIMARY UNIQUE paramnb
        .        
           
    DEFINE PUBLIC PROPERTY objProductParams AS CLASS pm.OrderProductParam NO-UNDO
        GET.
        SET.       
        
    DEFINE PUBLIC PROPERTY objAggrementCustomer AS CLASS om.OrderCustomer NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY aoFixedPOCustomer AS CLASS om.OrderCustomer NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY aoMobilePOCustomer AS CLASS om.OrderCustomer NO-UNDO 
        GET.
        SET.
	
	DEFINE PUBLIC PROPERTY fixNumberType AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY fixedNumber AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY mobileNumberType AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY mobileNumber AS CHARACTER NO-UNDO
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
       
	CONSTRUCTOR PUBLIC OrderProductFLine ( INPUT iObjOrder AS CLASS om.Order  ):
	    
		SUPER (iObjOrder).
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProductFLine ( ):
	    
	    
	    IF VALID-OBJECT (objAggrementCustomer) THEN 
	        DELETE OBJECT objAggrementCustomer.
	        
	    IF VALID-OBJECT (objAggrementCustomer) THEN 
            DELETE OBJECT objAggrementCustomer.
         
	    IF VALID-OBJECT (objAggrementCustomer) THEN 
            DELETE OBJECT objAggrementCustomer.
             
         
	    IF VALID-OBJECT (objAggrementCustomer) THEN 
            DELETE OBJECT objAggrementCustomer.
            
	END DESTRUCTOR.
	
    METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( objFixedLineAttrStruct AS CLASS JsonObject ):
        
        DEFINE VARIABLE lcCLIError     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE liCustNum      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lcMobSubFixNum AS CHARACTER NO-UNDO.
        DEFINE VARIABLE loAttributes   AS CLASS JsonObject NO-UNDO.        
        
        IF Func.ValidateOrder:misCLITypeExists(THIS-OBJECT:productID) = NO THEN
            UNDO , THROW NEW ParamError (SUBSTITUTE("Unknown Product ID : &1 ", THIS-OBJECT:productID)).
            
        IF Func.ValidateOrder:mIsConvergentORFixedOnly(THIS-OBJECT:productID) = FALSE THEN
            UNDO,THROW NEW ParamError (SUBSTITUTE("Product &1 is not convergent or fixed only.", THIS-OBJECT:productID)). 
        
        IF Func.ValidateOrder:mIsFixedOnlyTariff(THIS-OBJECT:productID) THEN
            ASSIGN objOrder:isFixedOnly  =  YES.
        
        ASSIGN 
            loAttributes                 = NEW JsonObject()   
            loAttributes                 = objFixedLineAttrStruct:GetJsonObject("attributes")
            THIS-OBJECT:fixNumberType    = loAttributes:GetCharacter("fixedline_number_type")
            THIS-OBJECT:fixedNumber      = loAttributes:GetCharacter("fixedline_number").
            
        ASSIGN 
            THIS-OBJECT:mobileNumberType = loAttributes:GetCharacter("mobile_number_type") WHEN loAttributes:Has("mobile_number_type")
            THIS-OBJECT:mobileNumber     = loAttributes:GetCharacter("mobile_number")      WHEN loAttributes:Has("mobile_number")
            objOrder:mobileNumberType     = THIS-OBJECT:mobileNumberType
            objOrder:lcCLI                = THIS-OBJECT:mobileNumber WHEN THIS-OBJECT:mobileNumber > ""
            objOrder:cliType              = THIS-OBJECT:productID.
            
        IF LOOKUP(THIS-OBJECT:fixNumberType,"new,mnp,renewal,stc") = 0 THEN
            UNDO,THROW NEW ParamError (SUBSTITUTE("Unknown fixedline_number_type: ", THIS-OBJECT:fixNumberType)).   
        
        IF Func.ValidateOrder:misFilledFixedNumber(THIS-OBJECT:fixedNumber) THEN
            UNDO,THROW NEW ParamError (SUBSTITUTE("Fixed number &1 already exists. ", THIS-OBJECT:fixedNumber)). 
        
        IF loAttributes:Has("mobile_number_type")
        AND LOOKUP(THIS-OBJECT:mobileNumberType,"new,mnp,renewal,stc") = 0 THEN
            UNDO , THROW NEW ParamError (SUBSTITUTE("Unknown mobile_number_type: ", THIS-OBJECT:mobileNumberType)).
        
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
            
            ASSIGN liCustNum = Func.ValidateOrder:mGetMobSubCustNum(INPUT THIS-OBJECT:mobileNumber).
            
            IF liCustNum = 0 THEN
                UNDO , THROW NEW ParamError("Customer not found for the renewal/stc order.").
            
            IF Func.ValidateOrder:misCustomerExists(INPUT objOrder:objCustomer:CustId , 
                                                    INPUT objOrder:objCustomer:CustIdType ,
                                                    INPUT liCustNum) = FALSE THEN
                UNDO , THROW NEW ParamError("Customer not found with matching id and id-type.").
            
            ASSIGN objOrder:isRenewalSTCOrder  = TRUE 
                   objOrder:MsSeq              = Func.ValidateOrder:mGetMsSeq(THIS-OBJECT:mobileNumber).
            
        END.
        
        IF LOOKUP(THIS-OBJECT:fixNumberType,"renewal,stc") > 0 AND
           THIS-OBJECT:fixedNumber  > ""                       AND
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
        
        IF THIS-OBJECT:fixNumberType       =  {&FUSION_FIXED_NUMBER_TYPE_MNP} THEN
            ASSIGN objOrder:orderType      =  {&ORDER_TYPE_MNP}.
        ELSE IF THIS-OBJECT:fixNumberType  =  {&FUSION_FIXED_NUMBER_TYPE_NEW} THEN
            ASSIGN objOrder:orderType      =  {&ORDER_TYPE_NEW}.  
        
        mCreateDataOrderProductParamData(loAttributes,"fixedline_number_type","Char").
        mCreateDataOrderProductParamData(loAttributes,"fixedline_number","Char").
        mCreateDataOrderProductParamData(loAttributes,"estimated_speed","Char").
        mCreateDataOrderProductParamData(loAttributes,"serial_number","Char").
        
        IF THIS-OBJECT:mobileNumberType > "" THEN
            mCreateDataOrderProductParamData(loAttributes,"mobile_number_type","Char").
        
        IF THIS-OBJECT:mobileNumber > "" THEN
            mCreateDataOrderProductParamData(loAttributes,"mobile_number","Char").  
                 
        IF loAttributes:Has("fixedline_old_operator_code") THEN
        DO:
            ASSIGN THIS-OBJECT:fixedCurrOperCode = loAttributes:GetCharacter("fixedline_old_operator_code").
            mCreateDataOrderProductParamData(loAttributes,"fixedline_old_operator_code","Char").
        END. 
        
        IF loAttributes:Has("fixedline_old_operator_name") THEN
        DO:
            ASSIGN THIS-OBJECT:FixedCurrOper    =  loAttributes:GetCharacter("fixedline_old_operator_name").
            mCreateDataOrderProductParamData(loAttributes,"fixedline_old_operator_name","Char").
        END.
        
        IF loAttributes:Has("mobile_old_operator_code") THEN
            mCreateDataOrderProductParamData(loAttributes,"mobile_old_operator_code","Char").
        
        IF loAttributes:Has("mobile_old_operator_name") THEN
            mCreateDataOrderProductParamData(loAttributes,"mobile_old_operator_name","Char").
        
        IF loAttributes:Has("mnptime") THEN
        DO:
            ASSIGN THIS-OBJECT:fixedMNPTime    =   loAttributes:GetCharacter("mnptime").
            mCreateDataOrderProductParamData(loAttributes,"mnptime","Char").
        END.

        ASSIGN objAggrementCustomer           =  NEW om.OrderCustomer(objOrder)
               objAggrementCustomer:RowType   =  {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}.
               
        /*TO-DO PC : do we need to send the fix number type or mobile number type as input parameter*/
               
        objOrder:objCustomer:mCheckSubscriptionLimit(INPUT THIS-OBJECT:fixNumberType).
               
        IF objOrder:objCustomer:isNewCustomer THEN 
            objAggrementCustomer:mCreateRowCustomerData(objOrder:objCustomer:DefaultAddressObj). 
        
        IF loAttributes:Has("fixed_line_holder")
        THEN DO:
            
            ASSIGN aoFixedPOCustomer         =  NEW om.OrderCustomer(objOrder)
                   aoFixedPOCustomer:RowType =  {&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER}.
            
            IF objOrder:objCustomer:isNewCustomer  THEN
            DO:
                aoFixedPOCustomer:mCreateRowCustomerData(objOrder:objCustomer:DefaultAddressObj).
                /*aoFixedPOCustomer:mCreateHolderData(loAttributes:GetJsonObject("fixed_line_holder")).*/
            END.            
        END.
        
        IF loAttributes:Has("mobile_line_holder") THEN
        DO:
            
            ASSIGN 
                aoMobilePOCustomer         = NEW om.OrderCustomer(objOrder)
                aoMobilePOCustomer:RowType = {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER}.
            
            IF objOrder:objCustomer:isNewCustomer  
            THEN DO:
                aoMobilePOCustomer:mCreateRowCustomerData(objOrder:objCustomer:DefaultAddressObj).
                /*aoMobilePOCustomer:mCreateHolderData(loAttributes:GetJsonObject("mobile_line_holder")).*/
            END.
            
        END.
        
        ASSIGN objOrder:statusCode = {&ORDER_STATUS_PENDING_FIXED_LINE}.
        
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        FOR EACH ttOrderProductParam:
            CAST(ttOrderProductParam.opobject, pm.OrderProductParam):mStoreData().
        END.        
        
        IF VALID-OBJECT(objAggrementCustomer)
        THEN objAggrementCustomer:mStoreData().
        
        IF VALID-OBJECT(aoMobilePOCustomer)
        THEN aoMobilePOCustomer:mStoreData().
        
        IF VALID-OBJECT(aoFixedPOCustomer)
        THEN aoFixedPOCustomer:mStoreData().
        
        
    END METHOD.
    
    METHOD PUBLIC VOID mCreateDataOrderProductParamData  (INPUT loAttributes AS CLASS JsonObject ,
                                                          INPUT Attribute    AS CHARACTER,
                                                          INPUT AttrType     AS CHARACTER  ):  
                                                              
        DEFINE VARIABLE liNextParamNum AS INTEGER NO-UNDO.                                                            
       
        objProductParams = NEW pm.OrderProductParam(THIS-OBJECT) .
        objProductParams:mCreateData(loAttributes,Attribute,AttrType).
       
        ASSIGN liNextParamNum = mNextParamNb().
                                            
        CREATE ttOrderProductParam.
        ASSIGN 
            ttOrderProductParam.opobject     = objProductParams
            ttOrderProductParam.paramnb      = liNextParamNum. 
                  
    END METHOD.
    
    METHOD PUBLIC INTEGER mNextParamNb():

        FIND LAST ttOrderProductParam USE-INDEX paramnb NO-ERROR.
               
        IF NOT AVAILABLE ttOrderProductParam THEN RETURN 1.
        
        RETURN ( ttOrderProductParam.paramnb + 1 ).

    END METHOD.   

END CLASS.