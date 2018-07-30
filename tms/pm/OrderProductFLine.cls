 
/*------------------------------------------------------------------------
   File        : OrderProductFLine
   Purpose     : 
   Syntax      : 
   Description : This will store the product data for the Fixed Line for the Convergent order
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
	
	DEFINE PUBLIC PROPERTY FixNumberType AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY FixedNumber AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY MobileNumberType AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY MobileNumber AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY aoSubscriptionParams AS CLASS pm.OrderProductParam NO-UNDO
        GET.
        SET.       
        
    DEFINE PUBLIC PROPERTY aoAggrementCustomer AS CLASS om.OrderCustomer NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY aoFixedPOCustomer AS CLASS om.OrderCustomer NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY aoMobilePOCustomer AS CLASS om.OrderCustomer NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY EstimatedSpeed AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY SerialNumber AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY FixedMNPTime AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY FixedCurrOper AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY FixedCurrOperCode AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY FixedLineCustomerType AS CHARACTER NO-UNDO
       GET.
       SET.
       
	CONSTRUCTOR PUBLIC OrderProductFLine ( INPUT ioOrder AS CLASS om.Order  ):
	    
		SUPER (ioOrder).
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProductFLine ( ):
	    
	    ASSIGN aoAggrementCustomer     =   ?
               aoFixedPOCustomer       =   ?
               aoMobilePOCustomer      =   ?
               aoSubscriptionParams    =   ?.

	END DESTRUCTOR.
	
    METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( ioItemObject AS CLASS JsonObject ):
        
        DEFINE VARIABLE lcCLIError     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE liCustNum      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lcMobSubFixNum AS CHARACTER NO-UNDO.
        
        
        IF Func.ValidateOrder:misCLITypeExists(THIS-OBJECT:ProductID) = NO 
        THEN UNDO , THROW NEW ParamError (SUBSTITUTE("Unknown Product ID : &1 ", THIS-OBJECT:ProductID)).
            
        IF Func.ValidateOrder:mIsConvergentORFixedOnly(THIS-OBJECT:ProductID) = FALSE 
        THEN UNDO,THROW NEW ParamError (SUBSTITUTE("Product &1 is not convergent or fixed only.", THIS-OBJECT:ProductID)). 
        
        IF Func.ValidateOrder:mIsFixedOnlyTariff(THIS-OBJECT:ProductID)
        THEN ASSIGN aoOrder:isFixedOnly  =  YES.
        
        DEFINE VARIABLE loAttributes AS CLASS JsonObject NO-UNDO.
        
        ASSIGN 
            loAttributes                 = NEW JsonObject()   
            loAttributes                 = ioItemObject:GetJsonObject("attributes")
            THIS-OBJECT:FixNumberType    = loAttributes:GetCharacter("fixedline_number_type")
            THIS-OBJECT:FixedNumber      = loAttributes:GetCharacter("fixedline_number").
            
        ASSIGN 
            THIS-OBJECT:MobileNumberType = loAttributes:GetCharacter("mobile_number_type") WHEN loAttributes:Has("mobile_number_type")
            THIS-OBJECT:MobileNumber     = loAttributes:GetCharacter("mobile_number")      WHEN loAttributes:Has("mobile_number")
            aoOrder:MobileNumberType     = THIS-OBJECT:MobileNumberType
            aoOrder:lcCLI                = THIS-OBJECT:MobileNumber WHEN THIS-OBJECT:MobileNumber > ""
            aoOrder:CLIType              = THIS-OBJECT:ProductID.
            
        IF LOOKUP(THIS-OBJECT:FixNumberType,"new,mnp,renewal,stc") = 0 
        THEN UNDO,THROW NEW ParamError (SUBSTITUTE("Unknown fixedline_number_type: ", THIS-OBJECT:FixNumberType)).   
        
        IF Func.ValidateOrder:misFilledFixedNumber(THIS-OBJECT:FixedNumber) 
        THEN UNDO,THROW NEW ParamError (SUBSTITUTE("Fixed number &1 already exists. ", THIS-OBJECT:FixedNumber)). 
        
        IF loAttributes:Has("mobile_number_type")
        AND LOOKUP(THIS-OBJECT:MobileNumberType,"new,mnp,renewal,stc") = 0 
        THEN UNDO , THROW NEW ParamError (SUBSTITUTE("Unknown mobile_number_type: ", THIS-OBJECT:MobileNumberType)).
        
        IF THIS-OBJECT:MobileNumber > "" 
        THEN DO:
            ASSIGN 
                lcCLIError = Func.ValidateOrder:mCheckMSISDN(THIS-OBJECT:MobileNumber,THIS-OBJECT:MobileNumberType).
            IF lcCLIError > ""    
                THEN UNDO , THROW NEW ParamError(lcCLIError).
        END.
        
        IF THIS-OBJECT:MobileNumber > "" AND
           LOOKUP(THIS-OBJECT:MobileNumberType,"renewal,stc") > 0
        THEN DO:
            
            IF Func.ValidateOrder:misSubscriptionExists(INPUT THIS-OBJECT:MobileNumber) = FALSE 
            THEN UNDO , THROW NEW ParamError(SUBSTITUTE("Mobsub with msisdn &1 not found", THIS-OBJECT:MobileNumber)).
            
            ASSIGN liCustNum = Func.ValidateOrder:mGetMobSubCustNum(INPUT THIS-OBJECT:MobileNumber).
            
            IF  liCustNum = 0
            THEN UNDO , THROW NEW ParamError("Customer not found for the renewal/stc order.").
            
            IF Func.ValidateOrder:misCustomerExists(INPUT aoOrder:aoCustomer:CustId , 
                                                    INPUT aoOrder:aoCustomer:CustIdType ,
                                                    INPUT liCustNum) = FALSE 
            THEN UNDO , THROW NEW ParamError("Customer not found with matching id and id-type.").
            
            ASSIGN aoOrder:isRenewalSTCOrder  = TRUE 
                   aoOrder:MsSeq              = Func.ValidateOrder:mGetMsSeq(THIS-OBJECT:MobileNumber).
            
        END.
        
        IF LOOKUP(THIS-OBJECT:FixNumberType,"renewal,stc") > 0 AND
           THIS-OBJECT:FixedNumber  > ""                       AND
           THIS-OBJECT:MobileNumber > ""                          /*This needs to be non-zero*/
        THEN DO:
           /*Mobsub check is already above misSubscriptionExists*/ 
                   
            ASSIGN 
                lcMobSubFixNum = Func.ValidateOrder:mGetSubscriptionFixedNumber(INPUT THIS-OBJECT:MobileNumber).
                
            IF lcMobSubFixNum   EQ  "" OR 
               lcMobSubFixNum   EQ  ?  OR
               lcMobSubFixNum   NE  THIS-OBJECT:FixedNumber
            THEN UNDO , THROW NEW ParamError(SUBSTITUTE("Mobsub with Fixed Number &1 not found", THIS-OBJECT:FixedNumber)).    
            
        END.
        
        IF THIS-OBJECT:MobileNumber > "" AND  
           CAN-FIND(FIRST MobSub WHERE MobSub.Brand         =   Syst.Var:gcBrand AND 
                                       MobSub.FixedNumber   =   THIS-OBJECT:FixedNumber AND 
                                       MobSub.CLI          <>   THIS-OBJECT:MobileNumber)
        THEN UNDO , THROW NEW ParamError("Subscription already exists with Fixed Number " + THIS-OBJECT:FixedNumber).
         
        IF Func.ValidateOrder:mCheckOngoingFixedOrders(INPUT THIS-OBJECT:FixedNumber ,
                                                  INPUT THIS-OBJECT:FixNumberType)
        THEN UNDO , THROW NEW ParamError (SUBSTITUTE("Ongoing order for number &1 " , THIS-OBJECT:FixedNumber)).
        
        IF NOT aoOrder:OrderChannel BEGINS "FUSION" 
        THEN UNDO , THROW NEW ParamError (SUBST("Incorrect fusion order channel &1",aoOrder:OrderChannel)).  
        
        IF Func.ValidateOrder:misDuplicateContractExists(INPUT aoOrder:ContractId ,
                                                         INPUT THIS-OBJECT:MobileNumber )
        THEN UNDO , THROW NEW ParamError(SUBSTITUTE("Duplicate contract ID &1", aoOrder:ContractId)). 
        
        IF THIS-OBJECT:FixNumberType = {&FUSION_FIXED_NUMBER_TYPE_MNP} AND
           THIS-OBJECT:FixedNumber   = ""
        THEN UNDO , THROW NEW ParamError("fixed_line_number is mandatory with fixed_line_number_type=MNP").
        
        IF THIS-OBJECT:FixNumberType = {&FUSION_FIXED_NUMBER_TYPE_MNP} AND
           loAttributes:GetCharacter("fixedline_old_operator_code") = ""
        THEN UNDO , THROW NEW ParamError("fixed_line_mnp_old_operator_code is mandatory with fixed_line_number_type=MNP").
        
        IF THIS-OBJECT:MobileNumber > "" AND 
           Func.ValidateOrder:mIsMigrationCLI(INPUT THIS-OBJECT:MobileNumber)
        THEN UNDO , THROW  NEW ParamError("Requested number is in migration"). 
        
        IF THIS-OBJECT:FixNumberType       =  {&FUSION_FIXED_NUMBER_TYPE_MNP}
        THEN ASSIGN aoOrder:OrderType      =  {&ORDER_TYPE_MNP}.
        ELSE IF THIS-OBJECT:FixNumberType  =  {&FUSION_FIXED_NUMBER_TYPE_NEW}
        THEN ASSIGN aoOrder:OrderType      =  {&ORDER_TYPE_NEW}.  
        
        mCreateDataOrderProductParamData(loAttributes,"fixedline_number_type","Char").
        mCreateDataOrderProductParamData(loAttributes,"fixedline_number","Char").
        mCreateDataOrderProductParamData(loAttributes,"estimated_speed","Char").
        mCreateDataOrderProductParamData(loAttributes,"serial_number","Char").
        
        IF THIS-OBJECT:MobileNumberType > "" 
        THEN mCreateDataOrderProductParamData(loAttributes,"mobile_number_type","Char").
        
        IF THIS-OBJECT:MobileNumber > ""
        THEN mCreateDataOrderProductParamData(loAttributes,"mobile_number","Char").  
                 
        IF loAttributes:Has("fixedline_old_operator_code")
        THEN DO:
            ASSIGN THIS-OBJECT:FixedCurrOperCode = loAttributes:GetCharacter("fixedline_old_operator_code").
            mCreateDataOrderProductParamData(loAttributes,"fixedline_old_operator_code","Char").
        END. 
        
        IF loAttributes:Has("fixedline_old_operator_name")
        THEN DO:
            ASSIGN THIS-OBJECT:FixedCurrOper    =  loAttributes:GetCharacter("fixedline_old_operator_name").
            mCreateDataOrderProductParamData(loAttributes,"fixedline_old_operator_name","Char").
        END.
        
        IF loAttributes:Has("mobile_old_operator_code")
        THEN mCreateDataOrderProductParamData(loAttributes,"mobile_old_operator_code","Char").
        
        IF loAttributes:Has("mobile_old_operator_name")
        THEN mCreateDataOrderProductParamData(loAttributes,"mobile_old_operator_name","Char").
        
        IF loAttributes:Has("mnptime")
        THEN DO:
            THIS-OBJECT:FixedMNPTime    =   loAttributes:GetCharacter("mnptime").
            mCreateDataOrderProductParamData(loAttributes,"mnptime","Char").
        END.
        
        
        ASSIGN aoAggrementCustomer           =  NEW om.OrderCustomer(aoOrder)
               aoAggrementCustomer:RowType   =  {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}.
               
        /*TO-DO PC : do we need to send the fix number type or mobile number type as input parameter*/
               
        aoOrder:aoCustomer:mCheckSubscriptionLimit(INPUT THIS-OBJECT:FixNumberType).
               
        IF aoOrder:aoCustomer:isNewCustomer THEN 
        aoAggrementCustomer:mCreateRowCustomerData(aoOrder:aoCustomer:DefaultAddressObj). 
        
        IF loAttributes:Has("fixed_line_holder")
        THEN DO:
            
            ASSIGN aoFixedPOCustomer         =  NEW om.OrderCustomer(aoOrder)
                   aoFixedPOCustomer:RowType =  {&ORDERCUSTOMER_ROWTYPE_FIXED_POUSER}.
            
            IF aoOrder:aoCustomer:isNewCustomer  
            THEN DO:
                aoFixedPOCustomer:mCreateRowCustomerData(aoOrder:aoCustomer:DefaultAddressObj).
                /*aoFixedPOCustomer:mCreateHolderData(loAttributes:GetJsonObject("fixed_line_holder")).*/
            END.
            
        END.
        
        IF loAttributes:Has("mobile_line_holder")
        THEN DO:
            
            ASSIGN 
                aoMobilePOCustomer         = NEW om.OrderCustomer(aoOrder)
                aoMobilePOCustomer:RowType = {&ORDERCUSTOMER_ROWTYPE_MOBILE_POUSER}.
            
            IF aoOrder:aoCustomer:isNewCustomer  
            THEN DO:
                aoMobilePOCustomer:mCreateRowCustomerData(aoOrder:aoCustomer:DefaultAddressObj).
                /*aoMobilePOCustomer:mCreateHolderData(loAttributes:GetJsonObject("mobile_line_holder")).*/
            END.
            
        END.
        
        ASSIGN aoOrder:StatusCode = {&ORDER_STATUS_PENDING_FIXED_LINE}.
        
    END METHOD.
    
    METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
        
        FOR EACH ttOrderProductParam:
            CAST(ttOrderProductParam.opobject, pm.OrderProductParam):mStoreData().
        END.        
        
        IF VALID-OBJECT(aoAggrementCustomer)
        THEN aoAggrementCustomer:mStoreData().
        
        IF VALID-OBJECT(aoMobilePOCustomer)
        THEN aoMobilePOCustomer:mStoreData().
        
        IF VALID-OBJECT(aoFixedPOCustomer)
        THEN aoFixedPOCustomer:mStoreData().
        
        
    END METHOD.
    
    METHOD PUBLIC VOID mCreateDataOrderProductParamData  (INPUT loAttributes AS CLASS JsonObject ,
                                                          INPUT Attribute    AS CHARACTER,
                                                          INPUT AttrType     AS CHARACTER  ):  
                                                              
        DEFINE VARIABLE liNextParamNum AS INTEGER NO-UNDO.                                                            
       
        aoSubscriptionParams = NEW pm.OrderProductParam(THIS-OBJECT) .
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