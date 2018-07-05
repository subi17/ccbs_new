 
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
       
    DEFINE PUBLIC PROPERTY aoSubscriptionParams AS CLASS bss.cls.OrderProductParam NO-UNDO
        GET.
        SET.       
        
    DEFINE PUBLIC PROPERTY aoAggrementCustomer AS CLASS bss.cls.OrderCustomer NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY aoFixedPOCustomer AS CLASS bss.cls.OrderCustomer NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY aoMobilePOCustomer AS CLASS bss.cls.OrderCustomer NO-UNDO 
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
       
	CONSTRUCTOR PUBLIC OrderProductFLine ( INPUT ioOrder AS CLASS bss.cls.Order  ):
	    
		SUPER (ioOrder).
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProductFLine ( ):
	    
	    ASSIGN aoAggrementCustomer     =   ?
               aoFixedPOCustomer       =   ?
               aoMobilePOCustomer      =   ?
               aoSubscriptionParams    =   ?.

	END DESTRUCTOR.
	
    METHOD PUBLIC OVERRIDE VOID mCreateDataForType ( ioItemObject AS CLASS JsonObject ):
        
        
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
            THIS-OBJECT:MobileNumber     = loAttributes:GetCharacter("mobile_number")      WHEN loAttributes:Has("mobile_number").
            
        IF LOOKUP(THIS-OBJECT:FixNumberType,"new,mnp,renewal,stc") = 0 
        THEN UNDO,THROW NEW ParamError (SUBSTITUTE("Unknown fixedline_number_type: ", THIS-OBJECT:FixNumberType)).   
        
        IF Func.ValidateOrder:misFilledFixedNumber(THIS-OBJECT:FixedNumber) 
        THEN UNDO,THROW NEW ParamError (SUBSTITUTE("Fixed number &1 already exists. ", THIS-OBJECT:FixedNumber)). 
        
        IF THIS-OBJECT:MobileNumber > "" AND Func.ValidateOrder:misFilledCLI(THIS-OBJECT:MobileNumber) 
        THEN UNDO , THROW NEW ParamError (SUBSTITUTE("Mobile Number &1 is already assigned.", THIS-OBJECT:MobileNumber)).
        
        IF THIS-OBJECT:FixNumberType  =  "MNP"
        THEN ASSIGN aoOrder:OrderType  =  {&ORDER_TYPE_MNP}.
        ELSE IF THIS-OBJECT:FixNumberType  =  "NEW"
        THEN ASSIGN aoOrder:OrderType  = {&ORDER_TYPE_NEW}.  
        
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
            THIS-OBJECT:FixedMNPTime    =   loAttributes:GetCharacter(mnptime).
            mCreateDataOrderProductParamData(loAttributes,"mnptime","Char").
        END.
        
        
        ASSIGN aoAggrementCustomer           =  NEW bss.cls.OrderCustomer(aoOrder)
               aoAggrementCustomer:RowType   =  {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}.
               
        IF aoOrder:aoCustomer:isNewCustomer THEN 
        aoAggrementCustomer:mCreateRowCustomerData(aoOrder:aoCustomer:DefaultAddressObj). 
        
        IF loAttributes:Has("fixed_line_holder")
        THEN DO:
            
            ASSIGN aoFixedPOCustomer         =  NEW bss.cls.OrderCustomer(aoOrder)
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
                aoMobilePOCustomer         = NEW bss.cls.OrderCustomer(aoOrder)
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
            CAST(ttOrderProductParam.opobject, bss.cls.OrderProductParam):mStoreData().
        END.        
        
        mstoreOrderFusion().
        
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
    
    METHOD PUBLIC VOID mstoreOrderFusion() :
        
        DEFINE VARIABLE llROIClose AS LOGICAL NO-UNDO. 
        DEFINE VARIABLE ldaOrderDate AS DATE NO-UNDO.
        DEFINE VARIABLE liOrderTime  AS INTEGER  NO-UNDO.
        DEFINE VARIABLE lcFixedLineAdslLinkState AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcFixedLineCustomerType AS CHARACTER NO-UNDO.
        DEFINE VARIABLE ocErrMsg AS CHARACTER NO-UNDO.
        
        
        Func.Common:mSplitTS(Order.CrStamp,OUTPUT ldaOrderDate,OUTPUT liOrderTime).
        
        IF (THIS-OBJECT:FixedLineCustomerType EQ "NONE" OR THIS-OBJECT:FixedLineCustomerType EQ "ADSL")  
            AND THIS-OBJECT:FixedLineCustomerType = "MNP" 
        THEN lcFixedLineAdslLinkState = "O".
        ELSE    lcFixedLineAdslLinkState = "V".
        
        llROIClose =  (aoOrder:ROIResult EQ "risk" AND LOOKUP(aoOrder:ROILevel,"7,8") > 0).
        
        CREATE OrderFusion.
        ASSIGN OrderFusion.Brand              =     Syst.Var:gcBrand
               OrderFusion.OrderID            =     aoOrder:OrderId
               OrderFusion.FusionStatus       =     {&FUSION_FIXED_NUMBER_TYPE_NEW} WHEN NOT llROIClose
               OrderFusion.OrderDate          =     ldaOrderDate
               OrderFusion.Salesman           =     aoOrder:Salesman
               OrderFusion.FixedNumberType    =     THIS-OBJECT:FixNumberType
               OrderFusion.FixedNumber        =     THIS-OBJECT:FixedNumber
               OrderFusion.Product            =     THIS-OBJECT:ProductID
               /*OrderFusion.CustomerType       =     "DECIDE THIS"*/
               OrderFusion.FixedMNPTime       =     THIS-OBJECT:FixedMNPTime
               OrderFusion.FixedCurrOper      =     THIS-OBJECT:FixedCurrOper
               OrderFusion.UpdateTS           =     Func.Common:mMakeTS()
               OrderFusion.FixedCurrOperCode  =     THIS-OBJECT:FixedCurrOperCode
               OrderFusion.SerialNumber       =     THIS-OBJECT:SerialNumber
               OrderFusion.EstimatedDataSpeed =     THIS-OBJECT:EstimatedSpeed
               OrderFusion.ADSLLinkState      =     THIS-OBJECT:lcFixedLineAdslLinkState
               .
               
        IF aoOrder:StatusCode = {&ORDER_STATUS_PENDING_FIXED_LINE}
        THEN DO:
            
            RUN Mc/orderprocessing.p(INPUT OrderFusion.OrderID , 
                                     INPUT {&ORDER_STATUS_PENDING_FIXED_LINE} ,
                                     INPUT OrderFusion.FixedNumber,
                                     OUTPUT ocErrMsg  ).
                                     
        END.
        
    END METHOD.
    

END CLASS.