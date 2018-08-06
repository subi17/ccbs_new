 
 /*------------------------------------------------------------------------
    File        : OrderInspection
    Purpose     : To Store the Inspection related data to the order
    Syntax      : 
    Description : 
    Author(s)   : Koundinya Maddali
    Created     : Thu May 31 12:19:06 IST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS om.OrderInspection IMPLEMENTS bss.cls.IObjectStorage: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
	
	DEFINE PUBLIC PROPERTY roiResult AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY roiDescription AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY roiLevel AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY roiRuleId AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY roiRiskCode AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY roiDeliverySecure AS INTEGER  NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY roiOrderStatus AS CHARACTER NO-UNDO
       GET.
       SET.
	
    DEFINE PROTECTED VARIABLE objOrder AS CLASS om.Order NO-UNDO.
		
	CONSTRUCTOR PUBLIC OrderInspection ( iobjOrder AS CLASS om.Order  ):
		
		ASSIGN objOrder = iobjOrder. 
		
	END CONSTRUCTOR.
	
    METHOD PUBLIC VOID mCreateData (ioPaymentObject AS CLASS JsonObject) :   
        
        ASSIGN 
            THIS-OBJECT:roiResult          =  ioPaymentObject:GetCharacter("result")          WHEN ioPaymentObject:Has("result")
            THIS-OBJECT:roiDescription     =  ioPaymentObject:GetCharacter("description")     WHEN ioPaymentObject:Has("description")
            THIS-OBJECT:roiLevel           =  ioPaymentObject:GetCharacter("level")           WHEN ioPaymentObject:Has("level")
            THIS-OBJECT:roiRuleId          =  ioPaymentObject:GetCharacter("rule_id")         WHEN ioPaymentObject:Has("rule_id")
            THIS-OBJECT:roiRiskCode        =  ioPaymentObject:GetCharacter("risk_code")       WHEN ioPaymentObject:Has("risk_code")
            THIS-OBJECT:roiDeliverySecure  =  ioPaymentObject:GetInteger("delivery_secure")   WHEN ioPaymentObject:Has("delivery_secure")
            objOrder:roiClose              =  (THIS-OBJECT:roiResult EQ "risk" AND LOOKUP(THIS-OBJECT:roiLevel,"7,8") > 0)
            .
        
    END METHOD.
    
    METHOD PUBLIC VOID mStoreData() :
        
        FIND FIRST Order WHERE Order.Brand   = Syst.Var:gcBrand 
                           AND Order.OrderID = objOrder:OrderId EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                           
       IF LOCKED(Order) THEN  
           UNDO, THROW NEW InternalError("Order: " + STRING(Order.OrderID) + " is locked in OrderInspection.").
                           
        IF NOT AVAILABLE Order THEN 
            UNDO, THROW NEW InternalError("Order: " + STRING(Order.OrderID) + " is not available in OrderInspection.").
         
        mValidateOrderInspectionRules(Order.OrderID).      
        ASSIGN objOrder:roiOrderStatus     =  THIS-OBJECT:roiOrderStatus.
        
        ASSIGN 
            Order.ROIResult      =   THIS-OBJECT:roiResult
            Order.roiLevel       =   INTEGER(THIS-OBJECT:roiLevel)  
            Order.RiskCode       =   THIS-OBJECT:roiRiskCode 
            Order.DeliverySecure =   THIS-OBJECT:roiDeliverySecure
            .
           
    END METHOD.
    
    METHOD PUBLIC VOID mValidateOrderInspectionRules(INPUT iOrderID AS INTEGER ) :
        
        IF THIS-OBJECT:roiResult = "risk"                 AND 
           LOOKUP(THIS-OBJECT:roiLevel,"1,2,3,4,7,8") = 0 
        THEN UNDO, THROW NEW ParamError(SUBST("Unsupported Real Time Inspection Risk Level: &1", THIS-OBJECT:roiLevel)).
             
        CASE THIS-OBJECT:roiResult:
            WHEN "risk" THEN 
            DO:
                IF LOOKUP(THIS-OBJECT:roiLevel,"7,8") > 0 THEN 
                DO:
                    ASSIGN
                        THIS-OBJECT:roiOrderStatus = THIS-OBJECT:roiLevel
                        Order.SendToROI  = {&ROI_HISTORY_TO_SEND} WHEN Order.Ordertype NE {&ORDER_TYPE_STC}.
                        mOrderStamp(iOrderID,"Close",0.0). 
                END.
                ELSE THIS-OBJECT:roiOrderStatus = STRING(40 + INTEGER(THIS-OBJECT:roiLevel)).
            
                IF THIS-OBJECT:roiRuleId NE ''
                THEN Func.Common:mWriteMemoWithType("Order",
                                                    STRING(iOrderID),
                                                    0,
                                                    "ROI Risk Rule_Id",
                                                    THIS-OBJECT:roiRuleId,
                                                    "",
                                                    "BSS RPC ROI").
                IF THIS-OBJECT:roiDescription NE ''
                THEN Func.Common:mWriteMemoWithType("Order",
                                                    STRING(iOrderID),
                                                    0,
                                                    "ROI Description",
                                                    THIS-OBJECT:roiDescription,
                                                    "",
                                                    "BSS RPC ROI").

            END.
            WHEN "exception"
            THEN Func.Common:mWriteMemoWithType("Order",
                                                STRING(iOrderID),
                                                0,
                                                "ROI Description",
                                                THIS-OBJECT:roiDescription,
                                                "",     
                                                "BSS RPC ROI").
            WHEN "unexpected response"
            THEN Func.Common:mWriteMemoWithType("Order",
                                                STRING(iOrderID),
                                                0,
                                                "ROI Unexpected Response",
                                                THIS-OBJECT:roiDescription,
                                                "",
                                                "BSS RPC ROI").
            WHEN "busy" OR 
            WHEN "concern" OR
            WHEN "ParamsException" OR 
            WHEN "inspectionException" THEN 
            DO:
                THIS-OBJECT:roiOrderStatus = {&ORDER_STATUS_ROI_LEVEL_3}.
                IF THIS-OBJECT:roiDescription NE ''
                THEN Func.Common:mWriteMemoWithType("Order",
                                                    STRING(iOrderID),
                                                    0,
                                                    "ROI Description",
                                                    THIS-OBJECT:roiDescription,
                                                    "",
                                                    "BSS RPC ROI").
            END.
        END. /*END CASE*/
    END METHOD.
    
    METHOD PUBLIC LOGICAL mOrderStamp( INPUT iiOrderID   AS INTEGER,
                                       INPUT icStampType AS CHARACTER,
                                       INPUT idStamp     AS DECIMAL) :
        
        DEF BUFFER bfOrderTimeStamp FOR OrderTimeStamp.

        DEF VAR liStampType AS INT NO-UNDO.
       
        liStampType = mOrderStampType(icStampType).
   
        IF liStampType = 0 THEN RETURN FALSE.
   
        FIND FIRST bfOrderTimeStamp WHERE
                   bfOrderTimeStamp.Brand   = Syst.Var:gcBrand   AND
                   bfOrderTimeStamp.OrderID = iiOrderID AND
                   bfOrderTimeStamp.RowType = liStampType EXCLUSIVE-LOCK NO-ERROR.
              
        IF NOT AVAILABLE bfOrderTimeStamp THEN 
        DO:
            CREATE bfOrderTimeStamp.
            ASSIGN 
                bfOrderTimeStamp.Brand   = Syst.Var:gcBrand
                bfOrderTimeStamp.OrderID = iiOrderID 
                bfOrderTimeStamp.RowType = liStampType.
        END. 

        /* use current time if not given */ 
        IF idStamp = 0 THEN idStamp = Func.Common:mMakeTS().
   
        bfOrderTimeStamp.TimeStamp = idStamp.    

        RELEASE bfOrderTimeStamp.
        
    END METHOD.
    
    METHOD PUBLIC INTEGER mOrderStampType (INPUT icStampType AS CHARACTER) :
        
        CASE icStampType:

            WHEN "Change" OR
            WHEN "Ch"     OR
            WHEN "1"  THEN 
                RETURN {&ORDERTIMESTAMP_CHANGE}.
   
            WHEN "Delivery" OR
            WHEN "Del"      OR
            WHEN "2"  THEN 
                RETURN {&ORDERTIMESTAMP_DELIVERY}.

            WHEN "Close" OR
            WHEN "Cl"    OR
            WHEN "3"  THEN 
                RETURN {&ORDERTIMESTAMP_CLOSE}.

            WHEN "Print" OR
            WHEN "Pr"    OR
            WHEN "4"  THEN 
                RETURN {&ORDERTIMESTAMP_PRINT}.
   
            WHEN "SimOnly" OR
            WHEN "Si"    OR
            WHEN "5"  THEN 
                RETURN {&ORDERTIMESTAMP_SIMONLY}.

            WHEN "SendToLogistics" OR
            WHEN "Se"    OR
            WHEN "6"  THEN 
                RETURN {&ORDERTIMESTAMP_SEND}.

            OTHERWISE 
            RETURN 0.
   
        END CASE. 
        
    END METHOD. 
    
    DESTRUCTOR PUBLIC OrderInspection ( ):
    
        IF VALID-OBJECT (objOrder) THEN 
            DELETE OBJECT objOrder.

    END DESTRUCTOR.

END CLASS.