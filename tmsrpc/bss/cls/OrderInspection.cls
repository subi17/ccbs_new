 
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

CLASS bss.cls.OrderInspection IMPLEMENTS bss.cls.IObjectStorage: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
	
	DEFINE PUBLIC PROPERTY ROIResult AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY ROIDescription AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY ROILevel AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY ROIRuleID AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY ROIRiskCode AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY ROIDeliverySecure AS INTEGER NO-UNDO
       GET.
       SET.
	
    DEFINE PROTECTED VARIABLE aoOrder AS CLASS bss.cls.Order NO-UNDO.
		
	CONSTRUCTOR PUBLIC OrderInspection ( ioOrder AS CLASS bss.cls.Order  ):
		
		ASSIGN aoOrder = ioOrder. 
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderInspection ( ):
	    
	    ASSIGN aoOrder = ?.

	END DESTRUCTOR.
	
    METHOD PUBLIC VOID mCreateData (ioPaymentObject AS CLASS JsonObject) :   
        
        ASSIGN 
            THIS-OBJECT:ROIResult         =  ioPaymentObject:GetCharacter("result")          WHEN ioPaymentObject:Has("result")
            THIS-OBJECT:ROIDescription    =  ioPaymentObject:GetCharacter("description")     WHEN ioPaymentObject:Has("description")
            THIS-OBJECT:ROILevel          =  ioPaymentObject:GetCharacter("level")           WHEN ioPaymentObject:Has("level")
            THIS-OBJECT:ROIRuleID         =  ioPaymentObject:GetCharacter("rule-id")         WHEN ioPaymentObject:Has("rule-id")
            THIS-OBJECT:ROIRiskCode       =  ioPaymentObject:GetCharacter("risk-code")       WHEN ioPaymentObject:Has("risk-code")
            THIS-OBJECT:ROIDeliverySecure =  ioPaymentObject:GetInteger("delivery-secure")   WHEN ioPaymentObject:Has("delivery-secure")
            .
        
    END METHOD.
    
    METHOD PUBLIC VOID mStoreData() :
        
        FIND FIRST Order WHERE Order.Brand   = Syst.Var:gcBrand 
                           AND Order.OrderID = aoOrder:OrderId EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
                           
       IF LOCKED(Order) THEN 
           UNDO, THROW NEW InternalError("Order: " + STRING(Order.OrderID) + " is locked in OrderInspection.").
                           
        IF NOT AVAILABLE Order THEN 
            UNDO, THROW NEW InternalError("Order: " + STRING(Order.OrderID) + " is not available in OrderInspection.").
         
        mValidateOrderInspectionRules(Order.OrderID).        
        
        ASSIGN 
            Order.ROIResult      =   THIS-OBJECT:ROIResult
            Order.ROILevel       =   INT(THIS-OBJECT:ROILevel)  
            Order.RiskCode       =   THIS-OBJECT:ROIRiskCode 
            Order.DeliverySecure =   THIS-OBJECT:ROIDeliverySecure.
           
    END METHOD.
    
    METHOD PUBLIC VOID mValidateOrderInspectionRules(INPUT iOrderID AS INTEGER ) :
        
        IF THIS-OBJECT:ROIResult = "risk"                 AND 
           LOOKUP(THIS-OBJECT:ROILevel,"1,2,3,4,7,8") = 0 THEN
             UNDO, THROW NEW ParamError(SUBST("Unsupported Real Time Inspection Risk Level: &1", THIS-OBJECT:ROILevel)).
             
        CASE THIS-OBJECT:ROIResult:
            WHEN "risk" THEN 
            DO:
                IF LOOKUP(THIS-OBJECT:ROILevel,"7,8") > 0 THEN 
                DO:
                    ASSIGN
                        Order.StatusCode = THIS-OBJECT:ROILevel
                        Order.SendToROI  = {&ROI_HISTORY_TO_SEND} WHEN Order.Ordertype NE {&ORDER_TYPE_STC}.
                        mOrderStamp(iOrderID,"Close",0.0). 
                END.
                ELSE Order.StatusCode = STRING(40 + INTEGER(THIS-OBJECT:ROILevel)).
            
               IF THIS-OBJECT:ROIRuleID NE ''
               THEN Func.Common:mWriteMemoWithType("Order",
                                                   STRING(iOrderID),
                                                   0,
                                                   "ROI Risk Rule_Id",
                                                   THIS-OBJECT:ROIRuleID,
                                                   "",
                                                   "BSS RPC ROI").
               IF THIS-OBJECT:ROIDescription NE ''
               THEN Func.Common:mWriteMemoWithType("Order",
                                                   STRING(iOrderID),
                                                   0,
                                                   "ROI Description",
                                                   THIS-OBJECT:ROIDescription,
                                                   "",
                                                   "BSS RPC ROI").

            END.
            WHEN "exception"
            THEN Func.Common:mWriteMemoWithType("Order",
                                                STRING(iOrderID),
                                                0,
                                                "ROI Description",
                                                THIS-OBJECT:ROIDescription,
                                                "",     
                                                "BSS RPC ROI").
            WHEN "unexpected response"
            THEN Func.Common:mWriteMemoWithType("Order",
                                                STRING(iOrderID),
                                                0,
                                                "ROI Unexpected Response",
                                                THIS-OBJECT:ROIDescription,
                                                "",
                                                "BSS RPC ROI").
            WHEN "busy" OR 
            WHEN "concern" OR
            WHEN "ParamsException" OR 
            WHEN "inspectionException" THEN 
            DO:
                Order.StatusCode = "43".
                IF THIS-OBJECT:ROIDescription NE ''
                THEN Func.Common:mWriteMemoWithType("Order",
                                                    STRING(iOrderID),
                                                    0,
                                                    "ROI Description",
                                                    THIS-OBJECT:ROIDescription,
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

END CLASS.