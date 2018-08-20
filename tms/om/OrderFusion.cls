 
/*------------------------------------------------------------------------
   File        : OrderFusion
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

CLASS om.OrderFusion:
    
    DEFINE PUBLIC PROPERTY objOrderProduct AS CLASS pm.OrderProduct NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY fixNumberType AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY fixedNumber AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY estimateSpeed AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY serialNumber AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY fixedCurrOper AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY fixedCurrOperCode AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY customerType AS CHARACTER NO-UNDO
        GET.
        SET.        
            
    DEFINE PUBLIC PROPERTY objFusionMessage AS CLASS om.FusionMessage NO-UNDO 
        GET.
        SET.        
        
    DEFINE PUBLIC PROPERTY adslLinkState AS CHARACTER NO-UNDO
       GET.
       SET.
            
    CONSTRUCTOR PUBLIC OrderFusion(INPUT iobjOrderProduct AS CLASS pm.OrderProduct):
        
        ASSIGN 
            objOrderProduct = iobjOrderProduct.
        
    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID mCreateData(INPUT iObjAttributeStruct AS CLASS JsonObject):
        
        ASSIGN 
            THIS-OBJECT:fixNumberType = iObjAttributeStruct:GetCharacter("fixedline_number_type")
            THIS-OBJECT:fixedNumber   = iObjAttributeStruct:GetCharacter("fixedline_number")
            THIS-OBJECT:estimateSpeed = Func.Common:mConvertSpeedtoBits(iObjAttributeStruct:GetCharacter("estimated_speed"))
            THIS-OBJECT:serialNumber  = iObjAttributeStruct:GetCharacter("serial_number")
            THIS-OBJECT:customerType  = iObjAttributeStruct:GetCharacter("broadband_old_product_type") WHEN iObjAttributeStruct:Has("broadband_old_product_type")
            THIS-OBJECT:customerType  = (IF THIS-OBJECT:customerType = "" THEN "NONE" ELSE THIS-OBJECT:customerType)
            objFusionMessage          = NEW om.FusionMessage(objOrderProduct).

        IF (THIS-OBJECT:customerType EQ "NONE" OR THIS-OBJECT:customerType EQ "ADSL") AND 
            THIS-OBJECT:CustomerType = "MNP" THEN 
            ASSIGN 
                THIS-OBJECT:adslLinkState = "O".
        ELSE 
            ASSIGN 
                THIS-OBJECT:adslLinkState = "V".

        objFusionMessage:mCreateData({&FUSIONMESSAGE_TYPE_CREATE_ORDER}).
        
    END METHOD.
    
    METHOD PUBLIC VOID mStoreData():
        
        DEFINE VARIABLE ldaOrderDate AS DATE NO-UNDO.
        DEFINE VARIABLE liOrderTime AS INTEGER NO-UNDO.

        Func.Common:mSplitTS(objOrderProduct:objOrder:createdStamp,OUTPUT ldaOrderDate,OUTPUT liOrderTime).
        
        CREATE OrderFusion.
        ASSIGN
            OrderFusion.Brand              = Syst.Var:gcBrand
            OrderFusion.OrderId            = objOrderProduct:objOrder:orderId
            OrderFusion.OrderProductId     = objOrderProduct:orderProductID
            OrderFusion.MsSeq              = objOrderProduct:objOrder:finalMsSeq
            OrderFusion.FusionStatus       = {&FUSION_ORDER_STATUS_NEW} WHEN NOT objOrderProduct:objOrder:roiClose
            OrderFusion.OrderDate          = ldaOrderDate
            OrderFusion.SalesMan           = objOrderProduct:objOrder:salesMan
            OrderFusion.FixedNumberType    = THIS-OBJECT:fixNumberType
            OrderFusion.FixedNumber        = THIS-OBJECT:fixedNumber
            OrderFusion.Product            = objOrderProduct:productId
            OrderFusion.FixedCurrOper      = THIS-OBJECT:fixedCurrOper
            OrderFusion.UpdateTS           = Func.Common:mMakeTS() 
            OrderFusion.FixedCurrOperCode  = THIS-OBJECT:fixedCurrOperCode
            OrderFusion.SerialNumber       = THIS-OBJECT:serialNumber
            OrderFusion.EstimatedDataSpeed = THIS-OBJECT:estimateSpeed
            OrderFusion.CustomerType       = THIS-OBJECT:customerType
            OrderFusion.ADSLLinkState      = THIS-OBJECT:adslLinkState.
                         
         /*Reserve number fusion message will be handled by digital hub from now*/
                
         IF objOrderProduct:objOrder:statusCode = {&ORDER_STATUS_PENDING_FIXED_LINE} THEN 
         DO:             
             IF CAN-FIND(FIRST FusionMessage NO-LOCK WHERE
                               FusionMessage.OrderID       = objOrderProduct:objOrder:orderId AND
                               FusionMessage.MessageType   = {&FUSIONMESSAGE_TYPE_CREATE_ORDER} AND
                               FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_NEW}) THEN 
             
                 UNDO,THROW NEW ParamError("Fusion message ongoing.").
                 
             IF VALID-OBJECT (objFusionMessage) THEN 
                 objFusionMessage:mStoreData().     
         END.
                
    END METHOD.
    
    DESTRUCTOR PUBLIC OrderFusion():
        
        IF VALID-OBJECT(objOrderProduct) THEN 
            DELETE OBJECT objOrderProduct.
            
        IF VALID-OBJECT(objFusionMessage) THEN 
            DELETE OBJECT objFusionMessage.
        
    END DESTRUCTOR.

END CLASS.