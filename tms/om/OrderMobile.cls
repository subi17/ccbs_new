 /*------------------------------------------------------------------------
   File        : OrderMobile
   Purpose     : 
   Syntax      : 
   Description : This will store the OrderMobile 
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

CLASS om.OrderMobile:
    
    DEFINE PUBLIC PROPERTY objOrderProduct AS CLASS pm.OrderProduct NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY product AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY numberType AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY currOper AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY statusCode AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY createdTs AS DECIMAL NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY updatedTs AS DECIMAL NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY cancellationReason AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY CLI AS CHARACTER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY requestPortingDate AS DATE NO-UNDO
        GET.
        SET. 
       
    DEFINE PUBLIC PROPERTY portingTime AS DECIMAL NO-UNDO
        GET.
        SET. 
    
    DEFINE PUBLIC PROPERTY mnpStatus AS INTEGER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY oldIcc AS CHARACTER NO-UNDO
        GET.
        SET.
    
    DEFINE PUBLIC PROPERTY oldPayType AS LOGICAL NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY payType AS LOGICAL NO-UNDO
        GET.
        SET.
    
    CONSTRUCTOR PUBLIC OrderMobile(INPUT iobjOrderProduct AS CLASS pm.OrderProduct):
        
        ASSIGN objOrderProduct = iobjOrderProduct.
        
    END CONSTRUCTOR.
    
    METHOD PUBLIC VOID mCreateData(INPUT iObjAttributeStruct AS CLASS JsonObject):
        
        DEFINE VARIABLE lcPayType AS CHARACTER NO-UNDO.
        
        ASSIGN 
            THIS-OBJECT:product             = objOrderProduct:productId
            THIS-OBJECT:numberType          = iObjAttributeStruct:GetCharacter("mobile_number_type")                                               
            THIS-OBJECT:currOper            = iObjAttributeStruct:GetCharacter("mobile_old_operator_name") 
                                              WHEN iObjAttributeStruct:Has("mobile_old_operator_name")
            THIS-OBJECT:CLI                 = iObjAttributeStruct:GetCharacter("mobile_number")
            THIS-OBJECT:requestPortingDate  = iObjAttributeStruct:GetDate("porting_date") 
                                              WHEN iObjAttributeStruct:Has("porting_date")
            THIS-OBJECT:portingTime         = iObjAttributeStruct:GetDecimal("mnptime") 
                                              WHEN iObjAttributeStruct:Has("mnptime")  
            THIS-OBJECT:mnpStatus           = INTEGER(THIS-OBJECT:numberType = {&TYPE_MNP})   
            THIS-OBJECT:oldIcc              = iObjAttributeStruct:GetCharacter("mobile_old_icc") 
                                              WHEN iObjAttributeStruct:Has("mobile_old_icc")
            lcPayType                       = iObjAttributeStruct:GetCharacter("mobile_old_product_type") 
                                              WHEN iObjAttributeStruct:Has("mobile_old_product_type")                                 
            THIS-OBJECT:oldPayType          = (lcPayType = "prepaid") 
            .                                                                          
        
    END METHOD.
    
    METHOD PUBLIC VOID mStoreData():
        
        CREATE OrderMobile.
        ASSIGN OrderMobile.OrderId              = objOrderProduct:objOrder:orderId
               OrderMobile.OrderProductId       = objOrderProduct:orderProductID
               OrderMobile.MsSeq                = objOrderProduct:objOrder:finalMsSeq
               OrderMobile.ICC                  = objOrderProduct:objOrder:lcICC
               OrderMobile.Product              = THIS-OBJECT:product
               OrderMobile.NumberType           = THIS-OBJECT:numberType
               OrderMobile.CurrOper             = THIS-OBJECT:currOper
               OrderMobile.StatusCode           = objOrderProduct:objOrder:statusCode
               OrderMobile.CreatedTS            = Func.Common:mMakeTS()
               OrderMobile.UpdatedTS            = OrderMobile.CreatedTs
               OrderMobile.CancellationReason   = THIS-OBJECT:cancellationReason
               OrderMobile.CLI                  = THIS-OBJECT:CLI
               OrderMobile.RequestedPortingDate = THIS-OBJECT:requestPortingDate
               OrderMobile.PortingTime          = THIS-OBJECT:portingTime
               OrderMobile.MnpStatus            = THIS-OBJECT:mnpStatus
               OrderMobile.oldIcc               = THIS-OBJECT:oldIcc
               OrderMobile.oldPayType           = THIS-OBJECT:oldPayType
               OrderMobile.payType              = objOrderProduct:objOrder:payType.
               
        
    END METHOD.
    
    DESTRUCTOR PUBLIC OrderMobile():
        
        IF VALID-OBJECT(objOrderProduct) THEN 
            DELETE OBJECT objOrderProduct.
        
    END DESTRUCTOR.
    
END CLASS.
