 
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

CLASS om.FusionMessage :
    
    DEFINE PUBLIC PROPERTY objOrderProduct AS CLASS pm.OrderProduct NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY prefix AS CHARACTER NO-UNDO
        GET.
        SET.    
       
    DEFINE PUBLIC PROPERTY orderType AS CHARACTER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY messageType AS CHARACTER NO-UNDO
       GET.
       SET.                 
    CONSTRUCTOR PUBLIC FusionMessage(INPUT iobjOrderProduct AS CLASS pm.OrderProduct):
        
        ASSIGN 
            objOrderProduct = iobjOrderProduct.
        
    END CONSTRUCTOR.

    METHOD PUBLIC VOID mCreateData(INPUT icMessageType AS CHARACTER):
        
        ASSIGN 
            THIS-OBJECT:messageType = icMessageType. 
       
        mGetOrderType().
        
    END METHOD.
    
    METHOD PUBLIC VOID mStoreData():
        
        CREATE FusionMessage.
        ASSIGN
            FusionMessage.MessageSeq    = NEXT-VALUE(FusionMessageSeq)
            FusionMessage.OrderID       = objOrderProduct:objOrder:orderId
            FusionMessage.MsSeq         = objOrderProduct:objOrder:finalMsSeq
            FusionMessage.CreatedTS     = Func.Common:mMakeTS()
            FusionMessage.MessageType   = THIS-OBJECT:messageType
            FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_NEW}
            FusionMessage.Source        = {&FUSIONMESSAGE_SOURCE_TMS}
            FusionMessage.OrderType     = THIS-OBJECT:prefix + " " + THIS-OBJECT:orderType
            FusionMessage.UpdateTS      = FusionMessage.CreatedTS.
        
        
    END METHOD.
    
    METHOD PUBLIC VOID mGetOrderType():
        
        DEFINE BUFFER bfCLIType FOR CLIType.
        
        FIND FIRST bfCLIType WHERE 
                   bfCLIType.CLIType = objOrderProduct:productId NO-LOCK NO-ERROR.
                   
        IF NOT AVAILABLE bfCLIType THEN 
            UNDO , THROW NEW ParamError(SUBSTITUTE("Product &1 not found.",objOrderProduct:productId)).
            
        IF THIS-OBJECT:messageType EQ {&FUSIONMESSAGE_TYPE_CANCEL_ORDER} THEN 
            ASSIGN 
                THIS-OBJECT:prefix = "Cancelación".
        ELSE 
            ASSIGN 
                THIS-OBJECT:prefix = "Alta".
                
        IF bfCLIType.fixedLineType = 1 THEN 
            ASSIGN THIS-OBJECT:orderType = "xDSL".
        ELSE IF bfCLIType.fixedLineType = 2 THEN 
            ASSIGN THIS-OBJECT:orderType = "FTTH".
            
        IF bfCLIType.cliType BEGINS "CONTFHNB" THEN
            ASSIGN THIS-OBJECT:OrderType =  THIS-OBJECT:orderType + " NEBA".
            
        ASSIGN 
            THIS-OBJECT:orderType = THIS-OBJECT:orderType + " VOIP".
                
    END METHOD.
    
    DESTRUCTOR PUBLIC FusionMessage():
        
        IF VALID-OBJECT(objOrderProduct) THEN 
            DELETE OBJECT objOrderProduct.
        
    END DESTRUCTOR.
    
END CLASS.
