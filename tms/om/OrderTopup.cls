/*------------------------------------------------------------------------
    File        : OrderTopup
    Purpose     : 
    Syntax      : 
    Description : This Will store the OrderTopUp details
    Author(s)   : Koundinya Maddali
    Created     : Wed Jun 27 12:16:29 IST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.ParamError.
USING profcgi.RPC.JSON.InternalError.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.JsonObject.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS om.OrderTopup: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE PUBLIC PROPERTY  Amount AS DECIMAL NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY  DispAmount AS DECIMAL NO-UNDO
        GET.
        SET. 
        
    DEFINE PUBLIC PROPERTY TopupType AS INTEGER NO-UNDO
        GET.
        SET.  
       
    DEFINE PUBLIC PROPERTY objOrder AS CLASS om.Order  NO-UNDO 
        GET.
        SET.
       
    CONSTRUCTOR PUBLIC OrderTopup ( iobjOrder AS CLASS om.Order ):        
        ASSIGN 
            objOrder = iobjOrder.        
    END CONSTRUCTOR.

    DESTRUCTOR PUBLIC OrderTopup ( ):        
       IF VALID-OBJECT(objOrder) THEN 
           DELETE OBJECT objOrder.    
    END DESTRUCTOR.
    
    METHOD PUBLIC VOID mCreateData(INPUT iiAmount       AS DECIMAL,
                                   INPUT iiDispAmount   AS DECIMAL,
                                   INPUT iiTopupType    AS INTEGER) :
        
        ASSIGN 
            THIS-OBJECT:Amount     = iiAmount
            THIS-OBJECT:DispAmount = iiDispAmount
            THIS-OBJECT:TopupType  = {&INITIAL_TOPUP}.
        
    END METHOD.
    
    METHOD PUBLIC VOID mStoreData(INPUT iiOrderProductID AS INTEGER) :
        
        CREATE OrderTopUp.
        ASSIGN 
            OrderTopUp.Brand          = Syst.Var:gcBrand
            OrderTopUp.OrderID        = objOrder:OrderId
            OrderTopUp.OrderProductID = iiOrderProductID
            OrderTopUp.Amount         = THIS-OBJECT:Amount
            OrderTopUp.DisplayAmount  = THIS-OBJECT:DispAmount
            OrderTopUp.TopupType      = THIS-OBJECT:TopupType
            .
                    
    END METHOD.    

END CLASS.