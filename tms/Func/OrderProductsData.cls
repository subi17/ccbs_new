 
 /*------------------------------------------------------------------------
    File        : OrderProducts
    Purpose     : 
    Syntax      : 
    Description : This function will have the function related to retrieval of the order data
    Author(s)   : Koundinya Maddali
    Created     : Tue Jul 03 15:51:41 IST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS Func.OrderProductsData FINAL: 
    
    METHOD PUBLIC STATIC CHARACTER mGetOrderCLI(INPUT liOrderID AS INTEGER) :
        
        DEFINE BUFFER bfOrderProduct     FOR OrderProduct.
        DEFINE BUFFER bfOrderProdctParam FOR OrderProductParam.
        
        FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID          =  liOrderID
                                    AND bfOrderProduct.ActionType       =  {&ORDER_PRODUCT_FIXED_LINE}
                                  NO-LOCK NO-ERROR.
                                  
        IF NOT AVAILABLE bfOrderProduct
        THEN DO:
            FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID          =  liOrderID
                                        AND bfOrderProduct.ActionType       =  {&ORDER_PRODUCT_SUBSCRIPTION}
                                      NO-LOCK NO-ERROR.
        END.
                                  
        IF AVAILABLE bfOrderProduct 
        THEN DO:
            
            FIND FIRST bfOrderProdctParam  WHERE bfOrderProdctParam.OrderID             =  bfOrderProduct.OrderID
                                             AND bfOrderProdctParam.OrderProductID      =  bfOrderProduct.OrderProductID
                                             AND bfOrderProductParam.ParamName          =  {&ORDER_PRODUCT_PARAM_MOBILE_NUMBER}
                                             NO-LOCK NO-ERROR.
                                             
            IF AVAILABLE bfOrderProdctParam
            THEN RETURN bfOrderProdctParam.CharValue.
            
        END. 
       
        RETURN "".
        
    END METHOD.
    
    METHOD PUBLIC STATIC CHARACTER mGetOrderICC(INPUT liOrderID AS INTEGER) :
        
        DEFINE BUFFER bfOrderProduct     FOR OrderProduct.
        DEFINE BUFFER bfOrderProdctParam FOR OrderProductParam.
        
        FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID          =  liOrderID
                                    AND bfOrderProduct.ActionType       =  {&ORDER_PRODUCT_SIM}
                                  NO-LOCK NO-ERROR.

        IF AVAILABLE bfOrderProduct 
        THEN DO:
            
            FIND FIRST bfOrderProdctParam  WHERE bfOrderProdctParam.OrderID             =  bfOrderProduct.OrderID
                                             AND bfOrderProdctParam.OrderProductID      =  bfOrderProduct.OrderProductID
                                             AND bfOrderProductParam.ParamName          =  {&ORDER_PRODUCT_PARAM_ICC}
                                           NO-LOCK NO-ERROR.
                                             
            IF AVAILABLE bfOrderProdctParam
            THEN RETURN bfOrderProdctParam.CharValue.
            
        END. 
       
        RETURN "".
        
    END METHOD.
    
    METHOD PUBLIC STATIC CHARACTER mGetOrderCLIType(INPUT liOrderID AS INTEGER) :
        
        DEFINE BUFFER bfOrderProduct     FOR OrderProduct.
        
        FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID     =  liOrderID
                                   AND bfOrderProduct.ActionType   =  {&ORDER_PRODUCT_FIXED_LINE}
                                  NO-LOCK NO-ERROR.
                                  
        IF NOT AVAILABLE bfOrderProduct
        THEN DO:
            FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID      =  liOrderID
                                        AND bfOrderProduct.ActionType   =  {&ORDER_PRODUCT_SUBSCRIPTION}
                                      NO-LOCK NO-ERROR.
        END.
        
        IF AVAILABLE bfOrderProduct
        THEN RETURN bfOrderProduct.ProductID.
        
        RETURN "".
        
    END METHOD.
    

END CLASS.