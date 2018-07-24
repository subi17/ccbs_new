 
 /*------------------------------------------------------------------------
    File        : OrderProducts
    Purpose     : 
    Syntax      : 
    Description : This function will have the methods related to retrieval of the order data and related products
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
        DEFINE VARIABLE lcParamType  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcParamValue AS CHARACTER NO-UNDO.
        
        FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID          =  liOrderID
                                    AND bfOrderProduct.ActionType       =  {&ORDER_PRODUCT_FIXED_LINE}
                                  NO-LOCK NO-ERROR.
                                  
        IF NOT AVAILABLE bfOrderProduct
        THEN DO:
            FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID          =  liOrderID
                                        AND bfOrderProduct.ActionType       =  {&ORDER_PRODUCT_SUBSCRIPTION}
                                      NO-LOCK NO-ERROR.
        END.
        
        IF NOT AVAILABLE bfOrderProduct THEN RETURN "".
        
        Func.OrderProductsData:mGetOrderProductParamValue(INPUT bfOrderProduct.OrderID ,
                                                          INPUT bfOrderProduct.OrderProductID,
                                                          INPUT {&ORDER_PRODUCT_PARAM_MOBILE_NUMBER},
                                                          OUTPUT lcParamType ,
                                                          OUTPUT lcParamValue).
                                                          
        IF lcParamType = {&ORDER_PRODUCT_PARAM_CHAR_VALUE} /*CLI Is char value in db while order saving*/
        THEN RETURN lcParamValue.
        
        RETURN "".                                                         
                                  
    END METHOD.
    
    METHOD PUBLIC STATIC CHARACTER mGetOrderICC(INPUT liOrderID AS INTEGER) :
        
        DEFINE BUFFER bfOrderProduct     FOR OrderProduct.
        DEFINE BUFFER bfOrderProdctParam FOR OrderProductParam.
        
        DEFINE VARIABLE lcParamType  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcParamValue AS CHARACTER NO-UNDO.
        
        FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID          =  liOrderID
                                    AND bfOrderProduct.ActionType       =  {&ORDER_PRODUCT_SIM}
                                  NO-LOCK NO-ERROR.
                                  
        IF NOT AVAILABLE bfOrderProduct THEN RETURN "".   
        
        Func.OrderProductsData:mGetOrderProductParamValue(INPUT bfOrderProduct.OrderID ,
                                                          INPUT bfOrderProduct.OrderProductID,
                                                          INPUT {&ORDER_PRODUCT_PARAM_ICC},
                                                          OUTPUT lcParamType ,
                                                          OUTPUT lcParamValue).    
                                                          
                                                                                            
        IF lcParamType = {&ORDER_PRODUCT_PARAM_CHAR_VALUE}  /* ICC is charvalue in orderproductparam*/
        THEN RETURN lcParamValue.
        
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
    
    METHOD PUBLIC STATIC CHARACTER mGetFixNumber(INPUT liOrderID AS INTEGER ) :
        
        DEFINE BUFFER bfOrderProduct FOR OrderProduct.
        
        DEFINE VARIABLE lcParamType  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcParamValue AS CHARACTER NO-UNDO.
        
        FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID     =  liOrderID
                                   AND bfOrderProduct.ActionType   =  {&ORDER_PRODUCT_FIXED_LINE}
                                  NO-LOCK NO-ERROR.
                                  
        IF NOT AVAILABLE bfOrderProduct THEN RETURN "".
        
        Func.OrderProductsData:mGetOrderProductParamValue(INPUT bfOrderProduct.OrderID ,
                                                          INPUT bfOrderProduct.OrderProductID,
                                                          INPUT {&ORDER_PRODUCT_PARAM_FIX_NUMBER},
                                                          OUTPUT lcParamType ,
                                                          OUTPUT lcParamValue). 
                                                          
        IF lcParamType = {&ORDER_PRODUCT_PARAM_CHAR_VALUE} THEN /* ICC is charvalue in orderproductparam*/
            RETURN lcParamValue.
        
        RETURN "".                                                              
                                                                                                          
    END METHOD.
    
    METHOD PUBLIC STATIC CHARACTER mGetMobileNumberType(INPUT liOrderID AS INTEGER) :
        
        DEFINE BUFFER bfOrderProduct FOR OrderProduct.
        
        DEFINE VARIABLE lcParamType  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcParamValue AS CHARACTER NO-UNDO.
        
        FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID     =  liOrderID
                                    AND bfOrderProduct.ActionType  =  {&ORDER_PRODUCT_FIXED_LINE}
                                  NO-LOCK NO-ERROR.
                                  
        IF NOT AVAILABLE bfOrderProduct
        THEN DO:
            FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID      =  liOrderID
                                        AND bfOrderProduct.ActionType   =  {&ORDER_PRODUCT_SUBSCRIPTION}
                                      NO-LOCK NO-ERROR.
        END.
        
        IF NOT AVAILABLE bfOrderProduct THEN RETURN "".
        
        Func.OrderProductsData:mGetOrderProductParamValue(INPUT bfOrderProduct.OrderID ,
                                                          INPUT bfOrderProduct.OrderProductID,
                                                          INPUT {&ORDER_PRODUCT_PARAM_MOBILE_NUMBER_TYPE},
                                                          OUTPUT lcParamType ,
                                                          OUTPUT lcParamValue).    
                                                          
                                                                                            
        IF lcParamType = {&ORDER_PRODUCT_PARAM_CHAR_VALUE}  /* ICC is charvalue in orderproductparam*/
        THEN RETURN lcParamValue.
        
        RETURN "".
        
    END METHOD.
    
    METHOD PUBLIC STATIC VOID mGetOrderProductParamValue(INPUT iiOrderID     AS INTEGER , 
                                                         INPUT iiProductID   AS INTEGER ,
                                                         INPUT iiParamName   AS CHARACTER,
                                                         OUTPUT ocParamType  AS CHARACTER,
                                                         OUTPUT ocParamValue AS CHARACTER) :
                                                             
                                                                  
        DEFINE BUFFER bfOrderProductParam FOR OrderProductParam.
        
        FIND FIRST bfOrderProductParam  WHERE bfOrderProductParam.OrderID            =  iiOrderID
                                          AND bfOrderProductParam.OrderProductID     =  iiProductID
                                          AND bfOrderProductParam.ParamName          =  iiParamName
                                       NO-LOCK NO-ERROR.      
                                       
        IF AVAILABLE bfOrderProductParam 
        THEN DO:
            
            ASSIGN ocParamType = bfOrderProductParam.ValueType.
            
            IF ocParamType  = {&ORDER_PRODUCT_PARAM_CHAR_VALUE} THEN
                ASSIGN ocParamValue  =  bfOrderProductParam.CharValue.
            ELSE IF ocParamType  = {&ORDER_PRODUCT_PARAM_DATE_VALUE} THEN
                ASSIGN ocParamValue  = STRING(bfOrderProductParam.CharValue , "99/99/99").
            ELSE IF ocParamType  = {&ORDER_PRODUCT_PARAM_DEC_VALUE} THEN
                ASSIGN ocParamValue  = STRING(bfOrderProductParam.DecValue).
            ELSE IF ocParamType  = {&ORDER_PRODUCT_PARAM_INT_VALUE} THEN
                ASSIGN ocParamValue  = STRING(bfOrderProductParam.IntValue).
            
        END.                            
                                                              
    END METHOD.               
    
    METHOD PUBLIC STATIC CHARACTER mGetCurrentOperator(INPUT iiOrderID AS INTEGER ) :
        
        DEFINE BUFFER bfOrderProduct FOR OrderProduct.
        
        DEFINE VARIABLE lcParamType  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcParamValue AS CHARACTER NO-UNDO.
        
        FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID      =  iiOrderID
                                    AND bfOrderProduct.ActionType   =  {&ORDER_PRODUCT_FIXED_LINE}
                                  NO-LOCK NO-ERROR.
                                  
        IF NOT AVAILABLE bfOrderProduct
        THEN DO:
            
            FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID     =  iiOrderID
                                        AND bfOrderProduct.ActionType  =  {&ORDER_PRODUCT_SUBSCRIPTION}
                                      NO-LOCK NO-ERROR.
                                      
        END.
        
        IF NOT AVAILABLE bfOrderProduct THEN RETURN "".
        
        Func.OrderProductsData:mGetOrderProductParamValue(INPUT bfOrderProduct.OrderID ,
                                                          INPUT bfOrderProduct.OrderProductID,
                                                          INPUT {&ORDER_PRODUCT_PARAM_MOBILE_OLD_OPERATOR},
                                                          OUTPUT lcParamType ,
                                                          OUTPUT lcParamValue).  
        
        IF lcParamType = {&ORDER_PRODUCT_PARAM_CHAR_VALUE} THEN 
            RETURN lcParamValue.
            
        RETURN "".
        
    END METHOD.          
    
    METHOD PUBLIC STATIC CHARACTER mGetOrderMobileICC(INPUT iiOrderID AS INTEGER):
        
        DEFINE BUFFER bfpcOrderMobile FOR OrderMobile.
        
        FIND FIRST bfpcOrderMobile  
            WHERE  bfpcOrderMobile.Brand     = Syst.Var:gcBrand  
              AND  bfpcOrderMobile.OrderID   = iiOrderID NO-LOCK NO-ERROR.
            
        IF NOT AVAILABLE bfpcOrderMobile THEN RETURN "".
        
        RETURN bfpcOrderMobile.ICC.
        
    END METHOD.              
    
    METHOD PUBLIC STATIC LOGICAL mHasInitialTopup(INPUT iiOrderID AS INTEGER) :
        
        DEFINE BUFFER bfOrderProduct        FOR OrderProduct.
        DEFINE BUFFER bfOrderProductParam   FOR OrderProductParam.
        
        FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID     =  iiOrderID
                                    AND bfOrderProduct.ActionType  =  {&ORDER_PRODUCT_SUBSCRIPTION}
                                    NO-LOCK NO-ERROR.
                                    
        IF NOT AVAILABLE bfOrderProduct THEN RETURN NO.
        
        RETURN (CAN-FIND(FIRST bfOrderProductParam WHERE bfOrderProductParam.OrderID            =   bfOrderProduct.OrderID
                                                     AND bfOrderProductParam.OrderProductID     =   bfOrderProduct.OrderProductID
                                                     AND bfOrderProductParam.ParamName          =   {&ORDER_PRODUCT_PARAM_INITIAL_TOPUP})).                                
        
    END METHOD.    

    METHOD PUBLIC STATIC DECIMAL mGetTopupAmount(INPUT iiOrderID         AS INTEGER ,
                                                 INPUT iiOrderProductID  AS INTEGER ,
                                                 INPUT iiAmountType      AS CHARACTER ) :
        
        DEFINE BUFFER bfOrderProduct FOR OrderProduct.
        
        DEFINE VARIABLE lcParamType  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcParamValue AS CHARACTER NO-UNDO.
        
        FIND FIRST bfOrderProduct WHERE bfOrderProduct.OrderID     =  iiOrderID
                                    AND bfOrderProduct.ActionType  =  {&ORDER_PRODUCT_SUBSCRIPTION}
                                  NO-LOCK NO-ERROR.
          
        IF NOT AVAILABLE bfOrderProduct THEN RETURN 0.
        
        Func.OrderProductsData:mGetOrderProductParamValue(INPUT bfOrderProduct.OrderID ,
                                                          INPUT bfOrderProduct.OrderProductID,
                                                          INPUT iiAmountType,
                                                          OUTPUT lcParamType ,
                                                          OUTPUT lcParamValue). 
                                                          
        IF lcParamType = {&ORDER_PRODUCT_PARAM_DEC_VALUE} THEN 
            RETURN DECIMAL(lcParamValue).
            
        RETURN 0.                                                                                                       
                                                     
    END METHOD.    
    
END CLASS.