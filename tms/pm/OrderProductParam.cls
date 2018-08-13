 
 /*------------------------------------------------------------------------
    File        : OrderProductParam
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Apr 23 12:30:10 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamValidator.
USING profcgi.RPC.JSON.ParamTemplateFactory.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS pm.OrderProductParam IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY charValue AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY dateValue AS DATE NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY decValue AS DECIMAL NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY intValue AS INTEGER NO-UNDO
      GET.
      SET.      

   DEFINE PUBLIC PROPERTY paramName AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY valueType AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY objOrderProduct AS CLASS pm.OrderProduct NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY objParamStruct AS CLASS JsonObject NO-UNDO
       GET.
       SET.

   CONSTRUCTOR PUBLIC OrderProductParam ( iObjOrderProduct AS CLASS pm.OrderProduct ):
      ASSIGN objOrderProduct = iObjOrderProduct.   
   END CONSTRUCTOR.

   METHOD PUBLIC VOID mCreateData
     ( ioJsonObject AS CLASS JsonObject ):
      UNDO, THROW NEW InternalError("OrderProductParam doesn't support " +
                                    "mCreateData method with JsonObject " +
                                    "parameter").
   END METHOD.
	
	METHOD PUBLIC VOID mCreateData
      ( icPropertyValue   AS CHARACTER,
        icPropertyName    AS CHARACTER,
        icDataType        AS CHARACTER ):
        
        CASE icDataType:
            WHEN "Char" THEN 
            DO:
                ASSIGN 
                    THIS-OBJECT:charValue = icPropertyValue NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                    UNDO, THROW NEW InternalError(SUBSTITUTE("Invalid value in the parameter &1 for data type conversion &2.",icPropertyValue,icDataType)).
            END.            
            WHEN "Date" THEN
            DO:
                ASSIGN 
                    THIS-OBJECT:dateValue = DATE(icPropertyValue) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                    UNDO, THROW NEW InternalError(SUBSTITUTE("Invalid value in the parameter &1 for data type conversion &2.",icPropertyValue,icDataType)).   
            END.
      
            WHEN "Int" THEN 
            DO:
                ASSIGN 
                    THIS-OBJECT:intValue = INTEGER(icPropertyValue) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                    UNDO, THROW NEW InternalError(SUBSTITUTE("Invalid value in the parameter &1 for data type conversion &2.",icPropertyValue,icDataType)).    
            END.
      
            WHEN "Dec" THEN 
            DO:
                ASSIGN 
                    THIS-OBJECT:decValue = DECIMAL(icPropertyValue) NO-ERROR.  
                IF ERROR-STATUS:ERROR THEN 
                    UNDO, THROW NEW InternalError(SUBSTITUTE("Invalid value in the parameter &1 for data type conversion &2.",icPropertyValue,icDataType)).  
            END. 
            OTHERWISE            
                UNDO, THROW NEW InternalError(SUBSTITUTE("Called mCreateOrderProductParam " +
                                              "with unsupported datatype '&1'",
                                              icDataType)).
        END CASE.

      ASSIGN
         THIS-OBJECT:paramName = icPropertyName
         THIS-OBJECT:valueType = icDataType.

	END METHOD.
	
   METHOD PUBLIC VOID mStoreData():

      CREATE OrderProductParam.
      ASSIGN
         OrderProductParam.CharValue      = THIS-OBJECT:charValue
         OrderProductParam.DateValue      = THIS-OBJECT:dateValue
         OrderProductParam.DecValue       = THIS-OBJECT:decValue
         OrderProductParam.IntValue       = THIS-OBJECT:intValue
         OrderProductParam.OrderProductID = objOrderProduct:OrderProductID
         OrderProductParam.OrderID        = objOrderProduct:OrderID
         OrderProductParam.ParamName      = THIS-OBJECT:paramName
         OrderProductParam.ValueType      = THIS-OBJECT:valueType.

   END METHOD.
   
   DESTRUCTOR PUBLIC OrderProductParam():
       
       IF VALID-OBJECT (objOrderProduct) THEN 
           DELETE OBJECT objOrderProduct.
           
   END DESTRUCTOR.

END CLASS.
