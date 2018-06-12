 
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

CLASS bss.cls.OrderProductParam IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY CharValue AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY DateValue AS DATE NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY DecValue AS DECIMAL NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY IntValue AS INTEGER NO-UNDO
      GET.
      SET.      

   DEFINE PUBLIC PROPERTY ParamName AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY ValueType AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoOrderProduct AS CLASS bss.cls.OrderProduct NO-UNDO
      GET.
      SET.

   CONSTRUCTOR PUBLIC OrderProductParam
     ( ioOrderProduct AS CLASS bss.cls.OrderProduct ):
      aoOrderProduct = ioOrderProduct.   
   END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProductParam():
	   aoOrderProduct = ?.
	END DESTRUCTOR.

   METHOD PUBLIC VOID mCreateData
     ( ioJsonObject AS CLASS JsonObject ):
      UNDO, THROW NEW InternalError("OrderProductParam doesn't support " +
                                    "mCreateData method with JsonObject " +
                                    "parameter").
   END METHOD.
	
	METHOD PUBLIC VOID mCreateData
      ( ioJsonObject   AS CLASS JsonObject,
        icPropertyName AS CHARACTER,
        icDataType     AS CHARACTER ):

      CASE icDataType:
         WHEN "Char" THEN CharValue = ioJsonObject:GetCharacter(icPropertyName).
         WHEN "Date" THEN DateValue = ioJsonObject:GetDate(icPropertyName).
         WHEN "Int"  THEN IntValue  = ioJsonObject:GetInteger(icPropertyName).
         WHEN "Dec"  THEN DecValue  = ioJsonObject:GetDecimal(icPropertyName).
         OTHERWISE UNDO, THROW NEW InternalError
                                 (SUBSTITUTE("Called mCreateOrderProductParam " +
                                             "with unsupported datatype '&1'",
                                             icDataType)).
      END CASE.

      ASSIGN
         ParamName = icPropertyName
         ValueType = icDataType.

	END METHOD.

   METHOD PUBLIC VOID mStoreData():

      CREATE OrderProductParam.
      ASSIGN
         OrderProductParam.CharValue      = THIS-OBJECT:CharValue
         OrderProductParam.DateValue      = THIS-OBJECT:DateValue
         OrderProductParam.DecValue       = THIS-OBJECT:DecValue
         OrderProductParam.IntValue       = THIS-OBJECT:IntValue
         OrderProductParam.OrderProductID = aoOrderProduct:OrderProductID
         OrderProductParam.OrderID        = aoOrderProduct:OrderID
         OrderProductParam.ParamName      = THIS-OBJECT:ParamName
         OrderProductParam.ValueType      = THIS-OBJECT:ValueType.

   END METHOD.

END CLASS.
