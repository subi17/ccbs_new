 
 /*------------------------------------------------------------------------
    File        : OrderProduct
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Apr 23 12:30:10 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS bss.cls.OrderProduct ABSTRACT IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PROTECTED BUFFER abOrderProduct FOR OrderProduct.

   DEFINE PUBLIC PROPERTY OrderProductID AS INTEGER NO-UNDO
      GET.
      SET.
   
   DEFINE PUBLIC PROPERTY OrderID AS INTEGER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY ActionType AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY ProductID AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY ProductOfferingID AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoOrderProductFactory AS CLASS bss.cls.OrderProductFactory NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC VARIABLE aoOrder AS CLASS bss.cls.Order NO-UNDO.

	CONSTRUCTOR PUBLIC OrderProduct
     ( ioOrder AS CLASS bss.cls.Order ):
      aoOrder = ioOrder.	
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProduct ( ):
      IF VALID-OBJECT(aoOrderProductFactory)
      THEN DELETE OBJECT aoOrderProductFactory.
      aoOrder = ?.
	END DESTRUCTOR.

   METHOD PUBLIC ABSTRACT VOID mCreateDataForType(ioItemObject AS CLASS JsonObject).

	METHOD PUBLIC VOID mCreateData
	  ( ioItemObject AS CLASS JsonObject ):

      ASSIGN
         ActionType         = ioItemObject:GetCharacter("type")
         ProductID          = ioItemObject:GetCharacter("product-id") 
         ProductOfferingID  = ioItemObject:GetCharacter("product-offering-id").

      mCreateDataForType(ioItemObject).

      IF ioItemObject:Has("items")
      THEN DO:
         aoOrderProductFactory = NEW bss.cls.OrderProductFactory().
         aoOrderProductFactory:mCreateData(ioItemObject:GetJsonArray("items"), aoOrder).
      END.

	END METHOD.

   METHOD PUBLIC ABSTRACT VOID mStoreDataForType().

   METHOD PUBLIC VOID mStoreData():

      CREATE abOrderProduct.
      ASSIGN
         abOrderProduct.OrderProductId     = NEXT-VALUE(OrderProductID)
         abOrderProduct.OrderId            = aoOrder:OrderId
         THIS-OBJECT:OrderProductID        = abOrderProduct.OrderProductId
         THIS-OBJECT:OrderID               = abOrderProduct.OrderId
         abOrderProduct.ActionType         = THIS-OBJECT:ActionType
         abOrderProduct.ProductID          = THIS-OBJECT:ProductID
         abOrderProduct.ProductOfferingID  = THIS-OBJECT:ProductOfferingID
         .

      mStoreDataForType().

      IF VALID-OBJECT(aoOrderProductFactory)
      THEN aoOrderProductFactory:mStoreData().

   END METHOD.

   METHOD PUBLIC VOID mResult
      (ioJsonArray AS CLASS JsonArray):

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("order-item-id", THIS-OBJECT:OrderProductID).
      ioJsonArray:Add(loJsonObject).

      IF VALID-OBJECT(aoOrderProductFactory)
      THEN aoOrderProductFactory:mResult(loJsonObject).

   END METHOD.

END CLASS.
