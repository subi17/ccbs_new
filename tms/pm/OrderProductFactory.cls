 
 /*------------------------------------------------------------------------
    File        : Order
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Apr 23 12:30:10 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.ParamError.
USING profcgi.RPC.JSON.InternalError.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS pm.OrderProductFactory IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
   
   DEFINE PRIVATE TEMP-TABLE ttOrderProduct NO-UNDO
      FIELD opobject AS CLASS Progress.Lang.Object
      FIELD ordernb  AS INTEGER
      INDEX ordernb IS PRIMARY UNIQUE ordernb
      .

    DEFINE PUBLIC PROPERTY isChildArray AS LOGICAL NO-UNDO
    GET.
    SET(INPUT ilParent AS LOGICAL):
        
        ASSIGN THIS-OBJECT:isChildArray  =  ilParent.  
        
    END SET.
    
    DEFINE PUBLIC PROPERTY ParentID AS INTEGER NO-UNDO
       GET.
       SET(INPUT iiParentID AS INTEGER):
           
           ASSIGN THIS-OBJECT:ParentID  = iiParentID.
           
       END.
       

	CONSTRUCTOR PUBLIC OrderProductFactory():
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProductFactory():
      FOR EACH ttOrderProduct:
         IF VALID-OBJECT(ttOrderProduct.opobject)
         THEN DELETE OBJECT ttOrderProduct.opobject.
         DELETE ttOrderProduct.
      END.
	END DESTRUCTOR.

   METHOD PUBLIC VOID mCreateData
     ( ioJsonObject AS CLASS JsonObject ):
      UNDO, THROW NEW InternalError("OrderProductFactory doesn't support " +
                                    "mCreateData method with JsonObject " +
                                    "parameter").
   END METHOD.

   METHOD PRIVATE INTEGER mNextOrderNb():

      FIND LAST ttOrderProduct USE-INDEX ordernb.
      IF NOT AVAILABLE ttOrderProduct
      THEN RETURN 1.

      RETURN ttOrderProduct.ordernb + 1.

   END METHOD.

	METHOD PUBLIC VOID mCreateData
	  ( ioItemsArray AS CLASS JsonArray,
	    ioOrder      AS CLASS om.Order ):

      DEFINE VARIABLE lii AS INTEGER NO-UNDO.
      DEFINE VARIABLE loItemObject AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE loOrderProduct AS CLASS pm.OrderProduct NO-UNDO.

      DO lii = 1 TO ioItemsArray:Length:
         loItemObject = ioItemsArray:GetJsonObject(lii).
         CASE loItemObject:GetCharacter("type"):
            WHEN {&ORDER_PRODUCT_ACC}
            THEN loOrderProduct = NEW pm.OrderProductACC(ioOrder).
            WHEN {&ORDER_PRODUCT_MAIN}
            THEN loOrderProduct = NEW pm.OrderProductPC(ioOrder).
            WHEN {&ORDER_PRODUCT_SUBSCRIPTION}
            THEN loOrderProduct = NEW pm.OrderProductSubscription(ioOrder).
            WHEN {&ORDER_PRODUCT_SIM}
            THEN loOrderProduct = NEW pm.OrderProductSIM(ioOrder).
            WHEN {&ORDER_PRODUCT_FIXED_LINE}
            THEN loOrderProduct = NEW pm.OrderProductFLine(ioOrder).
            WHEN {&ORDER_PRODUCT_ACCESSORY}
            THEN loOrderProduct = NEW pm.OrderProductAccessory(ioOrder).
            OTHERWISE
            UNDO, THROW
              NEW ParamError(SUBSTITUTE("Invalid items type '&1'",
                                        loItemObject:GetCharacter("type"))).
         END.

         CREATE ttOrderProduct.
         ASSIGN
            ttOrderProduct.opobject = loOrderProduct
            ttOrderProduct.ordernb  = mNextOrderNb().

         loOrderProduct:mCreateData(loItemObject).
      END.

	END METHOD.

   METHOD PUBLIC VOID mStoreData():

      FOR EACH ttOrderProduct:
         CAST(ttOrderProduct.opobject, pm.OrderProduct):ParentID   =  THIS-OBJECT:ParentID.
         CAST(ttOrderProduct.opobject, pm.OrderProduct):mStoreData().
      END.
      

   END METHOD.

   METHOD PUBLIC VOID mResult
      (ioJsonObject AS CLASS JsonObject):

      DEFINE VARIABLE loJsonArray AS CLASS JsonArray NO-UNDO.

      loJsonArray = NEW JsonArray().

      FOR EACH ttOrderProduct:
         CAST(ttOrderProduct.opobject, pm.OrderProduct):mResult(loJsonArray).
      END.

      ioJsonObject:Add("items", loJsonArray).

   END METHOD.

END CLASS.