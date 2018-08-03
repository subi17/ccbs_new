 
 /*------------------------------------------------------------------------
    File        : OrderAction
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

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS om.OrderAction IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY ItemType AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY ItemKey AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY ItemParam AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PROTECTED VARIABLE objOrder AS CLASS om.Order NO-UNDO.
   
	CONSTRUCTOR PUBLIC OrderAction
     ( iObjOrder AS CLASS om.Order ):
         
      ASSIGN objOrder = iObjOrder.
      
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderAction ( ):
	    
	    IF VALID-OBJECT (objOrder) THEN 
	        DELETE OBJECT objOrder.
	        
	END DESTRUCTOR.

   METHOD PUBLIC VOID mCreateData
     ( ioJsonObject AS CLASS JsonObject ):
      UNDO, THROW NEW InternalError("OrderAction doesn't support " +
                                    "mCreateData method with JsonObject " +
                                    "parameter").
   END METHOD.

	METHOD PUBLIC VOID mCreateData
	  (icItemType  AS CHARACTER,
	   icItemKey   AS CHARACTER,
	   icItemParam AS CHARACTER):

      ASSIGN
         THIS-OBJECT:ItemType  = icItemType
         THIS-OBJECT:ItemKey   = icItemKey
         THIS-OBJECT:ItemParam = icItemParam.

	END METHOD.

   METHOD PUBLIC VOID mStoreData():

      CREATE OrderAction.
      ASSIGN
         OrderAction.Brand     = Syst.Var:gcBrand
         OrderAction.OrderId   = objOrder:OrderId
         OrderAction.ItemType  = THIS-OBJECT:ItemType
         OrderAction.ItemKey   = THIS-OBJECT:ItemKey
         OrderAction.ItemParam = THIS-OBJECT:ItemParam.

   END METHOD.

END CLASS.