 
 /*------------------------------------------------------------------------
    File        : Memo
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

CLASS bss.cls.Memo IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY MemoTitle AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY MemoText AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoOrderProduct AS CLASS bss.cls.OrderProduct NO-UNDO
      GET.
      SET.

   CONSTRUCTOR PUBLIC Memo
     ( ioOrderProduct AS CLASS bss.cls.OrderProduct ):
      aoOrderProduct = ioOrderProduct.   
   END CONSTRUCTOR.

	DESTRUCTOR PUBLIC Memo():
	   aoOrderProduct = ?.
	END DESTRUCTOR.
	
	METHOD PUBLIC VOID mCreateData
	  (ioJsonObject AS CLASS JsonObject):

      ASSIGN
         MemoTitle = ioJsonObject:GetCharacter("title")
         MemoText  = ioJsonObject:GetCharacter("content").

	END METHOD.

   METHOD PUBLIC VOID mStoreData():

      Func.Common:mWriteMemo("OrderProduct",
                             STRING(aoOrderProduct:OrderProductId),
                             aoOrderProduct:aoOrder:aoCustomer:CustNum,
                             THIS-OBJECT:MemoTitle,
                             THIS-OBJECT:MemoText).

   END METHOD.

END CLASS.