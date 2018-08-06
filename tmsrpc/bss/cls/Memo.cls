 
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

   DEFINE PUBLIC PROPERTY objOrderProduct AS CLASS pm.OrderProduct NO-UNDO
      GET.
      SET.

    CONSTRUCTOR PUBLIC Memo
        ( ioOrderProduct AS CLASS pm.OrderProduct ):
        objOrderProduct = ioOrderProduct.   
    END CONSTRUCTOR.	
	
    METHOD PUBLIC VOID mCreateData
        (ioJsonObject AS CLASS JsonObject):

        ASSIGN
            MemoTitle = ioJsonObject:GetCharacter("title")
            MemoText  = ioJsonObject:GetCharacter("content").

    END METHOD.

    METHOD PUBLIC VOID mStoreData():

        Func.Common:mWriteMemo("OrderProduct",
                               STRING(objOrderProduct:OrderProductId),
                               objOrderProduct:objOrder:objCustomer:CustNum,
                               THIS-OBJECT:MemoTitle,
                               THIS-OBJECT:MemoText).

    END METHOD.
   
    DESTRUCTOR PUBLIC Memo():
       
        IF VALID-OBJECT(objOrderProduct) THEN 
            DELETE OBJECT objOrderProduct.
       
    END DESTRUCTOR.

END CLASS.