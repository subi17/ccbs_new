 
 /*------------------------------------------------------------------------
    File        : Customer
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : tlamminmaki
    Created     : Mon Apr 23 12:30:10 EEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS bss.cls.Customer IMPLEMENTS bss.cls.IObjectStorage:
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY CustNum AS INTEGER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Category AS CHARACTER NO-UNDO
      GET.
      SET.

	CONSTRUCTOR PUBLIC Customer():
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC Customer():
	END DESTRUCTOR.

	METHOD PUBLIC VOID mFindData
	  (iiCustNum AS INTEGER):

      DEFINE BUFFER Customer FOR Customer.

      FIND Customer NO-LOCK WHERE Customer.CustNum = iiCustNum NO-ERROR.

      IF NOT AVAILABLE Customer
      THEN UNDO, THROW NEW ParamError
               (SUBSTITUTE("customer-not-found|&1", iiCustNum)).

      ASSIGN
         THIS-OBJECT:CustNum  = Customer.CustNum
         THIS-OBJECT:Category = Customer.Category.

	END METHOD.

   METHOD PUBLIC VOID mCreateData
     (ioJsonObject AS CLASS JsonObject):

      UNDO, THROW NEW InternalError("mCreateCustomer is not implemented").

   END METHOD.

   METHOD PUBLIC VOID mStoreData():

      IF THIS-OBJECT:CustNum > 0
      THEN RETURN.

      UNDO, THROW NEW InternalError("mStoreData is not implemented").

   END METHOD.

END CLASS.