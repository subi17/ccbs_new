 
 /*------------------------------------------------------------------------
    File        : OrderProductACC
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Apr 23 12:30:10 EEST 2018
    Notes       : QUESTION: Do we need to check charge limits???
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS bss.cls.OrderProductACC INHERITS bss.cls.OrderProduct: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY aoMemo AS CLASS bss.cls.Memo NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoExecutionDate AS CLASS bss.cls.OrderProductParam NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoOrderCustomer AS CLASS bss.cls.OrderCustomer NO-UNDO
      GET.
      SET.

	CONSTRUCTOR PUBLIC OrderProductACC
      ( ioOrder AS CLASS bss.cls.Order ):
      SUPER(ioOrder).
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProductACC ( ):

      IF VALID-OBJECT(aoMemo)
      THEN DELETE OBJECT aoMemo.

      IF VALID-OBJECT(aoExecutionDate)
      THEN DELETE OBJECT aoExecutionDate.

      IF VALID-OBJECT(aoOrderCustomer)
      THEN DELETE OBJECT aoOrderCustomer.

	END DESTRUCTOR.

   METHOD PUBLIC OVERRIDE VOID mStoreDataForType():
      
      aoMemo:mStoreData().
      aoExecutionDate:mStoreData().
      aoOrderCustomer:mStoreData().
      
   END METHOD.

	METHOD PUBLIC OVERRIDE VOID mCreateDataForType
	  ( ioItemObject AS CLASS JsonObject ):

      IF aoOrder:aoCustomer:CustNum EQ 0
      THEN UNDO, THROW NEW ParamError("ACC source customer cannot be a new customer").

      DEFINE VARIABLE loAttributes  AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE loMemo        AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE lcReturnValue AS CHARACTER        NO-UNDO.

      ASSIGN
         loAttributes  = ioItemObject:GetJsonObject("attributes")
         aoOrder:MsSeq = loAttributes:GetInteger("subscription-id")
         lcReturnValue = Func.ValidateACC:mPreCheckSubscriptionForACC(aoOrder:MsSeq).

      IF lcReturnValue > ""
      THEN UNDO, THROW NEW InternalError(lcReturnValue)

      lcReturnValue = Func.ValidateACC:mCheckSubscriptionForACC(aoOrder:MsSeq,
                                                                0,
                                                                {&REQUEST_SOURCE_NEWTON}).

      IF lcReturnValue > ""
      THEN UNDO, THROW NEW InternalError(SUBSTRING(lcReturnValue,INDEX(lcReturnValue,"|") + 1)).

      IF loAttributes:Has("memo")
      THEN DO:
         aoMemo = NEW bss.cls.Memo(THIS-OBJECT).
         aoMemo:mCreateMemo(loAttributes:GetJsonObject("memo")).
      END.
      
      aoExecutionDate = NEW bss.cls.OrderProductParam(THIS-OBJECT).
      aoExecutionDate:mCreateOrderProductParam(loAttributes,
                                               "execution-date",
                                               "Date").

      IF (NOT loAttributes:Has("custnum")) AND
         (NOT loAttributes:Has("customer"))
      THEN UNDO, THROW NEW ParamError("Either custnum or customer property is mandatory").

      aoOrderCustomer = NEW bss.cls.OrderCustomer(aoOrder).

      IF loAttributes:Has("custnum")
      THEN DO:
         aoOrderCustomer:mCreateData(loAttributes:GetInteger("custnum")).
         lcReturnValue = Func.ValidateACC:mExistingCustomerACCCompability
                                          (aoOrder:aoCustomer:Category,
                                           aoOrderCustomer:Category,
                                           aoOrderCustomer:CustNum,
                                           aoOrderCustomer:CustIdType,
                                           aoOrderCustomer:OrgId).
         IF lcReturnValue > ""
         THEN UNDO, THROW NEW InternalError(lcReturnValue).
      END.

      /* OrderCustomer data will be updated from the customer property */
      IF loAttributes:Has("customer")
      THEN aoOrderCustomer:mCreateData(loAttributes:GetJsonObject("customer")).

      IF loAttributes:Has("custnum")
      THEN DO:
         lcReturnValue = Func.ValidateACC:mCheckTargetCustomerForACC(aoOrderCustomer:CustNum).
         IF lcReturnValue > ""
         THEN UNDO, THROW NEW InternalError(SUBSTRING(lcReturnValue,INDEX(lcReturnValue,"|") + 1)).
      END.
      ELSE DO:
         lcReturnValue = Func.ValidateACC:mNewCustomerACCCompability(aoOrder:aoCustomer:Category,
                                                                     aoOrderCustomer:OrgId, /* or CustId ??? */
                                                                     aoOrderCustomer:CustIdType).
         IF lcReturnValue > ""
         THEN UNDO, THROW NEW InternalError(lcReturnValue).
      END.

      IF aoOrderCustomer:CustNum EQ aoOrder:aoCustomer:CustNum
      THEN UNDO, THROW NEW ParamError("New agreement customer is the same as current one").

	END METHOD.

END CLASS.