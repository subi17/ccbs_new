 
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

   DEFINE PUBLIC PROPERTY aoMandate AS CLASS bss.cls.OrderAction NO-UNDO
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

      IF VALID-OBJECT(aoMandate)
      THEN DELETE OBJECT aoMandate.

	END DESTRUCTOR.

   METHOD PUBLIC OVERRIDE VOID mStoreDataForType():

      DEFINE VARIABLE liRequestID AS INTEGER NO-UNDO.

      IF VALID-OBJECT(aoMandate)
      THEN aoMandate:mStoreData().

      IF VALID-OBJECT(aoMemo)
      THEN aoMemo:mStoreData().

      aoExecutionDate:mStoreData().
      aoOrderCustomer:mStoreData().

      RUN Mm/acc_order.p(INPUT aoOrder:OrderId, OUTPUT liRequestID).

      IF RETURN-VALUE > ""
      THEN UNDO, THROW NEW InternalError(SUBSTITUTE("acc_order failed with error '&1'", RETURN-VALUE)).

      IF liRequestID EQ 0
      THEN UNDO, THROW NEW InternalError("Failed to create ACC request").

   END METHOD.

	METHOD PUBLIC OVERRIDE VOID mCreateDataForType
	  ( ioItemObject AS CLASS JsonObject ):

      IF aoOrder:aoCustomer:CustNum EQ 0
      THEN UNDO, THROW NEW ParamError("ACC source customer cannot be a new customer").


      DEFINE VARIABLE lcSubType     AS CHARACTER        NO-UNDO.
      DEFINE VARIABLE loAttributes  AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE loMemo        AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE lcReturnValue AS CHARACTER        NO-UNDO.

      ASSIGN
         loAttributes  = ioItemObject:GetJsonObject("attributes")
         lcSubType = ioItemObject:GetCharacter("subtype").

      IF lcSubType BEGINS "existing" AND NOT loAttributes:Has("custnum")
      THEN UNDO, THROW NEW ParamError
                  (SUBSTITUTE("An existing customer id is mandatory when " +
                             "subtype is '&1'", lcSubType)).

      IF lcSubType BEGINS "new" AND NOT loAttributes:Has("customer")
      THEN UNDO, THROW NEW ParamError
                  (SUBSTITUTE("A customer object data is mandatory when " +
                              "subtype is '&1'", lcSubType)).

      ASSIGN
         aoOrder:MsSeq = loAttributes:GetInteger("subscription-id")
         lcReturnValue = Func.ValidateACC:mPreCheckSubscriptionForACC(aoOrder:MsSeq).

      IF lcReturnValue > ""
      THEN UNDO, THROW NEW InternalError(lcReturnValue).

      lcReturnValue = Func.ValidateACC:mCheckSubscriptionForACC(aoOrder:MsSeq,
                                                                0,
                                                                0,
                                                                {&REQUEST_SOURCE_NEWTON}).

      IF lcReturnValue > ""
      THEN UNDO, THROW NEW InternalError(SUBSTRING(lcReturnValue,INDEX(lcReturnValue,"|") + 1)).

      IF loAttributes:Has("mandate")
      THEN DO:
         aoMandate = NEW bss.cls.OrderAction(aoOrder).
         aoMandate:mCreateData("Mandate", loAttributes:GetCharacter("mandate"), "").
      END.

      IF loAttributes:Has("memo")
      THEN DO:
         aoMemo = NEW bss.cls.Memo(THIS-OBJECT).
         aoMemo:mCreateData(loAttributes:GetJsonObject("memo")).
      END.
      
      aoExecutionDate = NEW bss.cls.OrderProductParam(THIS-OBJECT).
      aoExecutionDate:mCreateData(loAttributes,
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
                                           aoOrderCustomer:CustId).
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
                                                                     aoOrderCustomer:CustId,
                                                                     aoOrderCustomer:CustIdType).
         IF lcReturnValue > ""
         THEN UNDO, THROW NEW InternalError(lcReturnValue).
      END.

      IF aoOrderCustomer:CustNum EQ aoOrder:aoCustomer:CustNum
      THEN UNDO, THROW NEW ParamError("New agreement customer is the same as current one").

      mSubTypeValidations(lcSubType).

	END METHOD.

   METHOD PRIVATE VOID mSubTypeValidations
      (icSubType AS CHARACTER):

      DEFINE VARIABLE llPrePaid AS LOGICAL NO-UNDO.
      DEFINE VARIABLE llCompany AS LOGICAL NO-UNDO.
      DEFINE BUFFER MobSub FOR MobSub.

      CASE icSubType:
         WHEN "new-corporate-postpaid"
         THEN ASSIGN
                 llPrePaid = NO
                 llCompany = YES.
         WHEN "new-corporate-prepaid"
         THEN ASSIGN
                 llPrePaid = YES
                 llCompany = YES.
         WHEN "new-consumer-postpaid"
         THEN ASSIGN
                 llPrePaid = NO
                 llCompany = NO.
         WHEN "new-consumer-prepaid"
         THEN ASSIGN
                 llPrePaid = YES
                 llCompany = NO.
         WHEN "existing-customer"
         THEN RETURN.
         OTHERWISE UNDO, THROW NEW ParamError
                     (SUBSTITUTE("Invalid subtype '&1'",icSubType)).
      END CASE.

      IF llCompany AND aoOrderCustomer:CustIdType NE "CIF"
      THEN UNDO, THROW NEW ParamError
                     (SUBSTITUTE("Company id is mandatory when " +
                                 "subtype is in value '&1'",icSubType)).

      IF NOT CAN-FIND(FIRST Mobsub NO-LOCK WHERE
                            MobSub.MsSeq   EQ aoOrder:MsSeq AND
                            MobSub.PayType EQ llPrePaid)
      THEN UNDO, THROW NEW ParamError
                     (SUBSTITUTE("Subscription which MsSeq '&1' is &2 subscription " +
                                 "which contradicts the subtype '&3'",
                                 aoOrder:MsSeq,
                                 STRING(llPrepaid,"prepaid/postpaid"),
                                 icSubType)).

      IF (NOT llPrePaid) AND
         aoOrderCustomer:BankCode EQ "" OR
         NOT VALID-OBJECT(aoMandate)
      THEN UNDO, THROW NEW ParamError
                     (SUBSTITUTE("Both bankaccount and mandate fields are " +
                                 "mandatory when subtype is in value '&1'",
                                 icSubType)).

      IF llPrePaid AND
         VALID-OBJECT(aoMandate)
      THEN UNDO, THROW NEW ParamError
                     (SUBSTITUTE("mandate field is not allowed " +
                                 "when subtype is in value '&1'",
                                 icSubType)).

      IF (NOT llPrePaid) AND (aoOrderCustomer:CustIdType     EQ "Passport" OR
                              aoOrderCustomer:AuthCustIdType EQ "Passport")
      THEN UNDO, THROW NEW ParamError("Passport is not accepted for postpaid subscription").


   END METHOD.

END CLASS.
