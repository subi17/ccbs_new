 
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

CLASS pm.OrderProductACC INHERITS pm.OrderProduct:
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY aoMemo AS CLASS bss.cls.Memo NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoExecutionDate AS CLASS pm.OrderProductParam NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoOrderCustomer AS CLASS om.OrderCustomer NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoMandate AS CLASS om.OrderAction NO-UNDO
      GET.
      SET.

	CONSTRUCTOR PUBLIC OrderProductACC
      ( ioOrder AS CLASS om.Order ):
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

      RUN Mm/acc_order.p(INPUT objOrder:OrderId, OUTPUT liRequestID).

      IF RETURN-VALUE > ""
      THEN UNDO, THROW NEW InternalError(SUBSTITUTE("acc_order failed with error '&1'", RETURN-VALUE)).

      IF liRequestID EQ 0
      THEN UNDO, THROW NEW InternalError("Failed to create ACC request").

   END METHOD.

	METHOD PUBLIC OVERRIDE VOID mCreateDataForType
	  ( ioItemObject AS CLASS JsonObject ):

      IF objOrder:objCustomer:CustNum EQ 0
      THEN UNDO, THROW NEW ParamError("donor-customer-has-to-exist").

      DEFINE VARIABLE lcSubType     AS CHARACTER        NO-UNDO.
      DEFINE VARIABLE loAttributes  AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE loMemo        AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE lcReturnValue AS CHARACTER        NO-UNDO.
      DEFINE VARIABLE lcReqSource   AS CHARACTER        NO-UNDO.

      ASSIGN
         loAttributes  = ioItemObject:GetJsonObject("attributes")
         lcSubType = ioItemObject:GetCharacter("subtype").

      IF lcSubType BEGINS "existing" AND NOT loAttributes:Has("custnum")
      THEN UNDO, THROW NEW ParamError
                  (SUBSTITUTE("existing-customer-id-required|&1", lcSubType)).

      IF lcSubType BEGINS "new" AND NOT loAttributes:Has("customer")
      THEN UNDO, THROW NEW ParamError
                  (SUBSTITUTE("customer-data-required|&1", lcSubType)).

      ASSIGN
         objOrder:MsSeq = loAttributes:GetInteger("subscription-id")
         lcReturnValue = Func.ValidateACC:mPreCheckSubscriptionForACC(objOrder:MsSeq).

      IF lcReturnValue > ""
      THEN UNDO, THROW NEW InternalError(lcReturnValue).

      IF objOrder:orderChannel = "newton"
      THEN lcReqSource = {&REQUEST_SOURCE_NEWTON}.
      ELSE IF objOrder:orderChannel = "retail_newton"
      THEN lcReqSource = {&REQUEST_SOURCE_RETAIL_NEWTON}.
      ELSE UNDO, THROW NEW ParamError
                  (SUBSTITUTE("Invalid order channel|&1", objOrder:orderChannel)).

      lcReturnValue = Func.ValidateACC:mCheckSubscriptionForACC(objOrder:MsSeq,
                                                                0,
                                                                0,
                                                                lcReqSource).

      IF lcReturnValue > ""
      THEN UNDO, THROW NEW InternalError(SUBSTRING(lcReturnValue,INDEX(lcReturnValue,"|") + 1)).

      IF loAttributes:Has("mandate")
      THEN DO:
         aoMandate = NEW om.OrderAction(objOrder).
         aoMandate:mCreateData("Mandate", loAttributes:GetCharacter("mandate"), "").
      END.

      IF loAttributes:Has("memo")
      THEN DO:
         aoMemo = NEW bss.cls.Memo(THIS-OBJECT).
         aoMemo:mCreateData(loAttributes:GetJsonObject("memo")).
      END.

      aoExecutionDate = NEW pm.OrderProductParam(THIS-OBJECT).
      aoExecutionDate:mCreateData(STRING(loAttributes:GetDate("execution-date")),
                                  "execution-date",
                                  "Date").


      IF (NOT loAttributes:Has("custnum")) AND
         (NOT loAttributes:Has("customer"))
      THEN UNDO, THROW NEW ParamError("customer-id-or-data-required").

      ASSIGN
         aoOrderCustomer         = NEW om.OrderCustomer(objOrder)
         aoOrderCustomer:RowType = {&ORDERCUSTOMER_ROWTYPE_ACC}
         objOrder:orderType       = {&ORDER_TYPE_ACC}
         objOrder:statusCode      = {&ORDER_STATUS_ONGOING}.

      IF loAttributes:Has("custnum")
      THEN DO:
         aoOrderCustomer:mCreateData(loAttributes:GetInteger("custnum")).
         lcReturnValue = Func.ValidateACC:mExistingCustomerACCCompability
                                          (objOrder:objCustomer:Category,
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
      
      IF NOT aoOrderCustomer:BankCode > ""  AND
         aoOrder:PayType EQ FALSE 
      THEN UNDO, THROW NEW ParamError(SUBSTITUTE("bank_account_invalid|&1",
                                      aoOrderCustomer:BankCode)).

      IF loAttributes:Has("custnum")
      THEN DO:
         lcReturnValue = Func.ValidateACC:mCheckTargetCustomerForACC(aoOrderCustomer:CustNum).
         IF lcReturnValue > ""
         THEN UNDO, THROW NEW InternalError(SUBSTRING(lcReturnValue,INDEX(lcReturnValue,"|") + 1)).
      END.
      ELSE DO:
         lcReturnValue = Func.ValidateACC:mNewCustomerACCCompability(objOrder:objCustomer:Category,
                                                                     aoOrderCustomer:CustId,
                                                                     aoOrderCustomer:CustIdType).
         IF lcReturnValue > ""
         THEN UNDO, THROW NEW InternalError(lcReturnValue).
      END.

      IF aoOrderCustomer:CustNum EQ objOrder:objCustomer:CustNum
      THEN UNDO, THROW NEW ParamError("new-and-donor-customer-same").

      IF objOrder:mobSubCustNum NE objOrder:objCustomer:CustNum
      THEN UNDO, THROW NEW
               ParamError(SUBSTITUTE("subscription-customer-and-donor-customer-mismatch|&1",
                          objOrder:objCustomer:CustNum)).

      IF aoOrderCustomer:Country NE "ES"
      THEN UNDO, THROW NEW ParamError("country-only-es-valid").

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
         WHEN "existing-customer" OR WHEN "existing-customer-update"
         THEN RETURN.
         OTHERWISE UNDO, THROW NEW ParamError
                     (SUBSTITUTE("invalid-type|&1",icSubType)).
      END CASE.

      IF llCompany AND aoOrderCustomer:CustIdType NE "CIF"
      THEN UNDO, THROW NEW ParamError
                     (SUBSTITUTE("company-id-required|&1",icSubType)).

      IF NOT CAN-FIND(FIRST Mobsub NO-LOCK WHERE
                            MobSub.MsSeq   EQ objOrder:MsSeq AND
                            MobSub.PayType EQ llPrePaid)
      THEN UNDO, THROW NEW ParamError
                     (SUBSTITUTE("subscription-type-mismatch|&1",
                                 icSubType)).

      IF (NOT llPrePaid) AND
         (aoOrderCustomer:BankCode EQ "" OR
          NOT VALID-OBJECT(aoMandate))
      THEN UNDO, THROW NEW ParamError
                     (SUBSTITUTE("bank-account-and-mandate-required|&1",
                                 icSubType)).

      IF llPrePaid AND
         VALID-OBJECT(aoMandate)
      THEN UNDO, THROW NEW ParamError
                     (SUBSTITUTE("mandate-not-allowed|&1",
                                 icSubType)).

      IF (NOT llPrePaid) AND (aoOrderCustomer:CustIdType     EQ "Passport" OR
                              aoOrderCustomer:AuthCustIdType EQ "Passport")
      THEN UNDO, THROW NEW ParamError("passport-in-postpaid-not-allowed").

      IF CAN-FIND(FIRST Customer NO-LOCK WHERE
                        Customer.Brand = Syst.Var:gcBrand AND
                        Customer.OrgID = aoOrderCustomer:CustID AND
                        Customer.CustIDType = aoOrderCustomer:CustIDType AND
                        Customer.Roles NE "inactive")
      THEN UNDO, THROW NEW ParamError(SUBST("customer-already-exists|&1", icSubType)).


   END METHOD.

END CLASS.
