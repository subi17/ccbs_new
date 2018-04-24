 
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

      DEFINE VARIABLE loAttributes AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE loMemo       AS CLASS JsonObject NO-UNDO.

      ASSIGN
         loAttributes  = ioItemObject:GetJsonObject("attributes")
         aoOrder:MsSeq = loAttributes:GetInteger("subscription-id").

      mPreCheckSubscriptionForACC().

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
         mCheckACCCompability().
      END.
      
      IF loAttributes:Has("customer")
      THEN aoOrderCustomer:mCreateData(loAttributes:GetJsonObject("customer")).

/*
      IF NOT loAttributes:Has("custnum")
      THEN mNewCustomerACCCompability().
*/

      IF aoOrderCustomer:CustNum EQ aoOrder:aoCustomer:CustNum
      THEN UNDO, THROW NEW ParamError("New agreement customer is the same as current one").

	END METHOD.

   METHOD PRIVATE LOGICAL mGetPROStatus
      ( icCategory AS CHARACTER,
        ilOld      AS LOGICAL ):

      DEFINE BUFFER CustCat FOR CustCat.

      FOR CustCat NO-LOCK WHERE
          CustCat.Brand    EQ Syst.Var:gcBrand AND
          CustCat.Category EQ icCategory:
          RETURN CustCat.PRO.   
      END.

      UNDO, THROW NEW InternalError
            (SUBSTITUTE("Incorrect &1 customer category '&2'",
                        STRING(ilOld,"old/new"),
                        icCategory)).

   END METHOD.

/*
   METHOD PRIVATE VOID mNewCustomerACCCompability():
   
      DEFINE VARIABLE llSRCPRO       AS LOGICAL NO-UNDO.
      DEFINE VARIABLE llSelfEmployed AS LOGICAL NO-UNDO.
   
      llSRCPRO = mGetPROStatus(aoOrder:aoCustomer:Category, YES).
   
      IF llSRCPRO
      THEN llSelfEmployed = mIsSelfEmpl(aoOrder:aoCustomer:Category).

      IF NOT fSubscriptionLimitCheck(INPUT aoOrderCustomer:OrgId,
                                     INPUT aoOrderCustomer:CustIdType,
                                     llProCust,
                                     llSelfEmployed, 
                                     1,
                                     OUTPUT liSubLimit,
                                     OUTPUT liSubs,
                                     OUTPUT liActLimit,
                                     OUTPUT liActs)
      THEN UNDO, THROW NEW InternalError("Subscription limit exceeded").

   END METHOD.
*/

   /* check self employee */
   METHOD PRIVATE LOGICAL mIsSelfEmpl
      (icCategory AS CHARACTER):
   
      DEFINE BUFFER CustCat FOR CustCat.

      FOR CustCat NO-LOCK WHERE
          CustCat.Brand    EQ Syst.Var:gcBrand AND
          CustCat.Category EQ icCategory:
          RETURN INDEX(custcat.catname, "self") > 0.   
      END.

      RETURN FALSE.
   
   END.

   METHOD PRIVATE VOID mExistingCustomerACCCompability():
   
      DEFINE VARIABLE llSRCPRO AS LOGICAL NO-UNDO.      
      DEFINE VARIABLE llDSTPRO AS LOGICAL NO-UNDO.      
   
      ASSIGN
         llSRCPRO = mGetPROStatus(aoOrder:aoCustomer:Category, YES)
         llDSTPRO = mGetPROStatus(aoOrderCustomer:Category, NO).
   
      IF llSRCPRO NE llDSTPRO
      THEN DO:
         IF NOT llSRCPRO AND llDSTPRO
         THEN UNDO, THROW NEW InternalError("ACC is not allowed from NON-PRO " +
                                            "to PRO customer").

         /* Check for any active/ongoing subscriptions */
         IF CAN-FIND(FIRST MobSub NO-LOCK WHERE
                           MobSub.Brand   = Syst.Var:gcBrand        AND 
                           MobSub.AgrCust = aoOrderCustomer:CustNum AND 
                           MobSub.Cli     > "") OR 
            mCheckOngoingOrders()
         THEN UNDO, THROW NEW InternalError("ACC is not allowed from PRO " +
                                            "to NON-PRO customer due to " +
                                            "active subscriptions or " +
                                            "ongoing order").
      END.   
   
   END METHOD.

   METHOD PRIVATE LOGICAL mCheckOngoingOrders():

      DEFINE BUFFER OrderCustomer FOR OrderCustomer.
      DEFINE BUFFER Order FOR Order.
      
      FOR EACH OrderCustomer NO-LOCK WHERE
               OrderCustomer.Brand      EQ Syst.Var:gcBrand           AND
               OrderCustomer.CustId     EQ aoOrderCustomer:OrgId      AND
               OrderCustomer.CustIdType EQ aoOrderCustomer:CustIdType AND
               OrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
         FIRST Order NO-LOCK WHERE
               Order.Brand              EQ Syst.Var:gcBrand AND
               Order.OrderId            EQ Ordercustomer.OrderId AND
               Order.MsSeq              NE 0 AND
               LOOKUP(Order.StatusCode, {&ORDER_INACTIVE_STATUSES}) = 0:
         RETURN TRUE.
      END.
      RETURN FALSE.
   
   END METHOD.

   METHOD PRIVATE VOID mPreCheckSubscriptionForACC():

      DEFINE BUFFER MobSub FOR MobSub.

      FOR MobSub NO-LOCK WHERE
          MobSub.MsSeq = aoOrder:MsSeq:

         IF MobSub.MultiSimId > 0 AND
            MobSub.MultiSimType = {&MULTISIMTYPE_SECONDARY}
         THEN UNDO, THROW NEW ParamError("ACC is not allowed for multi SIM " +
                                         "secondary subscription").

         IF CAN-FIND(FIRST CLIType WHERE
                           CLIType.Brand = Syst.Var:gcBrand AND
                           CLIType.CLIType = (IF MobSub.TariffBundle > ""
                                              THEN MobSub.TariffBundle
                                              ELSE MobSub.CLIType) AND
                           CLIType.LineType = {&CLITYPE_LINETYPE_ADDITIONAL})
         THEN UNDO, THROW NEW ParamError("ACC is not allowed for additional line").

      END.

   END METHOD.

END CLASS.