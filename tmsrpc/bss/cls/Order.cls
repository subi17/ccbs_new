 
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
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS bss.cls.Order IMPLEMENTS bss.cls.IObjectStorage:
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY OrderId AS INTEGER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoCustomer AS CLASS bss.cls.Customer NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY OrderPaymentData AS CLASS JsonObject NO-UNDO
       GET.
       SET.    
    
   DEFINE PUBLIC PROPERTY OrderInspectionData AS CLASS JsonObject NO-UNDO
       GET.
       SET.

   DEFINE PUBLIC PROPERTY ContractId AS CHARACTER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY loCustAcct AS CLASS JsonObject NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Salesman AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY OrderChannel AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY CrStamp AS DECIMAL NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY CLI AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY MobSubCustNum AS INTEGER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY isACCOrder AS LOGICAL NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY lcCLI AS CHARACTER NO-UNDO
      GET.
      SET(icCLI AS CHARACTER):
          
          ASSIGN THIS-OBJECT:lcCLI = icCLI.
          
      END SET.

   /* A value for this property is populated in OrderProduct class */
   DEFINE PUBLIC PROPERTY MsSeq AS INTEGER NO-UNDO
      GET.
      SET(iiMSSeq AS INTEGER):
         DEFINE BUFFER MobSub FOR MobSub.
         FIND MobSub NO-LOCK WHERE MobSub.MSSeq = iiMSSeq NO-ERROR.
         IF NOT AVAILABLE MobSub
         THEN DO:
            IF CAN-FIND(TermMobSub NO-LOCK WHERE TermMobSub.MSSeq = iiMSSeq)
            THEN UNDO, THROW NEW ParamError
                  (SUBSTITUTE("subscription_not_active|&1", iiMSSeq)).

            UNDO, THROW NEW ParamError
                  (SUBSTITUTE("subscription_not_found|&1", iiMSSeq)).
         END.
         ASSIGN
            THIS-OBJECT:MsSeq         = iiMSSeq
            THIS-OBJECT:CLI           = MobSub.CLI
            THIS-OBJECT:MobSubCustNum = MobSub.CustNum.
      END SET.

   DEFINE PUBLIC PROPERTY aoOrderProductFactory AS CLASS bss.cls.OrderProductFactory NO-UNDO
      GET.
      SET.

	CONSTRUCTOR PUBLIC Order():
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC Order():

      IF VALID-OBJECT(aoCustomer)
      THEN DELETE OBJECT aoCustomer.

      IF VALID-OBJECT(aoOrderProductFactory)
      THEN DELETE OBJECT aoOrderProductFactory.
      
      ASSIGN OrderPaymentData      =  ?
             OrderInspectionData   =  ?
             loCustAcct            =  ?
             .

	END DESTRUCTOR.

	METHOD PUBLIC VOID mCreateData
	  (ioJsonObject AS CLASS JsonObject):

      DEFINE VARIABLE loAgrCust     AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE ldaCreateDate AS DATE             NO-UNDO.
      
      ASSIGN
         loAgrCust            =   ioJsonObject:GetJsonObject("agreement-customer") WHEN ioJsonObject:Has("agreement-customer")
         loCustAcct           =   ioJsonObject:GetJsonObject("customer-account")   WHEN ioJsonObject:Has("customer-account")
         OrderPaymentData     =   ioJsonObject:GetJsonObject("payment")            WHEN ioJsonObject:Has("payment")
         OrderInspectionData  =   ioJsonObject:GetJsonObject("order-inspection")   WHEN ioJsonObject:Has("order-inspection")
         aoCustomer           =   NEW bss.cls.Customer().
         
      IF VALID-OBJECT(loAgrCust) 
      THEN DO:          
          IF loAgrCust:Has("custnum")
          THEN aoCustomer:mFindData(loAgrCust:GetInteger("custnum")).
          ELSE aoCustomer:mCreateData(loAgrCust).
          
          ASSIGN THIS-OBJECT:isACCOrder = TRUE.
      END. 
      ELSE IF VALID-OBJECT(loCustAcct)
      THEN DO:
          IF loCustAcct:Has("custnum")
          THEN aoCustomer:mFindData(loCustAcct:GetInteger("custnum")).
      END.      

      ASSIGN
         ldaCreateDate            = ioJsonObject:GetDate("created-at")
         THIS-OBJECT:ContractId   = ioJsonObject:GetCharacter("contractid") 
         THIS-OBJECT:Salesman     = ioJsonObject:GetCharacter("salesman")
         THIS-OBJECT:OrderChannel = ioJsonObject:GetCharacter("channel")
         THIS-OBJECT:CrStamp      = YEAR(ldaCreateDate) * 10000 +
                                    MONTH(ldaCreateDate) * 100 +
                                    DAY(ldaCreateDate)
         .

      IF ioJsonObject:Has("items")
      THEN DO:
         aoOrderProductFactory = NEW bss.cls.OrderProductFactory().
         aoOrderProductFactory:mCreateData(ioJsonObject:GetJsonArray("items"), THIS-OBJECT).
      END.
      ELSE UNDO, THROW NEW ParamError("order_items_are_mandatory").

	END METHOD.

   METHOD PUBLIC VOID mStoreData():

      /* If there is a new customer data we need to create it first */
      aoCustomer:mStoreData().

      CREATE Order.
      ASSIGN
         Order.Brand         = Syst.Var:gcBrand
         Order.OrderId       = NEXT-VALUE(OrderId)
         THIS-OBJECT:OrderId = Order.OrderId /* The OrderProducts needs this */
         Order.StatusCode    = {&ORDER_STATUS_ONGOING}
         Order.OrderType     = (IF THIS-OBJECT:isACCOrder THEN {&ORDER_TYPE_ACC} ELSE {&ORDER_TYPE_NEW})
         Order.Source        = "bssapi"
         Order.CustNum       = aoCustomer:CustNum
         Order.MsSeq         = (IF THIS-OBJECT:isACCOrder THEN THIS-OBJECT:MsSeq ELSE NEXT-VALUE(MobSub))
         Order.CLI           = (IF THIS-OBJECT:isACCOrder THEN THIS-OBJECT:CLI   ELSE THIS-OBJECT:lcCLI)
         Order.ContractID    = THIS-OBJECT:ContractId
         Order.SalesMan      = THIS-OBJECT:Salesman
         Order.OrderChannel  = THIS-OBJECT:OrderChannel
         Order.CrStamp       = THIS-OBJECT:CrStamp.

      aoOrderProductFactory:mStoreData().

   END METHOD.

   METHOD PUBLIC JsonObject mResult():

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("order-id", THIS-OBJECT:OrderId).

      aoOrderProductFactory:mResult(loJsonObject).

      RETURN loJsonObject.

   END METHOD.

END CLASS.
