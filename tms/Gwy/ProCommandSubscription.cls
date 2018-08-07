 
 /*------------------------------------------------------------------------
    File        : ProCommandDSS
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.
USING Gwy.ProCommandOrderEnum.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.ProCommandDSS INHERITS Gwy.ProCommand: 

   DEFINE PRIVATE VARIABLE aoOrderEnum AS CLASS ProCommandOrderEnum NO-UNDO.
   
   DEFINE PRIVATE BUFFER Order FOR Order.
   DEFINE PRIVATE BUFFER OrderCustomer FOR OrderCustomer. 

   CONSTRUCTOR ProCommandDSS
      ( iiMsRequest AS INTEGER ):

      SUPER(iiMsRequest).
      
      DEFINE VARIABLE loJson    AS CLASS JsonObject NO-UNDO.

      CASE MsRequest.ReqType:
         WHEN {&REQTYPE_SUBSCRIPTION_CREATE}
         THEN aoOrderEnum = ProCommandOrderEnum:CREATION.
         WHEN {&REQTYPE_SUBSCRIPTION_TERMINATION}
         THEN aoOrderEnum = ProCommandOrderEnum:TERMINATION.
         WHEN {&REQTYPE_SUBSCRIPTION_REACTIVATION}
         THEN aoOrderEnum = ProCommandOrderEnum:PORTABILITY.
         OTHERWISE UNDO, THROW NEW AppError(SUBSTITUTE("ProCommandSubscription cannot " +
                                                       "handle ReqType &1",
                                                       MsRequest.ReqType).
      END CASE.

      IF MsRequest.ReqIParam5 > 0
      THEN FIND Order NO-LOCK WHERE
                Order.Brand = "1" AND
                Order.OrderId = MsRequest.ReqIParam5 NO-ERROR.
      ELSE FOR EACH Order NO-LOCK WHERE
                    Order.MSSeq = aiMSSeq AND
                    LOOKUP(STRING(Order.OrderType),"0,1,3") > 0
               BY Order.CrStamp DESC:
               LEAVE.
           END.

      IF NOT AVAILABLE Order
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find order for msrequest &1",
                                               MsRequest.MsRequest)).

      FIND OrderCustomer NO-LOCK WHERE
           OrderCustomer.Brand   = "1"           AND
           OrderCustomer.OrderID = Order.OrderId AND
           OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
      NO-ERROR.

      IF NOT AVAILABLE OrderCustomer
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find agreement ordercustomer for order &1",
                                               Order.OrderId)).

      acProCommandTarget = "BPM".
      mSetProCommandId().
      
      loJson = mOrderObject().

      loJson:Add("Client", mClientObject()).
      loJson:Add("Services", mServicesObject()).

      FINALLY:
         IF VALID-OBJECT(loJson)
         THEN aoCommandLine = loJson.
      END FINALLY.

   END CONSTRUCTOR.

   METHOD PRIVATE JsonObject mNameValue
      ( icName  AS CHARACTER,
        icValue AS CHARACTER ):

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("name", icName).
      loJsonObject:Add("value", icValue).

      RETURN loJsonObject.

   END METHOD.
   
   METHOD PRIVATE JsonObject mOrderObject():

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("orderId", icName).
      loJsonObject:Add("orderType", IF aoOrderEnum EQ ProCommandOrderEnum:TERMINATION
                                    THEN "Disconnection"
                                    ELSE "InOrder").
      loJsonObject:Add("Sellchannel", "TMSB2C").
      loJsonObject:Add("Selldate", DATETIME-TZ(Func.Common:mTimeStamp2DateTime(MsRequest.ActStamp))).
      loJsonObject:Add("Seller", CAPS(multitenancy.TenantInformation:mGetEffectiveBrand())).
      loJsonObject:Add("createdBy", MsRequest.UserCode).
      loJsonObject:Add("createdDate", DATETIME-TZ(Func.Common:mTimeStamp2DateTime(MsRequest.CreStamp))).

      RETURN loJsonObject.

   END METHOD.

   METHOD PRIVATE JsonObject mContactObject():

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE loContact    AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().

      /* Mitä tänne???
      loJsonObject:Add("clientID", OrderCustomer.CustId).
      loJsonObject:Add("Type", OrderCustomer.CustIdType).
      */

      RETURN loJsonObject.

   END METHOD.

   METHOD PRIVATE JsonObject mClientObject():

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("clientID", OrderCustomer.CustId).
      loJsonObject:Add("Type", OrderCustomer.CustIdType).
      loJsonObject:Add(mContactObject()).
  
      RETURN loJsonObject.

   END METHOD.

END CLASS.