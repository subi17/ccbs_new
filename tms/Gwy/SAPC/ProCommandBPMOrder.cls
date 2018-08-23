 
 /*------------------------------------------------------------------------
    File        : ProCommandBPMOrder
    Purpose     : BPM messages where up to date order information
                  is available (e.g. subscription creation)
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.JsonObject.
USING Gwy.SAPC.OrderEnum.
USING Gwy.SAPC.ProCommandBPM.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.ProCommandBPMOrder INHERITS ProCommandBPM: 

   DEFINE PRIVATE BUFFER OrderCustomer FOR OrderCustomer.

   CONSTRUCTOR ProCommandBPMOrder
      ( iiMsRequest AS INTEGER ):

      SUPER(iiMsRequest).

      DEFINE BUFFER Order FOR Order.

      CASE MsRequest.ReqType:
         WHEN {&REQTYPE_SUBSCRIPTION_CREATE}
         THEN ASSIGN
                 aoOrderEnum      = OrderEnum:SUBSCRIPTION_CREATE
                 acProCommandType = "SUBSCRIPTION_CREATE".
         OTHERWISE UNDO, THROW NEW AppError(SUBSTITUTE("ProCommandBPM cannot " +
                                                       "handle ReqType &1",
                                                       MsRequest.ReqType)).
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

      ASSIGN
         acCLI              = Order.CLI
         aiOrderId          = Order.OrderId
         acICC              = Order.ICC
         acCLIType          = Order.CLIType
         alPayType          = Order.PayType
         acDonorOperator    = mDonorOperator(aiOrderId) WHEN Order.MNPStatus > 0.

      mCreateCommandLine(OrderCustomer.CustId, OrderCustomer.Category).

   END CONSTRUCTOR.

   METHOD PROTECTED OVERRIDE JsonObject mContactObject():

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().

      loJsonObject:Add("firstName", OrderCustomer.FirstName).
      loJsonObject:Add("middleName", OrderCustomer.Surname1).
      loJsonObject:Add("lastName", IF OrderCustomer.Surname2 NE ""
                                   THEN OrderCustomer.Surname2
                                   ELSE OrderCustomer.Surname1).
      loJsonObject:Add("documentNumber", OrderCustomer.CustID).
      loJsonObject:Add("documentType", OrderCustomer.CustIDType).
      loJsonObject:Add("email", OrderCustomer.Email).
      loJsonObject:Add("phoneNumber", IF OrderCustomer.MobileNumber > ""
                                      THEN OrderCustomer.MobileNumber
                                      ELSE OrderCustomer.FixedNumber).

      RETURN loJsonObject.

   END METHOD.

END CLASS.