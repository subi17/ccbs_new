 
 /*------------------------------------------------------------------------
    File        : ProCommandBPM
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.
USING Gwy.SAPC.OrderEnum.
USING Gwy.SAPC.Service.IService.
USING Gwy.SAPC.ProCommandOrder.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.ProCommandBPM INHERITS ProCommandOrder: 
   
   DEFINE PRIVATE BUFFER Order FOR Order.
   DEFINE PRIVATE BUFFER OrderCustomer FOR OrderCustomer.

   CONSTRUCTOR ProCommandBPM
      ( iiMsRequest AS INTEGER ):

      SUPER(iiMsRequest).

      DEFINE VARIABLE loJson        AS CLASS JsonObject   NO-UNDO.

      CASE MsRequest.ReqType:
         WHEN {&REQTYPE_SUBSCRIPTION_CREATE}
         THEN ASSIGN
                 aoOrderEnum      = OrderEnum:SUBSCRIPTION_CREATE
                 acProCommandType = "SUBSCRIPTION_CREATE".
         WHEN {&REQTYPE_SUBSCRIPTION_TERMINATION}
         THEN ASSIGN
                 aoOrderEnum = OrderEnum:SUBSCRIPTION_TERMINATE
                 acProCommandType = "SUBSCRIPTION_TERMINATE".
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
         acProCommandTarget = STRING(Gwy.SAPC.TargetEnum:BPM)
         acCLI              = Order.CLI
         aiOrderId          = Order.OrderId
         acICC              = Order.ICC
         acCLIType          = Order.CLIType
         alPayType          = Order.PayType.

      loJson = mOrderObject(IF aoOrderEnum EQ OrderEnum:TERMINATION
                            THEN "Disconnection"
                            ELSE "InOrder").

      loJson:Add("Client", mClientObject()).
      
      aoService = NEW Gwy.SAPC.Service.MobileLineSAPC.UnderRoot(THIS-OBJECT).

      aoService:mCreateServices(loJson).

      FINALLY:
         IF VALID-OBJECT(loJson)
         THEN aoCommandLine = loJson.
      END FINALLY.

   END CONSTRUCTOR.
   
/*
   METHOD PRIVATE JsonObject mContactObject():

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
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
*/

   METHOD PRIVATE JsonObject mClientObject():

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("clientID", OrderCustomer.CustId).
      loJsonObject:Add("Type", mGetSegment()).
      /*loJsonObject:Add("Contact", mContactObject()).*/
  
      RETURN loJsonObject.

   END METHOD.

   METHOD PRIVATE CHARACTER mGetSegment():

      FOR
         FIRST CustCat FIELDS (Brand Category Segment) NO-LOCK WHERE
            CustCat.Brand    = "1" AND
            CustCat.Category = OrderCustomer.Category:
         RETURN CustCat.Segment.
      END.

      RETURN "".

   END METHOD.

END CLASS.