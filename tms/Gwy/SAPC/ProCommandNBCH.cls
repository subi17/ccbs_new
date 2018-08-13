 
 /*------------------------------------------------------------------------
    File        : ProCommandNBCH
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

CLASS Gwy.SAPC.ProCommandNBCH INHERITS ProCommandOrder: 
   
   DEFINE PRIVATE BUFFER MobSub FOR MobSub.

   CONSTRUCTOR ProCommandNBCH
      ( iiMsRequest AS INTEGER ):

      SUPER(iiMsRequest).

      DEFINE VARIABLE loJson        AS CLASS JsonObject   NO-UNDO.
      DEFINE VARIABLE loService     AS CLASS IService     NO-UNDO.

      IF MsRequest.ReqCparam1 <> "SHAPER"
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("ProCommandNBCH cannot " +
                                               "handle &1 request",
                                               MsRequest.ReqCparam1)).

      CASE MsRequest.ReqType:
         WHEN {&REQTYPE_SERVICE_CHANGE}
         THEN ASSIGN
                 aoOrderEnum      = OrderEnum:CREATION
                 acProCommandType = "SUBSCRIPTION_CREATE".
         OTHERWISE UNDO, THROW NEW AppError(SUBSTITUTE("ProCommandNBCH cannot " +
                                                       "handle ReqType &1",
                                                       MsRequest.ReqType)).
      END CASE.

      FIND MobSub NO-LOCK WHERE 
          MobSub.MsSeq = aiMsSeq
      NO-ERROR.
      
      IF NOT AVAILABLE MobSub
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find mobsub &1 for msrequest &2",
                                               aiMsSeq, MsRequest.MsRequest)).

      ASSIGN
         acProCommandTarget = STRING(Gwy.SAPC.TargetEnum:NB_CH)
         acCLI              = MobSub.CLI
         acICC              = MobSub.ICC
         acCLIType          = MobSub.CLIType
         alPayType          = MobSub.PayType.

      loJson = mOrderObject("Change").

      loJson:Add("Client", mClientObject()).
      
      loService = NEW Gwy.SAPC.Service.MobileLineSAPC.UnderRoot().

      IF VALID-OBJECT(loService)
      THEN loService:mCreateServices(loJson, THIS-OBJECT).

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