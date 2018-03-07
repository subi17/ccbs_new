{Syst/tmsconst.i}
{Func/log.i}
{Func/masmovileif.i}
{Mc/orderfusion.i}

DEF INPUT PARAM piMessageSeq AS INT NO-UNDO.

DEF VAR lcFixedNumber AS CHAR NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR lcResultCode AS CHAR NO-UNDO. 
DEF VAR lcResultDesc AS CHAR NO-UNDO. 

FIND FusionMessage EXCLUSIVE-LOCK WHERE
     FusionMessage.MessageSeq = piMessageSeq NO-ERROR.

IF NOT AVAIL FusionMessage THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                              "FusionMessage not found").

IF FusionMessage.MessageType NE {&FUSIONMESSAGE_TYPE_CREATE_ORDER} THEN
   RETURN SUBST("Incorrect message type: &1", FusionMessage.MessageType).

FIND FIRST Order NO-LOCK WHERE
           Order.Brand = Syst.Var:gcBrand AND
           Order.OrderId = FusionMessage.OrderID NO-ERROR.
IF NOT AVAIL Order THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                              "Order not found").

FIND OrderFusion EXCLUSIVE-LOCK WHERE
     OrderFusion.Brand = Syst.Var:gcBrand AND
     OrderFusion.OrderID = FusionMessage.OrderID NO-ERROR.
IF NOT AVAIL OrderFusion THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                              "OrderFusion not found").

IF Order.StatusCode NE {&ORDER_STATUS_PENDING_FIXED_LINE} THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
            SUBST("Incorrect order status &1", Order.Statuscode)).

IF OrderFusion.FusionStatus NE {&FUSION_ORDER_STATUS_NEW} THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
               SUBST("Incorrect fusion status &1",
                     OrderFusion.FusionStatus)).

IF NOT OrderFusion.FixedNumber > "" THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                             "FixedNumber not assigned").

lcError = fInitMMConnection().
IF lcError NE "" THEN RETURN lcError.

lcError = fMasCreate_FixedLineOrder(Order.OrderID,
                                    OUTPUT lcResultCode,
                                    OUTPUT lcResultDesc).

IF lcError EQ "OK" THEN DO:

   ASSIGN
      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_INITIALIZED}
      OrderFusion.UpdateTS = Func.Common:mMakeTS()
      FusionMessage.UpdateTS = OrderFusion.UpdateTS
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_HANDLED}
      FusionMessage.ResponseCode = lcResultCode 
      FusionMessage.AdditionalInfo = lcResultDesc.

   RETURN "OK".

END.
ELSE DO:

   ASSIGN
      FusionMessage.UpdateTS = Func.Common:mMakeTS()
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.ResponseCode = (IF lcResultCode > ""
                                  THEN lcResultCode
                                  ELSE "ERROR")
      FusionMessage.AdditionalInfo = (IF lcResultDesc > ""
                                      THEN lcResultDesc
                                      ELSE lcError).

   IF fCanRetryFusionMessage(
      BUFFER FusionMessage,
      lcError,
      lcResultCode,
      lcResultDesc) THEN RETURN "RETRY:" +
         SUBST("&1, &2, &3", lcError, lcResultCode, lcResultDesc).

   ASSIGN
      OrderFusion.UpdateTS = FusionMessage.UpdateTS
      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_ERROR}
      OrderFusion.FusionStatusDesc = "Create order failed".

   Func.Common:mWriteMemoWithType("Order",
                                  STRING(Order.OrderId),
                                  0,
                                  "Masmovil order creation failed",
                                  SUBST("ErrorCode: &1", (IF lcResultDesc > ""
                                                          THEN lcResultDesc 
                                                          ELSE lcError)),
                                  "",
                                  "TMS").

   RETURN SUBST("&1, &2, &3", lcError, lcResultCode, lcResultDesc).
END.

FINALLY:
   xmlrpc_finalize().
END.
