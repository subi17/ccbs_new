{Syst/tmsconst.i}
{Func/log.i}
{Func/date.i}
{Func/masmovileif.i}
{Mc/orderfusion.i}
{Func/memo.i}

DEF INPUT PARAM piMessageSeq AS INT NO-UNDO.

DEF BUFFER bFusionMessage FOR FusionMessage.

DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR lcResultCode AS CHAR NO-UNDO. 
DEF VAR lcResultDesc AS CHAR NO-UNDO. 
DEF VAR ldaOrigCancelDate AS DATE NO-UNDO. 

FIND FusionMessage EXCLUSIVE-LOCK WHERE
     FusionMessage.MessageSeq = piMessageSeq.
          
IF FusionMessage.MessageType NE {&FUSIONMESSAGE_TYPE_CANCEL_ORDER} THEN
   RETURN SUBST("Incorrect message type: &1", FusionMessage.MessageType).

FIND Order EXCLUSIVE-LOCK WHERE
     Order.Brand = Syst.Parameters:gcBrand AND
     Order.OrderId = FusionMessage.OrderID.

FIND OrderFusion EXCLUSIVE-LOCK WHERE
     OrderFusion.Brand = Syst.Parameters:gcBrand AND
     OrderFusion.OrderID = FusionMessage.OrderID.

IF Order.StatusCode NE {&ORDER_STATUS_PENDING_FIXED_LINE} THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
            SUBST("Incorrect order status &1", Order.Statuscode)).

IF OrderFusion.FusionStatus NE {&FUSION_ORDER_STATUS_INITIALIZED} THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
               SUBST("Incorrect fusion status &1",
                     OrderFusion.FusionStatus)).

IF NOT OrderFusion.FixedNumber > "" THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                             "FixedNumber not assigned").

lcError = fInitMMConnection().
IF lcError NE "" THEN RETURN lcError.

FIND LAST bFusionMessage NO-LOCK WHERE
          bFusionMessage.OrderId = Order.OrderID AND
          bFusionMessage.MessageType = {&FUSIONMESSAGE_TYPE_CANCEL_ORDER}
   USE-INDEX OrderID.

fTS2Date(bFusionMessage.CreatedTS, OUTPUT ldaOrigCancelDate).

lcError = fMasCancel_FixedLineOrder(Order.OrderID,
                                    ldaOrigCancelDate,
                                    "Cancelled by Yoigo",
                                    OUTPUT lcResultCode,
                                    OUTPUT lcResultDesc).

IF lcError EQ "OK" THEN DO:

   ASSIGN
      FusionMessage.HandledTS = fMakeTS()
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_HANDLED}
      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_PENDING_CANCELLED}
      OrderFusion.UpdateTS = FusionMessage.HandledTS 
      Order.StatusCode = {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL}
      Order.SendToROI  = {&ROI_HISTORY_TO_SEND} WHEN 
         Order.Ordertype NE {&ORDER_TYPE_STC}.

   fMarkOrderStamp(Order.OrderID,"Change",0.0).

   RETURN "OK".

END.
ELSE DO:

   ASSIGN
      FusionMessage.HandledTS = fMakeTS()
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

   fCreateMemo("Order",
               STRING(Order.OrderId),
               0,
               "Masmovil order cancellation failed",
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
