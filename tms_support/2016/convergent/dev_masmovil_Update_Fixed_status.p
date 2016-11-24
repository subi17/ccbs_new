/* USE ONLY IN DEVELOPMENT ENVIRONMENT
Remember to update correct lcOrderId
Because Masmovin interface is not running in development then 
this script can be used to change status.
Works best for STC orders because there is already mobile
subscription data. Others will be only partially completed.
*/


{Syst/commpaa.i}
gcBrand = "1".
{Syst/tmsconst.i}
{Func/fexternalapi.i}
{Mc/orderfusion.i}
{Func/orderfunc.i}
{timestamp.i}

DEF VAR lcNotificationID AS CHAR NO-UNDO INIT "1".
DEF VAR ldeNotificationTime AS DEC NO-UNDO.
DEF VAR lcNotificationType AS CHAR NO-UNDO INIT "O".
DEF VAR lcOrderId AS CHAR NO-UNDO INIT "80000319". /* <---- UPDATE CORRECT ID */
DEF VAR liOrderID AS INT NO-UNDO.
DEF VAR lcNotificationStatus AS CHAR NO-UNDO. /*struct*/

DEF VAR lcStatusFields AS CHAR NO-UNDO.

DEF VAR lcOrderType AS CHAR NO-UNDO.
DEF VAR lcStatus AS CHAR NO-UNDO INIT "CERRADA".
DEF VAR lcStatusDescription AS CHAR NO-UNDO INIT "hello".
DEF VAR lcAdditionalInfo AS CHAR NO-UNDO.
DEF VAR ldeLastDate AS DEC NO-UNDO.
DEF VAR lcresultStruct AS CHAR NO-UNDO.

DEF BUFFER bOrderFusion FOR OrderFusion.

ldeNotificationTime = fMakeTS().
ldeLastDate = fMakeTS().

IF lcOrderId BEGINS "Y" THEN
   lcOrderId = SUBSTRING(lcOrderId,2).

liOrderID = INT(lcOrderId) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
   MESSAGE "Incorrect orderID syntax" VIEW-AS ALERT-BOX.
   RETURN.
END.

FIND FIRST Order NO-LOCK WHERE
           Order.Brand = gcBrand AND
           Order.OrderID = liOrderID NO-ERROR.
IF NOT AVAIL Order THEN DO:
   MESSAGE "Order not found" VIEW-AS ALERT-BOX.
   RETURN.
END.

FIND FIRST OrderFusion EXCLUSIVE-LOCK WHERE
           OrderFusion.Brand = gcBrand AND
           OrderFusion.OrderId = liOrderID NO-ERROR.
IF NOT AVAIL OrderFusion THEN DO:
   MESSAGE "Order type is incorrect" VIEW-AS ALERT-BOX.
   RETURN.
END.

IF lcStatus EQ "CERRADA" AND
   NOT OrderFusion.FixedNumber > "" THEN DO:
      IF CAN-FIND(FIRST Mobsub NO-LOCK WHERE
                        Mobsub.FixedNumber EQ ("9" + lcOrderId)) THEN DO:
         MESSAGE "Order fixed number is in use" + ("9" + lcOrderId) VIEW-AS ALERT-BOX.
         RETURN.
      END.
      IF CAN-FIND(FIRST bOrderFusion NO-LOCK WHERE
                        bOrderFusion.FixedNumber EQ ("9" + lcOrderId)) THEN DO:
         MESSAGE "Order fixed number is missing" + ("9" + lcOrderId) VIEW-AS ALERT-BOX.
         RETURN.
      END.
END.

ASSIGN OrderFusion.FixedNumber = ("9" + lcOrderId).

/* HANDLING */
CREATE FusionMessage.
ASSIGN
   FusionMessage.MessageSeq = NEXT-VALUE(FusionMessageSeq)
   FusionMessage.OrderID = liOrderID
   FusionMessage.MsSeq = Order.MsSeq
   FusionMessage.CreatedTS = fMakeTS()
   FusionMessage.UpdateTS = FusionMessage.CreatedTS
   FusionMessage.MessageID = lcNotificationID
   FusionMessage.MessageType = {&FUSIONMESSAGE_TYPE_UPDATE_STATUS}
   FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_HANDLED}
   FusionMessage.Source = {&FUSIONMESSAGE_SOURCE_MASMOVIL}
   FusionMessage.FixedStatus = lcStatus
   FusionMessage.FixedStatusDesc = lcStatusDescription
   FusionMessage.FixedStatusTS = ldeNotificationTime
   FusionMessage.OrderType = lcOrderType
   FusionMessage.AdditionalInfo = lcAdditionalInfo.

/* handle only work order statuses */
IF lcNotificationType NE "O" THEN DO:

   FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_WRONG_TYPE}.

END.
IF OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_FINALIZED} AND
   FusionMessage.FixedStatus NE "CERRADA" THEN DO:

   ASSIGN
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.AdditionalInfo = "Status update is not allowed after CERRADA".

   MESSAGE "Status update is not allowed after CERRADA" VIEW-AS ALERT-BOX.
   RETURN.
END.

IF OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_CANCELLED} AND
   FusionMessage.FixedStatus NE "CANCELADA" THEN DO:

   ASSIGN
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.AdditionalInfo = "Status update is not allowed after CANCELADA".

   MESSAGE "Status update is not allowed after CANCELADA" VIEW-AS ALERT-BOX.
   RETURN.
END.

IF ldeNotificationTime < OrderFusion.FixedStatusTS THEN DO:

   ASSIGN
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.AdditionalInfo = "notificationTime is older than the value in the latest received message".

   MESSAGE "notificationTime is older than the value in the latest received message" VIEW-AS ALERT-BOX.
   RETURN.
END.

ASSIGN
   OrderFusion.FixedStatus = lcStatus
   OrderFusion.FixedStatusTS = ldeNotificationTime
   OrderFusion.UpdateTS = FusionMessage.CreatedTS.

CASE FusionMessage.FixedStatus:

   WHEN "CERRADA PARCIAL" OR
   WHEN "EN PROCESO - NO CANCELABLE" THEN DO:

      IF OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_INITIALIZED} OR
         OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_PENDING_CANCELLED}
      THEN OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_ONGOING}.

      IF FusionMessage.FixedStatus EQ "EN PROCESO - NO CANCELABLE" AND
         Order.OrderType EQ {&ORDER_TYPE_STC} AND
         NOT CAN-FIND(FIRST mobsub NO-LOCK WHERE
                            mobsub.msseq = Order.MsSeq) THEN DO:

         fSetOrderStatus(Order.Orderid, {&ORDER_STATUS_IN_CONTROL}).

         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                 "Order",
                 STRING(Order.OrderID),
                 Order.CustNum,
                 "Order handling stopped",
                 "Subscription is terminated, Convergent order cannot proceed").
      END.

   END.
   WHEN "INCIDENCIA DATOS" OR
   WHEN "INCIDENCIA ACTIVACION SERVICIOS" OR
   WHEN "INCIDENCIA PROV JAZZTEL" THEN DO:
      ASSIGN OrderFusion.CancellationReason = lcAdditionalInfo.
   END.
   WHEN "CITADA" THEN DO:
      ASSIGN OrderFusion.AppointmentDate = lcAdditionalInfo.
   END.
   /* installation done */
   WHEN "CERRADA" THEN DO:

      ASSIGN OrderFusion.FixedInstallationTS = ldeLastDate.
             OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_FINALIZED}.

      /* NOTE: do not change the memo text (checked in ordersender.i) */
      IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} THEN
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                          "Order",
                          STRING(Order.OrderID),
                          Order.CustNum,
                          "Order cancellation failed",
                          "Fixed Cancellation failed because installation was already in place").

      IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} OR
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} THEN DO:
         RUN orderinctrl.p(Order.OrderId, 0, TRUE).
      END.
      ELSE FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}.
   END.

   WHEN "PENDIENTE CANCELAR" OR
   WHEN "CANCELACION EN PROCESO" THEN DO:

      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_PENDING_CANCELLED}.

      IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} THEN .
      ELSE IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} THEN
         fSetOrderStatus(Order.OrderId,
                         {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL}).
      ELSE FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}.
   END.

   /* installation cancelled */
   WHEN "CANCELADA" THEN DO:

      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_CANCELLED}.

      IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} OR
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_MOBILE_LINE} OR
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} THEN DO:

         RUN closeorder.p(Order.OrderID, TRUE).

         IF RETURN-VALUE NE "" THEN DO:
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                             "Order",
                             STRING(Order.OrderID),
                             Order.CustNum,
                             "Order closing failed",
                             RETURN-VALUE).
            FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}.
         END.
      END.
      ELSE FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}.
   END.

END CASE.

RELEASE OrderFusion.
RELEASE FusionMessage.


   MESSAGE "Success" VIEW-AS ALERT-BOX.
