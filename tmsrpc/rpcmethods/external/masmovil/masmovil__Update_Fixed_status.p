{Syst/commpaa.i}
gcBrand = "1".
{Syst/tmsconst.i}
{Func/fexternalapi.i}
{xmlrpc/xmlrpc_access.i}
{Mc/orderfusion.i}
{Func/orderfunc.i}

DEF VAR top_struct AS CHAR NO-UNDO.
DEF VAR lcTopStruct AS CHAR NO-UNDO.

DEF VAR lcNotificationID AS CHAR NO-UNDO.
DEF VAR lcNotificationTime AS DEC NO-UNDO.
DEF VAR lcNotificationType AS CHAR NO-UNDO.
DEF VAR lcOrderId AS CHAR NO-UNDO.
DEF VAR liOrderID AS INT NO-UNDO. 
DEF VAR lcNotificationStatus AS CHAR NO-UNDO. /*struct*/

DEF VAR lcStatusFields AS CHAR NO-UNDO. 

DEF VAR lcOrderType AS CHAR NO-UNDO. 
DEF VAR lcStatus AS CHAR NO-UNDO. 
DEF VAR lcStatusDescription AS CHAR NO-UNDO. 
DEF VAR lcAdditionalInfo AS CHAR NO-UNDO. 
DEF VAR ldeLastDate AS DEC NO-UNDO. 

top_struct = get_struct(param_toplevel_id, "0").

lcTopStruct = validate_struct(top_struct,"notificationID!,notificationTime!,notificationType!,orderID!,Status!").

IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   lcNotificationID = get_string(top_struct,"notificationID")
   lcNotificationTime = get_timestamp(top_struct,"notificationtime")
   lcNotificationType = get_string(top_struct,"notificationType")
   lcOrderId = get_string(top_struct,"orderID")
   lcNotificationStatus = get_struct(top_struct,"Status").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStatusFields = validate_struct(lcNotificationStatus,"OrderType,ServiceType,Status!,StatusDescription!,additionalInfo,lastDate!").
   
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   lcOrderType = get_string(lcNotificationStatus, "OrderType")
      WHEN LOOKUP("OrderType", lcStatusFields) > 0 
   lcStatus = get_string(lcNotificationStatus, "Status")
   lcStatusDescription = get_string(lcNotificationStatus, "StatusDescription")
      WHEN LOOKUP("StatusDescription", lcStatusFields) > 0 
   lcAdditionalInfo = get_string(lcNotificationStatus, "additionalInfo")
      WHEN LOOKUP("additionalInfo", lcStatusFields) > 0 
   ldeLastDate = get_timestamp(lcNotificationStatus, "lastDate").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF lcOrderId BEGINS "Y" THEN
   lcOrderId = SUBSTRING(lcOrderId,2).

liOrderID = INT(lcOrderId) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
   RETURN appl_err("Incorrect orderID syntax").

FIND FIRST Order NO-LOCK WHERE
           Order.Brand = gcBrand AND
           Order.OrderID = liOrderID NO-ERROR.
IF NOT AVAIL Order THEN 
   RETURN appl_err("Order not found").

FIND FIRST OrderFusion EXCLUSIVE-LOCK WHERE
           OrderFusion.Brand = gcBrand AND
           OrderFusion.OrderId = liOrderID NO-ERROR.
IF NOT AVAIL OrderFusion THEN 
   RETURN appl_err("Order data is missing").

/* HANDLING */

CREATE FusionMessage.
ASSIGN
   FusionMessage.MessageSeq = NEXT-VALUE(FusionMessageSeq)
   FusionMessage.OrderID = liOrderID
   FusionMessage.CreatedTS = fMakeTS()
   FusionMessage.MessageID = lcNotificationID
   FusionMessage.MessageType = {&FUSIONMESSAGE_TYPE_UPDATE_STATUS}
   FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_HANDLED}
   FusionMessage.Source = {&FUSIONMESSAGE_SOURCE_MASMOVIL}
   FusionMessage.FixedStatus = lcStatus
   FusionMessage.FixedStatusDesc = lcStatusDescription
   FusionMessage.FixedStatusTS = lcNotificationTime
   FusionMessage.OrderType = lcOrderType
   FusionMessage.AdditionalInfo = lcAdditionalInfo.

CASE FusionMessage.FixedStatus:

   WHEN "CERRADA PARCIAL" OR
   WHEN "EN PROCESO - NO CANCELABLE" THEN DO:

      IF OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_INITIALIZED} THEN
         OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_ONGOING}.
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

      /* MNP/NEW indirect channel */
      IF Order.OrderType NE {&ORDER_TYPE_STC} AND
         LOOKUP(Order.OrderChannel, {&ORDER_CHANNEL_INDIRECT}) > 0 THEN DO:

         ASSIGN 
            OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_PENDING_FINALIZED}.
            
         IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} THEN DO:
            /* TODO: create fixed line subs. creation request */
            fSetOrderStatus(Order.OrderId, 
                            {&ORDER_STATUS_PENDING_MOBILE_LINE}).
         END.
         ELSE . /* TODO: error handling */
         
      END.
      /* STC or MNP/NEW direct channel */
      ELSE DO:

         ASSIGN OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_FINALIZED}.
         
         IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} THEN DO:

            IF Order.OrderType EQ {&ORDER_TYPE_MNP} THEN DO:
               /* TODO: create fixed line subs. creation request */
               fSetOrderStatus(OrderFusion.OrderId,{&ORDER_STATUS_MNP}).
            END.
            ELSE fSetOrderStatus(OrderFusion.OrderId,{&ORDER_STATUS_NEW}).
         END.
         ELSE . /* TODO: error handling */

      END.
   END.
   /* installation cancelled */ 
   WHEN "CANCELADA" THEN DO:

      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_CANCELLED}.
         
      IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} OR
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_MOBILE_LINE} THEN DO:
         RUN closeorder.p(Order.OrderID, TRUE).
      END.
      ELSE . /* TODO: error handling*/ 
   END.

END CASE.

RELEASE OrderFusion.
RELEASE FusionMessage.

add_boolean(response_toplevel_id,?,TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
