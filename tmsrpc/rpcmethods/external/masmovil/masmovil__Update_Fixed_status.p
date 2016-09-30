{Syst/commpaa.i}
gcBrand = "1".
{Syst/tmsconst.i}
{Func/fexternalapi.i}
{xmlrpc/xmlrpc_access.i}
{Mc/orderfusion.i}
{Func/orderfunc.i}

&GLOBAL-DEFINE RESULT_SUCCESS "00"
&GLOBAL-DEFINE RESULT_INVALID_FORMAT "01"
&GLOBAL-DEFINE RESULT_INVALID_ORDERID "02"

DEF VAR top_struct AS CHAR NO-UNDO.
DEF VAR lcTopStruct AS CHAR NO-UNDO.

DEF VAR lcNotificationID AS CHAR NO-UNDO.
DEF VAR lcNotificationTime AS CHAR NO-UNDO.
DEF VAR ldeNotificationTime AS DEC NO-UNDO.
DEF VAR lcNotificationType AS CHAR NO-UNDO.
DEF VAR lcOrderId AS CHAR NO-UNDO.
DEF VAR liOrderID AS INT NO-UNDO. 
DEF VAR lcNotificationStatus AS CHAR NO-UNDO. /*struct*/

DEF VAR lcStatusFields AS CHAR NO-UNDO. 

DEF VAR lcOrderType AS CHAR NO-UNDO. 
DEF VAR lcStatus AS CHAR NO-UNDO. 
DEF VAR lcStatusDescription AS CHAR NO-UNDO. 
DEF VAR lcAdditionalInfo AS CHAR NO-UNDO. 
DEF VAR lcLastDate AS CHAR NO-UNDO. 
DEF VAR ldeLastDate AS DEC NO-UNDO. 
DEF VAR lcresultStruct AS CHAR NO-UNDO. 

top_struct = get_struct(param_toplevel_id, "0").

lcTopStruct = validate_struct(top_struct,"notificationID!,notificationTime!,notificationType!,orderID!,Status!").

IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   lcNotificationID = get_string(top_struct,"notificationID")
   lcNotificationTime = get_string(top_struct,"notificationtime")
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
   lcLastDate = get_string(lcNotificationStatus, "lastDate").

IF gi_xmlrpc_error NE 0 THEN RETURN.


lcresultStruct = add_struct(response_toplevel_id,"").

ldeNotificationTime = _iso8601_to_timestamp(lcNotificationTime).
IF ldeNotificationTime EQ ? THEN DO:
   add_string(lcresultStruct, "resultCode", {&RESULT_INVALID_FORMAT}).
   add_string(lcresultStruct, "resultDescription", 
              "notificationTime is not a dateTime").
END.

ldeLastDate = _iso8601_to_timestamp(lcLastDate).
IF ldeLastDate EQ ? THEN DO:
   add_string(lcresultStruct, "resultCode", {&RESULT_INVALID_FORMAT}).
   add_string(lcresultStruct, "resultDescription", 
              "lastDate is not a dateTime").
   RETURN.
END.

IF lcOrderId BEGINS "Y" THEN
   lcOrderId = SUBSTRING(lcOrderId,2).

liOrderID = INT(lcOrderId) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
   add_string(lcresultStruct, "resultCode", {&RESULT_INVALID_FORMAT}).
   add_string(lcresultStruct, "resultDescription", 
              "Incorrect orderID syntax").
   RETURN.
END.

FIND FIRST Order NO-LOCK WHERE
           Order.Brand = gcBrand AND
           Order.OrderID = liOrderID NO-ERROR.
IF NOT AVAIL Order THEN DO:
   add_string(lcresultStruct, "resultCode", {&RESULT_INVALID_ORDERID}).
   add_string(lcresultStruct, "resultDescription", 
              "Order not found").
   RETURN.
END.

FIND FIRST OrderFusion EXCLUSIVE-LOCK WHERE
           OrderFusion.Brand = gcBrand AND
           OrderFusion.OrderId = liOrderID NO-ERROR.
IF NOT AVAIL OrderFusion THEN DO:
   add_string(lcresultStruct, "resultCode", {&RESULT_INVALID_ORDERID}).
   add_string(lcresultStruct, "resultDescription", 
              "Order type is incorrect").
   RETURN.
END.

/* HANDLING */

CREATE FusionMessage.
ASSIGN
   FusionMessage.MessageSeq = NEXT-VALUE(FusionMessageSeq)
   FusionMessage.OrderID = liOrderID
   FusionMessage.MsSeq = Order.MsSeq
   FusionMessage.CreatedTS = fMakeTS()
   FusionMessage.MessageID = lcNotificationID
   FusionMessage.MessageType = {&FUSIONMESSAGE_TYPE_UPDATE_STATUS}
   FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_HANDLED}
   FusionMessage.Source = {&FUSIONMESSAGE_SOURCE_MASMOVIL}
   FusionMessage.FixedStatus = lcStatus
   FusionMessage.FixedStatusDesc = lcStatusDescription
   FusionMessage.FixedStatusTS = ldeNotificationTime
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
             OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_FINALIZED}.
         
      IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} THEN DO:
         /* order handler will create fixed line request
            and mark status back to 79 if needed (pos orders without ICC) */
         IF Order.OrderType EQ {&ORDER_TYPE_MNP} THEN
            fSetOrderStatus(OrderFusion.OrderId,{&ORDER_STATUS_MNP}).
         ELSE fSetOrderStatus(OrderFusion.OrderId,{&ORDER_STATUS_NEW}).
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

add_string(lcresultStruct, "resultCode", {&RESULT_SUCCESS}).
add_string(lcresultStruct, "resultDescription", "success").

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
