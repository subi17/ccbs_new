DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
gcBrand = "1".
{Syst/tmsconst.i}
{Func/fexternalapi.i}
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Mc/orderfusion.i}
{Func/orderfunc.i}

&GLOBAL-DEFINE RESULT_SUCCESS "00"
&GLOBAL-DEFINE RESULT_INVALID_FORMAT "01"
&GLOBAL-DEFINE RESULT_INVALID_ORDERID "02"
&GLOBAL-DEFINE RESULT_INVALID_STATUS "03"

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

IF lcStatus EQ "CERRADA" AND
   NOT OrderFusion.FixedNumber > "" THEN DO:
   add_string(lcresultStruct, "resultCode", {&RESULT_INVALID_STATUS}).
   add_string(lcresultStruct, "resultDescription", 
              "Order fixed number is missing").
   RETURN.
END.

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

   add_string(lcresultStruct, "resultCode", {&RESULT_SUCCESS}).
   add_string(lcresultStruct, "resultDescription", "success").
END.
   
IF OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_FINALIZED} AND
   FusionMessage.FixedStatus NE "CERRADA" THEN DO:
      
   ASSIGN
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.AdditionalInfo = "Status update is not allowed after CERRADA".
   
   add_string(lcresultStruct, "resultCode", {&RESULT_INVALID_STATUS}).
   add_string(lcresultStruct, "resultDescription", 
              FusionMessage.AdditionalInfo).
   RETURN.
END.

IF OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_CANCELLED} AND
   FusionMessage.FixedStatus NE "CANCELADA" THEN DO:
    
   ASSIGN
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.AdditionalInfo = "Status update is not allowed after CANCELADA".
   
   add_string(lcresultStruct, "resultCode", {&RESULT_INVALID_STATUS}).
   add_string(lcresultStruct, "resultDescription", FusionMessage.AdditionalInfo).
   RETURN.
END.

IF ldeNotificationTime < OrderFusion.FixedStatusTS THEN DO:
      
   ASSIGN
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.AdditionalInfo = "notificationTime is older than the value in the latest received message".
   
   add_string(lcresultStruct, "resultCode", {&RESULT_INVALID_STATUS}).
   add_string(lcresultStruct, "resultDescription", 
              FusionMessage.AdditionalInfo).
   RETURN.
END.

ASSIGN
   OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_INITIALIZED} /*YTS-10051*/
      WHEN OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_ERROR} OR
           OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_NEW}
   OrderFusion.FusionStatusDesc = ""
      WHEN OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_ERROR} OR
           OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_NEW}
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
         RUN Mc/orderinctrl.p(Order.OrderId, 0, TRUE).
      END.
      ELSE FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}.
   END.

   WHEN "PENDIENTE CANCELAR" OR
   WHEN "CANCELACION EN PROCESO" THEN DO:
      
      ASSIGN 
         OrderFusion.CancellationReason = lcAdditionalInfo.
         OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_PENDING_CANCELLED}.

      IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} THEN .
      ELSE IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} THEN
         fSetOrderStatus(Order.OrderId,
                         {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL}).
      ELSE FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}.
   END.

   /* installation cancelled */ 
   WHEN "CANCELADA" THEN DO:

      ASSIGN OrderFusion.CancellationReason = lcAdditionalInfo
             OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_CANCELLED}.
         
      IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} OR
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_MOBILE_LINE} OR
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} THEN DO:

         RUN Mc/closeorder.p(Order.OrderID, TRUE).

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

add_string(lcresultStruct, "resultCode", {&RESULT_SUCCESS}).
add_string(lcresultStruct, "resultDescription", "success").

FINALLY:
   ghAuthLog::TransactionId = "690".
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
END.
