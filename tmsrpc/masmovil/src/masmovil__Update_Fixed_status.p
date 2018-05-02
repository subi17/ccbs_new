DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
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

DEF VAR lcTenant            AS CHAR NO-UNDO.
DEF VAR lcOrderType         AS CHAR NO-UNDO. 
DEF VAR lcStatus            AS CHAR NO-UNDO. 
DEF VAR lcStatusDescription AS CHAR NO-UNDO. 
DEF VAR lcAdditionalInfo    AS CHAR NO-UNDO. /* struct */ 
DEF VAR lcLastDate          AS CHAR NO-UNDO. 
DEF VAR ldeLastDate         AS DEC  NO-UNDO. 
DEF VAR lcresultStruct      AS CHAR NO-UNDO. 
DEF VAR liDBCount           AS INTE NO-UNDO.

/* Variables for AdditionalInfo struct. */
DEF VAR lcAdditionalInfoFields AS CHAR    NO-UNDO.
DEF VAR llOldStructure         AS LOGICAL NO-UNDO.

DEF VAR lcCita       AS CHAR NO-UNDO.
DEF VAR lcCanDS      AS CHAR NO-UNDO.
DEF VAR lcPortStat   AS CHAR NO-UNDO.
DEF VAR lcPortDate   AS CHAR NO-UNDO.
DEF VAR lcRouterStat AS CHAR NO-UNDO.
DEF VAR lcIUA        AS CHAR NO-UNDO.
DEF VAR lcNebaErr    AS CHAR NO-UNDO.

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
   lcLastDate = get_string(lcNotificationStatus, "lastDate").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN lcAdditionalInfo = get_struct(lcNotificationStatus, "additionalInfo")
   WHEN LOOKUP("additionalInfo", lcStatusFields) > 0 .
IF gi_xmlrpc_error NE 0 THEN DO:
   gi_xmlrpc_error = 0.
   ASSIGN lcAdditionalInfo = get_string(lcNotificationStatus, "additionalInfo")
      WHEN LOOKUP("additionalInfo", lcStatusFields) > 0. 
   IF gi_xmlrpc_error NE 0 THEN 
      RETURN.
   ELSE
      llOldStructure = TRUE.
END.

IF NOT llOldStructure AND LOOKUP("additionalInfo", lcStatusFields) > 0 THEN DO:
   lcAdditionalInfoFields = validate_struct(lcAdditionalInfo,"cita,canDS,portStat,portDate,IUA,routerStat").   
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   ASSIGN
     lcCita = get_string(lcAdditionalInfo, "cita")
        WHEN LOOKUP("cita", lcAdditionalInfoFields) > 0 
     lcCanDS = get_string(lcAdditionalInfo, "canDS")
        WHEN LOOKUP("canDS", lcAdditionalInfoFields) > 0 
     lcPortStat = get_string(lcAdditionalInfo, "portStat")
        WHEN LOOKUP("portStat", lcAdditionalInfoFields) > 0 
     lcPortDate = get_string(lcAdditionalInfo, "portDate")
        WHEN LOOKUP("portDate", lcAdditionalInfoFields) > 0 
     lcIUA =  get_string(lcAdditionalInfo, "IUA")
        WHEN LOOKUP("IUA", lcAdditionalInfoFields) > 0
     lcRouterStat = get_string(lcAdditionalInfo, "routerStat")
        WHEN LOOKUP("routerStat", lcAdditionalInfoFields) > 0. 
   IF gi_xmlrpc_error NE 0 THEN RETURN.
END.


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

FOR FIRST Order WHERE Order.Brand = Syst.Var:gcBrand AND Order.OrderId = liOrderID TENANT-WHERE TENANT-ID() > -1 NO-LOCK:
    ASSIGN lcTenant = BUFFER-TENANT-NAME(Order).                
END.

IF NOT AVAIL Order OR lcTenant = "" THEN DO:
   add_string(lcresultStruct, "resultCode", {&RESULT_INVALID_ORDERID}).
   add_string(lcresultStruct, "resultDescription", "Order not found").
   RETURN.
END.

DO liDBCount = 1 TO NUM-DBS:
    SET-EFFECTIVE-TENANT(lcTenant, LDBNAME(liDBCount)).
END.

FIND FIRST OrderFusion EXCLUSIVE-LOCK WHERE
           OrderFusion.Brand = Syst.Var:gcBrand AND
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
   FusionMessage.CreatedTS = Func.Common:mMakeTS()
   FusionMessage.UpdateTS = FusionMessage.CreatedTS
   FusionMessage.MessageID = lcNotificationID
   FusionMessage.MessageType = {&FUSIONMESSAGE_TYPE_UPDATE_STATUS}
   FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_HANDLED}
   FusionMessage.Source = {&FUSIONMESSAGE_SOURCE_MASMOVIL}
   FusionMessage.FixedStatus = lcStatus
   FusionMessage.FixedStatusDesc = lcStatusDescription
   FusionMessage.FixedStatusTS = ldeNotificationTime
   FusionMessage.OrderType = lcOrderType
.

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
         
         Func.Common:mWriteMemo("Order",
                 STRING(Order.OrderID),
                 Order.CustNum,
                 "Order handling stopped",
                 "Subscription is terminated, Convergent order cannot proceed").
      END.

   END.

   WHEN "CITADA" THEN DO:
       IF llOldStructure THEN
          ASSIGN OrderFusion.AppointmentDate = lcAdditionalInfo.
       ELSE
          ASSIGN OrderFusion.AppointmentDate = lcCita WHEN lcCita <> "".
   END.
   
   /*NEBA*/
   WHEN "PENDIENTE_INSTALATION" THEN DO:
      /*NEBA: no actions in this phase*/
      
   END.

   /* installation done */
   WHEN "CERRADA" THEN DO:
       
      ASSIGN 
         OrderFusion.FixedInstallationTS = ldeLastDate
         OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_FINALIZED}.
      
      /* NOTE: do not change the memo text (checked in ordersender.i) */
      IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} THEN
         Func.Common:mWriteMemo("Order",
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
      IF llOldStructure THEN
         ASSIGN OrderFusion.CancellationReason = lcAdditionalInfo.
      ELSE 
         ASSIGN OrderFusion.CancellationReason = lcCanDS WHEN lcCanDS <> "".

      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_PENDING_CANCELLED}.

      IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} THEN .
      ELSE IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} THEN
         fSetOrderStatus(Order.OrderId,
                         {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL}).
      ELSE FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}.
   END.

   /* installation cancelled */ 
   WHEN "CANCELADA" THEN DO:
      /*NEBA TODO*/
      /*If this is cancelled in NEBA PERMANENCY period,
        the penalty must be handled.*/
     IF Order.Clitype MATCHES ("*CONTFHNB*") THEN DO:
        Func.Common:mWriteMemo("Order",
                             STRING(Order.OrderID),
                             Order.CustNum,
                             "NEBA cancellation starts",
                             "").
 
        RUN Mm/neba_cancellation_action.p(OrderFusion.OrderID, 
                                          OUTPUT lcNebaErr).
        IF lcNebaErr NE "" THEN DO:
           Func.Common:mWriteMemo("Order",
                             STRING(Order.OrderID),
                             Order.CustNum,
                             "NEBA cancellation fee creation failed",
                             lcNebaErr).
           FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}.
        END.
        ELSE DO:
           Func.Common:mWriteMemo("Order",
                             STRING(Order.OrderID),
                             Order.CustNum,
                             "NEBA cancellation done",
                             "").
        END.
        
     END.

      IF llOldStructure THEN
         ASSIGN OrderFusion.CancellationReason = lcAdditionalInfo.
      ELSE
         ASSIGN OrderFusion.CancellationReason = lcCanDS WHEN lcCanDS <> "".

      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_CANCELLED}.
         
      IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} OR
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_MOBILE_LINE} OR
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL} THEN DO:

         RUN Mc/closeorder.p(Order.OrderID, TRUE).

         IF RETURN-VALUE NE "" THEN DO:
            Func.Common:mWriteMemo("Order",
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

IF llOldStructure THEN
   ASSIGN FusionMessage.AdditionalInfo = lcAdditionalInfo.
ELSE DO:
   IF LOOKUP(FusionMessage.FixedStatus,"CERRADA,CERRADA PARCIAL,CITADA,INCIDENCIA RED,INCIDENCIA TECNICO EN CASA") <> 0 THEN
      ASSIGN FusionMessage.AdditionalInfo = lcCita WHEN lcCita <> "".
   IF LOOKUP(FusionMessage.FixedStatus,"CANCELADA,CANCELACION EN PROCESO") <> 0 THEN
      ASSIGN FusionMessage.AdditionalInfo = lcCanDS WHEN lcCanDS <> "".
END.

ASSIGN
   OrderFusion.IUA        = lcIUA        WHEN lcIUA        <> ""
   OrderFusion.portStat   = lcPortStat   WHEN lcPortStat   <> ""
   OrderFusion.portDate   = lcPortDate   WHEN lcPortDate   <> ""
   OrderFusion.routerStat = lcRouterStat WHEN lcRouterStat <> "".

RELEASE OrderFusion.
RELEASE FusionMessage.

add_string(lcresultStruct, "resultCode", {&RESULT_SUCCESS}).
add_string(lcresultStruct, "resultDescription", "success").

FINALLY:
   ghAuthLog::TransactionId = "690".
   END.
