&IF "{&ORDERFUSION_I}" NE "YES"
&THEN
&GLOBAL-DEFINE ORDERFUSION_I YES

{Func/date.i}
{Syst/tmsconst.i}

FUNCTION fFusionMessageError RETURNS CHAR
 (BUFFER ibFusionMessage FOR FusionMessage,
  icErrorDesc AS CHAR):

   FIND CURRENT ibFusionMessage EXCLUSIVE-LOCK.
         
   ASSIGN
      ibFusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      ibFusionMessage.AdditionalInfo = icErrorDesc
      ibFusionMessage.HandledTS = fMakeTS().

   RELEASE ibFusionMessage.

   RETURN icErrorDesc.
END.

FUNCTION _fCreateFusionMessage RETURNS LOGICAL
 (iiOrderID AS INT,
  icMessageType AS CHAR):
   
   CREATE FusionMessage.
   ASSIGN
      FusionMessage.MessageSeq = NEXT-VALUE(FusionMessageSeq)
      FusionMessage.OrderID = iiOrderID
      FusionMessage.CreatedTS = fMakeTS()
      FusionMessage.MessageID = GUID(GENERATE-UUID)
      FusionMessage.MessageType = icMessageType
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_NEW}
      FusionMessage.Source = {&FUSIONMESSAGE_SOURCE_TMS}. 

END.

FUNCTION fCreateFusionReserveNumberMessage RETURNS LOGICAL
 (iiOrderID AS INT,
  OUTPUT ocError AS CHAR):

   DEF BUFFER OrderFusion FOR OrderFusion.

   FIND OrderFusion NO-LOCK WHERE
        OrderFusion.Brand = Syst.Parameters:gcBrand AND
        OrderFusion.OrderID = iiOrderId NO-ERROR.
   IF NOT AVAIL OrderFusion THEN DO:
      ocError = "ERROR:Order data not found".
      RETURN FALSE.
   END.

   IF OrderFusion.FusionStatus NE {&FUSION_ORDER_STATUS_NEW} THEN DO:
      ocError = SUBST("ERROR:Incorrect fusion status &1", 
                OrderFusion.FusionStatus).
      RETURN FALSE.
   END.

   IF CAN-FIND(FIRST FusionMessage NO-LOCK WHERE
         FusionMessage.OrderID = OrderFusion.OrderID AND
         FusionMessage.MessageType = {&FUSIONMESSAGE_TYPE_RESERVE_NUMBER} AND
         FusionMessage.MessageStatus NE {&FUSIONMESSAGE_STATUS_ERROR}) THEN DO:
      ocError = "ERROR:Ongoing message".
      RETURN FALSE.
   END.

   _fCreateFusionMessage(OrderFusion.OrderId,
                         {&FUSIONMESSAGE_TYPE_RESERVE_NUMBER}).

   RETURN TRUE.
END.

FUNCTION fCreateFusionCreateOrderMessage RETURNS LOGICAL
 (iiOrderID AS INT,
  OUTPUT ocError AS CHAR):

   DEF BUFFER OrderFusion FOR OrderFusion.

   FIND OrderFusion NO-LOCK WHERE
        OrderFusion.Brand = Syst.Parameters:gcBrand AND
        OrderFusion.OrderID = iiOrderId NO-ERROR.
   IF NOT AVAIL OrderFusion THEN DO:
      ocError = "ERROR:Order data not found".
      RETURN FALSE.
   END.

   IF OrderFusion.FusionStatus NE {&FUSION_ORDER_STATUS_NEW} THEN DO:
      ocError = SUBST("ERROR:Incorrect fusion status &1",
                      OrderFusion.FusionStatus).
      RETURN FALSE.
   END.

   IF OrderFusion.FixedNumber = "" OR
      OrderFusion.FixedNumber EQ ? THEN DO:
      ocError = "ERROR:Fixed number is missing".
      RETURN FALSE.
   END.

   IF CAN-FIND(FIRST FusionMessage NO-LOCK WHERE
         FusionMessage.OrderID = OrderFusion.OrderID AND
         FusionMessage.MessageType = {&FUSIONMESSAGE_TYPE_CREATE_ORDER} AND
         FusionMessage.MessageStatus NE {&FUSIONMESSAGE_STATUS_ERROR}) THEN DO:
     ocError = "ERROR:Ongoing message".
     RETURN FALSE.
   END.

   _fCreateFusionMessage(OrderFusion.OrderId,
                         {&FUSIONMESSAGE_TYPE_CREATE_ORDER}).

   RETURN TRUE.
END.

&ENDIF
