{commali.i}
{tmsconst.i}
{log.i}
{date.i}
{masmovileif.i}
{orderfusion.i}

DEF INPUT PARAM piMessageSeq AS INT NO-UNDO.

DEF VAR lcFixedNumber AS CHAR NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR lcResultCode AS CHAR NO-UNDO. 
DEF VAR lcResultDesc AS CHAR NO-UNDO. 
DEF VAR llError AS LOG NO-UNDO. 
DEF VAR liCount AS INT NO-UNDO. 

DEF BUFFER bFusionMessage FOR FusionMessage.

FIND FusionMessage NO-LOCK WHERE
     FusionMessage.MessageSeq = piMessageSeq NO-ERROR.

IF NOT AVAIL FusionMessage THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                              "FusionMessage not found").

FIND FIRST Order NO-LOCK WHERE
           Order.Brand = gcBrand AND
           Order.OrderId = FusionMessage.OrderID NO-ERROR.
IF NOT AVAIL Order THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                              "Order not found").

FIND FIRST OrderFusion NO-LOCK WHERE
           OrderFusion.Brand = gcBrand AND
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


lcError = fMasCreate_FixedLineOrder(Order.OrderID,
                                    OUTPUT lcResultCode,
                                    OUTPUT lcResultDesc).

FIND CURRENT FusionMessage EXCLUSIVE-LOCK.

IF lcError EQ "" THEN DO:

   FIND CURRENT OrderFusion EXCLUSIVE-LOCK.
   
   ASSIGN
      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_ONGOING}
      FusionMessage.HandledTS = fMakeTS()
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_HANDLED}.

   RELEASE OrderFusion.
   RELEASE FusionMessage.

   RETURN "".

END.
ELSE DO:

   ASSIGN
      FusionMessage.HandledTS = fMakeTS()
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.AdditionalInfo = (IF lcResultDesc > "" THEN 
                                      lcResultDesc ELSE lcError).

   /* automatic resending check */
   IF lcError BEGINS "NW_ERROR" OR
      LOOKUP(lcResultCode,{&MASMOVIL_RETRY_ERROR_CODES}) > 0 THEN DO:

      FOR EACH bFusionMessage NO-LOCK WHERE
               bFusionMessage.OrderID = FusionMessage.OrderID AND
               bFusionMessage.MessageType = FusionMessage.MessageType:
         liCount = liCount + 1.
      END.

      IF liCount < 5 THEN DO:
         RELEASE FusionMessage.
         RETURN "RETRY".
      END.
   END.

   FIND CURRENT OrderFusion EXCLUSIVE-LOCK.

   ASSIGN
      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_ERROR}
      OrderFusion.AdditionalInfo = "Masmovil fixed number reservation failed".

   DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
              "Order",
              STRING(Order.OrderId),
              0,
              OrderFusion.AdditionalInfo,
              lcError).

   RELEASE OrderFusion.
   RELEASE FusionMessage.

   fLogError(lcerror).
END.

RETURN "".

