{tmsconst.i}
{log.i}
{date.i}
{masmovileif.i}
{orderfusion.i}
{memo.i}

DEF INPUT PARAM piMessageSeq AS INT NO-UNDO.

DEF VAR lcFixedNumber AS CHAR NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR lcErrorDesc AS CHAR NO-UNDO. 
DEF VAR liCount AS INT NO-UNDO. 

DEF BUFFER bFusionMessage FOR FusionMessage.

FIND FusionMessage NO-LOCK WHERE
     FusionMessage.MessageSeq = piMessageSeq NO-ERROR.

IF NOT AVAIL FusionMessage THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                              "FusionMessage not found").

FIND FIRST Order NO-LOCK WHERE
           Order.Brand = Syst.Parameters:gcBrand AND
           Order.OrderId = FusionMessage.OrderID NO-ERROR.
IF NOT AVAIL Order THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                              "Order not found").

FIND FIRST OrderFusion NO-LOCK WHERE
           OrderFusion.Brand = Order.Brand AND
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

IF OrderFusion.FixedNumber > "" THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                             "FixedNumber already assigned").

FIND FIRST OrderCustomer NO-LOCK WHERE
           OrderCustomer.Brand = Order.Brand AND
           OrderCustomer.OrderID = OrderFusion.OrderID AND
           OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL} 
   NO-ERROR.
IF NOT AVAIL OrderCustomer THEN
   RETURN fFusionMessageError(BUFFER FusionMessage,
                              "OrderCustomer not found").

lcError = fMasGet_FixedNbr(OrderCustomer.ZipCode,
                 OUTPUT lcFixedNumber,
                 OUTPUT lcErrorDesc).

FIND CURRENT FusionMessage EXCLUSIVE-LOCK.

IF lcError EQ "" THEN DO:

   FIND CURRENT OrderFusion EXCLUSIVE-LOCK.
   
   ASSIGN
      OrderFusion.FixedNumber = lcFixedNumber
      FusionMessage.HandledTS = fMakeTS()
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_HANDLED}.

   IF NOT fCreateFusionCreateOrderMessage(OrderFusion.OrderId,
                                          OUTPUT lcError) THEN DO:

      ASSIGN
         OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_ERROR}
         OrderFusion.AdditionalInfo =
            SUBST("Create order message failed: &1", lcError).

      RELEASE OrderFusion.
   END.

   RETURN "".

END.
ELSE DO:

   ASSIGN
      FusionMessage.HandledTS = fMakeTS()
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.AdditionalInfo = lcErrorDesc. 
   
   IF lcError BEGINS "NW_ERROR" THEN DO:

      FOR EACH bFusionMessage NO-LOCK WHERE
               bFusionMessage.OrderID = FusionMessage.OrderID AND
               bFusionMessage.MessageType = FusionMessage.MessageType:
         liCount = liCount + 1.
      END.

      IF liCount < 5 THEN RETURN "RETRY".
      ELSE DO:

         FIND OrderFusion EXCLUSIVE-LOCK WHERE
              OrderFusion.OrderID = FusionMessage.OrderID NO-ERROR.

         ASSIGN
            OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_ERROR}
            OrderFusion.AdditionalInfo = lcErrorDesc.

         fCreateMemo("Order",
                     STRING(Order.OrderId),
                     0,
                     "Masmovil fixed number reservation failed",
                     lcErrorDesc,
                     "",
                     "TMS").

         RETURN "".
      END.
   END.

   RELEASE FusionMessage.

   RETURN lcError.
END.

RETURN "".

