{Syst/tmsconst.i}
{Func/log.i}
{Func/date.i}
{Func/masmovileif.i}
{Mc/orderfusion.i}
{Func/memo.i}

DEF INPUT PARAM piMessageSeq AS INT NO-UNDO.

DEF VAR lcFixedNumber AS CHAR NO-UNDO. 
DEF VAR lcError AS CHAR NO-UNDO. 
DEF VAR lcResultCode AS CHAR NO-UNDO. 
DEF VAR lcResultDesc AS CHAR NO-UNDO. 
DEF VAR liCount AS INT NO-UNDO. 

DEF BUFFER bFusionMessage FOR FusionMessage.

FIND FusionMessage EXCLUSIVE-LOCK WHERE
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

FIND OrderFusion EXCLUSIVE-LOCK WHERE
     OrderFusion.Brand = Syst.Parameters:gcBrand AND
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


lcError = fInitMMConnection().
IF lcError NE "" THEN RETURN lcError.

lcError = fMasGet_FixedNbr(OrderCustomer.ZipCode,
                 OUTPUT lcFixedNumber,
                 OUTPUT lcResultCode,
                 OUTPUT lcResultDesc).

IF lcError EQ "OK" THEN DO:

   ASSIGN
      OrderFusion.FixedNumber = lcFixedNumber
      OrderFusion.UpdateTS = fMakeTS()
      FusionMessage.HandledTS = OrderFusion.UpdateTS
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_HANDLED}.

   IF NOT fCreateFusionCreateOrderMessage(OrderFusion.OrderId,
                                          OUTPUT lcError) THEN DO:

      ASSIGN
         OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_ERROR}
         OrderFusion.FusionStatusDesc =
            SUBST("Create order message failed: &1", lcError).

   END.

   RETURN "OK".

END.
ELSE DO:

   ASSIGN
      OrderFusion.UpdateTS = fMakeTS()
      FusionMessage.HandledTS = OrderFusion.UpdateTS
      FusionMessage.MessageStatus = {&FUSIONMESSAGE_STATUS_ERROR}
      FusionMessage.AdditionalInfo = (IF lcResultDesc > "" THEN 
                                         lcResultDesc ELSE lcError).

   IF fCanRetryFusionMessage(
      BUFFER FusionMessage,
      lcError,
      lcResultCode,
      lcResultDesc) THEN RETURN "RETRY".

   ASSIGN
      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_ERROR}
      OrderFusion.FusionStatusDesc = "Fixed number reservation failed".

   fCreateMemo("Order",
               STRING(Order.OrderId),
               0,
               "Masmovil fixed number reservation failed",
               "",
               "",
               "TMS").

   RETURN SUBST("&1,&2,&3", lcError, lcResultCode, lcResultDesc).
END.

RETURN "OK".

FINALLY:
   xmlrpc_finalize().
END.
