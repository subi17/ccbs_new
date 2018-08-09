{Syst/commali.i}
{Syst/eventval.i}
{Syst/tmsconst.i}
{Func/fixedlinefunc.i}
{Mc/orderfusion.i}

DEF INPUT PARAM piOrderID AS INT NO-UNDO. 

DEF VAR lhBuff             AS HANDLE NO-UNDO.
DEF VAR lcError            AS CHAR NO-UNDO. 

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun 
   {Func/lib/eventlog.i}
END.

/* check order exist */
/* check order exist */
FIND Order NO-LOCK WHERE
     Order.Brand = Syst.Var:gcBrand AND
     Order.OrderId = piOrderId NO-ERROR.
IF NOT AVAIL Order THEN 
   RETURN SUBST("Unknown Order id &1",STRING(piOrderId)).

FIND FIRST OrderFusion EXCLUSIVE-LOCK WHERE
           OrderFusion.Brand = Order.Brand AND
           OrderFusion.OrderId = Order.OrderId NO-ERROR.
IF NOT AVAIL OrderFusion OR
   NOT fIsConvergenceTariff(Order.CLIType) THEN 
   RETURN "Not convergent order".

IF Order.StatusCode EQ {&ORDER_STATUS_COMPANY_NEW} OR
   Order.StatusCode EQ {&ORDER_STATUS_COMPANY_MNP} OR
   Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_1} OR
   Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_2} OR
   Order.StatusCode EQ {&ORDER_STATUS_ROI_LEVEL_3} OR
   Order.StatusCode EQ {&ORDER_STATUS_IN_CONTROL} OR
   Order.StatusCode EQ {&ORDER_STATUS_MNP_REJECTED} OR
   Order.StatusCode EQ {&ORDER_STATUS_MORE_DOC_NEEDED}
   OR 
   (Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} AND
   (OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_ERROR} OR
    OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_CANCELLED} OR
    OrderFusion.FusionStatus EQ {&FUSION_ORDER_STATUS_NEW})) THEN DO:

   RUN Mc/closeorder.p(Order.OrderId,TRUE).

   IF RETURN-VALUE NE "" THEN
      RETURN "Order closing failed: " + STRING(RETURN-VALUE).

   IF llDoEvent THEN DO:
      lhBuff = BUFFER OrderFusion:HANDLE.
      RUN StarEventInitialize(lhBuff).
      RUN StarEventSetOldBuffer(lhBuff).
   END.

   ASSIGN
      OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_CANCELLED}
      OrderFusion.UpdateTS = Func.Common:mMakeTS().
   
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBuff).

   RETURN "OK: Order closed".
END.
ELSE IF Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE} THEN DO:

   IF fCreateFusionCancelOrderMessage(OrderFusion.OrderID,
                                      OUTPUT lcError) EQ FALSE THEN
      RETURN SUBST("Cancel message creation failed: &1", lcError).

   RETURN "OK: Fixed line order cancellation message created".

END.
ELSE RETURN SUBST("Incorrect order status: &1", Order.StatusCode).

RETURN "OK".
   
FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
END.
