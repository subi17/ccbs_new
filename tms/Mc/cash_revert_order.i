&IF "{&CASH_REVERT_ORDER_I}" NE "YES" 
&THEN
&GLOBAL-DEFINE CASH_REVERT_ORDER_I YES

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/fcreditreq.i}

FUNCTION fCashRevertOrder RETURNS CHARACTER
   ( iiOrderID AS INTEGER):

   DEFINE VARIABLE lcResult AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liReq    AS INTEGER   NO-UNDO.

   DEFINE BUFFER Invoice FOR Invoice.
   DEFINE BUFFER Order FOR Order.
   
   FIND Order NO-LOCK WHERE
      Order.Brand   = gcBrand   AND
      Order.OrderID = iiOrderID
   NO-ERROR.
   
   IF NOT AVAILABLE Order
   THEN RETURN "ERROR:Order not available". 

   IF Order.InvNum > 0
   THEN DO:
      IF NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE Invoice.InvNum = Order.InvNum AND Invoice.CrInvNum > 0)
      THEN lcResult = fCashInvoiceCreditnote(Order.Invnum, "1010").

      IF lcResult > "" THEN
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                           "Order",
                           STRING(Order.OrderId),
                           Order.Custnum,
                           "CREDIT NOTE CREATION FAILED",
                           lcResult).
   END.

   IF Order.OrderType EQ {&ORDER_TYPE_RENEWAL}
   THEN DO:
      IF CAN-FIND(FIRST MsRequest NO-LOCK WHERE
                        MsRequest.MsSeq      = Order.MsSeq                     AND
                        MsRequest.ReqType    = {&REQTYPE_REVERT_RENEWAL_ORDER} AND
                        MsRequest.ReqStatus  NE {&REQUEST_STATUS_CANCELLED}    AND
                        MsRequest.ReqIParam1 = Order.OrderId                   AND
                        MsRequest.ReqSource  = {&REQUEST_SOURCE_ORDER_CANCELLATION})
      THEN RETURN "".

      liReq = fRevertRenewalOrderRequest(Order.MsSeq,
                                         Order.OrderId,
                                         katun,
                                         fMakeTS(),
                                         {&REQUEST_SOURCE_ORDER_CANCELLATION},
                                         OUTPUT lcResult).

      IF liReq = 0
      THEN DO:
         DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                           "Order",
                           STRING(Order.OrderId),
                           Order.Custnum,
                           "RENEWAL REVERTION FAILED",
                           lcResult).
         RETURN "ERROR:Revert renewal failed:" + lcResult.
      END.
   END.

   RETURN "".

END.

&ENDIF