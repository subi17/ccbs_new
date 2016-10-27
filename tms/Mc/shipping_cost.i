&IF "{&SHIPPING_COST_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE SHIPPING_COST_I YES

{Syst/commali.i}
{Syst/tmsconst.i}

FUNCTION fTerminalOrder RETURNS LOGICAL
   (iiInvNum AS INTEGER):

   DEFINE BUFFER Invoice  FOR Invoice.
   DEFINE BUFFER InvRow   FOR InvRow.
   DEFINE VARIABLE lii AS INTEGER NO-UNDO.

   FOR
      FIRST Invoice FIELDS (InvNum) NO-LOCK WHERE
         Invoice.InvNum = iiInvNum,
      EACH InvRow FIELDS (InvNum BillCode) NO-LOCK WHERE
         InvRow.InvNum   =  Invoice.InvNum:
      lii = lii + 1.
      IF lii > 1 THEN RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fGetOrderAction RETURNS DECIMAL
   (iiOrderID  AS INTEGER,
    icItemType AS CHARACTER):

   DEFINE VARIABLE ldeReturnValue AS DECIMAL INITIAL ? NO-UNDO.

   FOR FIRST OrderAction NO-LOCK WHERE
      OrderAction.Brand   = gcBrand   AND
      OrderAction.OrderId = iiOrderID AND
      OrderAction.ItemType = icItemType:

      ldeReturnValue = DECIMAL(OrderAction.ItemKey) NO-ERROR.

      IF ERROR-STATUS:ERROR
      THEN RETURN ?.
   END. 

   RETURN ldeReturnValue.

END FUNCTION.

&ENDIF