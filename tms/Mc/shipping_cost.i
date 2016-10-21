&IF "{&SHIPPING_COST_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE SHIPPING_COST_I YES

{Syst/commali.i}
{Syst/tmsconst.i}

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