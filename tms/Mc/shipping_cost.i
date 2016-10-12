&IF "{&SHIPPING_COST_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE SHIPPING_COST_I YES

{Syst/commali.i}
{Syst/tmsconst.i}

FUNCTION fGetShippingCost RETURNS DECIMAL
   (iiOrderID AS INTEGER):

   DEFINE VARIABLE ldeReturnValue AS DECIMAL INITIAL ? NO-UNDO.

   FOR FIRST OrderAction NO-LOCK WHERE
      OrderAction.Brand   = gcBrand   AND
      OrderAction.OrderId = iiOrderID AND
      OrderAction.ItemKey = {&ORDER_FEEMODEL_SHIPPING_COST}:

      ldeReturnValue = DECIMAL(OrderAction.ItemParam) NO-ERROR.

      IF ERROR-STATUS:ERROR
      THEN RETURN ?.
   END. 

   RETURN ldeReturnValue.

END FUNCTION.

&ENDIF