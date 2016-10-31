&IF "{&SHIPPING_COST_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE SHIPPING_COST_I YES

{Syst/commali.i}
{Syst/tmsconst.i}

FUNCTION fTerminalOrder RETURNS LOGICAL
   (INPUT ilPayType AS LOGICAL,
    INPUT icOffer AS CHARACTER,
    INPUT ideStamp  AS DECIMAL):

   DEFINE BUFFER OfferItem FOR OfferItem.
   DEFINE BUFFER BillItem  FOR BillItem.

   /* Prepaid Order */
   IF ilPayType
   THEN RETURN FALSE.

   /* Terminal Financing in direct channel deployment on 09.07.2014 8:00 CET */
   IF ideStamp < 20140709.28800
   THEN RETURN FALSE.

   /* Check Terminal billcode */
   FOR EACH OfferItem NO-LOCK WHERE
            OfferItem.Brand     = gcBrand   AND
            OfferItem.Offer     = icOffer   AND
            OfferItem.ItemType  = "BillItem" AND
            OfferItem.EndStamp  >= ideStamp,
      FIRST BillItem NO-LOCK WHERE
            BillItem.Brand    = gcBrand           AND
            BillItem.BillCode = OfferItem.ItemKey AND
            BillItem.BIGroup  = "7":
      IF OfferItem.BeginStamp <= ideStamp
      THEN RETURN TRUE.
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