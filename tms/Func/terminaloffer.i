&IF "{&terminaloffer_i}" NE "YES"
&THEN

&GLOBAL-DEFINE terminaloffer_i YES

FUNCTION fGetTerminalOfferItemId RETURN INTEGER
  (INPUT pcOffer AS CHARACTER, 
   INPUT pcGroup AS CHARACTER,
   INPUT pdeTime AS DECIMAL):

   DEFINE VARIABLE iOfferItemId AS INTEGER NO-UNDO. 
   iOfferItemId = -1.

   DEFINE BUFFER xOfferItem FOR OfferItem.
   DEFINE BUFFER xBillItem  FOR BillItem.

   LoopItems:
   FOR EACH xOfferItem WHERE 
      xOfferItem.Brand = Syst.Var:gcBrand  AND
      xOfferItem.Offer     = pcOffer AND 
      xOfferItem.ItemType  = "BillItem" AND
      xOfferItem.EndStamp   >= pdeTime AND
      xOfferItem.BeginStamp <= pdeTime NO-LOCK:
      FIND xBillItem WHERE 
           xBillItem.Brand = Syst.Var:gcBrand AND 
           xBillItem.BillCode = xOfferItem.ItemKey AND
           xBillItem.BIGroup = pcGroup NO-LOCK NO-ERROR.
      IF AVAIL xBillItem THEN DO:
         iOfferItemId = xOfferItem.OfferItemId.
         LEAVE LoopItems.
      END.
   END.
   RETURN iOfferItemId.
END.

&ENDIF