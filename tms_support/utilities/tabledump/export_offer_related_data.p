DEFINE STREAM sout.
OUTPUT stream sout to Offer.d.

DEFINE STREAM souti.
OUTPUT stream souti to OfferItem.d.

DEFINE STREAM soutc.
OUTPUT stream soutc to OfferCriteria.d.

FOR EACH Offer NO-LOCK WHERE
         Offer.Brand  = "1" AND
         Offer.Active = TRUE:

   DISPLAY 'Exporting Offer ' Offer.Offer WITH FRAME a.

   EXPORT STREAM sout Offer.
  
   FOR EACH OfferItem NO-LOCK WHERE
            OfferItem.Brand = "1" AND
            OfferItem.Offer = Offer.Offer:
     EXPORT STREAM souti OfferItem.
   END.  

   FOR EACH OfferCriteria NO-LOCK WHERE
            OfferCriteria.Brand = "1" AND
            OfferCriteria.Offer = Offer.Offer:
     EXPORT STREAM soutc OfferCriteria.
   END.
END.

OUTPUT STREAM sout CLOSE.
OUTPUT STREAM souti CLOSE.
OUTPUT STREAM soutc CLOSE.

/*
/* Normally the billitem, reptext and terminalconf are already dumped.
   Thus commented out... */
OUTPUT stream sout to billitem.d.
OUTPUT stream souti to reptext.d.

DISPLAY 'Exporting Terminal prices' WITH FRAME b.

FOR EACH billitem NO-LOCK WHERE
         billitem.brand = "1" AND
         billitem.bigroup = "7" OR
         billitem.bigroup = "43":

   EXPORT STREAM sout billitem.

   FOR EACH RepText NO-LOCK WHERE
             RepText.Brand     = "1"    AND
             RepText.TextType  = 1 AND
             RepText.LinkCode  = BillItem.BillCode:
      EXPORT STREAM souti reptext.
   END.

END.

OUTPUT STREAM sout CLOSE.
OUTPUT STREAM souti CLOSE.

OUTPUT stream sout to termconf.d.
DISPLAY 'Exporting Terminal prices' WITH FRAME b.
FOR EACH terminalconf NO-LOCK:
  EXPORT STREAM sout terminalconf.
END.

OUTPUT STREAM sout CLOSE.
*/