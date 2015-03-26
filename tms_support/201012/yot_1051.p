{commpaa.i}
katun = "Qvantel".
gcBrand = "1".

DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 

etime(true).
DEFINE VARIABLE i AS INTEGER label "FAT created" NO-UNDO. 
DEFINE VARIABLE liSkipped AS INTEGER label "FAT already exist" NO-UNDO. 
DEFINE VARIABLE liError AS INTEGER label "Error"  NO-UNDO. 
DEFINE VARIABLE liOngoing AS INTEGER label "Ongoing order" NO-UNDo.
DEFINE VARIABLE lcMSISDN AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeFat AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldaActivationDate AS DATE NO-UNDO. 

def stream sout.
output stream sout to /apps/yoigo/tms_support/201012/yot_1046.txt.
      
put stream sout unformatted
   "MSISDN|SUBSCR.ID|FAT CREATED|PERIOD|REASON" skip.

DEFINE VARIABLE liTest AS INTEGER NO-UNDO. 
LOOPPI:
FOR EACH order where
   order.brand = gcBrand and
   order.crstamp >= 20101201 AND
   order.crstamp < 20111201 NO-LOCK by order.crstamp:

   if lookup(order.statuscode,"7,8") > 0 THEN NEXT LOOPPI.
   if order.ordertype = 2 then next.
   
   IF CAN-FIND(FIRST OfferItem WHERE
                     OfferItem.Brand = gcBrand AND
                     OfferItem.Offer = Order.Offer AND
                     OfferItem.ItemType = "BundleItem" AND
                     OfferItem.ItemKey = "MDUB" AND
                     OfferItem.EndStamp >= Order.CrStamp AND
                     OfferItem.BeginStamp <= Order.CrStamp) OR
      CAN-FIND(FIRST OrderAction WHERE
                     OrderAction.Brand = gcBrand AND
                     OrderAction.OrderId = Order.OrderID AND
                     OrderAction.ItemType = "BundleItem" AND
                     OrderAction.ItemKey = "MDUB") THEN DO:

      disp order.crstamp i liSkipped liOngoing liError with frame a 1 down.
      pause 0.

      ldeFat = 0.
/*
      liTest = litest + 1.
      if litest <= 100 then next LOOPPI.
*/
      if order.statuscode ne "6" then do:
         put stream sout unformatted
            order.cli "|" 
            order.msseq "|0||"
            "SUBSCRIPTION NOT ACTIVATE YET" skip.
         liOngoing = liOngoing + 1.
         next LOOPPI.
      end.
   
      find MobSub where
           MobSub.msseq = order.msseq NO-LOCK no-error.

      IF AVAIL MobSub then do:
         lcMSISDN = MobSub.cli.
         ldaActivationDate = MobSub.ActivationDate.
      end.
      else do:
         
         find termmobsub where
              termmobsub.msseq = order.msseq NO-LOCK no-error.
         
         IF NOT AVAIL termmobsub then do:
         
            put stream sout unformatted
               order.cli "|" 
               order.msseq "|0||"
               "ERROR:Subscription not found" skip.
             liError = liError + 1.
             next LOOPPI.

         end.
         
         lcMSISDN = termmobsub.cli.
         ldaActivationDate = TermMobSub.ActivationDate.
      end.
      
      if can-find(first fatime where
                        fatime.brand = gcbrand and
                        fatime.msseq = order.msseq and
                        fatime.ftgrp = "BONO8CP" use-index MobSub) then do:
      
         FOR EACH fatime where
                  fatime.brand = gcbrand and
                  fatime.msseq = order.msseq and
                  fatime.ftgrp = "BONO8CP" use-index MobSub:
         /*    ldeFat = ldeFat + fatime.amt. */
            put stream sout unformatted
               lcMSISDN "|" 
               order.msseq "|"
               fatime.amt "|"
               fatime.period "|"
               "FAT ALREADY EXIST" skip.
         end.
         liSkipped = liSkipped + 1.
         next LOOPPI.
      end.

      RUN creafat.p (order.CustNum,
                  order.MsSeq,
                  "BONO8CP",
                  ?, /* amount */
                  0, /* percent */
                  ?, /* vat incl. */
                  YEAR(ldaActivationDate) * 100 +
                  MONTH(ldaActivationDate),
                  999999,
                  OUTPUT lcError).
         
      IF lcError > "" THEN DO:
         put stream sout
            lcMSISDN "|" 
            order.msseq "|0||"
            "ERROR:" lcError.
         liError = liError + 1.
         next LOOPPI.
      END.
         
      FOR EACH fatime where
               fatime.brand = gcbrand and
               fatime.msseq = order.msseq and
               fatime.ftgrp = "BONO8CP" use-index Mobsub:
        /*  ldeFat = ldeFat + fatime.amt. */
         put stream sout unformatted
            lcMSISDN "|" 
            order.msseq "|"
            fatime.amt "|"
            fatime.period "|"
            "DONE" skip.
      end.

      
      i = i + 1.
   end.

end.
disp i liSkipped liOngoing liError.
MESSAGE "DONE".
