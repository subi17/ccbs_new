{Syst/commpaa.i}
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
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

input from yot_1051_extra.input.
def stream sout.
output stream sout to yot_1051_extra.txt.
      
put stream sout unformatted
   "MSISDN|SUBSCR.ID|FAT CREATED|PERIOD|REASON" skip.

DEFINE VARIABLE liTest AS INTEGER NO-UNDO. 
LOOPPI:
REPEAT:
     
   import unformatted lcLine.
   
   find MobSub where
        MobSub.cli = lcLine NO-LOCK no-error.

   IF AVAIL MobSub then do:
      lcMSISDN = MobSub.cli.
      ldaActivationDate = MobSub.ActivationDate.
   end.
   else do:
      put stream sout unformatted
         lcLine "|" 
         "ERROR:Subscription not found" skip.
       liError = liError + 1.
       next LOOPPI.
   end.
   
   if can-find(first fatime where
                     fatime.brand = gcbrand and
                     fatime.msseq = mobsub.msseq and
                     fatime.ftgrp = "BONO8CP" use-index MobSub) then do:
   
      FOR EACH fatime where
               fatime.brand = gcbrand and
               fatime.msseq = mobsub.msseq and
               fatime.ftgrp = "BONO8CP" use-index MobSub:
         put stream sout unformatted
            lcMSISDN "|" 
            mobsub.msseq "|"
            fatime.amt "|"
            fatime.period "|"
            "FAT ALREADY EXIST" skip.
      end.
      liSkipped = liSkipped + 1.
      next LOOPPI.
   end.

   RUN Mc/creafat.p (mobsub.CustNum,
               mobsub.MsSeq,
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
         mobsub.msseq "|0||"
         "ERROR:" lcError.
      liError = liError + 1.
      next LOOPPI.
   END.
      
   FOR EACH fatime where
            fatime.brand = gcbrand and
            fatime.msseq = mobsub.msseq and
            fatime.ftgrp = "BONO8CP" use-index Mobsub:
      put stream sout unformatted
         lcMSISDN "|" 
         mobsub.msseq "|"
         fatime.amt "|"
         fatime.period "|"
         "DONE" skip.
   end.

   i = i + 1.

end.
disp i liSkipped liOngoing liError.
MESSAGE "DONE".
