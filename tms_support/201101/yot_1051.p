{commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{date.i}
{mdub.i}

input from yot_1051.input.
def stream sout.
output stream sout to yot_1051.txt.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeActStamp AS DECIMAL NO-UNDO. 
DEFINE VARIABLE liRequest AS INTEGER NO-UNDO. 
DEFINE VARIABLE ocResult AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 

LOOPPI:
repeat trans:
   import unformatted lcLine.

   find mobsub where
        mobsub.cli = lcLine NO-LOCK.
   
   if can-find(first fatime NO-LOCK where
                     fatime.brand = gcbrand and
                     fatime.msseq = mobsub.msseq and
                     fatime.ftgrp = "BONO8CP" use-index MobSub) then do:
   
      FOR EACH fatime NO-LOCK where
               fatime.brand = gcbrand and
               fatime.msseq = mobsub.msseq and
               fatime.ftgrp = "BONO8CP" use-index MobSub:
         put stream sout unformatted
            mobsub.cli "|" 
            mobsub.msseq "|"
            fatime.amt "|"
            fatime.period "|"
            "FAT ALREADY EXIST" skip.
      end.
      next LOOPPI.
   end.

   find first mservicelimit where
      mservicelimit.msseq = mobsub.msseq and
      mservicelimit.slseq = 10 and
      endts > fmakeTS() NO-LOCK no-error.
   IF AVAIL mservicelimit then do:
   END.
   else do:
      liRequest = fBundleRequest(
                                 (if mobsub.activationts >= 20110101 THEN
                                     mobsub.activationts else 20110101.00001),
                                 "",   /* creator */
                                 MobSub.MsSeq,   /* MobSub  sequence */
                                 "MDUB",   /* Bundle id */
                                 1,  /* 1- activation */
                                 0, /* not origing request */
                                 {&REQUEST_SOURCE_SCRIPT},
                                 OUTPUT ocResult).
      IF liRequest = 0 THEN DO:
            put stream sout unformatted
                  mobsub.cli "|" 
                  mobsub.msseq "|"
                  "|"
                  "|"
            "ERROR:Bundle request not created; " ocResult skip.
      END.
      else DO:
            put stream sout unformatted
                  mobsub.cli "|" 
                  mobsub.msseq "|" 
                  "|"
                  "|"
            "Bundle request created; " liRequest skip.
      END.
   end.
      
   RUN creafat.p (mobsub.CustNum,
               mobsub.MsSeq,
               "BONO8CP",
               "",
               "",
               ?, /* amount */
               0, /* percent */
               ?, /* vat incl. */
               201101,
               999999,
               OUTPUT lcError).
      
   IF lcError > "" THEN DO:
      put stream sout
         mobsub.cli "|" 
         mobsub.msseq "|0||"
         "ERROR:" lcError.
      next LOOPPI.
   END.
      
   FOR EACH fatime NO-LOCK where
            fatime.brand = gcbrand and
            fatime.msseq = mobsub.msseq and
            fatime.ftgrp = "BONO8CP" use-index Mobsub:
      put stream sout unformatted
         mobsub.cli "|" 
         mobsub.msseq "|"
         fatime.amt "|"
         fatime.period "|"
         "DONE" skip.
   end.

end.
