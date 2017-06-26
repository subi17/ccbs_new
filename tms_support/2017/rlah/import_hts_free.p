{Rate/rate_roamzone.i}
DEFINE TEMP-TABLE ttBDestTrans NO-UNDO LIKE BDEstTrans.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 


DEFINE VARIABLE lcFiles AS CHARACTER NO-UNDO. 
lcFiles = "hts_short.txt,hts_prefix.txt,free_prefix.txt,free_short.txt".

DEFINE VARIABLE liLoop AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcFile AS CHARACTER NO-UNDO. 

find bdest NO-LOCK where
     bdest.bdest  = "roam_eu".

do liLoop = 1 to num-entries(lcFiles).

   lcFile = entry(liLoop,lcFiles).
   input from value(lcFile).

   repeat:
      import unformatted lcLine.
      lcLine = trim(lcLine).
      if lcLine begins "prefix" then next.
      create ttBDestTrans.
      assign
         ttBDestTrans.bdestid = bdest.bdestid
         ttBDestTrans.Bdest  = (if lcFile begins "free_" then "ROAM_EU_FREE"
                                else "ROAM_EU_HTS")
         ttBdestTrans.Translatenumber = lcLine
         ttBdestTrans.RatingZone = (if index(lcFile,"short") > 0 THEN "SHORT" else
                                    "PREFIX")
         ttBdestTrans.FromDate = 6/15/2017
         ttBdestTrans.Todate = 12/31/2049.
   end.
   input close.
end.

FOR EACH ttBdestTrans:
   disp ttBdestTrans.
   FIND FIRST bdesttrans NO-LOCK where
              bdesttrans.BdestID eq ttBdestTrans.bdestid and
              bdesttrans.translatenumber = ttBdestTrans.translatenumber no-error.
   IF not avail bdesttrans then do:
      create bdesttrans.
      buffer-copy ttBdestTrans to bdesttrans.
   end.
end.
