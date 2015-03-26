DEFINE VARIABLE cFile AS CHARACTER NO-UNDO. 
cFile = "/apps/snet/200903/mobsubs_with_one_of_sim_20090317.txt".
DEFINE VARIABLE cOutFile AS CHARACTER NO-UNDO. 
cOutFile = "/apps/snet/200903/ycm_1359_sims_in_mobsubs_20090317.txt".

DEFINE VARIABLE cMsSeq AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iMsSeq AS INTEGER NO-UNDO. 
define stream smobsubs.
define stream ssims.
output stream ssims to value(cOutFile).
input stream smobsubs from value(cFile).
repeat:
   import stream smobsubs unformatted cmsseq.
   iMsSeq = INT(cMsSeq) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
   DO:
       PUT STREAM ssims UNFORMATTED "MsSeq = " cMsSeq 
         " is not integer." SKIP.
   END.
   ELSE
   DO:
      FIND Mobsub WHERE Mobsub.MsSeq = iMsSeq NO-LOCK NO-ERROR.
      IF AVAIL Mobsub THEN
      DO:
         PUT STREAM ssims UNFORMATTED Mobsub.ICC SKIP.
         FIND SIM WHERE SIM.ICC = Mobsub.ICC NO-LOCK NO-ERROR.
         IF AVAIL SIM THEN
         DO:
            PUT STREAM ssims UNFORMATTED "SimStat = " SIM.SimStat SKIP. 
         END.
         ELSE
            PUT STREAm ssims UNFORMATTED "SIM does not exist" SKIP.
      END.
      ELSE
      DO:
         PUT STREAM ssims UNFORMATTED "Mobsub with MsSeq = " iMsSeq 
           " does not exist." SKIP.
      END.
   END.
end.
input stream smobsubs close.
output stream ssims close.
