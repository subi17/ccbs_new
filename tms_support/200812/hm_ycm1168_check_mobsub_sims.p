DEFINE VARIABLE cMsSeqFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCrFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cICCChangeFile AS CHARACTER NO-UNDO. 
cMsSeqFile = "/apps/snet/200812/mobsubs_with_one_of_sim.txt".
cCrFile = "/apps/snet/200812/mobsubs_created_latest_at_20081222.txt".
cICCChangeFile = "/apps/snet/200812/mobsub_created_latest_at_20081222_with_no_ICC_Change.txt".

DEFINE VARIABLE iMsSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE cMsSeq AS CHARACTER NO-UNDO. 
DEFINE STREAM sCrLog.
DEFINE STREAM sICCCLog.
DEFINE STREAM sFile.
INPUT STREAM sFile FROM VALUE(cMsSeqFile).
OUTPUT STREAM sCrLog TO VALUE(cCrFile).
OUTPUT STREAM sICCCLog TO VALUE(cICCChangeFile).
REPEAT:
  IMPORT STREAM sFile UNFORMATTED cMsSeq.
  iMsSeq = INTEGER(cMsSeq) NO-ERROR.
  IF iMsSeq > 0 THEN
  DO:
   FIND Mobsub WHERE Mobsub.MsSeq = iMsSeq NO-LOCK NO-ERROR.
     IF AVAIL Mobsub THEN
     DO:
        IF Mobsub.CreationDate <= DATE(12,22,2008) THEN
        DO:
           PUT STREAM sCrLog UNFORMATTED Mobsub.MsSeq " " Mobsub.CreationDate SKIP.
           DEFINE VARIABLE lICCChangeFound AS LOGICAL NO-UNDO. 
           FIND FIRST MsRequest NO-LOCK 
               WHERE MsRequest.MsSeq = Mobsub.MsSeq AND
                     MsRequest.ReqType = 15 AND
                     MsRequest.DoneStamp >= 20081222 NO-ERROR.
           IF NOT AVAIL MsRequest THEN
              PUT STREAM sICCCLog UNFORMATTED Mobsub.MsSeq " " Mobsub.CreationDate SKIP.
        END.
     END. 
  END.
END.
INPUT STREAM sFile CLOSE.
OUTPUT STREAM sCrLog CLOSE.
OUTPUT STREAM sICCCLog CLOSE.

