DEFINE VARIABLE cMsSeqFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cLogFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cMsSeq AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMsSeq AS INTEGER NO-UNDO. 

cMsSeqFile = "/apps/snet/200812/mobsub_created_latest_at_20081222_with_no_ICC_Change.txt".
cLogFile = "/apps/snet/200812/hm_ycm_1168_mobsubs_with_orders.log".

DEFINE STREAM sFile.
DEFINE STREAM sLog.
INPUT STREAM sFile FROM VALUE(cMsSeqFile).
OUTPUT STREAM sLog TO VALUE(cLogFile).

DEFINE VARIABLE dtStamp AS DECIMAL NO-UNDO. 
dtStamp = DECIMAL(20081222 + (14 * 3600 + 28 * 60) / 100000).
DISP dtStamp FORMAT "99999999.99999".

REPEAT:

  IMPORT STREAM sFile UNFORMATTED cMsSeq.
  cMsSeq = ENTRY(1, cMsSeq, " ").
  iMsSeq = INTEGER(cMsSeq) NO-ERROR.
  IF iMsSeq > 0 THEN
  DO:
   FIND Mobsub WHERE Mobsub.MsSeq = iMsSeq NO-LOCK NO-ERROR.
     IF AVAIL Mobsub THEN
     DO:
        FIND Order WHERE Order.MsSeq = Mobsub.MsSeq NO-LOCK NO-ERROR.
        IF AVAIL Order THEN
        DO:
           IF Order.CrStamp < dtStamp THEN
           DO:
              PUT STREAM sLog UNFORMATTED "!!! Order for MsSeq " Mobsub.MsSeq 
                 " orderid " Order.OrderId " was created " Order.CrStamp 
                 " earlier than " dtStamp SKIP.
           END.
           ELSE
           DO:
              PUT STREAM sLog UNFORMATTED "Order for MsSeq " Mobsub.MsSeq 
                 " orderid " Order.OrderId " was created " Order.CrStamp 
                 " after or same time than " dtStamp SKIP.
           END.
        END.
        ELSE
           PUT STREAM sLog UNFORMATTED "Order for MsSeq " Mobsub.MsSeq 
              " did not exist" SKIP.
     END.

  END.
END.

INPUT STREAM sFile CLOSE.
OUTPUT STREAM sLog CLOSE.
