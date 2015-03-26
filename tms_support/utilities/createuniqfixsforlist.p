{/home/harrim/utilities/ttinpmobsub.i}
DEFINE VARIABLE iCountForSubDir AS INTEGER NO-UNDO. 
DEFINE VARIABLE cMsSeqListFile  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cLogFile        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cMsSeq          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iMsSeq          AS INTEGER NO-UNDO. 
DEFINE VARIABLE iIdxInSubDir    AS INTEGER NO-UNDO. 
DEFINE VARIABLE iSubDir         AS INTEGER NO-UNDO. 
DEFINE VARIABLE iCount          AS INTEGER NO-UNDO. 
DEFINE VARIABLE cRootDir        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cDir            AS CHARACTER NO-UNDO. 

DEFINE VARIABLE iDispInterval   AS INTEGER INIT 2000 NO-UNDO. 

ASSIGN iCount = 0
       iCountForSubDir = 100
       cMsSeqListFile = "/data/fixtures/allmobsub/msseqs.txt"
       cLogFile = "/data/fixtures/allmobsub/log2.txt"
       cRootDir = "/data/fixtures/allmobsub".

DEFINE STREAM sLog.
DEFINE STREAM sMsSeq.

INPUT STREAM sMsSeq FROM VALUE (cMsSeqListFile).
OUTPUT STREAM sLog TO VALUE(cLogFile).
REPEAT:
   IMPORT STREAM sMsSeq UNFORMATTED cMsSeq.
   iMsSeq = INTEGER(cMsSeq) NO-ERROR.
   IF NOT ERROR-STATUS:ERROR THEN
   DO:
      iCount = iCount + 1.
      iSubDir = TRUNCATE((iCount - 1) / iCountForSubDir, 0).
      cDir = cRootDir + "/fix" + STRING(iSubDir * 100 + 1) + "_" + 
                                STRING((iSubDir + 1)* 100) +
                                "/fix" + STRING(iCount) + "/". 
      RUN /home/harrim/utilities/fixdumpexport.p(cDir, iMsSeq, iMsSeq, 1, cDir + "log.txt",
          "", 0, "", TABLE ttinpmobsub). 
      PUT STREAM sLog UNFORMATTED cMsSeq " output to dir " cDir SKIP.
      IF iCount MOD iDispInterval = 0 THEN
         DISP iCount. 
   END.
   ELSE
     PUT STREAM sLog UNFORMATTED cMsSeq " was not integer" SKIP.
END.
OUTPUT STREAM sLog CLOSE.
INPUT STREAM sMsSeq CLOSE.


