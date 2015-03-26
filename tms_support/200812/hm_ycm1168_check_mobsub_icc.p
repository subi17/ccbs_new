DEFINE VARIABLE icFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE icLog  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFile AS CHARACTER NO-UNDO. 

icFile = "/apps/snet/200812/hm_ycm1168.input".
icLog  = "/apps/snet/200812/hm_ycm1168_mobsub_20081229.log".

def stream sin.
input stream sin from value(icFile).

def stream slog.
output stream slog to value(icLog).

/* DEFINE VARIABLE lMsReqFound AS LOGICAL NO-UNDO. */

repeat:

   import stream sin unformatted lcFile.

   FIND sim where sim.icc = lcFile NO-LOCK NO-ERROR.

   IF not avail sim THEN DO:
      put stream slog unformatted sim.icc " NOT FOUND" SKIP.
   END.


   ELSE DO:
      FIND mobsub WHERE mobsub.icc = sim.icc NO-LOCK NO-ERROR.
      IF AVAIL mobsub THEN
      DO:
         IF mobsub.CreationDate < DATE(12,22,2008) THEN
         DO:
            put stream slog unformatted mobsub.msseq skip.
         END.
      END.
      DISP sim.icc.
   END.

END.

output stream slog close.
input stream sin close.
