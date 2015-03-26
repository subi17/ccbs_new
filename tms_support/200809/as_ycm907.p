DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOut AS CHARACTER NO-UNDO. 

DEF STREAM sout.
OUTPUT STREAM sout to /apps/snet/200809/as_ycm907.output.

lcInputFile = "/apps/snet/200809/as_ycm907.input".

INPUT FROM VALUE(lcInputFile).

REPEAT:
   IMPORT UNFORMATTED lcLine.
   FIND FIRST SIM WHERE ICC = lcLine NO-LOCK NO-ERROR.
   IF SIM.Stock = "CC" AND SIM.SimStat EQ 1 THEN DO:
      FIND CURRENT SIM EXCLUSIVE-LOCK NO-ERROR.
      SIM.Stock = "NEW".
      PUT STREAM sout UNFORMATTED SIM.ICC " " SIM.Stock " " SIM.SimStat " OK" SKIP.
   END.
      ELSE DO:
      PUT STREAM sout UNFORMATTED SIM.ICC " " SIM.Stock " " SIM.SimStat " SKIPPED" SKIP.
   END.
END.  

output stream sout close.
