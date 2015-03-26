
/* YOT-878 */



DEFINE VARIABLE icNewStat as int no-undo.
DEFINE VARIABLE ilSimulate as log no-undo.
DEFINE VARIABLE lcFile AS CHARACTER NO-UNDO.

def stream sin.
input stream sin from "XFE01084.COD".

def stream slog.
output stream slog to "XFE01084.log".

DEFINE TEMP-TABLE ttICCInUse
FIELD icc AS CHAR
FIELD msseq AS INT
INDEX icc IS PRIMARY UNIQUE icc. 

icNewStat = 7. /* LOST */
ilSimulate = FALSE.
icc_loop:
repeat:

   import stream sin unformatted lcFile.
   lcFile = TRIM(lcFile).
   IF LENGTH(lcFile) NE 19 THEN 
   DO:
      MESSAGE "Could not trim input line " lcFile 
        " to contain only 19 chars as ICC"
        VIEW-AS ALERT-BOX.
      RETURN.
   END.

   FIND sim where sim.icc = lcFile NO-LOCK NO-ERROR.
   IF not avail sim THEN DO:
      put stream slog unformatted lcFile " NOT FOUND" SKIP.
   END.
   ELSE DO:
      
      IF sim.simstat = 4 then do:
         put stream slog unformatted "sim with icc " sim.icc " is in use " skip.
         next icc_loop.
      end.
     
      put stream slog unformatted 
          sim.icc " " sim.stock " " sim.simstat "->" icNewStat skip.

      IF NOT ilSimulate THEN DO:
            find current sim EXCLUSIVE-LOCK.
            assign sim.simstat = icNewStat.
      END.
    END.
END.

output stream slog close.
input stream sin close. 
