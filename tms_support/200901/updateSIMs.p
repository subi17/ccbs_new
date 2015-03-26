def input param icFile as char no-undo.
def input param icLog as char no-undo.
def input param iReqOldStat as int no-undo.
def input param icNewStat as int no-undo.
def input param ilSimulate as log no-undo.

DEFINE VARIABLE lcFile AS CHARACTER NO-UNDO.

def stream sin.
input stream sin from value(icFile).

def stream slog.
output stream slog to value(icLog).

repeat:

   import stream sin unformatted lcFile.

   FIND sim where sim.icc = lcFile NO-LOCK NO-ERROR.

   IF not avail sim THEN DO:
      put stream slog unformatted sim.icc " NOT FOUND" SKIP.
   END.

   ELSE DO:
      IF sim.simstat eq iReqOldStat THEN
      do:
         put stream slog unformatted 
            sim.icc " " sim.stock " " sim.simstat "->" icNewStat skip.

         IF NOT ilSimulate THEN
         DO:
            find current sim EXCLUSIVE-LOCK.
            assign sim.simstat = icNewStat.
         END.
      end.
      else
         put stream slog unformatted "sim with icc " sim.icc " was not in status " 
             iReqOldStat " as expected" SKIP.
   END.
END.

output stream slog close.
input stream sin close. 
