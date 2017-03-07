{Syst/testpaa.i}
katun = "anttis".

def stream sread.
input stream sread from /apps/snet/200802/ycm326.icc.

def stream slog.
output stream slog to /apps/snet/200802/ycm326.log.

def var lcline    as char no-undo.
def var lcms      as char no-undo.
def var lcNewStat as char no-undo.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
repeat:

   import stream sread unformatted lcline.
   
   find first sim where sim.icc = lcLine no-lock no-error.
   if not avail sim then do:
      MESSAGE "Not found: " +  lcLine VIEW-AS ALERT-BOX.
      next.
   END.
  

   if sim.msseq = 0 then
   FIND FIRST mobsub where mobsub.icc = sim.icc NO-LOCK NO-ERROR.
   else
   FIND FIRST mobsub where mobsub.msseq = sim.msseq NO-LOCK NO-ERROR.

   
   if avail mobsub then do:
      lcms = mobsub.cli.
      lcNewStat = string(sim.simstat).
   end.
   else do:
      lcms = "not found".
      lcNewStat = "7".
   end.

   put stream slog unformatted
          lcLine        chr(9)
          lcms          chr(9)
          string(sim.simstat) + " -> " + lcNewStat skip.
    
   if not avail mobsub then do:
      find current sim EXCLUSIVE-LOCK no-error.
      sim.simstat = 7. 
   end.
   
   i = i + 1.
   disp i.
end.

input stream sread close.
output stream slog close.

