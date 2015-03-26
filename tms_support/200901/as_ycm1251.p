def stream sin.
def stream slog.
output stream slog to /apps/snet/200901/as_ycm1251.log.

PROCEDURE pReadfile:
   def input param icFile as char.
   def input param iiSimStat as char.

   DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

   input stream sin from value(icFile).

   repeat:
      
      import stream sin unformatted lcLine.
      
      FIND SIM WHERE
         SIM.ICC = lcLine NO-LOCK NO-ERROR.
      IF AVAIL SIM THEN DO:
         if sim.simstat ne 1 then do: 
             do trans:
               if sim.msseq > 0 THEN DO:
                  find mobsub where mobsub.msseq = sim.msseq NO-LOCK NO-ERROr.
                  IF AVAIL mobsub then disp mobsub.msseq.
                  next.
               END.
               find current sim EXCLUSIVE-LOCK. 
               put stream slog unformatted sim.icc "|" sim.simstat "|" sim.stock "|" iiSimStat "|OK "skip. 
               
               assign sim.simstat = iiSimstat.
               release sim.
              
            end.
         end.
         else
         put stream slog unformatted sim.icc "|" sim.simstat "|"  sim.stock "|" iiSimstat 
            "|ERROR - has wrong status" skip. 
      END.
      ELSE DO:
        put stream slog unformatted lcLine "|" "|" "|" iiSimStat "|ERROR - not avail" skip.
      ENd.
      RELEASE sim.
   END.
      

   input stream sin close.
END PROCEDURE. 

run pReadfile("/apps/snet/200901/as_ycm1251.input", 1). 

output stream slog close.
