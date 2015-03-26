def stream sin.
def stream slog.
output stream slog to /apps/snet/200901/as_ycm1249.log.

PROCEDURE pReadfile:
   def input param icFile as char.
   def input param icStock as char.

   DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

   input stream sin from value(icFile).

   repeat:
      
      import stream sin unformatted lcLine.
      
      FIND SIM WHERE
         SIM.ICC = lcLine NO-LOCK NO-ERROR.
      IF AVAIL SIM THEN DO:
         if sim.simstat = 1 then do: 
            do trans:
               find current sim EXCLUSIVE-LOCK.
               put stream slog unformatted sim.icc "|" sim.simstat "|" sim.stock "|" icStock "|OK "skip. 
               assign sim.stock = icStock.
               release sim.
            end.
         end.
         else
         put stream slog unformatted sim.icc "|" sim.simstat "|"  sim.stock "|" icStock 
            "|ERROR - has wrong status" skip. 
      END.
      ELSE DO:
        put stream slog unformatted lcLine "|" "|" "|" icStock "|ERROR - not avail" skip.
      ENd.
      RELEASE sim.
   END.
      

   input stream sin close.
END PROCEDURE. 

run pReadfile("/apps/snet/200901/ICC+CANARIASMNP.txt", "MNP_CAN_ISL"). 
run pReadfile("/apps/snet/200901/ICC+CANARIASNEW.txt", "NEW_CAN_ISL"). 

output stream slog close.
