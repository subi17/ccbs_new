input from move_ICC_to_RETAILER.txt.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

def stream slog.
output stream slog to as_yot616.log.

repeat:
   import unformatted lcLine.
   find sim where
      sim.icc = lcLine NO-LOCK.

   put stream slog unformatted sim.icc "|" sim.stock "|" sim.simstat "|".

   if sim.simstat = 1 then do:
      do trans:
         find current sim EXCLUSIVE-LOCK.
         sim.stock = "RETAILER".
         release sim.
      end.
      put stream slog unformatted "RETAILER".
   end.   

   put stream slog skip.
end.
