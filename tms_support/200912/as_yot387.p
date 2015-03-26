input from as_yot387.input.

DEFINE VARIABLE lcICC AS CHARACTER NO-UNDO format "x(30)". 

def stream sout.
output stream sout to as_yot387.log.

repeat:
   import unformatted lcICC.

   find sim where
      sim.icc = lcICC and
      sim.stock = "MNP" NO-LOCK.
   
   IF sim.simstat NE 1 THEN DO:
      
      put stream sout unformatted sim.icc "|" sim.simstat "|" sim.stock 
         skip.
      next.

   END.
   ELSE DO:
      DO TRANS:
         FIND CURRENT sim EXCLUSIVE-LOCK.
         assign sim.stock = "RETAILER".
         release sim.
      END.
   END.
end.
