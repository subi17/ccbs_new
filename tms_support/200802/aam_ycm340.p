def var i as int no-undo.
def var j as int no-undo.

def buffer bsim for sim.

for each mobsub no-lock,
   first sim where sim.icc = mobsub.icc no-lock:

   i = i + 1.
    
   if i mod 1000 = 0 then do:
      pause 0.
      disp i j with 1 down.
   end.
   
   if mobsub.msseq = sim.msseq then next.
   
   j = j + 1.

   find bsim where recid(bsim) = recid(sim) exclusive-lock.
   bsim.msseq = mobsub.msseq.
end.

disp i j.

