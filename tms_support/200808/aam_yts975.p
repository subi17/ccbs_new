def var lcsim as char no-undo.

lcsim = "8934040508010303473,8934040508010303481,8934040508010303499," +
        "8934040508010303507,8934040508010303515,8934040508010303523," +
        "8934040508010303531,8934040508010303549,8934040508010303556".

def var i as int no-undo.

do i = 1 to num-entries(lcsim):

   find first sim where sim.icc = entry(i,lcsim) no-lock.
   
   find first order no-lock where order.msseq = sim.msseq.
  
   disp i sim.simstat sim.msseq order.orderid order.crstamp with 10 down.
   down.

   if sim.simstat = 21 then do:
      find current sim exclusive-lock.
      sim.simstat = 20.
   end.
end.