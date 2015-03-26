def var i as int no-undo.
def var j as int no-undo.

def buffer bcdr for mobcdr.


for each mobcdr no-lock use-index date where
         mobcdr.datest >= 6/1/9 and
         mobcdr.datest <= 6/24/9 and
         mobcdr.spocmt = 92 and
         mobcdr.errorcode ne 8042 and
         mobcdr.datain + mobcdr.dataout = 0 and
         mobcdr.amount = 0,
   first invseq no-lock where
         invseq.invseq = mobcdr.invseq:
         
   i = i + 1.
          
   if invseq.billed = false then do:
      find bcdr where recid(bcdr) = recid(mobcdr) exclusive-lock.
      bcdr.invseq = 0.
      bcdr.errorcode = 8042.
      j = j + 1.
   end.

   pause 0.
   disp i j mobcdr.datest mobcdr.timest with 1 down.
   
end.         


        
