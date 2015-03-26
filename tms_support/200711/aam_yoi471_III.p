def stream slog.
output stream slog to /apps/snet/200711/aam_yoi471_III.log append.

def var i as int no-undo.

def buffer bsub for mobsub.

for each mobsub no-lock where
         mobsub.brand = "1" and
         mobsub.repcodes = "x":
         
   i = i + 1.
         
   put stream slog unformatted
     mobsub.cli chr(9)
     mobsub.msseq skip.
   
   find bsub where recid(bsub) = recid(mobsub) exclusive-lock.
   bsub.repcodes = "".

   pause 0.
   disp i with 1 down.
    
end.
