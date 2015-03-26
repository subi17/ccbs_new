{timestamp.i}

def var lccli as char no-undo.

def buffer bcdr for mobcdr.

for each mobcdr no-lock use-index spocmt where
         mobcdr.spocmt = 33:

   pause 0.
   disp datest 
        fts2hms(readints) format "x(19)"
        gsmbnr
        cli
        roaming.
        
   if mobcdr.roaming = 0 then do:
      find first bcdr where recid(bcdr) = recid(mobcdr) exclusive-lock.
      assign 
         lccli = bcdr.gsmbnr
         bcdr.gsmbnr = bcdr.cli
         bcdr.cli = lccli
         bcdr.roaming = 1.
   end.
      
end. 