def var i as int no-undo.
def var j as int no-undo.

def buffer bcdr for mobcdr.


for each mobcdr no-lock use-index date where
         mobcdr.errorcode = 8049 and
         mobcdr.datest >= 10/1/8 and
         mobcdr.datest <= 10/31/8
   on stop undo, leave:

   i = i + 1.
   
   if i mod 1000 = 0 then do:
      pause 0.
      disp i j mobcdr.datest with 1 down.
   end.
   
   if lookup(mobcdr.gsmbnr,"010,012") > 0 then do:
      j = j + 1.
      
      find bcdr where recid(bcdr) = recid(mobcdr) exclusive-lock.
      bcdr.errorcode = 9100.
   end.

end.

disp i j.
