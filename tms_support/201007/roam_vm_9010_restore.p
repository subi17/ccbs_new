def var i as int no-undo.
def var j as int no-undo.
def var lcforward as char no-undo.

def buffer bcdr for mobcdr.


for each mobcdr no-lock use-index errorcode where
         mobcdr.errorcode = 9010 and
         mobcdr.spocmt = 7 and
         mobcdr.datest >= 7/12/10,
   first mcdrdtl2 no-lock where
         mcdrdtl2.datest = mobcdr.datest and
         mcdrdtl2.dtlseq = mobcdr.dtlseq:
         
   lcforward = entry(46,mcdrdtl2.detail,"|").
   
   if lcforward = "B" then do:
      find first bcdr where recid(bcdr) = recid(mobcdr) exclusive-lock.
      assign 
         bcdr.spocmt = 33
         j = j + 1.
   end.

   i = i + 1.
   if i mod 100 = 0 then do:
      pause 0.
      disp i j mobcdr.spocmt mobcdr.datest with 1 down.
   end.
      
end.

disp i j.
