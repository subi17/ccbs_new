def var i as int no-undo.
def var j as int no-undo.
def var lcforward as char no-undo.

def buffer bcdr for mobcdr.


for each mobcdr no-lock use-index errorcode where
         mobcdr.errorcode = 9010 and
         mobcdr.spocmt = 7 and
         mobcdr.datest < 7/12/10:
        
   find first bcdr use-index cli where
              bcdr.cli = mobcdr.cli and
              bcdr.datest = mobcdr.datest and
              bcdr.timest = mobcdr.timest and
              lookup(string(bcdr.spocmt),"3,4") > 0 no-lock no-error.
   if not available bcdr or 
      lookup(bcdr.gsmbnr,"633,633633633,632,633633632,") = 0
   then next.

   find first mcdrdtl2 where
              mcdrdtl2.datest = bcdr.datest and
              mcdrdtl2.dtlseq = bcdr.dtlseq no-lock no-error.
   if not available mcdrdtl2 then next.
          
   lcforward = entry(68,mcdrdtl2.detail,"|").
   
   if lcforward = "1" then do:
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
