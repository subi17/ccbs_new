def var i as int no-undo.
def var j as int no-undo.

def var ldadate as date no-undo.

def buffer bcdr for mobcdr.

do ldadate = 8/1/9 to today:

   for each mobcdr no-lock use-index gsmbnr where
            mobcdr.datest = ldadate and
            mobcdr.gsmbnr begins "112" and
            mobcdr.spocmt = 66,
      first invseq no-lock where
            invseq.invseq = mobcdr.invseq and
            invseq.billed = false:
            
      i = i + 1.
      
      /*      
      disp mobcdr.datest mobcdr.spocmt mobcdr.gsmbnr 
           with 40 down.
      */
      
      find bcdr where recid(bcdr) = recid(mobcdr) exclusive-lock.
      assign 
          bcdr.spocmt = 950
          bcdr.invseq = 0
          bcdr.errorcode = 8040.
      
   end.

end.

disp i.