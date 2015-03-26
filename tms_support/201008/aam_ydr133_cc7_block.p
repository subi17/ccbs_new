def var lcforward as char no-undo.
def var lcmsrn as char no-undo.
def buffer bcdr for mobcdr.

def buffer bupd for mobcdr.

for each mobcdr no-lock use-index errorcode where
         mobcdr.errorcode = 9900 and
         mobcdr.spocmt = 7 and
         mobcdr.datest >= 8/2/10 and
         mobcdr.datest <= 8/4/10,
   first mcdrdtl2 no-lock where
         mcdrdtl2.datest = mobcdr.datest and
         mcdrdtl2.dtlseq = mobcdr.dtlseq,
   first bcdr no-lock use-index cli where
         bcdr.cli = mobcdr.cli and
         bcdr.datest = mobcdr.datest and
         bcdr.timest = mobcdr.timest and
         bcdr.spocmt = 30 and 
         recid(bcdr) ne recid(mobcdr):
          
   assign
      lcforward = entry(46,mcdrdtl2.detail,"|")
      lcmsrn = entry(71,mcdrdtl2.detail,"|").
      
   disp mobcdr.datest string(mobcdr.timest,"hh:mm:ss")
        mobcdr.errorcode 
        lcforward format "x(4)" 
        lcmsrn format "x(14)"
        bcdr.spocmt bcdr.errorcode
        bcdr.gsmbnr format "x(12)".

   find first bupd where recid(bupd) = recid(mobcdr) exclusive-lock.
   bupd.errorcode = 8040.

   /*
   def var i as int no-undo.
   
   i = i + 1.
   if i mod 100 = 0 then do:
      pause 0.
      disp i mobcdr.datest with 1 down.
   end.
   */
end.
 