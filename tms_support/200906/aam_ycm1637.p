def var i     as int  no-undo.
def var j     as int  no-undo.
def var lccli as char no-undo.


def stream sread.
input stream sread from /apps/snet/200906/aam_ycm1637_subslist.txt.

def buffer bcdr for mobcdr.


repeat:
   import stream sread lccli.
   
   if lccli = "" then next.
   
   i = i + 1.
                                    
   j = 0. 
   for each mobcdr no-lock where
            mobcdr.cli = lccli and
            mobcdr.datest = 6/1/9:
             
      if mobcdr.errorcode > 0 then next.
       
      if mobcdr.invseq > 0 then do:
         find first invseq where invseq.invseq = mobcdr.invseq 
            no-lock no-error.      
         if available invseq and invseq.billed then next.
      end.

      /*          
      disp cli errorcode invseq spocmt format ">>9".                           
      */

      find first bcdr where recid(bcdr) = recid(mobcdr) exclusive-lock.
      assign
         bcdr.invseq = 0
         bcdr.errorcode = 8040.
         
      j = j + 1. 
   end.

   disp i lccli format "x(12)" j.
      
end.

input stream sread close.

