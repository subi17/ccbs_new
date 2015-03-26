def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.

def var lcline as char no-undo.
def var lccli  as char no-undo.
def var licust as int  no-undo.
def var limsseq as int no-undo.
def var lidone as int no-undo.
def var ldatodate as date no-undo.

def stream sread.
input stream sread from /apps/snet/200906/aam_ycm1634_xfera1013.txt.


repeat:
   import stream sread unformatted lcline.
   
   assign 
      lccli  = entry(1,lcline,"|")
      licust = integer(entry(4,lcline,"|")) 
      no-error.
     
   if error-status:error then next.
   
   if licust = 0 then next.
   
   i = i + 1. 
   
   limsseq = 0.
   
   find first mobsub where mobsub.cli = lccli no-lock no-error.
   if available mobsub then do:
   
      j = j + 1.
      /*
      if mobsub.agrcust ne licust then do:
         k = k + 1.
         disp mobsub.cli mobsub.agrcust licust label "File".
      end.
      else 
      */
      limsseq = mobsub.msseq.
      licust  = mobsub.invcust.
   end.

   else do:
      find first termmobsub where termmobsub.cli = lccli and
            termmobsub.agrcust = licust no-lock no-error.
      if available termmobsub then do:
         j = j + 1.
         limsseq = termmobsub.msseq.
         licust = termmobsub.invcust.
      end.
      else do:
         k = k + 1.
         disp termmobsub.cli termmobsub.agrcust licust.
      end.
   end.

   if limsseq = 0 then next. 
   
   if not can-find(first invattach where
                         invattach.custnum  = licust and
                         invattach.attach4  = limsseq)
   then do:
      lidone = lidone + 1.
      
      ldatodate = 7/31/9.
      do while true:
         if not can-find(first invattach where
                            invattach.custnum  = licust and
                            invattach.todate   = ldatodate)
         then leave.
         ldatodate = ldatodate - 1.
      end.
                         

      create invattach.
      assign
         invattach.custnum  = licust
         invattach.todate   = ldatodate
         invattach.fromdate = 7/1/9
         invattach.attach1  = 1 
         invattach.attach4  = limsseq.
   end.

   pause 0.
   disp i j k lidone with 1 down.

end.

disp i j k lidone .


   

