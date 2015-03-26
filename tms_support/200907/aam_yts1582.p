def var lccli as char no-undo.
def var lclist as char no-undo.
def var i as int no-undo.

lclist =
   "633362753,633362767,622095732,633362042,633362584,633362152,622397444".

do i = 1 to num-entries(lclist):

   lccli = entry(i,lclist).

   find first mobsub where mobsub.cli = lccli no-lock no-error.

   if not available mobsub then do:
      message "Mobsub missing:" lccli
      view-as alert-box.
      next.
   end.

   find first msrequest no-lock where
              msrequest.msseq = mobsub.msseq and
              msrequest.reqtype = 13.
              
   disp mobsub.cli
        msrequest.actstamp
        msrequest.reqstat
        msrequest.memo format "x(40)" with 20 down.
   down.     

   if msrequest.reqstat = 3 then do:
      find current msrequest exclusive-lock. 
      msrequest.reqstat = 2.
   end.

end.
