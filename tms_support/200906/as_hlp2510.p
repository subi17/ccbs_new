DEFINE VARIABLE i AS INTEGER NO-UNDO. 
def stream sout.
output stream sout to /apps/snet/200906/as_hlp2510.log append.
{testpaa.i}
katun = "anttis".
{msreqfunc.i}
FOR EACH msrequest where
   msrequest.brand = "1" and
   msrequest.reqtype = 35 and
   msrequest.reqstatus = 3 NO-LOCK:
   if msrequest.memo eq "Subrequest 1 failed" then do:
      i = i + 1.   
      if i > 2 then leave.
      put stream sout unformatted 
         msrequest.msrequest "|"
         msrequest.actstamp skip.
      fReqStatus(7,"").
   end.

ENd.

disp i.
