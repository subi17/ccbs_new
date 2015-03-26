{commpaa.i}
katun = "anttis".
gcBrand = "1".
{msreqfunc.i}

input from as_yot381.input.

def stream slog.
output stream slog to as_yot381.log.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
repeat:
   import unformatted lcLine.
   
   find mobsub where 
      mobsub.cli = lcLine NO-LOCK.
  
   find msrequest where
      msrequest.msseq = mobsub.msseq and
      msrequest.reqtype = 13 NO-LOCK.
 
   if msrequest.reqstatus = 3 then do:
      fReqStatus(2,"YOT-380"). 
      put stream slog mobsub.msseq "|" msrequest.msrequest "|3|" msrequest.reqstatus skip.
   end.

  /* 
   if mobsub.paytype = true then do:
     disp mobsub.cli.
     find first prepaidrequest where
         prepaidrequest.brand = "1" and
         prepaidrequest.msseq = mobsub.msseq NO-LOCK.
     if ppstatus ne 2 then
     disp prepaidrequest.source ppstatus.

   end. */
end.
