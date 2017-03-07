{Syst/commpaa.i}
katun = "anttis".
gcBrand  = "1".
{Func/msisdn.i}

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
def stream sread.
input stream sread from /apps/snet/200809/ycm849.input.

output to /apps/snet/200809/ycm849.log.
repeat:
   
   import stream sread unformatted lcline.

   FIND FIRST msisdn where
      msisdn.brand = "1" and
      msisdn.cli = lcline USE-INDEX CLI NO-LOCK NO-ERROR.
   
   if not avail msisdn then do:
      put unformatted lcline " not found!" skip.
      next.
   end.
   
   if msisdn.statuscode ne 4 then do:
      put unformatted msisdn.cli " has status " msisdn.statuscode skip.
      next.
   end.
   
   if msisdn.pos ne "PREACTIVATED" then do:
      put unformatted msisdn.cli  " is in stock" msisdn.pos skip.
      next.
   end.

   FIND FIRST mobsub where mobsub.cli = msisdn.cli NO-LOCK nO-eRROR.
   if avail mobsub then do:
      put unformatted msisdn.cli " is active" skip.
      next.
   end.
   
   fMakeMsidnHistory(recid(msisdn)).
   assign
      msisdn.pos = "POS"
      msisdn.statuscode = 1. 
   
   put unformatted msisdn.cli " ok" skip.
END.

output close.
