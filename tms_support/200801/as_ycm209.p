DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 
{testpaa.i}
katun = "anttis".
{date.i}
{msisdn.i}
define stream out.
OUTPUT STREAM out TO "/apps/snet/200801/as_ycm209_msisdn.bak".
DO i = 622757372 TO 622777371:
  FIND FIRST msisdn where
   msisdn.brand = "1" and
   msisdn.cli = string(i) 
   use-index cli NO-LOCK no-error.
   if avail msisdn then do:
      if msisdn.statuscode = 0 and msisdn.validto > fMakeTS() then do:
         j = j + 1.
         export stream out msisdn.
         find current msisdn exclusive-lock.
         fMakeMsidnHistory(recid(msisdn)).
         msisdn.statuscode = 1.
         msisdn.pos = "PREACTIVATED".
      end.
   end.
END.
output stream out close.
disp j.
