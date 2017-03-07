{Func/timestamp.i}
DEFINE VARIABLE ts AS DECIMAL NO-UNDO. 
ts = fMakeTS(). 
DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeValidFrom AS DECIMAL NO-UNDO.
def buffer msisdn2 for msisdn.
def stream sout.
output stream sout to /apps/snet/200801/msisdn_dup_fix_test.txt.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE oda AS DATE NO-UNDO. 
DEFINE VARIABLE oi AS INTEGER NO-UNDO. 
FOR EACH msisdn NO-LOCK
   WHERE brand = "1" use-index cli
   break by (msisdn.cli):
   
   if first-of (msisdn.cli) and msisdn.validto > ts then do:
      ldeValidFrom = msisdn.validfrom.
      next.
   end.
   
   if msisdn.validto > ts  then do:
      /*
      fSplitTS(input ldeValidFrom, output oda, output oi).
      if oi = 0 then MESSAGE msisdn.cli VIEW-AS ALERT-BOX.
      */
      ldeValidFrom = ldeValidFrom - 0.00001.
      put stream sout unformatted msisdn.cli " " msisdn.validfrom " " 
      ldeValidFrom SKIP.
     /* 
      find msisdn2 where recid(msisdn2) = recid(msisdn) EXCLUSIVE-LOCK.
      msisdn2.validto = ldeValidFrom.
      release msisdn2.
      */
   end.
   ldeValidFrom = msisdn.validfrom.

END.
output stream sout close.
