{Syst/commpaa.i}
katun = "anttis".
gcbrand = "1".

{Func/date.i}
{Func/msisdn.i}
DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 
ldeNow = fMakeTS().

def stream sout.
output stream sout to as_yot1057.txt.

def buffer msisdnbuf for msisdn.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 

LOOPPI:
FOR EACH msisdn NO-LOCK where 
   msisdn.pos = "PREACTIVATED" and 
   msisdn.statuscode eq 1 use-index pos break by msisdn.cli:
   
   if not first-of(msisdn.cli) then next.

   if can-find(first msisdnbuf where
                     msisdnbuf.cli = msisdn.cli and
                     msisdnbuf.validto > msisdn.validto) then next.
      
   if msisdn.brand ne "1" then do:
      put stream sout unformatted msisdn.cli "|ERROR:wrong brand" skip.
      NEXT LOOPPI.
   END.

   /* check that no active subscription exists */
   find mobsub where
        mobsub.cli = msisdn.cli NO-LOCK no-error.
   IF AVAIL mobsub THEN DO:
      put stream sout unformatted msisdn.cli "|ERROR:active subscription found" skip.
      NEXT LOOPPI.
   END.
      
   /* check that no duplicate timestamps exists */
   find first msisdnbuf where
      msisdnbuf.cli = msisdn.cli and
      msisdnbuf.validto > ldeNow and
      rowid(msisdnbuf) ne rowid(msisdn) NO-LOCK NO-ERROR.
   IF AVAIL msisdnbuf then do:
      put stream sout unformatted msisdn.cli "|ERROR:duplicate timestamp found" skip.
      NEXT LOOPPI.
   end.

   i = i + 1.
      
   find msisdnnumber where
        msisdnnumber.cli = msisdn.cli NO-LOCK.

    if msisdnnumber.rank ne 0 then next.

   do trans:

   /* change stock to ONLINE */
   
   fMakeMSIDNHistory(recid(msisdn)).
   assign
     msisdn.pos = "ONLINE"
     msisdn.statuscode = 1. 

   put stream sout unformatted  
      msisdn.cli skip.

   end.
end.

disp i.
