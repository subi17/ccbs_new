{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/timestamp.i}
{Func/msisdn.i}

DEFINE VARIABLE iCLi as int64 no-undo.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream slog.
def stream serr.
output stream slog to  /apps/snet/200911/as_yot323_2.log.
output stream serr to  /apps/snet/200911/as_yot323_2_err.log.

def buffer msisdnbuf for msisdn.
DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 
ldeNow = fMakeTS().

LOOPPI:
DO iCli = 633400000 to 633499999:

   FIND FIRST msisdnnumber where
      msisdnnumber.cli = string(iCli) NO-LOCK.
      
   FIND FIRST msisdn where msisdn.brand = gcBrand and 
      msisdn.cli = msisdnnumber.cli and 
      msisdn.statuscode = 0 and
      msisdn.validto > ldeNow NO-LOCK NO-ERROR.
   IF NOT AVAIL msisdn THEN DO:
      put stream serr unformatted msisdnnumber.cli "|ERROR:not correct status" skip.
      NEXT LOOPPI.
   END.
 /*  
   i = i + 1.
   if i > 23 then leave.
   */
   /* check that no duplicate timestamps exists */
   find first msisdnbuf where
      msisdnbuf.cli = msisdn.cli and
      msisdnbuf.validto > ldeNow and
      rowid(msisdnbuf) ne rowid(msisdn) NO-LOCK NO-ERROR.
   IF AVAIL msisdnbuf then do:
      put stream slog unformatted msisdn.cli "|ERROR:duplicate timestamp found" skip.
      NEXT LOOPPI.
   end.

   if msisdn.brand ne "1" then do:
      put stream slog unformatted msisdn.cli "ERROR:wrong brand" skip.
      NEXT LOOPPI.
   END.

   /* check that no active subscription exists */
   find mobsub where
        mobsub.cli = msisdn.cli NO-LOCK no-error.
   IF AVAIL mobsub THEN DO:
      put stream slog unformatted msisdn.cli "|ERROR:active subscription found" skip.
      NEXT LOOPPI.
   END.
   
   IF msisdnnumber.rank = 2 THEN DO:
      put stream slog unformatted msisdnnumber.cli "|SKIPPED:IS GOLD NUMBER (RANK 2)" skip.
      NEXT LOOPPI.
   END.
   
   IF msisdnnumber.rank = 1 THEN DO:
      put stream slog unformatted msisdnnumber.cli "|SKIPPED:IS GOLD NUMBER" skip.
      NEXT LOOPPI.
   END.
   
   IF msisdnnumber.rank ne 0 THEN DO:
      put stream slog unformatted msisdnnumber.cli "|SKIPPED:HAS RANK " msisdnnumber.rank skip.
      NEXT LOOPPI.
   END.
   
   if msisdn.pos ne "" then do:
      put stream slog unformatted msisdn.cli "ERROR:stock is defined " msisdn.pos skip.
      NEXT LOOPPI.
   END.
   
   /* change stock to ONLINE */
   do trans:
      fMakeMSIDNHistory(recid(msisdn)).
      assign
        msisdn.statuscode = 1.
        msisdn.pos = "ONLINE".
      put stream slog unformatted msisdn.cli "|OK:" skip. 
      release msisdn.
   end.
  
END.

output stream slog close.
