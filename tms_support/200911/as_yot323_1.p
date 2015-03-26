{commpaa.i}
katun = "anttis".
gcBrand = "1".
{timestamp.i}
{msisdn.i}
DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 
ldeNow = fMakeTS().

def buffer msisdnBuf for msisdn.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream slog.
output stream slog to /apps/snet/200911/as_yot323_1.log append.

LOOPPI:
FOR EACH msisdn NO-LOCK WHERE
   msisdn.pos = "PREACTIVATED" and
   msisdn.statuscode = 1 and
   msisdn.validto > ldeNow:
   
   i = i + 1.
/*   if i > 3 then leave.
*/
   if msisdn.brand ne "1" then do:
      put stream slog unformatted msisdn.cli "|ERROR:iwrong brand" skip.
      NEXT LOOPPI.
   END.

   /* check that no active subscription exists */
   find mobsub where
        mobsub.cli = msisdn.cli NO-LOCK no-error.
   IF AVAIL mobsub THEN DO:
      put stream slog unformatted msisdn.cli "|ERROR:active subscription found" skip.
      NEXT LOOPPI.
   END.

   /* check that no duplicate timestamps exists */
   find first msisdnbuf where
      msisdnbuf.cli = msisdn.cli and
      msisdnbuf.validto > ldeNow and
      rowid(msisdnbuf) ne rowid(msisdn) NO-LOCK NO-ERROR.
   IF AVAIL msisdnbuf then do:
      put stream slog unformatted msisdn.cli "|ERROR:duplicate timestamp found" skip.
      NEXT LOOPPI.
   end.
   
   /* check if number is gold number */
   find msisdnnumber where
      msisdnnumber.cli = msisdn.cli NO-LOCK.
   
   IF msisdnnumber.rank = 2 THEN DO:
      put stream slog unformatted msisdnnumber.cli "|SKIPPED:HAS RANK 2 (?)" skip.
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

   /* change stock to ONLINE */
   fMakeMSIDNHistory(recid(msisdn)).
   assign
     msisdn.pos = "ONLINE".
   put stream slog unformatted msisdn.cli "|OK" skip. 
   release msisdn.
end.
disp i.
