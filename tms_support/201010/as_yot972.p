{commpaa.i}
katun = "anttis".
gcbrand = "1".

{date.i}
{msisdn.i}
DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 
ldeNow = fMakeTS().

def stream sout.
output stream sout to as_yot972_2.txt append.
/*
put stream sout "MSISDN|STATUS|STOCK|RANK" skip.
*/
def buffer msisdnbuf for msisdn.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 

LOOPPI:
FOR EACH msisdn NO-LOCK where 
   msisdn.brand = "1" and
   msisdn.cli begins "6337" use-index cli break by msisdn.cli.
   
   i = i + 1.
   if i mod 100 = 0 then disp i.
   pause 0.

   if first-of(msisdn.cli) then do:
      
      j = j + 1.
      if j <= 20 then next LOOPPI.
      
      find msisdnnumber where
           msisdnnumber.cli = msisdn.cli NO-LOCK.

      if msisdnnumber.rank ne 0 then next.

      if lookup(msisdn.cli,"633700100,633700200,622700300") > 0 then next.
      
      if trim(msisdn.pos) ne "" then next.
      
      if msisdn.statuscode ne 0 then do:
         put stream sout unformatted msisdn.cli "|ERROR:wrong status code" skip.
         NEXT LOOPPI.
      end.
   
      if msisdn.brand ne "1" then do:
         put stream sout unformatted msisdn.cli "|ERRORiwrong brand" skip.
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
      
      put stream sout unformatted  
         msisdn.cli "|" 
         msisdn.statuscode "|"
         msisdn.pos "|"
         (if msisdnnumber.rank = 0 then "Common" else if msisdnnumber.rank = 1 then "Gold" else string(msisdnnumber.rank)) skip.
   
      /* change stock to ONLINE */
      fMakeMSIDNHistory(recid(msisdn)).
      assign
        msisdn.pos = "ONLINE"
        msisdn.statuscode = 1.

      release msisdn.
   end.
end.
