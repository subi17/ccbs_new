DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream sout.
output stream sout to /apps/snet/200812/as_ycm1131.txt.

DEFINE TEMP-TABLE ttCli
FIELD i AS CHAR
INDEX i IS PRIMARY UNIQUE i. 

FOR EACH msisdn where
   brand = "1" and
   cli begins "622" and
   validto = 99999999.99999 and
   pos = ""
   NO-LOCK /*i = 1 to 100*/:
   
   IF msisdn.statuscode ne 1 and
      msisdn.statuscode ne 0 then next.

   i = i + 1.

   FIND FIRST mobsub where
      mobsub.cli = msisdn.cli NO-LOCK NO-ERROR.

   IF AVAIL mobsub then 
      message mobsub.cli VIEW-AS ALERT-BOX.

   FIND FIRST ttCli where ttCli.i = msisdn.cli NO-LOCK NO-ERROR.

   IF AVAIL ttCLi THEN message ttCLi.i VIEW-AS ALERT-BOX.
   ELSE DO:
      create ttCLi.
      assign ttCLi.i = msisdn.cli.
   end.

   FIND FIRST msisdnnumber where
      msisdnnumber.cli = msisdn.cli NO-LOCK NO-ERROr.

   put stream sout unformatted msisdn.cli " " msisdnnumber.rank " "
      msisdn.statuscode skip.

   if i mod 500 = 0 then disp i.

END.

