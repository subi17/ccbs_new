{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/msisdn.i}
{Func/date.i}

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream sout.
def buffer msisdn2 for msisdn.
output stream sout to /home/anttis/ycm1074_gift4.txt.
FOR EACH msisdn2 NO-LOCK where
   pos = "gift" and
   statuscode = 4 and
   validto > fmakets():
   
   put stream sout unformatted msisdn2.cli "|" msisdn2.pos "|" msisdn2.statuscode "|" msisdn2.validfrom "|" msisdn2.validto skip.

   fMakeMsidnHistory(recid(msisdn2)).
   ASSIGN
      msisdn.pos = "POS"
      msisdn.statuscode = 1
      msisdn.orderid = 0 
      msisdn.msseq = ?. 
ENd.
output close.

output stream sout to /home/anttis/ycm1074_gift1.txt.

FOR EACH msisdn2 NO-LOCK where
   pos = "gift" and
   statuscode = 1 and
   validto > fmakets():
   
   put stream sout unformatted msisdn2.cli "|" msisdn2.pos "|" msisdn2.statuscode "|" msisdn2.validfrom "|" msisdn2.validto skip.
   
   fMakeMsidnHistory(recid(msisdn2)).
   ASSIGN
      msisdn.pos = "POS"
      msisdn.orderid = 0
      msisdn.msseq = ?. 
ENd.

output stream sout close.
