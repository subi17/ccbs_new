{Syst/commpaa.i}
Syst.Var:katun = "anttis".
Syst.Var:gcBrand = "1".
{Func/msisdn.i}

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream sout.
def buffer msisdn2 for msisdn.
output stream sout to /home/anttis/msisdn_loki.txt.
FOR EACH msisdn2 NO-LOCK where
   pos = "preactivated" and
   statuscode = 1 and
   validto > Func.Common:mMakeTS()  
   i = 1 to 15000:
   
   put stream sout unformatted msisdn2.cli "|" msisdn2.pos "|" msisdn2.statuscode "|" msisdn2.validfrom "|" msisdn2.validto skip.
   
   fMakeMsidnHistory(recid(msisdn2)).
   ASSIGN
      msisdn.pos = "CC".
ENd.

output stream sout close.

