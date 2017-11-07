{Syst/commpaa.i}
Syst.Var:katun = "anttis".
Syst.Var:gcBrand = "1".
{Func/msisdn.i}

find first msisdn where
   msisdn.brand = "1" and
   msisdn.cli = "622006262" NO-LOCK USE-INDEX CLI.

fMakeMsidnHistory(RECID(msisdn)).
assign msisdn.statuscode = 1.
