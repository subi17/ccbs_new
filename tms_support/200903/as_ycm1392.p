{Syst/commpaa.i}
katun = "anttis".
Syst.CUICommon:gcBrand = "1".
{Func/msisdn.i}

find first msisdn where
   msisdn.brand = "1" and
   msisdn.cli = "622167597" NO-LOCK USE-INDEX CLI.

fMakeMsidnHistory(RECID(msisdn)).
assign msisdn.statuscode = 1 
       msisdn.outoperator = "".
