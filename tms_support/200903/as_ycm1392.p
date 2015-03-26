{commpaa.i}
katun = "anttis".
gcBrand = "1".
{msisdn.i}

find first msisdn where
   msisdn.brand = "1" and
   msisdn.cli = "622167597" NO-LOCK USE-INDEX CLI.

fMakeMsidnHistory(RECID(msisdn)).
assign msisdn.statuscode = 1 
       msisdn.outoperator = "".
