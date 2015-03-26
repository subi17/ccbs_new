{commpaa.i}
katun = "anttis".
gcBrand = "1".
{msisdn.i}

find first msisdn where
   msisdn.brand = "1" and
   msisdn.cli = "622775222" NO-LOCK use-index cli NO-eRROR.

fMakeMsidnHistory(recid(msisdn)).
assign
   msisdn.statuscode = 6 
   msisdn.portingdate = ?. 

