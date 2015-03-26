{commpaa.i}
{msisdn.i}
katun = "anttis".
gcBrand = "1".

find first msisdn where
   msisdn.brand = "1" and
   msisdn.cli = "606107353" NO-LOCK use-index cli NO-eRROR.

fMakeMsidnHistory(recid(msisdn)).
msisdn.statuscode = 11.
