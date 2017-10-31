{Syst/commpaa.i}
{Func/msisdn.i}
Syst.CUICommon:katun = "anttis".
Syst.CUICommon:gcBrand = "1".

find first msisdn where
   msisdn.brand = "1" and
   msisdn.cli = "606107353" NO-LOCK use-index cli NO-eRROR.

fMakeMsidnHistory(recid(msisdn)).
msisdn.statuscode = 11.
