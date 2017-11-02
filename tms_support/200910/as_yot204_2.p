{Syst/commpaa.i}
Syst.CUICommon:katun = "anttis".
Syst.CUICommon:gcBrand = "1".
{Func/msisdn.i}

find first msisdn where
   msisdn.brand = "1" and
   msisdn.cli = "622775222" NO-LOCK use-index cli NO-eRROR.

fMakeMsidnHistory(recid(msisdn)).
assign
   msisdn.statuscode = 6 
   msisdn.portingdate = ?. 

