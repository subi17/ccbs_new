{Syst/commpaa.i}
Syst.CUICommon:katun = "anttis".
Syst.CUICommon:gcBrand = "1".
{Func/msisdn.i}

FIND FIRST MSISDN WHERE
   msisdn.brand = "1" and
   msisdn.cli = "622111882" NO-LOCK NO-ERROR.

fMakeMsidnHistory(recid(msisdn)).

assign
msisdn.custnum = 1001
msisdn.statuscode = 1
msisdn.outoperator = "".

