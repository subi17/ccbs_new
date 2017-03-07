{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/msreqfunc.i}

find msrequest where
   /*  msrequest.msrequest = 38483668 and */
     msrequest.msrequest = 38483667 and
     msrequest.reqtype   = 1 and
     msrequest.reqstatus = 0.

fReqStatus(4,"YTS-2739").

