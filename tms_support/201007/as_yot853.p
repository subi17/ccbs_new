{Syst/commpaa.i}
Syst.Var:katun = "anttis".
Syst.Var:gcBrand = "1".
{Func/msreqfunc.i}

find msrequest where
     msrequest.msrequest = 24390429 and
     msrequest.reqstatus = 3 NO-LOCK.

fReqStatus(2,"").

find msrequest where
     msrequest.msrequest = 24390428 and
     msrequest.reqstatus = 3 NO-LOCK.

fReqStatus(2,"").
