{Syst/commpaa.i}
Syst.Var:katun = "anttis".
Syst.Var:gcBrand = "1".

{Func/msreqfunc.i}

find msrequest where
     msrequest.msrequest = 30337730 and
     msrequest.reqstatus = 4 NO-LOCK.

freqstatus(6,"YOT-957").
