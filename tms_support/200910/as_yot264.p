{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "anttis".
{Func/msreqfunc.i}
find msrequest where
   msrequest.msrequest = 15583137 and
   msrequest.reqstatus = 6 NO-LOCK.

fReqStatus(9,"YOT-263").
