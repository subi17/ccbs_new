{commpaa.i}
katun = "anttis".
gcBrand = "1".
{msreqfunc.i}

find msrequest where
     msrequest.msrequest = 24390429 and
     msrequest.reqstatus = 3 NO-LOCK.

fReqStatus(2,"").

find msrequest where
     msrequest.msrequest = 24390428 and
     msrequest.reqstatus = 3 NO-LOCK.

fReqStatus(2,"").
