{Syst/commpaa.i}
katun = "anttis".
Syst.CUICommon:gcBrand = "1".
{Func/msreqfunc.i}

find msrequest where
   msrequest.msrequest = 15941011 and
   msrequest.reqtype = 1 and
   msrequest.reqstatus = 0 NO-LOCK.

IF AVAIL msrequest then
fReqStatus(4,"YOT-308").
