{Syst/commpaa.i}
Syst.CUICommon:katun = "anttis".
Syst.CUICommon:gcBrand = "1".
{Func/barrfunc.i}

DEFINE VARIABLE lrOLBRec as recid no-undo.
DEFINE VARIABLE lcB AS CHARACTER NO-UNDO. 
output to /home/anttis/as_yts1328.txt.
lrOLBRec = fCheckRestore(1658388,"OLB").

FIND MsRequest WHERE RECID(MsRequest) = lrOLBRec NO-LOCK NO-ERROR.
IF AVAILABLE MsRequest THEN lcB = MsRequest.ReqCParam1.

put unformatted msrequest.msrequest " " msrequest.reqcparam1 " " msrequest.reqcparam2 "->" skip.

