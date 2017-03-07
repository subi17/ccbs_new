{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/msreqfunc.i}

/*
find msrequest where
   msrequest.msrequest = 14902004 NO-LOCK.

find msrequest where
   msrequest.msrequest = 14883036 NO-LOCK.

ReqStatus(2,"YOT-194").
*/

DEFINE VARIABLE lcMsReqs AS CHARACTER NO-UNDO init "14901549 14900773 14900570 14899629 14899404 14899406 14899411 14898284 14898287".

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

do i = 1 to num-entries(lcMsReqs, " ") with frame a:
   find msrequest where
      msrequest.msrequest = int(entry(i, lcMsReqs, " ")) NO-LOCK.
   disp msrequest.cli msrequest.reqtype msrequest.reqstatus.
   fReqStatus(2,"YOT-194").
   release msrequest.
end.  
