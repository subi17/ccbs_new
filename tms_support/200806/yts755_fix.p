/* Check msrequest status counts */

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
def buffer msrs for msreqstatistic.
FOR EACH msreqstatistic NO-LOCK:
  i = 0.
  FOR EACH msrequest where 
   msrequest.brand = "1" and
   msrequest.reqtype = msreqstatistic.reqtype and 
   msrequest.reqstatus = msreqstatistic.reqstatus NO-LOCK:
   i = i + 1.
  END.
  
  find msrs where rowid(msrs) = rowid(msreqstatistic) EXCLUSIVE-LOCK NO-ERROR.
  msrs.reqstatuscount = i.
  release msrs.
END.

