/* Check msrequest status counts */

def stream sout.
output stream sout to /apps/snet/200806/yts775_check.txt.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
FOR EACH msreqstatistic NO-LOCK:
  i = 0.
  FOR EACH msrequest where 
   msrequest.brand = "1" and
   msrequest.reqtype = msreqstatistic.reqtype and 
   msrequest.reqstatus = msreqstatistic.reqstatus NO-LOCK:
   i = i + 1.
  END.
  put stream sout unformatted msreqstatistic.reqtype " " 
                  msreqstatistic.reqstatus " "
                  msreqstatistic.reqstatuscount " "
                  i " ".
  if msreqstatistic.reqstatuscount ne i THEN 
   put stream sout unformatted "!!!" skip.
  else put stream sout unformatted  skip.
END.

output stream sout close.
