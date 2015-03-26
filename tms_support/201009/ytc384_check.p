DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 
DEFINE VARIABLE liStatus AS INTEGER NO-UNDO. 

DEFINE VARIABLE statuses AS CHARACTER NO-UNDO. 
statuses = "0,1,2,3,4,5,6,9".

do j = 1 to num-entries(statuses) with frame a  15 down:
   i = 0.
   liStatus = int(entry(j,statuses)).
   FOR EACH msrequest where
      msrequest.brand = "1" and
      msrequest.reqtype = 13 and
      msrequest.reqstatus = liStatus and
      msrequest.actstamp = 20100915.84600 NO-LOCK:
      i = i + 1.
   end.
   disp liStatus i with frame a.
end.

