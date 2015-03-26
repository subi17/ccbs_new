DEFINE VARIABLE i AS INTEGER NO-UNDO. 
output to /tmp/pmg25.txt.
FOR EACH prepaidrequest  where 
   brand = "1" and
   ppstatus eq 2 and 
   tsrequest > 20080506 and tsrequest < 20080506.36000 and
   tsresponse > 20080506 and tsresponse < 20080506.36000  NO-LOCK:
   export prepaidrequest.
   i = i + 1.
END.
output close.
disp i.
