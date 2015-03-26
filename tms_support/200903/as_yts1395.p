DEFINE VARIABLE msreqs AS CHARACTER NO-UNDO. 
msreqs = "7396880 7415762 7427081 7793829".

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

do i = 1 to num-entries(msreqs," ") with frame a:
   find msrequest where
        msrequest.msrequest = int(entry(i, msreqs, " ")) EXCLUSIVE-LOCK NO-ERROR.
   msrequest.reqsource = "5". 
   disp msrequest.reqstatus msrequest.memo reqsource.
end.
