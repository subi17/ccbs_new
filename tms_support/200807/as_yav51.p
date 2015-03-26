DEFINE TEMP-TABLE tt
FIELD usercode AS CHAR
FIELD i AS INT
INDEX cust IS PRIMARY UNIQUE usercode. 

FOR EACH msrequest where 
   msrequest.brand = "1" and
   msrequest.reqtype = 6 and
   msrequest.reqsource = "6" and
   (msrequest.ReqIParam3 = 0 or msrequest.ReqIParam4 = 0) NO-LOCK:
   FIND FIRST tt where tt.usercode = msrequest.usercode EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAIL tt then do:
      disp msrequest.custnum.
      create tt.
      assign
         tt.usercode = msrequest.usercode.
         tt.i       = tt.i + 1.
   end.
   else do:
      tt.i = tt.i + 1.
   end.
   release tt.
END.

output to /apps/snet/200807/as_yav51.txt.
FOR EACH tt NO-LOCK:
   /* remove VISTA_  from usercode */
   IF tt.usercode begins "VISTA_" THEN
      put unformatted substring(tt.usercode,7,length(tt.usercode)) "|" tt.i skip.
   else
      put unformatted tt.usercode "|" tt.i skip.
END.

output close.

