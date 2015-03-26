FOR EACH tmspass where 
   tmspass.usercode = "twatts" EXCLUSIVE-LOCK:
   tmspass.usercode = "tonyw".
END.

FOR EACH tmsuser where 
   tmsuser.usercode = "twatts" EXCLUSIVE-LOCK:
   tmsuser.usercode = "tonyw".
END.
