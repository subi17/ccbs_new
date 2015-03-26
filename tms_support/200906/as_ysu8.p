DEFINE VARIABLE i AS INTEGER NO-UNDO. 
def stream sbak.
/*output stream sbak to /apps/snet/200906/as_ysu8.d.*/
output stream sbak to /apps/snet/200906/as_ysu8_0.d.
FOR EACH limit where
   limit.brand = "" and
   limit.limittype = 3 and
   limit.limitamt = 0 EXCLUSIVE-LOCK:
   export stream sbak limit. 
   delete limit. 
END.
