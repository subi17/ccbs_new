/* monitoring script for checking sms send queue length */
{timestamp.i}

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE d AS DECIMAL NO-UNDO. 
d = fmakets().

FOR EACH callalarm where 
         callalarm.brand = "1" and
         callalarm.delistat = 1 and
         callalarm.delitype = 1 and
         callalarm.actstamp < d NO-LOCK:
   i = i + 1.
end.  

message i.
quit.
