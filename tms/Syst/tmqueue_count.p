/* monitoring script for checking tmqueue length */
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

FOR EACH TMQueue NO-LOCK:
   i = i + 1.
end.  

message i.
quit.
