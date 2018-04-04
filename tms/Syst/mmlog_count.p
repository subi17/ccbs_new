/*SER-15060 monitoring script for checking mmlog queue length */
DEFINE VARIABLE i AS INTEGER NO-UNDO.

FOR EACH MMLog NO-LOCK:
   i = i + 1.
end.

message i.
quit.

