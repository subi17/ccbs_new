/*SER-15060 monitoring script for checking mmlog queue length */
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE STREAM sout.

OUTPUT STREAM sout TO VALUE("/opt/tools/monitoring/mmlog_monitoring.txt").

FOR EACH MMLog NO-LOCK:
   i = i + 1.
end.

put stream sout UNFORMATTED STRING(i).
quit.

