FIND FIRST eventlog where
  tablename = "customer" and
  key = "22610" NO-LOCK NO-error.

DEFINE VARIABLE i AS INTEGER NO-UNDO.

def stream sout.
output stream sout to /apps/snet/200802/as_yts477.txt.

DEFINE VARIABLE j AS INTEGER NO-UNDO. 
do i = 1 to num-entries(eventlog.modifiedfields,","):
  j = index(eventlog.datavalues,entry(i,eventlog.modifiedfields,",")).
  put stream sout unformatted
      "customer." entry(i,eventlog.modifiedfields,",") 
      " = " entry(2,substring(eventlog.datavalues,j),chr(255)) "." skip.
end.

output stream sout close.
