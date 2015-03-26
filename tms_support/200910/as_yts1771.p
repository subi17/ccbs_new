DEFINE VARIABLE lcCustnums AS CHARACTER NO-UNDO. 

lcCustnums = "23532,1386679,627697". 
DEFINE VARIABLE liCustnum AS INTEGER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def stream sout.
output stream sout to /apps/snet/200910/as_yts1771_limit.d.
do i = 1 to num-entries(lcCustnums):
   liCustnum = int(entry(i,lcCustnums)).
   FOR EACH limit where
       limit.custnum  = liCustnum and
       limit.limittype = 2 and
       limit.todate < today EXCLUSIVE-LOCK:
      export stream sout limit.
      delete limit.
   end.

end.
