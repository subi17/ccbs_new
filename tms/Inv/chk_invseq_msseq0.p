define input parameter icOutputFile as char.
DEFINE VARIABLE ok AS LOGICAL NO-UNDO init true.
def stream sout.
output stream sout to value(icOutputFile) append.
put stream sout unformatted 
  "TEST chk_invseq_msseq0.p" skip(1).

for each invseq no-lock where
         invseq.msseq = 0:
   if ok then ok = false.
   disp stream sout fromdate todate 
        can-find(first mobcdr where
                       mobcdr.invcust = invseq.custnum and
                       mobcdr.invseq  = invseq.invseq) skip.
end.

if ok then put stream sout unformatted "OK" skip(1).
else put stream sout unformatted skip(1).
output stream sout close.
