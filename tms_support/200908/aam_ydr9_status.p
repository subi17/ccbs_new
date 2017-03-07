{Syst/testpaa.i}
katun  = "ari".
{Func/barrfunc.i}


DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 

def stream sread.
input stream sread from /apps/snet/200908/remove_drest_barring_090811_II.txt.

def stream slog.
output stream slog to /apps/snet/200908/aam_ydr9_status_III.log.

DEFINE VARIABLE lcBarrStatus AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lrBarring as rowid no-undo.
DEFINE VARIABLE lcResult AS CHARACTER NO-UNDO. 

repeat:

   import stream sread lccli.
   if lccli = "" then next.
   
   pause 0.
   disp lccli format "x(12)".
   
   find mobsub where
      mobsub.cli = lcCli  NO-LOCK NO-ERROR.
   if not avail mobsub then next.

   lcBarrStatus = fCheckBarrStatus(mobsub.msseq, output lrBarring).
   
   put stream slog mobsub.cli " " lcBarrStatus skip.
end.


