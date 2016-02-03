{Syst/testpaa.i}
katun  = "ari".
{Func/barrfunc.i}


DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 

def stream sread.
input stream sread from /apps/snet/200908/remove_drest_barring_090811_II.txt.

def stream slog.
output stream slog to /apps/snet/200908/aam_ydr9_unbarr_III.log append.

DEFINE VARIABLE lcBarrStatus AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lrBarring as rowid no-undo.
DEFINE VARIABLE lcResult AS CHARACTER NO-UNDO. 

repeat:

   import stream sread lccli.
   if lccli = "" then next.
   
   find mobsub where
      mobsub.cli = lcCli  NO-LOCK NO-ERROR.
   if not avail mobsub then do:
      put stream slog unformatted lcCli " not found" skip.
      next.
   end.

   lcBarrStatus = fCheckBarrStatus(mobsub.msseq, output lrBarring).
   
   if lcBarrStatus = "OK" then do:
      put stream slog mobsub.cli " " lcBarrStatus skip.
      next.
   end.  
   
   else if lcBarrStatus = "D_REST" then do:

     /* create barring request */
     RUN barrengine (mobsub.MsSeq,
                     "UN" + lcBarrStatus,
                     "5",           /* source  */
                     "YDR-8",       /* creator */
                     fMakeTS() + 0.0012, /* activate, 2min delay */
                     "",                 /* SMS */
                     OUTPUT lcResult).
      put stream slog mobsub.cli " " lcBarrStatus " " lcResult skip.
   end.
   
   else put stream slog mobsub.cli " " lcBarrStatus " error" skip.
end.


