{testpaa.i}
katun  = "anttis".
{barrfunc.i}

input from /apps/snet/200908/barrings_from_Y_to_D_REST.txt.
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liMsSeq AS INTEGER NO-UNDO. 
def stream slog.
output stream slog to /apps/snet/200908/as_ydr9.log.
DEFINE VARIABLE lcBarrStatus AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcBarrComList AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lrBarring as rowid no-undo.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
repeat:
   import unformatted lcLine.
   
   liMsSeq = int(entry(1,lcLine," ")).
   lcCli = entry(2,lcLine," ").

   i = i + 1.
   
   if i mod 2500 = 0 then disp i.

   find mobsub where
      mobsub.msseq = liMsSeq  NO-LOCK NO-ERROR.

   IF NOT AVAIL mobsub then do:
      put stream slog unformatted lcLine " ERROR:Subscription not found" skip.
      next.
   end.
   
   if mobsub.cli ne lcCLi THEN DO:
      put stream slog unformatted lcLine " ERROR:MSISDN doesn't match" skip.
      next.
   END.
/*
   RUN checkmsbarring(
       INPUT mobsub.MsSeq,
       INPUT katun,
       OUTPUT lcBarrComList,
       OUTPUT lcBarrStatus).
*/
   lcBarrStatus = fCheckBarrStatus(mobsub.msseq, output lrBarring).
   
   IF lcBarrStatus EQ "D_REST" THEN DO:
      put stream slog unformatted lcLine " OK  " lcBarrStatus skip.
      next.
   END.

   IF lcBarrStatus NE "Y_REST" THEN DO:
      put stream slog unformatted lcLine " ERROR:wrong barring status " lcBarrStatus skip.
      next.
   END.
  
   find msrequest where
      rowid(msrequest) = lrBarring NO-LOCK NO-ERROR.
   
   IF msrequest.reqcparam1 NE "Y_REST" THEN DO:
      put stream slog unformatted lcLine " ERROR:wrong barring status " msrequest.reqcparam1 skip.
      next.
   END.
   
   find current msrequest EXCLUSIVE-LOCK.
   msrequest.reqcparam1 = "D_REST".
   release msrequest.

   put stream slog unformatted lcLine " OK " lcBarrStatus skip.

end.
