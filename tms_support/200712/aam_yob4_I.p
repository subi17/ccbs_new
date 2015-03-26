def var i     as int no-undo.
def var j     as int no-undo.
def var k     as int no-undo.
def var ldamt as dec no-undo.

def buffer bseq for invseq.
def buffer bcdr for mobcdr.

def stream slog.
def stream sseq.

output stream slog to /apps/snet/200712/aam_yob4_cdr.log append.
output stream sseq to /apps/snet/200712/aam_yob4_seq.log append.

for each invseq no-lock where
         invseq.billed = false and
         invseq.fromdate < 10/1/7:
         
   
   i = i + 1.

   /* nothing older than november will be billed */
   for each mobcdr no-lock where
            mobcdr.invcust = invseq.custnum and
            mobcdr.invseq  = invseq.invseq  and
            mobcdr.datest < 10/1/7:
     
      export stream slog mobcdr.

      find bcdr where recid(bcdr) = recid(mobcd) exclusive-lock.
      bcdr.errorcode = 8049.
      bcdr.invseq    = 0.
      j = j + 1.
   end.

   if not can-find(first mobcdr where
                         mobcdr.invcust = invseq.custnum and
                         mobcdr.invseq  = invseq.invseq)
   then do:

      export stream sseq invseq.
      
      find bseq where recid(bseq) = recid(invseq) exclusive-lock.
      delete bseq.

      k = k + 1.
   end.

   pause 0.
   disp i j k with 1 down.
end.
    

