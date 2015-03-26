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


for each invoice no-lock use-index invdate where      
         invoice.brand = "1" and
         invoice.invdate >= 11/1/7 and
         invoice.invdate <= 11/30/7 and
         invoice.invtype = 1,
   first invrow of invoice no-lock where
         invrow.rowtype = 5,
    each invseq no-lock where
         invseq.msseq  = invoice.msseq and
         invseq.billed = false and
         invseq.fromdate < 11/1/7:
         
   
   i = i + 1.

   ldamt = 0.
   for each mobcdr no-lock where
            mobcdr.invcust = invseq.custnum and
            mobcdr.invseq  = invseq.invseq  and
            mobcdr.datest < 11/1/7:
      ldamt = ldamt + mobcdr.amount.
   end.
            

   /* if mincons already billed and cdr amount less than that -> 
      move to unbillable */
   if ldamt <= 6 then do:
      j = j + 1.

      for each mobcdr no-lock where
               mobcdr.invcust = invseq.custnum and
               mobcdr.invseq  = invseq.invseq  and
               mobcdr.datest < 11/1/7:
         
         export stream slog mobcdr.
   
         find bcdr where recid(bcdr) = recid(mobcdr) exclusive-lock.
         bcdr.errorcode = 8049.
         bcdr.invseq    = 0.
      end.

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
         
         
