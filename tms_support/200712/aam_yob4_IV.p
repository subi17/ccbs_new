{Syst/testpaa.i}
katun = "ari".

def var i as int no-undo.

def stream slog.
output stream slog to /apps/snet/200712/rebill_0711.log append.

for each invoice no-lock use-index invdate where
         brand = "1" and
         invdate = 12/10/7 and
         invtype = 1 and
         printstate = 0,
   first invseq no-lock where
         invseq.msseq = invoice.msseq and
         invseq.billed = false and
         invseq.fromdate < 12/1/7:
         
         
   /*
   disp invoice.invnum invoice.custnum invoice.cli.
   disp invseq.
   disp can-find(first mobcdr where
                       mobcdr.invcust = invseq.custnum and
                       mobcdr.invseq  = invseq.invseq).
   */

   put stream slog unformatted
       invoice.custnum  chr(9)
       invoice.cli      chr(9)
       invoice.msseq    skip.
       
   run del_inv.p (invoice.invnum).
   
   i = i + 1.
   pause 0.
   disp i with 1 down.
end.         
 