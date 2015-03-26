def stream slog.
output stream slog  to /apps/snet/200909/as_yts1690.log2 append.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

def buffer pp for prepaidrequest.
def buffer pp_mod for prepaidrequest.
FOR EACH prepaidrequest where
   prepaidrequest.brand = "1" and
   prepaidrequest.source = "web order" and
   tsrequest > 20090923.25200  NO-LOCK:
   
   if respcode = 102 then do:
      
      find pp where 
         pp.brand = "1" and
         pp.msseq = prepaidrequest.msseq and
         pp.source ne "atm" and
         pp.request ne "rcg" and
         recid(pp) ne recid(prepaidrequest) NO-LOCK NO-eRROR.
      
      find pp_mod where
         recid(pp_mod) = recid(prepaidrequest) EXCLUSIVE-LOCK.

      pp_mod.ppstatus = 0.

      put stream slog unformatted prepaidrequest.pprequest " " prepaidrequest.cli " " avail(pp) " " prepaidrequest.tsrequest skip.
   end.
end.


disp i.
