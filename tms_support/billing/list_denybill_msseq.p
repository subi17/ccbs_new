{Func/timestamp.i}

def stream slog.

def var i       as int  no-undo.
def var limsseq as int  no-undo.

pause 0.
update 
   limsseq format ">>>>>>>>9"
   label "Subscription ID"
   help "Subscription ID"
   with overlay row 10 centered title " DENIALS " side-labels frame fcrit.
hide frame fcrit no-pause.

output stream slog to value("/tmp/denybill_msseq_" + 
                            string(limsseq) + ".txt").

put stream slog unformatted
   "Subscription ID"    chr(9)
   "Customer"           chr(9)
   "Code"               chr(9)
   "Valid From"         chr(9)
   "Valid To"           skip.

for each limit no-lock use-index msseq where
         limit.msseq     = limsseq and   
         limit.limittype = 3 and
         limit.tmruleseq = 0 and
         limit.limitid   = 0 
by limit.fromdate:
         
   i = i + 1.
   
   put stream slog unformatted
      limit.msseq      chr(9)
      limit.custnum    chr(9)
      limit.limitamt   chr(9)
      limit.fromdate   chr(9)
      limit.todate     skip.
      
end.

disp i.

output stream slog close.


