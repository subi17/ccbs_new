DEFINE VARIABLE clis AS CHARACTER NO-UNDO. 
clis = "622080530 622122538 622368608 633226875 649354025 655794614".
DEFINE VARIABLE lcCLi AS CHARACTER NO-UNDO. 

DEFINE VARIABLE i AS INTEGER NO-UNDO. 

do i = 1 to num-entries(clis, " ") with frame a:
   lcCLi = entry(i, clis, " ").
   FOR EACH  termmobsub where
      termmobsub.cli = lcCLi NO-LOCK:
      FOR EACH limit where
         limit.limittype = 3 and
         limit.msseq = termmobsub.msseq and
         limit.todate > today NO-LOCK:
         
         disp termmobsub.cli limit.limitamt limit.fromdate limit.todate with 1 col with frame a.
      END.
   end.
end.
