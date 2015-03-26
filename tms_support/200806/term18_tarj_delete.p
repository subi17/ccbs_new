def var i as int no-undo.
def var j as int no-undo.

def buffer bcli for dccli.

def stream slog.
output stream slog to /apps/snet/200806/term18_tarj_delete.log append.

for each dccli no-lock where
         dccli.brand = "1" and
         dccli.dcevent = "term18" and
         dccli.validto > today,
   first order no-lock where
         order.msseq = dccli.msseq and
         order.paytype = true,
   first msowner no-lock where
         msowner.msseq = dccli.msseq:
         
   i = i + 1.

   /*
   disp msowner.msseq 
        msowner.cli
        msowner.clitype
        dccli.validfrom 
        dccli.validto.
   */

   if i mod 100 = 0 then do:
      pause 0.
      disp i dccli.validfrom with 1 down.
   end.

   find bcli where recid(bcli) = recid(dccli) exclusive-lock.
   export stream slog bcli.
   delete bcli.

end.


disp i.
