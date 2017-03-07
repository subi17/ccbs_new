{Func/timestamp.i}

def var i as int no-undo.
def var j as int no-undo.

def var lldel  as log  no-undo.
def var ldtto  as date no-undo.
def var litime as int  no-undo.

def buffer bcli for dccli.

def stream slog2.
output stream slog2 to /apps/snet/200806/gprs_cont_terminate.log append.

for each dccli no-lock where
         dccli.brand = "1" and
         dccli.dcevent = "gprs" and
         dccli.validto > today,
   first termmobsub no-lock where
         termmobsub.msseq = dccli.msseq and
         termmobsub.clitype begins "cont":
         
   if can-find(first mobsub where mobsub.msseq = dccli.msseq) then next. 

   ldtto = ?.
   for first msowner no-lock where
             msowner.msseq = dccli.msseq and
             msowner.clitype = termmobsub.clitype:
             
      fsplitts(msowner.tsend,
               output ldtto,
               output litime).
   end.      
       
   if ldtto = ? then next.

   /* 
   disp msowner.msseq 
        msowner.cli
        msowner.clitype
        dccli.validfrom 
        dccli.validto.
   */
   
   find bcli where recid(bcli) = recid(dccli) exclusive-lock.
   
   put stream slog2 unformatted
      dccli.msseq  chr(9)
      dccli.dcevent chr(9)
      dccli.validto chr(9)
      ldtto         skip.
         
   bcli.validto = ldtto.
      
   j = j + 1.

   pause 0.
   disp i j with 1 down.
       
end.


disp i j.


