{Func/timestamp.i}

def var i as int no-undo.
def var j as int no-undo.

def var lldel  as log  no-undo.
def var ldtto  as date no-undo.
def var litime as int  no-undo.

def buffer bcli for dccli.

def stream slog.
output stream slog to /apps/snet/200806/gprs_tarj_delete.log append.

def stream slog2.
output stream slog2 to /apps/snet/200806/gprs_tarj_terminate.log append.

for each dccli no-lock where
         dccli.brand = "1" and
         dccli.dcevent = "gprs" and
         dccli.validto > today,
   first mobsub no-lock where
         mobsub.msseq = dccli.msseq and
         mobsub.clitype begins "tarj":
         

   lldel = false.
   for first order no-lock where
             order.msseq = dccli.msseq and
             order.paytype = true:
      lldel = true.
   end.

   if not lldel then do:
      ldtto = ?.
      for first msowner no-lock where
                msowner.msseq = dccli.msseq and
                msowner.paytype = false:
             
         fsplitts(msowner.tsend,
                  output ldtto,
                  output litime).
      end.      
       
      if ldtto = ? then lldel = true.      
   end.

   /* 
   disp msowner.msseq 
        msowner.cli
        msowner.clitype
        dccli.validfrom 
        dccli.validto.
   */
   
   find bcli where recid(bcli) = recid(dccli) exclusive-lock.
   
   if lldel then do: 
   
      i = i + 1.
      export stream slog bcli.
      delete bcli.
   end.
   
   else do:
      put stream slog2 unformatted
         dccli.msseq  chr(9)
         dccli.dcevent chr(9)
         dccli.validto chr(9)
         ldtto         skip.
         
      bcli.validto = ldtto.
      
      j = j + 1.
   end.

   if i mod 100 = 0 then do:
      pause 0.
      disp i j with 1 down.
   end.
       
end.


disp i j.


