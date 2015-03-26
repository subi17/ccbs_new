def stream slog.
output stream slog to /apps/snet/200902/aam_ytd15.log.

put stream slog unformatted
    "Error"   chr(9)
    "Qty"     skip.

def var liqty as int no-undo.
def var i     as int no-undo.


for each moberror no-lock:

   liqty = 0.
   
   for each mobcdr no-lock where
            mobcdr.errorcode = moberror.moberror:
            
       
      assign
         liqty = liqty + 1
         i = i + 1.
         
      if i mod 1000 = 0 then do:
         pause 0.
         disp i mobcdr.errorcode with 1 down.
      end.
   end.        

   put stream slog unformatted
      moberror.moberror chr(9)
      liqty skip.
end.

output stream slog close.
