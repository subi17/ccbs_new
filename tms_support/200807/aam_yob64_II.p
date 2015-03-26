def var i as int no-undo.
def var j as int no-undo.
def var lccli as char no-undo.

def stream sread.
input stream sread from /apps/snet/200807/deny_printing_080701.txt.

repeat:

   import stream sread unformatted lccli.
   
   i = i + 1.
   
   for first invoice no-lock use-index cli where
         invoice.brand = "1" and
         invoice.cli   = lccli and
         invoice.invdate = 7/1/8 and
         invoice.invtype = 1:

      j = j + 1.

      find binv where recid(binv) = recid(invoice) exclusive-lock.
      binv.InvCfg[1] = true.
   end.    
         
   if i mod 10 = 0 then do:
      pause 0.
      disp i j with 1 down.
   end.
end.   
      
disp i j.

