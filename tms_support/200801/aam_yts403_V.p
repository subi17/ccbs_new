def stream sread.
input stream sread from /apps/snet/200801/resend_to_dextra_080114.txt.

def var lccli as char no-undo.
def var i     as int  no-undo.
def var j     as int  no-undo.

repeat:

   import stream sread unformatted lccli.
   
   i = i + 1.
   
   for first order no-lock where
             order.brand = "1" and
             order.cli   = lccli and
             order.crstamp > 20071101 and
             lookup(order.statuscode,"1,3,7") = 0,
       first sim no-lock where
             sim.icc = order.icc and
             sim.simstat = 21:
             
      j = j + 1.       
   end.
   
   pause 0.
   disp i j with 1 down.
end.


