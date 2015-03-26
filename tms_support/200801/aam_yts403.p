def stream sread.
input stream sread from /apps/snet/200801/resend_to_dextra.txt.

def var lcline as char no-undo.
def var lccli  as char no-undo.
def var i      as int  no-undo.
def var j      as int  no-undo.

repeat:

   import stream sread unformatted lccli.
   
   for first order no-lock use-index cli where
             order.brand = "1" and
             order.cli   = lccli and
             order.crstamp > 20071201,
       first sim exclusive-lock where
             sim.icc     = order.icc and
             sim.simstat = 21:
             
      sim.simstat = 20.

      j = j + 1.
   end.
   
   i = i + 1.
   
   pause 0.
   disp i j with 1 down.
end.


   
