def var i     as int  no-undo.
def var j     as int  no-undo.
def var lccli as char no-undo.


def stream slog.
output stream slog to /apps/snet/200906/aam_ycm1637_rep.txt.

def stream sread.
input stream sread from /apps/snet/200906/aam_ycm1637_subslist.txt.

put stream slog unformatted
    "MSISDN"  chr(9)
    "Trans.Qty" skip.
    
repeat:
   import stream sread lccli.
   
   if lccli = "" then next.
   
   j = 0. 
   for each mobcdr no-lock use-index cli where
            mobcdr.cli = lccli and
            mobcdr.datest = 6/1/9 and
            mobcdr.errorcode = 8040:
      j = j + 1. 
   end.

   i = i + 1.
   put stream slog unformatted
      lccli chr(9) j skip.
      
end.

put stream slog unformatted
    "Total" chr(9)
    i skip.
    
input stream sread close.
output stream slog close.


