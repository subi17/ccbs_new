{testpaa.i}
katun = "anttis".

def stream sread.
input stream sread from /apps/snet/200709/change_ICC_stock_yoi422.txt.

def stream slog.
output stream slog to /apps/snet/200709/as_yoi422.log.

def var lcline  as char no-undo.
def var lcicc   as char  no-undo.
def var lcOldStock as char no-undo.
def var i       as int  no-undo.

repeat:

   import stream sread unformatted lcline.
   
   assign lcicc = entry(1,lcline,chr(9))
          i     = i + 1.
 
   find first sim where 
      sim.icc = lcicc and
      sim.brand = "1" exclusive-lock no-error.
   if not available sim then do:
      message "check icc:" lcicc
      view-as alert-box. 
      next.
   end.

   if sim.stock = "MNP" THEN DO:
      lcOldStock = sim.stock.
      IF sim.simstat = 1 THEN      
         sim.stock = "RETAILER".
      put stream slog unformatted
         sim.icc      chr(9)
         sim.simstat  chr(9)
         lcOldStock  + " -> " +
         sim.stock  skip.
   END.
   ELSE DO:
      put stream slog unformatted
               sim.icc           chr(9)
               "wrong sim stock" chr(9)
               sim.stock          skip.
   END.

end.


