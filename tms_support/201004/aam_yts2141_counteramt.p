{testpaa.i}
katun = "qvantel".

def stream sread.
input stream sread from /home/ari/work/aam_yts2141.log.

def var lcline as char no-undo.
def var lccli  as char no-undo.
def var i as int no-undo.
def var j as int no-undo.
def var ldamt as dec no-undo.
def var ldamt2 as dec no-undo.
def var ldcounter as dec no-undo.
def var llbarred as log no-undo.
def var lilimit as int no-undo.
def var k as int no-undo.

repeat:
    
   import stream sread unformatted lcline.

   lccli = entry(1,lcline,chr(9)).

   find first msowner where msowner.cli = lccli and
              msowner.tsend > 20100428
      no-lock no-error.
   if not available msowner then next.

   llbarred = false.
   find first mobsub where mobsub.cli = lccli no-lock no-error.
   if available mobsub and mobsub.msstat = 8 then llbarred = true.

   ldcounter = 0.
   for first tmcounter no-lock where
            tmcounter.msseq = msowner.msseq and
            tmruleseq = 3 and 
            tmcounter.todate = 4/30/10:
      ldcounter = tmcounter.amount.
      lilimit = tmcounter.limitid.
   end.
      
   ldamt = 0.
   for each mobcdr no-lock use-index cli where
            mobcdr.cli = lccli and
            mobcdr.datest >= 4/1/10 and
            mobcdr.datest <= 4/30/10 and
            mobcdr.errorcode = 0:
      ldamt = ldamt + mobcdr.amount.
      k = k + 1.
   end.         

   if round(ldcounter,2) ne round(ldamt,2) then do:
      j = j + 1.
      for first tmcounter exclusive-lock where
                tmcounter.msseq = msowner.msseq and
                tmruleseq = 3 and 
                tmcounter.todate = 4/30/10:
         tmcounter.amount = ldamt.
      end.
   end.
   
   i = i + 1.
   pause 0.
   disp i j k ldcounter ldamt with 1 down.

end.

input stream sread close.

