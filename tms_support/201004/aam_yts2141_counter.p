{Syst/testpaa.i}
katun = "qvantel".

def stream sread.
input stream sread from /home/ari/work/aam_yts2141.log.

def stream slog.
output stream slog to /apps/yoigo/tms_support/201004/aam_yts2141_counterIV.log
    append.
    
def var lcline as char no-undo.
def var lccli  as char no-undo.
def var i as int no-undo.
def var j as int no-undo.
def var ldamt as dec no-undo.
def var ldamt2 as dec no-undo.
def var ldcounter as dec no-undo.
def var llbarred as log no-undo.
def var lilimit as int no-undo.

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

   j = j + 1.
   if j < 6000 or j > 7000 then next.
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
   end.         

   if ldcounter ne ldamt then 
   for first tmcounter exclusive-lock where
            tmcounter.msseq = msowner.msseq and
            tmruleseq = 3 and 
            tmcounter.todate = 4/30/10:
      tmcounter.amount = ldamt.
   end.
   
   if lilimit = 0 then next. 

   i = i + 1.
      
   pause 0.
   disp i format ">>>>>"
        j format ">>>>>"
      lccli format "x(10)" ldamt format "->>>>>>>9.99" 
      ldcounter format "->>>>>>>9.99"
      llbarred.

   RUN Rate/cli_rate.p (lccli,
                   4/1/10,
                   4/30/10,
                   true).

   ldamt2 = 0.
   for each mobcdr no-lock use-index cli where
            mobcdr.cli = lccli and
            mobcdr.datest >= 4/1/10 and
            mobcdr.datest <= 4/30/10 and
            mobcdr.errorcode = 0:
      ldamt2 = ldamt2 + mobcdr.amount.
   end.         
   for first tmcounter exclusive-lock where
            tmcounter.msseq = msowner.msseq and
            tmruleseq = 3 and 
            tmcounter.todate = 4/30/10:
      tmcounter.amount = ldamt2.
   end.

   put stream slog unformatted
      i     chr(9)
      lccli chr(9)
      ldamt  chr(9)
      ldamt2 chr(9)
      llbarred skip.
  
   pause 0.
   disp ldamt2 format "->>>>>>>9.99".
   /*
   i = i + 1.
   pause 0.
   disp i j with 1 down.
   */

end.

input stream sread close.

