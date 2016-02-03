{Syst/testpaa.i}
katun = "qvantel".
{Func/barrfunc.i}

def stream sread.
input stream sread from /home/ari/work/aam_yts2141.log.

def stream slog.
output stream slog to /apps/yoigo/tms_support/201004/aam_yts2141_unbarr.log
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

def var libaselimit1 as dec no-undo.
def var libaselimit2 as dec no-undo.
def var liuselimit1 as dec no-undo.
def var liuselimit2 as dec no-undo.
def var lcbarrpack as char no-undo.
def var lcresult as char no-undo.

assign 
   libaselimit1 = 80
   libaselimit2 = 120.

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
   /*
   if j > 6000 then next. 
   */
   
   ldcounter = 0.
   
   assign 
      liuselimit1 = libaselimit1
      liuselimit2 = libaselimit2.
      
   FOR EACH Limit NO-LOCK USE-INDEX CustNum WHERE
            Limit.CustNum   = msowner.invCust           AND
            Limit.LimitType = 1                   AND
            Limit.TMRuleSeq = 3 AND
            Limit.ToDate   >= TODAY               AND
            Limit.FromDate <= TODAY:
      if limit.limitid = 1 then liuselimit1 = limit.limitamt.
      else if limit.limitid = 2 then liuselimit2 = limit.limitamt.
   end.

   for first tmcounter no-lock where
            tmcounter.msseq = msowner.msseq and
            tmruleseq = 3 and 
            tmcounter.todate = 4/30/10:
      ldcounter = tmcounter.amount.
      lilimit = tmcounter.limitid.
   end.
   
   if lilimit < 2 then next. 

   i = i + 1.
      
   lcbarrpack = fCheckStatus(msowner.msseq). 

   disp i format ">>>>>"
        j format ">>>>>"
      lccli format "x(10)" 
      ldcounter format "->>>>>>>9.99"
      llbarred
      (ldcounter > liuselimit2)
      liuselimit2
      lcbarrpack.

   if not llbarred then do:
      for first tmcounter exclusive-lock where
                tmcounter.msseq = msowner.msseq and
                tmruleseq = 3 and 
                tmcounter.todate = 4/30/10:
          if ldcounter > liuselimit1 then 
          tmcounter.limitid = 1.
          else tmcounter.limitid = 0.
      end.                   
   end.

   else if ldcounter < liuselimit2 and lcbarrpack = "y_rest" then do:
      RUN barrengine (msowner.MsSeq,
                   "UNY_REST",
                   "9",                /* source  */
                   katun,       /* creator */
                   fMakeTS() + 0.0012, /* activate, 2min delay */
                   "",                 /* SMS */
                   OUTPUT lcResult).
                      
       put stream slog unformatted
          msowner.cli  chr(9)
          msowner.msseq chr(9)
          lcbarrpack   chr(9)
          lcresult  skip.
                      
   end.

end.

input stream sread close.

