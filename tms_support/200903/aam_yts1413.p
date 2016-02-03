{Syst/testpaa.i}
katun = "ari".

{Func/barrfunc.i}


def var i as int no-undo.
def var lcresult as char no-undo. 

def stream slog.
def stream sread.

input stream sread from /apps/snet/200903/contrd1_barred.txt.
output stream slog to /apps/snet/200903/contrd1_unbarred.txt.

def var lcline as char no-undo.
def var lccli as char no-undo.

def var lcstat as char no-undo.


repeat:
    import stream sread unformatted lcline.
    
    lccli = entry(1,lcline,chr(9)).

   find first mobsub no-lock where
         mobsub.cli = lccli no-error.
   if not available mobsub then next.
   

   i = i + 1.
   pause 0.
   disp i with 1 down.
   

   lcstat = fCheckStatus(MobSub.MsSeq).
   if not lcstat begins "y_" then next.
   
   RUN barrengine.p (Mobsub.MsSeq,
                    "UN" + lcstat,     /* package for unbarring */
                      "5",                /* source  */
                      katun,             /* creator */
                      fMakeTS() + 0.0012,  /* activate */
                      "",                 /* sms-text */
                      OUTPUT lcResult).

   put stream slog unformatted
      mobsub.cli chr(9)
      mobsub.msseq skip.
end.

