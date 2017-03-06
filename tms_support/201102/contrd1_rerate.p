DEFINE VARIABLE i AS INTEGER NO-UNDO. 

input from contrd1_rerate.input.

def stream sout.
output stream sout to contrd1_rerate.log.
{Func/date.i}

{Syst/commpaa.i}
gcBrand = "1".
katun = "anttis".

DEFINE VARIABLE lcline AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liMsSeq AS INTEGER NO-UNDO. 

repeat:

   import unformatted lcline.

   liMsSeq = int(lcline).
   find first mobsub where
      mobsub.msseq = liMsSeq NO-LOCK no-error.

   IF not avail mobsub then next.

   put stream sout unformatted
      mobsub.msseq skip.

   RUN Rate/cli_rate.p (mobsub.cli,
                   2/1/11,
                   2/28/11,
                   true).   
end.

disp i.
