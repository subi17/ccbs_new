input from yob_354.txt.
def stream sout.
output stream sout to yob_354_2.txt.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInvnum AS CHARACTER NO-UNDO. 
repeat:
   import unformatted lcLine.
   lcInvnum = entry(7,lcLine,"|").
   find invoice where
        invoice.brand = "1" and
        invoice.extinvid = lcInvnum NO-LOCK no-error.

   IF AVAIL invoice then do:
      put stream sout unformatted
         invoice.extinvid "|"
         invoice.invamt "|"
         invoice.AmtExclVAT skip.
   end.
end.
