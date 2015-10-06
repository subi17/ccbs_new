DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInvnum AS CHARACTER NO-UNDO. 

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
def stream sout.
output stream sout to null_dd_state_201410.txt.

FOR EACH invoice EXCLUSIVE-LOCK where
         invoice.brand = "1" and
         invoice.invdate = 10/1/2014 and
         invoice.invtype = 1.

   if invoice.ddstate eq 1 then do:

      put stream sout unformatted
         invoice.extinvid "|"
         invoice.invnum "|"
         invoice.ddstate "|"
         invoice.ddbankacc "|"
         invoice.ddfile "|"
         invoice.invamt 
         skip.

      assign
         invoice.ddstate = 0
         invoice.ddbankacc = "" 
         invoice.ddfile = "" .  
   end.

   i = i + 1.
   if i mod 5000 = 0 then do:
      disp i with frame a.
      pause 0.
   end.  

end.
disp i.
