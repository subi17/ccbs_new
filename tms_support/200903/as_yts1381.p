input from /apps/snet/200903/as_yts1381.input.
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

repeat:
   import unformatted lcLine.

   find solog where
         solog.solog = int(lcLine) NO-LOCK NO-ERROR.
  
   IF AVAIL solog then do:
      find msrequest where
         msrequest.msrequest = solog.msrequest NO-LOCK.
      
      disp msrequest.reqtype msrequest.msseq msrequest.usercode msrequest.origrequest msrequest.actstamp.
   end.
END.

