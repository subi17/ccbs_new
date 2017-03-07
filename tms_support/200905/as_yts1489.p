def buffer bsolog for solog.
{Func/timestamp.i}
DEFINE VARIABLE ldeActstamp AS DECIMAL NO-UNDO. 
DEFINE VARIABLE x AS CHARACTER NO-UNDO. 

input from /apps/snet/200905/as_yts1489.input.

def stream slog.
output stream slog to /apps/snet/200905/as_yts1489.log.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
repeat:
   
   import unformatted lcLine.

   ldeActstamp = fmakets().
   FOR EACH mobsub where
      mobsub.cli = lcLine NO-LOCK: 
      FOR FIRST msrequest where
         msrequest.msseq = mobsub.msseq and
         msrequest.reqtype = 13 NO-LOCK:
         i = 0. 
         FOR LAST solog where
            solog.cli = msrequest.cli and
            solog.msrequest = msrequest.msrequest and
            solog.response = "ok" and
            index(solog.commline,"KI=") > 0 and 
            index(solog.commline,"CREATE") > 0 NO-LOCK: 
            i = 1.

            CREATE bSolog.
            ASSIGN
              bSolog.Solog = NEXT-VALUE(Solog).
            
            buffer-copy solog except 
            solog msalog completedts createdts TimeSlotTMS response to bsolog.
            
            ASSIGN
                bSolog.ActivationTS    = ldeActstamp 
                bSolog.CreatedTS    = ldeActstamp 
                bSolog.TimeSlotTMS  = ldeActstamp 
                bSolog.response = "" 
                bSoLog.Stat         = 0.
                x = replace(bsolog.commline,string(solog.solog),STRING(bsolog.solog)).
               bSolog.CommLine = x. 
             put stream slog unformatted bsolog.solog "|" bsolog.commline skip.
         END.
         if i = 0 then message "false" VIEW-AS ALERT-BOX.
         release bsolog.

      END.
   END.

end.
