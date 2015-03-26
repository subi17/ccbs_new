def stream sread.
input stream sread from /apps/snet/201001/Services_20100110_yot446.txt.

def var lcline as char no-undo.
def var lccli as char no-undo.
def var lcreq as char no-undo.
def var i as int no-undo.
def var j as int no-undo.
def var liorig as int no-undo.

def buffer breq for msrequest.
def buffer creq for msrequest.

def stream slog.
output stream slog to /apps/snet/201001/aam_yot446.log append.

repeat:

   import stream sread unformatted lcline.

   lccli = entry(2,lcline," ") no-error.

   if lccli = "" then next.

   find first mobsub where mobsub.cli = lccli no-lock no-error.
   if not available mobsub then next.

   /*
   disp mobsub.cli mobsub.msseq.
   */
   
   lcreq = "".
   j = 0.
   
   for each msrequest no-lock where
            msrequest.msseq = mobsub.msseq and
            msrequest.reqtype = 1 and
            msrequest.reqstat = 3 and
            msrequest.actstamp > 20091228:
        
      find first breq where breq.msrequest = msrequest.origrequest 
         no-lock no-error.
      if available breq and breq.reqstat = 3 then do:
         j = j + 1.
         for first creq exclusive-lock where
                   creq.msseq = mobsub.msseq and 
                   creq.reqiparam2 = breq.msrequest and
                   creq.reqstat = 3:
                   
            put stream slog unformatted
               mobsub.cli  chr(9)
               mobsub.msseq chr(9)
               creq.msrequest chr(9)
               creq.reqstat chr(9)
               0 skip.

            creq.reqstat = 0.   
         end.

         find current breq exclusive-lock.
         put stream slog unformatted
            mobsub.cli  chr(9)
            mobsub.msseq chr(9)
            breq.msrequest chr(9)
            breq.reqstat chr(9)
            7 skip.

         breq.reqstat = 7.
      end.
    
      put stream slog unformatted
         mobsub.cli  chr(9)
         mobsub.msseq chr(9)
         msrequest.msrequest chr(9)
         msrequest.reqstat chr(9)
         0 skip.

      find first breq where recid(breq) = recid(msrequest) exclusive-lock.
      breq.reqstat = 0.
   end.

   i = i + 1.
   
   pause 0.
   disp i with 1 down.
   
end.

input stream sread close.
output stream slog close.



