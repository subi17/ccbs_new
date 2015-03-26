def buffer breq for msrequest.
def buffer creq for msrequest.

def var llupd as log no-undo.

def stream slog.
output stream slog to /apps/snet/200902/aam_yts1325.log append.

for each msrequest no-lock where
         brand = "1" and
         reqtype = 1 and
         reqstat = 3 and
         actstamp > 20090101 and
         reqcparam1 = "shaper":
        
   
   disp msrequest.cli msrequest.msseq 
        msrequest.actstamp.
        
   llupd = false.
   
   if msrequest.origrequest > 0 then do:       
      find first breq no-lock where
            breq.msrequest = msrequest.origrequest.

      disp breq.reqtype breq.reqstat.

      if breq.reqtype = 18 and breq.reqstat = 3 and
         not can-find(first mobsub where mobsub.msseq = msrequest.msseq) 
      then llupd = true.
   end.

   else if index(msrequest.memo,"modify ok") > 0 then llupd = true. 
   
   if llupd then do:
      put stream slog unformatted
         msrequest.msrequest chr(9)
         msrequest.reqtype   chr(9)
         msrequest.msseq     chr(9)
         msrequest.cli       skip.
         
      find creq where recid(creq) = recid(msrequest) exclusive-lock.
      creq.reqstat = 2.
      
      if msrequest.origrequest > 0 and available breq then do:
         put stream slog unformatted
            breq.msrequest chr(9)
            breq.reqtype   chr(9)
            breq.msseq     chr(9)
            breq.cli       skip.
            
         find creq where recid(creq) = recid(breq) exclusive-lock.
         creq.reqstat = 2.
      end.   
   end.

end.