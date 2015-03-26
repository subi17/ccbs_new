def var llfound as log no-undo.

for each msrequest where
         brand = "1" and
         reqtype = 9 and
         actstamp > 20070619 and
         reqstat < 2,
   first msowner no-lock where
         msowner.msseq = msrequest.msseq:

   if msowner.tsend > 99999999 then next. 
   
   llfound = false.      
         
   for first dccli no-lock where
            dccli.msseq = msrequest.msseq and
            dccli.dcevent = msrequest.reqcparam3 and
            dccli.validto > today:
      llfound = true.
   end.
   
   if not llfound then do:
      msrequest.reqstat = 4.
      msrequest.memo = "Already terminated".
   end.
   
   disp msrequest.msseq
        msrequest.reqcparam3
        msrequest.cli actstamp reqstat createfees sendsms
        llfound
        termdate when available dccli
        validto  when available dccli
        dcevent  when available dccli
        msowner.tsend.
end.          
