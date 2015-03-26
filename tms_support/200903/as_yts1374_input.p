def stream sout.
output stream sout to /apps/snet/200903/as_yts1374.input.
FOR EACH mnpprocess where
   formrequest >= "00500628432" and
   formrequest <= "00500629173" and
   statuscode = 0 NO-LOCK: 
   find mnpmessage where
      mnpmessage.mnpseq = mnpprocess.mnpseq and
      mnpmessage.sender =  1 NO-LOCK.
   
   disp mnpprocess.formrequest
      mnpprocess.createdts
      mnpprocess.updatets
      mnpprocess.statuscode
      mnpmessage.statuscode.
   
   put stream sout unformatted mnpprocess.formrequest skip.
END.

output stream sout close.
