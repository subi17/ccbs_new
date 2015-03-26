{timestamp.i}
output to /apps/snet/200905/as_yts1479.log. 
FOR EACH mnpprocess where
   mnpprocess.statuscode = 0 NO-LOCK:
   FOR EACH mnpmessage where
      mnpmessage.mnpseq = mnpprocess.mnpseq and
      sender = 1 and
      statuscode ne 200 and 
      mnpmessage.createdts > 20090504  and 
      mnpmessage.createdts < 20090505  NO-LOCK.
   disp mnpmessage.messagetype format "x(20)"
      mnpprocess.formrequest format "x(16)" label "formRequestCode"
      fTS2HMS(mnpmessage.sentts) format "x(22)" label "Send Time"
      mnpmessage.statuscode label "HTTP Status".
   end.
END.
