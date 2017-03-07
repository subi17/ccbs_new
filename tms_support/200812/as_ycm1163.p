output to /apps/snet/200812/as_ycm1163.txt.
def frame a with 15 down.
{Func/timestamp.i}
FOR EACH mnpprocess where
   statuscode = 999 and  orderid > 500000 NO-LOCK:
   
   FOR EACH mnpmessage where
      mnpmessage.mnpseq = mnpprocess.mnpseq and 
      mnpmessage.sender = 1 and
      mnpmessage.messagetype = "portabilityRequest" NO-LOCK:
      
      if index(mnpmessage.xmlmessage, "jmz") > 0 then do:
      
      FIND FIRST order where
         order.brand = "1" and
         order.orderid = mnpprocess.orderid NO-LOCK NO-ERROR.

      disp 
         
         mnpprocess.orderid
         fts2hms(order.crstamp) format "x(20)"
         mnpprocess.formrequest format "x(20)"
         order.statuscode.
         /*mnpprocess.statuscode
         mnpmessage.xmlmessage view-as editor size 60 by 20. */
      end.
   END.
end.
   
