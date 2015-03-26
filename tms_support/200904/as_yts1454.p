DEFINE VARIABLE j AS INTEGER NO-UNDO. 

def stream sout.
output stream sout to /apps/snet/200904/as_yts1454.log.
def stream serror.
output stream serror to /apps/snet/200904/as_yts1454.error.

def buffer mnpp_buf for mnpprocess.
def buffer order_buf for order.

MAIN_LOOP:
FOR EACH mnpprocess where
   mnpprocess.statuscode = 7 and
   createdts > 20090201 NO-LOCK:
   
   find order where
      order.brand = "1" and
      order.orderid = mnpprocess.orderid NO-LOCK.
      
   
   j = 0.
   FOR EACH callalarm where
      callalarm.brand = "1" and
      callalarm.cli = order.cli and
      callalarm.delistat = 1 and
      callalarm.credittype = 12 and
      callalarm.custno = order.custnum and
      callalarm.actstamp > order.crstamp NO-LOCK:
      j = j + 1.
      if j > 1 then do:
         put stream serror unformatted mnpprocess.formrequest "|" order.cli "|" "ERROR - NOT UNIQUE MESSAGE" skip.
         NEXT MAIN_LOOP.
      end.
   end.
  
   if j = 1 then do:
      
      FOR EACH order_buf where
         order_buf.cli = order.cli NO-LOCK:
         FOR EACH mnpp_buf where
             mnpp_buf.orderid = order_buf.orderid and
             mnpp_buf.formrequest ne mnpprocess.formrequest NO-LOCK:
         put stream serror unformatted mnpprocess.formrequest "|" order.cli "|" "ERROR - NOT UNIQUE ORDER|" mnpp_buf.statuscode skip. 
            next MAIN_LOOP.
         end.
      end.
      
      FOR EACH callalarm where
         callalarm.brand = "1" and
         callalarm.cli = order.cli and
         callalarm.delistat = 1 and
         callalarm.credittype = 12 and
         callalarm.custno = order.custnum and
         callalarm.actstamp > order.crstamp EXCLUSIVE-LOCK:
         callalarm.delistat = 4.
         put stream sout unformatted order.orderid "|" mnpprocess.formrequest "|" callalarm.cli "|" mnpprocess.createdts "|" callalarm.actstamp "|CANCELLED|"  callalarm.delimsg skip.
      end.
   end.

END.

