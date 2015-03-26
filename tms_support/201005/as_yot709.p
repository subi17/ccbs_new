input from as_yot709.input.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.

repeat:
   import unformatted lcline.

   do trans:
      
      find mnpprocess where
         mnpprocess.formrequest = lcline and
         mnpprocess.statuscode = 5 EXCLUSIVE-LOCK.

      find order where
           order.brand = "1" and
           order.orderid = mnpprocess.orderid and
           order.mnpstatus = 6 EXCLUSIVE-LOCK.

      assign
         mnpprocess.statuscode = 6
         order.mnpstatus = 7.

   end.
end.


