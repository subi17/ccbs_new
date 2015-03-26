input from as_yot708.input.
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
repeat:
   import unformatted lcLine.
   FOR EACH mnpsub where
      mnpsub.cli = entry(1,lcLine,";") NO-LOCK:
      find mnpprocess where
           mnpprocess.mnpseq = mnpsub.mnpseq and
           mnpprocess.mnptype = 1 and
           lookup(string(mnpprocess.statuscode),"4,7,8,6") = 0 NO-LOCK no-error.
      IF AVAIL mnpprocess then do:
         find order where
              order.brand = "1" and
              order.orderid = mnpprocess.orderid NO-LOCK.
         disp mnpprocess.statuscode mnptype portrequest order.mnpstatus order.statuscode.
      end.
   END.
end.
