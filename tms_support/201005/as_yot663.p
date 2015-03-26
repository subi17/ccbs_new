input from mnp_old_ongoing.txt.
{commpaa.i}
katun = "anttis".
gcBrand = "1".
{orderfunc.i}
{tmsconst.i}

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEF STREAM slog.
output stream slog to as_yot663.log.

def buffer bMNPProcess FOR MNPProcess.

import unformatted lcLine.
repeat:
   import unformatted lcLine.
   
   find mnpprocess where
        mnpprocess.portrequest = entry(2, lcLine, "|") NO-LOCK.
   
   if mnpprocess.portingtime > 20100415 or
      mnpprocess.createdts > 20100315 then do:
      put stream slog unformatted lcline "|incorrect time" skip.
      next.
   end.
   
   find order where
        order.brand = "1" and
        order.orderid = mnpprocess.orderid NO-LOCK.
   
   if mnpprocess.statuscode ne 1 and 
      mnpprocess.statuscode ne 2 then do:
      put stream slog unformatted lcline "|incorrect mnpprocess.state " order.statuscode "," order.mnpstatus "," mnpprocess.statuscode "," order.orderchannel skip.
      next.
   END.
   
   if order.mnpstatus ne 2 and 
      order.mnpstatus ne 3 then do:
      put stream slog unformatted lcline "|incorrect order mnp state " order.statuscode "," order.mnpstatus "," mnpprocess.statuscode "," order.orderchannel skip.
      do trans:
         find current mnpprocess EXCLUSIVE-LOCK.
         assign
            mnpprocess.statuscode = 7
            mnpprocess.updatets = fMakeTS()
            mnpprocess.statusreason = "CANC_TECNI".
      end.
      next.
   END.

   if order.statuscode ne "12" then do:
      put stream slog unformatted lcline "|incorrect order state " order.statuscode "," order.mnpstatus "," mnpprocess.statuscode "," order.orderchannel skip.
      do trans:
         find current mnpprocess EXCLUSIVE-LOCK.
         assign
            mnpprocess.statuscode = 7
            mnpprocess.updatets = fMakeTS()
            mnpprocess.statusreason = "CANC_TECNI".
      end.
      next.
   end.

   find first bMNPProcess WHERE
            bMNPProcess.OrderId = Order.OrderId AND
            bMNPProcess.MNPType = {&MNP_TYPE_IN} AND
            bMNPProcess.StatusCode NE 8 AND
      rowid(bMNPProcess) NE rowid(mnpprocess) NO-LOCK NO-ERROR.
   IF AVAIL bMNPProcess THEN DO:
      put stream slog unformatted lcLine "|Other MNP process is ongoing" skip.
      do trans:
         find current mnpprocess EXCLUSIVE-LOCK.
         assign
            mnpprocess.statuscode = 7
            mnpprocess.updatets = fMakeTS()
            mnpprocess.statusreason = "CANC_TECNI".
      end.
      next.
   END.
   
   put stream slog unformatted lcLine "|OK " order.statuscode "," order.mnpstatus "," mnpprocess.statuscode "," order.orderchannel skip.

   do trans:
      fSetOrderStatus(Order.OrderId,"7"). 
      find current order EXCLUSIVE-LOCK.
      find current mnpprocess EXCLUSIVE-LOCK.
      assign
         mnpprocess.statuscode = 7
         mnpprocess.updatets = fMakeTS()
         mnpprocess.statusreason = "CANC_TECNI"
         order.mnpstatus = 8.
  end.


end.
