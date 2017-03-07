{Syst/commpaa.i}
katun = "YOT-649".
gcBrand = "1".
{Func/orderfunc.i}
{Syst/tmsconst.i}

input from as_yot650.input.
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

def stream slog.
output stream slog to as_yot650.log.

repeat:
   import unformatted lcline.
   find mnpprocess where
      mnpprocess.portrequest = lcline and
      mnptype = 1 EXCLUSIVE-LOCK.
   find order where
      order.brand = "1" and
      order.orderid = mnpprocess.orderid EXCLUSIVE-LOCK.
   put stream slog unformatted order.orderid "|" 
      order.cli "|" 
      mnpprocess.portrequest "|"
      order.statuscode "|"
      order.mnpstatus "|"
      mnpprocess.statuscode "|"
      mnpprocess.portingtime "|"
      mnpprocess.statusreason "|"
      order.orderchannel.

   find mobsub where
      mobsub.cli = ORDER.cli NO-LOCK no-error.
   IF AVAIL mobsub then do:

      put stream slog unformatted "|ERROR:Activated" skip.
      next.
   end.
   ELSE 
      put stream slog unformatted "|OK "skip.

        
   /* Cancel pending SMS messages */
   FOR EACH CallAlarm WHERE
            CallAlarm.Brand = gcBrand AND
            CallAlarm.CLI = Order.CLI AND
            CallAlarm.DeliStat = 1 AND
            CallAlarm.CreditType = 12 EXCLUSIVE-LOCK:
       CallAlarm.DeliStat = 4. /* CANCELLED */
   END.
   
   IF Order.StatusCode = "12" THEN DO:

      fSetOrderStatus(Order.OrderId,"7").
      
      /* YDR-16 */
      IF Order.OrderChannel = "POS" THEN DO:
        
         FIND SIM WHERE
            SIM.ICC = Order.ICC AND
            SIM.SimStat = 4 EXCLUSIVE-LOCK NO-ERROR.
         
         IF AVAIL SIM THEN DO:
            
            ASSIGN SIM.SIMStat = 1.

            CREATE ActionLog.
            ASSIGN
               ActionLog.ActionTS     = fMakeTS()
               ActionLog.Brand        = gcBrand  
               ActionLog.TableName    = "Order"  
               ActionLog.KeyValue     = STRING(Order.Orderid)
               ActionLog.UserCode     = katun
               ActionLog.ActionID     = "SIMRELEASE"
               ActionLog.ActionPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
               ActionLog.ActionStatus = 2
               ActionLog.ActionChar   = SIM.ICC.
            
            RELEASE SIM.
         END.
      END.
      
      Order.MNPStatus = 8. 

   END.

   FOR EACH MNPCancelProposal EXCLUSIVE-LOCK WHERE
      MNPCancelProposal.MnpSeq = MNPProcess.MNPSeq AND
      MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_NEW}:
      MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_CANCELLED}.
   END.

   ASSIGN         
      MNPProcess.UpdateTS = fMakeTS()
      MNPProcess.statusreason = "CANC_TECNI" WHEN MNPProcess.StatusReason EQ ""
      MNPProcess.StatusCode = {&MNP_ST_ACAN}. 
end.
