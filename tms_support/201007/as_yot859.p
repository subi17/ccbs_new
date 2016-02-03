{Syst/commpaa.i}
katun = "YOT-858".
gcBrand = "1".
{Func/orderfunc.i}
{Func/msreqfunc.i}
{Syst/eventval.i}
{Func/log.i}

  &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {lib/eventlog.i}

DEFINE VARIABLE lcCodes AS CHARACTER NO-UNDO. 
lcCodes = "00500111100720184824405 00500111100720194426511 00500311100720114109167".

DEFINE VARIABLE lcCode AS CHARACTER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 

do i = 1 to num-entries(lcCodes,  " ") with frame a:
   
   find mnpprocess where
        mnpprocess.portrequest = entry(i, lcCodes, " ") EXCLUSIVE-LOCK.

   find order where
        order.brand = "1" and
        order.orderid = mnpprocess.orderid EXCLUSIVE-LOCK.

   find msrequest where
      msrequest.msseq = order.msseq and
      msrequest.reqtype = 13 NO-LOCK.
   find sim where
      sim.icc = order.icc NO-LOCK.
   disp order.statuscode order.mnpstatus mnpprocess.statuscode 
    order.icc sim.simstat order.orderid statusreason msrequest.reqstatus.
        
   /* ACAN cannot come after porting */ 
   FIND FIRST MsRequest WHERE
              MsRequest.MsSeq     = Order.MsSeq AND
              MsRequest.ReqType   = ({&REQTYPE_SUBSCRIPTION_CREATE}) AND
              MsRequest.ReqStatus = ({&REQUEST_STATUS_NEW})
   NO-LOCK NO-ERROR.
   IF AVAIL MsRequest THEN DO:
      fReqStatus(4,"Cancelled MNP Process").
   END.
   /* ongoing subscription requests should not exist */
   ELSE DO:
      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq = Order.MsSeq AND
                 MsRequest.ReqType = ({&REQTYPE_SUBSCRIPTION_CREATE}) AND
                 MsRequest.ReqStatus NE ({&REQUEST_STATUS_NEW})
      NO-LOCK NO-ERROR.
      
      IF AVAIL MsRequest THEN DO:
         MESSAGE "Cancellation failed, ongoing subscription request" VIEW-AS ALERT-BOX.
         NEXT.
      END.
   END.
   
/* Cancel pending SMS messages */
   FOR EACH CallAlarm WHERE
            CallAlarm.Brand = gcBrand AND
            CallAlarm.CLI = Order.CLI AND
            CallAlarm.DeliStat = 1 AND
            CallAlarm.CreditType = 12 EXCLUSIVE-LOCK:
       CallAlarm.DeliStat = 4. /* CANCELLED */
   END.
            
   
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
  
   FOR EACH MNPCancelProposal EXCLUSIVE-LOCK WHERE
      MNPCancelProposal.MnpSeq = MNPProcess.MNPSeq AND
      MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_NEW}:
      MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_CANCELLED}.
   END.


   ASSIGN         
      MNPProcess.UpdateTS = fMakeTS()
      MNPProcess.StatusCode = {&MNP_ST_ACAN}
      Order.MNPStatus = MNPProcess.StatusCode + 1.

   FIND CURRENT Order NO-LOCK.

   /* YDR-70 */
   IF Order.OrderChannel = "pos" THEN DO:
      
      FIND OrderAccessory WHERE
           OrderAccessory.Brand = gcBrand AND
           OrderAccessory.OrderId = Order.OrderId AND
           OrderAccessory.TerminalType = ({&TERMINAL_TYPE_PHONE})
      EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL OrderAccessory AND OrderAccessory.IMEI NE "" THEN DO:
         
         IF llDoEvent THEN DO:
            DEFINE VARIABLE lhOrderAccessory AS HANDLE NO-UNDO.
            lhOrderAccessory = BUFFER OrderAccessory:HANDLE.
            RUN StarEventInitialize(lhOrderAccessory).
            RUN StarEventSetOldBuffer(lhOrderAccessory).
         END.
         
         OrderAccessory.IMEIStatus = ({&IMEI_STATUS_TO_BE_RELEASED}).

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOrderAccessory).

         RELEASE OrderAccessory.
      END.
   END.

end.  
