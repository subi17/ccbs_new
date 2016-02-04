{Syst/commpaa.i}
katun  = "anttis".
gcBrand = "1".
{Syst/tmsconst.i}
{Func/msreqfunc.i}
{Func/orderfunc.i}
{Mnp/mnp.i}
{Syst/eventval.i}
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

      {lib/eventlog.i}
END.

DEFINE VARIABLE llSimulate AS LOGICAL NO-UNDO.
llSimulate = false.

DEFINE VARIABLE lcProcesses AS CHARACTER NO-UNDO. 
lcProcesses = "00500311110310175025210 00500311110310175425687 00500311110311081601120 00500311110314082907817 00500411110315115611541".

DEFINE VARIABLE lcProcess AS CHARACTER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcSMS AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLang AS INTEGER NO-UNDO.

do i = 1 to num-entries(lcProcesses, " ") trans:

   lcProcess = entry(i, lcProcesses, " ").

   find mnpprocess where
        MNPProcess.portrequest = lcProcess and
        (MNPProcess.statuscode = 5 or
         MNPProcess.statuscode = 2) EXCLUSIVE-LOCK.

   find order where
        order.brand = gcBrand and
        order.orderid = MNPProcess.orderid and
        order.statuscode = "12" EXCLUSIVE-LOCK.
   find OrderCustomer of order where
        OrderCustomer.rowtype = 1 NO-LOCK.

   /* ACAN cannot come after porting */ 
   FIND FIRST MsRequest WHERE
              MsRequest.MsSeq     = Order.MsSeq AND
              MsRequest.ReqType   = ({&REQTYPE_SUBSCRIPTION_CREATE}) AND
              MsRequest.ReqStatus = ({&REQUEST_STATUS_NEW})
   NO-LOCK NO-ERROR.
   IF AVAIL MsRequest THEN DO:
      if not llSimulate then fReqStatus(4,"Cancelled MNP Process").
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
      if not llSimulate then CallAlarm.DeliStat = 4. /* CANCELLED */
   END.
            
   if not llSimulate then do:
      fSetOrderStatus(Order.OrderId,"7").
      fMarkOrderStamp(Order.OrderID,"Close",0.0).
   end.
   
   /* YDR-16 */
   IF not llSimulate and Order.OrderChannel = "POS" THEN DO:
     
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
   
   /* YOT-451 */
   IF not llSimulate and
      LOOKUP(Order.OrderChannel,"cc,telesales,self") > 0 THEN DO:
      
      IF MNPProcess.StatusCode = {&MNP_ST_ACON} THEN lcSMS = "MNPCanAfterConf".
      ELSE lcSMS = "MNPCanBeforeConf".
      
      liLang = INT(OrderCustomer.Language) NO-ERROR.
      
      fMNPCallAlarm(
          lcSMS,
          fMakeTS(),
          MNPProcess.FormRequest,
          Order.CLI,
          Order.CustNum,
          liLang,
          "800622600").
   END.

   if not llSimulate then do:
      FOR EACH MNPCancelProposal EXCLUSIVE-LOCK WHERE
         MNPCancelProposal.MnpSeq = MNPProcess.MNPSeq AND
         MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_NEW}:
         MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_CANCELLED}.
      END.

      ASSIGN         
         MNPProcess.UpdateTS = fMakeTS()
         MNPProcess.MNPUpdateTS = MNPProcess.UpdateTS
         MNPProcess.StatusCode = {&MNP_ST_ACAN}
         Order.MNPStatus = MNPProcess.StatusCode + 1.

      FIND CURRENT Order NO-LOCK.

    /*  RUN Mc/cancelorder.p(Order.OrderID). */

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

end.
