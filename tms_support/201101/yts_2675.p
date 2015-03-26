{commpaa.i}
katun = "Qvantel".
gcBrand = "1".
{tmsconst.i}
{msreqfunc.i}
{mnp.i}
{orderfunc.i}

input from st_20110211_0800.dat.log.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
def stream sout.
output stream sout to st_20110211_0800.dat.fixes.txt.

DEFINE VARIABLE lcNCStatus AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcPortRequest AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcReason AS CHARACTER NO-UNDO. 
DEFINE VARIABLE llSimulate AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lcNCTime AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeNCTime AS DECIMAL NO-UNDO. 
llSimulate = true.
DEFINE VARIABLE liOrderQty AS INTEGER NO-UNDO. 
DEFINE VARIABLE liLang AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcSMS AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcPortTime AS CHARACTER NO-UNDO.

def buffer bMNPSub for mnpsub.
def buffer bmnpprocess for mnpprocess.

repeat trans:
   import unformatted lcLine.
   
   if index(lcLine, "ERROR: ") = 0 then next.
   
   lcPortRequest = entry(1,lcLine,";").
   lcNCStatus = entry(2,lcLine,";").
   lcNCTime = ENTRY(3,lcLine,";").
   lcreason = entry(4,lcLine,";").
   lcPortTime = entry(5,lcLine,";").
      
   i = i + 1.
   disp i.
   pause 0.
      
   ldeNCTime = ?.
   ldeNCTime = fHMS2TS(date(int(substring(lcNCTime,5,2)),
       int(substring(lcNCTime,7,2)),
       int(substring(lcNCTime,1,4))),
       substring(lcNCTime,10)) NO-ERROR.
   
   if (index(lcLine, "ERROR: status is ACON") > 0 or 
      index(lcLine, "ERROR: status is ASOL") > 0 
      ) and
      lcNCStatus eq "ACAN" then do:

      if not llSimulate then
      find mnpprocess where
           mnpprocess.portrequest = lcPortRequest EXCLUSIVE-LOCK no-error.
      else
      find mnpprocess where
           mnpprocess.portrequest = lcPortRequest NO-LOCK no-error.
       
     IF NOT AVAIL mnpprocess or mnpprocess.mnptype ne 2 then next.
      
      IF LOOKUP(STRING(MNPProcess.StatusCode),"2,5") = 0 THEN DO:
         put stream sout unformatted "ERROR: " MNPProcess.portrequest ": Wrong process status " MNPProcess.statuscode skip.
         NEXT.
      END.
      
      put stream sout unformatted lcLine skip.

      /* Cancel termination requests */         
      FOR EACH MNPSub WHERE
         MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK:

         FIND FIRST MsRequest WHERE
                    MsRequest.MsSeq = MNPSub.MsSeq AND
                    MsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                    MsRequest.ReqStatus = {&REQUEST_STATUS_NEW}
         NO-LOCK NO-ERROR.
         IF AVAIL MsRequest THEN DO:
            if not llSimulate then
               fReqStatus({&REQUEST_STATUS_CANCELLED},"Cancelled MNP Process").
            put stream sout unformatted 
               "INFO: " MNPProcess.portrequest ": Cancelled MNP Process" skip.
         END.
      
         /* Cancel possible SMS messages */
         if not llSimulate then
         FOR EACH CallAlarm WHERE
                  CallAlarm.Brand = gcBrand AND
                  CallAlarm.CLI = MNPSub.CLI AND
                  CallAlarm.DeliStat = 1 AND
                  CallAlarm.CreditType = 12 EXCLUSIVE-LOCK:
           CallAlarm.DeliStat = 4. /* CANCELLED */
         END.
         
         FIND MobSub WHERE
            MobSub.MSSeq = MNPSub.MsSeq NO-LOCK NO-ERROR.

         IF AVAIL MobSub THEN 
            FIND Customer WHERE
                 Customer.Custnum = MobSub.Custnum 
                 NO-LOCK NO-ERROR.

         if not llSimulate then
         fMNPCallAlarm("MNPCancel",
                   fmakets(),
                   MNPProcess.FormRequest,
                   MNPSub.CLI,
                   (IF AVAIL MobSub THEN MobSub.Custnum ELSE 0),
                   (IF AVAIL Customer THEN Customer.Language ELSE 1),
                   "800622600").
        
         /* Release possible number return process that is on hold */
         if not llSimulate then
         FOR EACH bMNPSub WHERE
                  bMNPSub.CLI = MNPSub.CLI,
            FIRST bMNPProcess WHERE
               bMNPProcess.MNPSeq = bMNPSub.MNPSeq AND
               bMNPProcess.MNPType = {&MNP_TYPE_TERMINATION} AND
               bMNPProcess.StatusCode = {&MNP_ST_BDET} EXCLUSIVE-LOCK:

            ASSIGN
               bMNPProcess.UpdateTS = fMakeTS()
               bMNPProcess.StatusCode = {&MNP_ST_BNOT}.
            RELEASE bMNPProcess.
         END.

      END.
      
      if not llSimulate then
      FOR EACH MNPCancelProposal EXCLUSIVE-LOCK WHERE
         MNPCancelProposal.MNPSeq = MNPProcess.MNPSeq AND
         MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_NEW}:
         MNPCancelProposal.StatusCode = {&MNP_CANCEL_PROPOSAL_CANCELLED}.
      END.

      if not llSimulate then
      ASSIGN
         MNPProcess.StatusCode = {&MNP_ST_ACAN}
         MNPProcess.StatusReason = lcReason
         MNPProcess.UpdateTS = fMakeTS()
         MNPProcess.MNPUpdateTS = ldeNCTime.
      
      RELEASE MNPProcess.

   END.
   
   else if index(lcLine,"ERROR: status is ASOL") > 0 and
      lcNCStatus = "ACON" then do:
      

      if not llSimulate then
      find MNPProcess where
           MNPProcess.portrequest = lcPortRequest and
           MNPProcess.mnptype = 1 EXCLUSIVE-LOCK.
      else
      find MNPProcess where
           MNPProcess.portrequest = lcPortRequest and
           MNPProcess.mnptype = 1 NO-LOCK.

      if not llSimulate then
      find order where
           order.brand = "1" and
           order.orderid = MNPProcess.orderid EXCLUSIVE-LOCK.
      else
      find order where
           order.brand = "1" and
           order.orderid = MNPProcess.orderid NO-LOCK.

      put stream sout unformatted lcLine skip.
      
      IF LOOKUP(STRING(MNPProcess.StatusCode),"1,2") = 0 THEN DO:
         put stream sout unformatted "ERROR: " MNPProcess.portrequest ": Wrong process status " MNPProcess.statuscode skip.
         NEXT.
      END.
      IF LOOKUP(STRING(order.StatusCode),"12") = 0 THEN DO:
         put stream sout unformatted "ERROR: " MNPProcess.portrequest ": Wrong order status " order.statuscode skip.
         NEXT.
      END.
/*
      if entry(1,lcPortTime,"/") ne entry(1,string(MNPProcess.portingtime),".") then disp

            lcPortTime MNPProcess.portingtime. */

      /* double check activation */
      FIND FIRST MsRequest WHERE
                 MsRequest.MsSeq = Order.MsSeq AND
                 MsRequest.ReqType = ({&REQTYPE_SUBSCRIPTION_CREATE})
      NO-LOCK NO-ERROR.
      IF NOT AVAIL MsRequest THEN DO:
  
         if not llSimulate then
         RUN ordersender.p(MNPProcess.OrderId,
                         OUTPUT liOrderQty).        
         
      END.

      find ordercustomer of order where
           ordercustomer.rowtype = 1 NO-LOCK.

      if not llSimulate then do:

         liLang = INT(OrderCustomer.Language) NO-ERROR.
         
         fMNPCallAlarm("MNPConf",
                    MNPProcess.PortingTime,
                    MNPProcess.FormRequest,
                    Order.CLI,
                    Order.CustNum,
                    liLang,
                    "800622600").

         fMNPCallAlarm("MNPFinRem",
                    MNPProcess.portingtime,
                    MNPProcess.FormRequest,
                    Order.CLI,
                    Order.CustNum,
                    liLang,
                    "800622600").

          ASSIGN
            MNPProcess.StatusCode = {&MNP_ST_ACON}
            Order.MNPStatus = {&MNP_ST_ACON} + 1. 
      end.

      if not llSimulate then
      ASSIGN
         MNPProcess.StatusReason = lcReason
         MNPProcess.UpdateTS = fMakeTs()
         MNPProcess.MNPUpdateTS = ldeNCTime.

   end.
   else if index(lcLine,"ERROR: status is ASOL") > 0 and
     lcNCStatus = "AREC" then do:

     if not llSimulate then
     find MNPProcess where
          MNPProcess.portrequest = lcPortRequest and
          MNPProcess.mnptype = 1 EXCLUSIVE-LOCK.
     else
     find MNPProcess where
          MNPProcess.portrequest = lcPortRequest and
          MNPProcess.mnptype = 1 NO-LOCK.

     put stream sout unformatted lcLine skip.

     if not llSimulate then
     find order where
          order.brand = "1" and
          order.orderid = MNPProcess.orderid EXCLUSIVE-LOCK.
     else
     find order where
          order.brand = "1" and
          order.orderid = MNPProcess.orderid NO-LOCK.

      FIND FIRST OrderCustomer WHERE
               OrderCustomer.Brand   = Order.Brand   AND
               OrderCustomer.OrderId = Order.OrderId AND
               OrderCustomer.RowType = 1 NO-LOCK.
      
      liLang = INT(OrderCustomer.Language) NO-ERROR.
               
      IF LOOKUP(STRING(MNPProcess.StatusCode),"1,2") = 0 THEN DO:
         put stream sout unformatted "ERROR: " MNPProcess.portrequest " Wrong process status " MNPProcess.statuscode skip.
         next.
      END.
      
      lcSMS = "MNPReject".
      IF lcReason EQ "RECH_IDENT" THEN DO:
         IF Order.OrderChannel EQ "POS" THEN lcSMS = "MNPIdentPOS".
         ELSE lcSMS = "MNPIdentDirect".
      END.

      if not llSimulate then do:
         fMNPCallAlarm(lcSMS,
                      0.0,
                      MNPProcess.FormRequest,
                      Order.CLI,
                      Order.CustNum,
                      liLang,
                      "800622111").
      
         ASSIGN
            MNPProcess.StatusCode = {&MNP_ST_AREC}.
            Order.MNPStatus = {&MNP_ST_AREC} + 1.

         fSetOrderStatus(Order.OrderId,"73").

         assign
            MNPProcess.StatusReason = lcReason
            MNPProcess.UpdateTS = fMakeTS()
            MNPProcess.MNPUpdateTS = ldeNCTime.
      end.

   end.
   else next.

end.

