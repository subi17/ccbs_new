DEFINE INPUT PARAMETER pcMsSeqFile AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcLogFile AS CHARACTER NO-UNDO. 

DEFINE VARIABLE piMsSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE cValue AS CHARACTER NO-UNDO. 

DEFINE STREAM sMsSeq.
DEFINE STREAM sLogFile.

INPUT STREAM sMsSeq FROM VALUE (pcMsSeqFile).

FUNCTION fLog RETURN LOGICAL (INPUT pcMsg AS CHARACTER):
   PUT STREAM sLogFile UNFORMATTED pcMsg SKIP.
END.

FUNCTION fDeleteCLIRelated RETURN LOGICAL (INPUT pcCLI AS CHARACTER):
   FOR EACH MSISDN WHERE MSISDN.CLI = pcCLI EXCLUSIVE-LOCK:
      fLog("Delete MSISDN with CLI " + pcCLI).
      DELETE MSISDN.
   END.

   FOR EACH MSISDNNumber WHERE MSISDNNumber.CLI = pcCLI EXCLUSIVE-LOCK:
      fLog("Delete MSISDNNumber with CLI " + pcCLI).
      DELETE MSISDNNumber.
   END.

   FOR EACH CallAlarm WHERE CallAlarm.CLI = pcCLI EXCLUSIVE-LOCK:
      fLog("Delete CallAlarm with CLI " + pcCLI).
      DELETE CallAlarm.
   END.

   FOR EACH PrepCDR EXCLUSIVE-LOCK WHERE 
         PrepCDR.CLI = pcCLI:
      fLog("Delete PrepCDR with CLI " + pcCLI).
      DELETE PrepCDR.
   END.

   FOR EACH MobCDR EXCLUSIVE-LOCK WHERE 
         MobCDR.CLI = pcCLI:
      fLog("Delete MobCDR with CLI " + pcCLI).
      DELETE MobCDR.
   END.

   RETURN TRUE.
END.


FUNCTION fDeleteICCRelated RETURN LOGICAL (INPUT pcICC AS CHARACTER):
   FOR EACH SIM WHERE SIM.ICC = pcICC EXCLUSIVE-LOCK:
      fLog("Delete SIM with ICC " + pcICC).
      DELETE SIM.
   END.

   FOR EACH IMSI WHERE IMSI.ICC = pcICC AND 
       IMSI.IMSI = Mobsub.IMSI EXCLUSIVE-LOCK:
      fLog("Delete IMSI with IMSI " + IMSI.IMSI).
      DELETE IMSI.
   END.

   RETURN TRUE.
END.



FUNCTION fDeleteInvoiceRelated RETURN LOGICAL (INPUT piInvNum AS INTEGER):
   FOR EACH InvASub WHERE InvASub.InvNum = piInvNum EXCLUSIVE-LOCK:
      fLog("Delete InvASub with InvNum " + STRING(piInvNum)).
      DELETE InvASub.
   END.

   FOR EACH InvRow WHERE InvRow.InvNum = piInvNum EXCLUSIVE-LOCK:
      fLog("Delete InvRow with InvNum " + STRING(piInvNum)).
      DELETE InvRow.
   END.

   FIND Invoice WHERE Invoice.InvNum = piInvNum EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL Invoice THEN 
   DO:
      fLog("Delete Invoice with InvNum " + STRING(piInvNum)).
      DELETE Invoice.
   END.
   
   RETURN TRUE.
END.


FUNCTION fDeleteOrderRelated RETURN LOGICAL:
   FOR EACH OrderAccessory WHERE 
      OrderAccessory.OrderId = Order.OrderId EXCLUSIVE-LOCK:
      fLog("Delete OrderAccessory with Order " + STRING(Order.OrderId)).
      DELETE OrderAccessory.
   END.

   FOR EACH OrderCustomer WHERE 
      OrderCustomer.OrderId = Order.OrderId EXCLUSIVE-LOCK:
      fLog("Delete OrderCustomer with Order " + STRING(Order.OrderId)).
      DELETE OrderCustomer.
   END.

   FOR EACH OrderDelivery WHERE 
      OrderDelivery.OrderId = Order.OrderId EXCLUSIVE-LOCK:
      fLog("Delete OrderDelivery with Order " + STRING(Order.OrderId)).
      DELETE OrderDelivery.
   END.

   FOR EACH OrderTopup WHERE OrderTopup.OrderId = Order.OrderId EXCLUSIVE-LOCK:
      fLog("Delete OrderTopup with Order " + STRING(Order.OrderId)).
      DELETE OrderTopup.
   END.

   FOR EACH OrderTimeStamp WHERE 
      OrderTimeStamp.OrderId = Order.OrderId EXCLUSIVE-LOCK:
      fLog("Delete OrderTimeStamp with Order " + STRING(Order.OrderId)).
      DELETE OrderTimeStamp.
   END.

   FOR EACH OrderPayment WHERE 
      OrderPayment.OrderId = Order.OrderId EXCLUSIVE-LOCK:
      fLog("Delete OrderPayment with Order " + STRING(Order.OrderId)).
      DELETE OrderPayment.
   END.

   FOR EACH MnpProcess WHERE 
      MnpProcess.OrderId = Order.OrderId EXCLUSIVE-LOCK:
      fLog("Delete MNPProcess with Order " + STRING(Order.OrderId) + 
         " and MNPSeq = " + STRING(MNPProcess.MNPSeq)).
      FOR EACH MnpMessage WHERE 
          MnpMessage.MnpSeq = MnpProcess.MnpSeq EXCLUSIVE-LOCK:
           fLog("Delete MNPMessage with Order " + STRING(Order.OrderId) + 
              " and MNPSeq = " + STRING(MNPMessage.MNPSeq) + 
              " and createdTs " + 
              STRING(MNPMessage.CreatedTs)).
          DELETE MnpMessage.
      END.
      DELETE MnpProcess.
   END.

   FOR EACH Memo WHERE Memo.Hosttable = "order" AND 
      Memo.KeyValue = STRING(Order.OrderId) 
       EXCLUSIVE-LOCK:
       fLog("Delete Memo with MemoSeq " + STRING(Memo.MemoSeq)). 
       DELETE Memo.
   END.

   fDeleteInvoiceRelated(Order.InvNum).

   fLog("Delete Order with OrderId " + STRING(Order.OrderId)). 
   DELETE Order.

   RETURN TRUE.
END.

FUNCTION fDeleteCustomerRelated RETURN LOGICAL (INPUT piCustNum AS INTEGER):
   FOR EACH Limit WHERE Limit.CustNum = piCustNum EXCLUSIVE-LOCK:
      fLog("Delete Limit with CustNum " + STRING(piCustNum) + 
         " and Limit.TMRuleSeq = " + STRING(Limit.TMRuleSeq)).

      DELETE Limit.
   END.

   FOR EACH FaTime WHERE FaTime.CustNum = piCustNum EXCLUSIVE-LOCK:
      fLog("Delete FaTime with CustNum " + STRING(piCustNum) + 
         " and FaTime.InvNum = " + STRING(FATime.InvNum)).

      DELETE FaTime.
   END.

   FOR EACH Invoice WHERE Invoice.CustNum = piCustNum EXCLUSIVE-LOCK:
      fDeleteInvoiceRelated(Invoice.InvNum).
   END.

   FIND Customer WHERE Customer.CustNum = piCustNum EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL Customer THEN 
   DO:
      fLog("Delete Customer with CustNum " + STRING(piCustNum)). 
      DELETE Customer.
   END.

   RETURN TRUE.
END.


FUNCTION fDeleteMsSeqRelated RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   FOR EACH PrepaidRequest WHERE PrepaidRequest.MsSeq = piMsSeq EXCLUSIVE-LOCK:
      fLog("Delete PrepaidRequest with MsSeq " + STRING(piMsSeq) + 
        " and PPRequest " + STRING(PrepaidRequest.PPRequest)). 
      DELETE PrepaidRequest.
   END.
   
   FOR EACH DCCLI WHERE DCCLI.MsSeq = piMsSeq EXCLUSIVE-LOCK:
      fLog("Delete DCCLI with MsSeq " + STRING(piMsSeq) + 
        " and DCEvent " + STRING(DCCLI.DCEvent)). 

       DELETE DCCLI.
   END.

   FOR EACH DCCounter WHERE DCCounter.MsSeq = piMsSeq EXCLUSIVE-LOCK:
      fLog("Delete DCCounter with MsSeq " + STRING(piMsSeq) + 
        " and DCEvent " + STRING(DCCounter.DCEvent) + 
        " and date " + STRING(DCCounter.DCDate)). 
      DELETE DCCounter.
   END.

   FOR EACH MsRequest WHERE MsRequest.MsSeq = piMsSeq EXCLUSIVE-LOCK:
      fLog("Delete MsRequest with MsSeq " + STRING(piMsSeq) + 
        " and MsRequest " + STRING(MsRequest.MsRequest)).
      DELETE MsRequest.
   END.

   FOR EACH MsOwner WHERE MsOwner.MsSeq = piMsSeq EXCLUSIVE-LOCK:
      fLog("Delete MsOwner with MsSeq " + STRING(piMsSeq) + 
        " and MsOwner.CustNum " + STRING(MsOwner.CustNum) + 
        ", MsOwner.AgrCust " + STRING(MsOwner.AgrCust) +
        ", MsOwner.InvCust " + STRING(MsOwner.InvCust)).
      DELETE MsOwner.
   END.

   FOR EACH MnpSub WHERE MnpSub.MsSeq = piMsSeq EXCLUSIVE-LOCK:
      fLog("Delete MnpSub with MsSeq " + STRING(piMsSeq) + 
        " and MnpSub.MnpSeq " + STRING(MnpSub.MnpSeq)). 
      DELETE MnpSub.
   END.

   FOR EACH SubSer WHERE SubSer.MsSeq = piMsSeq EXCLUSIVE-LOCK:
      fLog("Delete SubSer with MsSeq " + STRING(piMsSeq) + 
        " and SubSer.ServPac " + STRING(SubSer.ServPac) + 
        " and SubSer.ServPac " + STRING(SubSer.ServCom)).
       DELETE SubSer.
   END.

   FOR EACH SubSerPara WHERE SubSerPara.MsSeq = piMsSeq EXCLUSIVE-LOCK:
      fLog("Delete SubSerPara with MsSeq " + STRING(piMsSeq) + 
        " and SubSer.ServCom " + STRING(SubSerPara.ServCom) + 
        " and SubSer.ParaName " + STRING(SubSerPara.ParaName)).
       DELETE SubSerPara.
   END.

   RETURN TRUE.
END.



OUTPUT STREAM sLogFile TO VALUE(pcLogFile).

REPEAT:
   IMPORT STREAM sMsSeq UNFORMATTED cValue.
   piMsSeq = INTEGER(cValue) NO-ERROR.
   IF NOT ERROR-STATUS:ERROR AND piMsSeq > 0 THEN
   DO:
      fLog("----------   Logging MsSeq = " + STRING(piMsSeq) + " begin ------").
      FIND Mobsub WHERE Mobsub.MsSeq = piMsSeq EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL Mobsub THEN
      DO:
         fDeleteCustomerRelated(Mobsub.CustNum).
         fDeleteCustomerRelated(Mobsub.AgrCust).
         fDeleteCustomerRelated(Mobsub.InvCust).
         FOR EACH Order WHERE Order.MsSeq = Mobsub.MsSeq EXCLUSIVE-LOCK:
             fDeleteCLIRelated(Order.CLI).
             fDeleteICCRelated(Order.ICC).
             fDeleteOrderRelated().
         END.
         fDeleteICCRelated(Mobsub.ICC).
         fDeleteCLIRelated(Mobsub.CLI).
         fDeleteMsSeqRelated(MobSub.MsSeq).

         DELETE Mobsub.
      END.
      fLog("----------   Logging MsSeq = " + STRING(piMsSeq) + " end ------").
   END.
END.


INPUT STREAM sMsSeq CLOSE.
OUTPUT STREAM sLogFile CLOSE.
