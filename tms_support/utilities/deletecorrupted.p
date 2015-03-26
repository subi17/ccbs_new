DEFINE STREAM sCorrData.

INPUT STREAM sCorrData FROM VALUE("/home/harrim/corrodata_20080908_v4.txt").

DEFINE VARIABLE cLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iMsSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE iOrderId AS INTEGER NO-UNDO. 
DEFINE VARIABLE iEquals AS INTEGER NO-UNDO. 
DEFINE VARIABLE iComma  AS INTEGER NO-UNDO. 

DEFINE STREAM sLog.
OUTPUT STREAM sLog TO VALUE("/home/harrim/fixcorrupted_20080908_v5.log").

DEFINE VARIABLE iLineNo AS INTEGER NO-UNDO. 
iLineNo = 1.

FUNCTION fLog RETURN LOGICAL (INPUT pcText AS CHARACTER):

  PUT STREAM sLog UNFORMATTED pcText SKIP.
END.


FUNCTION fDeleteCustomers RETURN LOGICAL (INPUT piCustNum AS INTEGER):
  FOR EACH Customer WHERE Customer.Brand = "1" AND
                          Customer.CustNum = piCustNum EXCLUSIVE-LOCK:
     DELETE Customer. 
       if error-status:error then 
          IF AVAIL Customer THEN
          DO:
             fLog("unable to delete available Customer with CustNum " + 
                  STRING(piCustNum)).
          END.
          ELSE
             fLog("unable to delete not available Customer" + 
                 STRING(piCustNum)).
  END.
END.




FUNCTION fDeleteInvRows RETURN LOGICAL (INPUT piInvNum AS INTEGER):
  FOR EACH InvRow WHERE InvRow.InvNum = piInvNum EXCLUSIVE-LOCK:
     fLog("Delete InvRow with InvNum = " + STRING(piInvNum)).
     DELETE InvRow.
       if error-status:error then 
          IF AVAIL InvRow THEN
          DO:
             fLog("unable to delete InvRow with InvNum " + 
                  STRING(InvRow.InvNum)).
          END.
          ELSE
             fLog("unable to delete undefined InvRow").
  END.
END.

FUNCTION fDeleteInvoices RETURN LOGICAL (INPUT piInvNum AS INTEGER):

  FOR EACH Invoice WHERE Invoice.Brand = "1" AND
                         Invoice.InvNum = piInvNum EXCLUSIVE-LOCK:
      fDeleteInvRows(Invoice.InvNum).
      fLog("Delete Invoice with InvNum = " + STRING(piInvNum)).
      DELETE Invoice.
       if error-status:error then 
          IF AVAIL Invoice THEN
          DO:
             fLog("unable to delete Invoice with InvNum " + 
                  STRING(Invoice.InvNum)).
          END.
          ELSE
             fLog("unable to delete undefined Invoice").
  END.
END.


FUNCTION fDeleteDCCLI RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   FOR EACH DCCLI WHERE DCCLI.MsSeq = piMsSeq EXCLUSIVE-LOCK:
      fLog("Delete DCCLI with MsSeq = " + STRING(piMsSeq)).
      DELETE DCCLI. 
      if error-status:error then 
      IF AVAIL DCCLI THEN
      DO:
         fLog("unable to delete dccli with msseq " + STRING(DCCLI.MsSeq)).
      END.
      ELSE
         fLog("unable to delete undefined dccli ").
   END.
END.


FUNCTION fDeleteDCCounter RETURN LOGICAL (INPUT piMsSeq AS INTEGER):

   FOR EACH DCCounter WHERE DCCounter.MsSeq = piMsSeq EXCLUSIVE-LOCK:
      fLog("Delete DCCounter with MsSeq = " + STRING(piMsSeq)).
      DELETE DCCounter. 
      if error-status:error then 
         fLog("unable to delete dccounter with msseq " +
              STRING(DCCounter.MsSeq)).
   END.

END.


FUNCTION fDeleteMsRequests RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
  FOR EACH MsRequest WHERE MsRequest.MsSeq = piMsSeq EXCLUSIVE-LOCK:
      fLog("Delete MsRequest with MsSeq = " + STRING(piMsSeq)).
      DELETE MsRequest.
      if error-status:error then 
         fLog("unable to delete MsRequest with msseq " +
              STRING(MsRequest.MsSeq)).
  END.
END.


FUNCTION fDeleteMnpProcesses RETURN LOGICAL:
    FOR EACH MnpProcess WHERE MnpProcess.OrderId = iOrderId EXCLUSIVE-LOCK:
       FOR EACH MnpMessage WHERE MnpMessage.MnpSeq = MnpProcess.MnpSeq 
          EXCLUSIVE-LOCK:

         fLog("Delete MnpMessage with MnpSeq = " + STRING(MnpProcess.MnpSeq)).
         DELETE MnpMessage.
          IF ERROR-STATUS:ERROR THEN 
             fLog("Unable to delete MnpMessage with MnpSeq " +
                STRING(MnpMessage.MnpSeq)).
       END.

       fLog("Delete MnpProcess with MnpSeq = " + STRING(MnpProcess.MnpSeq)).
       DELETE MnpProcess.
       IF ERROR-STATUS:ERROR THEN 
          fLog("Unable to delete MnpProcess with MnpSeq " + STRING(
             MnpProcess.MnpSeq)).
    END.
END.



FUNCTION fDeletePrepaidRequests RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   FOR EACH PrepaidRequest WHERE PrepaidRequest.Brand = "1" 
      AND PrepaidRequest.MsSeq = piMsSeq EXCLUSIVE-LOCK:
       fLog("Delete PrepaidRequest with MsSeq = " + STRING(piMsSeq)).
       DELETE PrepaidRequest.
       IF ERROR-STATUS:ERROR THEN 
          fLog("Unable to delete PrepaidRequest with MsSeq " + STRING(
             PrepaidRequest.MsSeq)).
   END.
END.


DEFINE VARIABLE lDeleteOnlyOrder AS LOGICAL NO-UNDO. 

FUNCTION fDeleteOrderData RETURN LOGICAL 
      (INPUT piOrderId AS INTEGER, INPUT piMsSeq AS INTEGER):


      IF piMsSeq ne 0 THEN
         FIND MobSub WHERE MobSub.MsSeq = piMsSeq EXCLUSIVE-LOCK NO-ERROR.

      FOR EACH OrderCustomer WHERE OrderCustomer.Brand = "1" AND 
          OrderCustomer.OrderId = piOrderId EXCLUSIVE-LOCK:
          DELETE OrderCustomer.
          IF ERROR-STATUS:ERROR THEN 
             fLog("Unable to delete OrderCustomer with Orderid " +
                STRING(piOrderId)).
      END.

      FOR EACH OrderTopup WHERE 
          OrderTopup.OrderId = piOrderId EXCLUSIVE-LOCK:
          DELETE OrderTopup.
          IF ERROR-STATUS:ERROR THEN 
             fLog("Unable to delete OrderTopup with Orderid " 
                  + STRING(piOrderId)).
      END.

      FOR EACH OrderAccessory WHERE 
          OrderAccessory.OrderId = piOrderId EXCLUSIVE-LOCK:
          DELETE OrderAccessory.
          IF ERROR-STATUS:ERROR THEN
             fLog("Unable to delete OrderAccessories with Orderid " +
                  STRING(piOrderId)).
      END.

      FOR EACH OrderPayment WHERE OrderPayment.Brand = "1" AND 
                                  OrderPayment.OrderId = piOrderId 
                                  EXCLUSIVE-LOCK:
         DELETE OrderPayment.
          IF ERROR-STATUS:ERROR THEN
             fLog("Unable to delete OrderPayment with Orderid " +
                  STRING(piOrderId)).
      END.

      FOR EACH Memo WHERE 
          Memo.Brand = "1" AND Memo.HostTable = "Order" AND 
                          Memo.KeyValue = STRING(piOrderId) 
                          EXCLUSIVE-LOCK:
          DELETE Memo.
          IF ERROR-STATUS:ERROR THEN
             fLog("Unable to delete Memo with Orderid " +
                  STRING(piOrderId)).
      END.

      FOR EACH SIM WHERE SIM.Brand = "1" AND 
          SIM.ICC = Order.ICC EXCLUSIVE-LOCK:
          DELETE SIM.
          IF ERROR-STATUS:ERROR THEN
             fLog("Unable to delete SIM of Order with Orderid " +
                  STRING(piOrderId)).
      END.

      IF piMsSeq ne 0 THEN
      DO:
         IF AVAIL MobSub THEN
         DO:
         FOR EACH MSISDN WHERE MSISDN.Brand = "1" AND 
             MSISDN.CLI = MobSub.CLI EXCLUSIVE-LOCK:
             DELETE MSISDN.
             IF ERROR-STATUS:ERROR THEN
                fLog("Unable to delete MSISDN of Order with Orderid " +
                     STRING(piOrderId)).
         END.
         END.
         ELSE
           fLog("MobSub with MsSeq = " + STRING(piMsSeq) + 
                " was out of reach when deleting MSISDN ").
      END.
    
      fDeleteInvoices(Order.InvNum).
     
      DELETE Order.
      IF ERROR-STATUS:ERROR THEN 
          fLog("Unable to delete Order with Orderid " + STRING(iOrderId)).
END.


FUNCTION fDeleteMobSubRelatedRecords RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
  fDeletePrepaidRequests(piMsSeq).
  fDeleteDCCounter(piMsSeq).
  fDeleteDCCLI(piMsSeq).

  DEFINE BUFFER xMobSub FOR MobSub.

  FIND xMobSub WHERE xMobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
  IF AVAIL xMobSub THEN
  DO:
     fDeleteCustomers(xMobSub.CustNum).
     fDeleteCustomers(xMobSub.AgrCust).
     fDeleteCustomers(xMobSub.InvCust).
  END.
END.


REPEAT:
   IMPORT STREAM sCorrdata UNFORMATTED cLine.
   IF cLine BEGINS "MobSub" THEN
   DO:
      iEquals = INDEX(cLine, "=").
      iComma  = INDEX(cLine, ",").
      iMsSeq  = INTEGER(
         SUBSTRING(cLine, iEquals + 1, iComma - iEquals - 1)) NO-ERROR.
      iEquals  = INDEX(cLine, "= ", iComma + 1).
      iOrderId = INTEGER(SUBSTRING(cLine, iEquals + 1)) NO-ERROR.

      IF iMsSeq ne 0 AND iOrderId ne 0 THEN
      DO:
         lDeleteOnlyOrder = FALSE.
         DEFINE VARIABLE cIcc AS CHARACTER NO-UNDO. 
         FOR EACH MobSub WHERE MobSub.MsSeq = iMsSeq EXCLUSIVE-LOCK:
            FOR EACH Order WHERE Order.Brand = "1" AND 
                     Order.OrderId = iOrderId EXCLUSIVE-LOCK:
               fDeleteOrderData(Order.OrderId, MobSub.MsSeq).                
            END.
            fDeleteMobSubRelatedRecords(MobSub.MsSeq).

            fLog("Deleted MobSub with MsSeq = " + STRING(MobSub.MsSeq)).
            DELETE MobSub.

         END.

         FOR EACH Order WHERE  
            Order.OrderId = iOrderId EXCLUSIVE-LOCK:
            fDeleteOrderData(iOrderId, 0).
            fLog("Deleted Order with OrderId = " + STRING(iOrderId)).
         END.
      END.
      ELSE IF iOrderId ne 0 THEN
      DO:
         lDeleteOnlyOrder = TRUE.
         FOR EACH Order WHERE Order.Brand = "1" AND 
            Order.OrderId = iOrderId EXCLUSIVE-LOCK:
            fDeleteOrderData(iOrderId, 0).
            fLog("Deleted Order with OrderId = " + STRING(iOrderId)).
         END.
      END.
      ELSE
      DO:
         PUT STREAM sLog UNFORMATTED "Error in parsing row " iLineNo SKIP.
      END.
   END.
   iLineNo = iLineNo + 1.
END.

INPUT STREAM sCorrData CLOSE.
OUTPUT STREAM sLog CLOSE.
   

