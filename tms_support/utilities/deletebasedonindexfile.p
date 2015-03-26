DEFINE VARIABLE lMustFindInvoice AS LOGICAL NO-UNDO.
DEFINE VARIABLE lMustFindInvRow AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lMustFindMnpProcess AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lMustFindMnpMessage AS LOGICAL NO-UNDO. 

DEFINE VARIABLE lDeniedInvoice AS LOGICAL NO-UNDO.
DEFINE VARIABLE lDeniedInvRow AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lDeniedMnpProcess AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lDeniedMnpMessage AS LOGICAL NO-UNDO. 


DEFINE VARIABLE cMustIncludeInFileName AS CHARACTER NO-UNDO. 
define variable cdeniedinfilename as character no-undo. 

DEFINE VARIABLE cInputDir AS CHARACTER NO-UNDO. 

DEFINE VARIABLE iCountMobSub AS INTEGER NO-UNDO. 
DEFINE VARIABLE iWantedMobSubCount AS INTEGER NO-UNDO. 

DEFINE VARIABLE iBeginMsSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE iEndMsSeq AS INTEGER NO-UNDO. 


DEFINE STREAM sIds.

iBeginMsSeq = 975700.

iEndMsSeq   = 2000001.
iCountMobSub = 0.
iWantedMobSubCount = 1000.
cInputDir = "/home/harrim/exportdata/deniedmnprocess/".

lMustFindInvoice = TRUE.
lMustFindInvRow = TRUE.
lMustFindMnpProcess = FALSE.
lMustFindMnpMessage = FALSE.

lDeniedInvoice    = FALSE.
lDeniedInvRow     = FALSE.
lDeniedMnpProcess = TRUE.
lDeniedMnpMessage = TRUE. 

DEFINE VARIABLE cMobSubFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cIdentifierFileName AS CHARACTER NO-UNDO. 


cMobSubFileName = 
   cInputDir + "mobsubs" + STRING(iBeginMsSeq) + "_" 
   + STRING(iEndMsSeq) + "_" + STRING(iWantedMobSubCount).



IF lMustFindInvoice THEN
   cMustIncludeInFileName = "_Invoice".
IF lMustFindInvRow THEN
   cMustIncludeInFileName = cMustIncludeInFilename + "_InvRow".
IF lMustFindMnpProcess THEN
   cMustIncludeInFileName = cMustIncludeInFilename + "_MnpProcess".
IF lMustFindMnpMessage THEN
   cMustIncludeInFileName = cMustIncludeInFilename + "_MnpMessage".

IF cMustIncludeInFileName <> "" THEN 
   cMustIncludeInFileName = cMustIncludeInFilename + "_records_required".

IF lDeniedInvoice THEN
   cDeniedInFileName = "_Invoice".
IF lDeniedInvRow THEN
   cDeniedInFileName = cDeniedInFilename + "_InvRow".
IF lDeniedMnpProcess THEN
   cDeniedInFileName = cDeniedInFilename + "_MnpProcess".
IF lDeniedMnpMessage THEN
   cDeniedInFileName = cDeniedInFilename + "_MnpMessage".

IF cDeniedInFileName <> "" THEN 
   cDeniedInFileName = cDeniedInFilename + "_records_denied".


cMobSubFileName = cMobSubFileName + cMustIncludeInFileName + cDeniedInFileName.
cIdentifierFilename = REPLACE(cMobsubFileName, "mobsubs", "indentifiers").


DEFINE VARIABLE cLine AS CHARACTER NO-UNDO. 



FUNCTION fReadid RETURN LOGICAL:
   IMPORT STREAM sIds UNFORMATTED cLine.
   cLine = TRIM(cLine).
   IF cLine eq "" THEN RETURN FALSE.
   RETURN TRUE.
END.

DEFINE TEMP-TABLE ttKeyField
   FIELD cFieldName AS CHARACTER  
   FIELD cFieldValue AS CHARACTER 
   INDEX idxField IS PRIMARY UNIQUE cFieldName cFieldValue.


FUNCTION fKeyExistsAndCreate RETURN LOGICAL
  (INPUT pcKey AS CHARACTER, INPUT pcValue AS CHARACTER):
  IF NOT CAN-FIND(ttKeyField WHERE cFieldName = pcKey AND
         cFieldValue = pcValue) THEN
  DO:
     CREATE ttKeyField.
     ASSIGN ttKeyField.cFieldName = pcKey
            ttKeyField.cFieldValue = pcValue.
     RETURN FALSE.
  END.
  RETURN TRUE.
END.



FUNCTION fKeyExists RETURN LOGICAL
  (INPUT pcKey AS CHARACTER, INPUT pcValue AS CHARACTER):
  IF NOT CAN-FIND(ttKeyField WHERE cFieldName = pcKey AND
         cFieldValue = pcValue) THEN
  DO:
     RETURN FALSE.
  END.
  RETURN TRUE.
END.



FUNCTION fAddKeyFields RETURN LOGICAL
    (INPUT pcKey AS CHARACTER):
  DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO. 

  lContinue = TRUE.
  DO WHILE lContinue:
     lContinue = fReadId().
     IF lContinue THEN
     DO:
        fKeyExistsAndCreate(pcKey, cLine).
     END.
  END.
END.

FUNCTION fRemoveMobSub RETURN LOGICAL (INPUT pcMsSeq AS CHARACTER):
   DEFINE VARIABLE iMsSeq AS INTEGER NO-UNDO. 
   iMsSeq = INTEGER(pcMsSeq) NO-ERROR.
   IF NOT ERROR-STATUS:ERROR THEN
   DO:
       FIND MobSub WHERE MobSub.MsSeq = iMsSeq EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL MobSub THEN
          DELETE MobSub.
   END.
END.

FUNCTION fIsOtherMobsubForCustomer RETURN LOGICAL:
   FOR EACH MobSub WHERE MobSub.Brand = "1" AND MobSub.CustNum = Customer.Custnum NO-LOCK:
      IF NOT fKeyExists("MobSub.MsSeq", STRING(MobSub.MsSeq)) THEN
         RETURN TRUE. 
   END.
   RETURN FALSE.
END.


FUNCTION fRemoveCustomer RETURN LOGICAL (INPUT pcCustNum AS CHARACTER):
   DEFINE VARIABLE iCustNum AS INTEGER NO-UNDO. 
   iCustNum = INTEGER(pcCustNum) NO-ERROR.
   IF NOT ERROR-STATUS:ERROR THEN
   DO:
       FIND Customer WHERE Customer.CustNum = iCustNum EXCLUSIVE-LOCK NO-ERROR.

       IF AVAIL Customer AND NOT fIsOtherMobSubForCustomer() THEN
          DELETE Customer.
   END.
END.


FUNCTION fRemoveInvoice RETURN LOGICAL (INPUT pcInvNum AS CHARACTER):
   DEFINE VARIABLE iInvNum AS INTEGER NO-UNDO. 
   iInvNum = INTEGER(pcInvNum) NO-ERROR.
   IF NOT ERROR-STATUS:ERROR THEN
   DO:
       FIND Invoice WHERE Invoice.InvNum = iInvNum EXCLUSIVE-LOCK NO-ERROR.

       IF AVAIL Invoice THEN
       DO:
          FOR EACH InvRow WHERE InvRow.InvNum = iInvNum EXCLUSIVE-LOCK:
              DELETE InvRow.
          END.
          DELETE Invoice.
       END.
   END.
END.



FUNCTION fRemoveMSISDN RETURN LOGICAL(INPUT pcCLI AS CHARACTER):
    FIND MSISDN WHERE MSISDN.CLI = pcCLI EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL MSISDN THEN
       DELETE MSISDN.
END.



FUNCTION fRemoveOrder RETURN LOGICAL (INPUT pcOrderId AS CHARACTER):
   DEFINE VARIABLE iOrderId AS INTEGER NO-UNDO. 
   iOrderId = INTEGER(pcOrderId) NO-ERROR.
   IF NOT ERROR-STATUS:ERROR THEN
   DO:
      FIND Order WHERE Order.Brand = "1" AND Order.OrderId = iOrderId EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL Order THEN
      DO:
         DELETE Order.

         FOR EACH OrderCustomer WHERE 
             OrderCustomer.Brand = "1" AND 
             OrderCustomer.OrderId = iOrderId EXCLUSIVE-LOCK:
           DELETE OrderCustomer.
         END.

         FOR EACH OrderTopup WHERE 
                OrderTopup.Brand = "1" AND 
                OrderTopup.OrderId = iOrderId EXCLUSIVE-LOCK:
            DELETE OrderTopup.
         END.

         FOR EACH OrderAccessory WHERE 
                  OrderAccessory.Brand = "1" AND 
                  OrderAccessory.OrderId = iOrderId EXCLUSIVE-LOCK:
            DELETE OrderAccessory.
         END.

         FOR EACH OrderPayment WHERE 
                  OrderPayment.Brand = "1" AND 
                  OrderPayment.OrderId = iOrderId EXCLUSIVE-LOCK:
            DELETE OrderPayment.
         END.
      END.
   END.
END.

FUNCTION fRemovePrepaidRequest RETURN LOGICAL (INPUT pcPPRequest AS CHARACTER):
   DEFINE VARIABLE iPPRequest AS INTEGER NO-UNDO. 
   iPPRequest = INTEGER(pcPPRequest) NO-ERROR.

   IF NOT ERROR-STATUS:ERROR THEN
   DO:
      FIND PrepaidRequest WHERE PrepaidRequest.Brand = "1" AND 
           PrepaidRequest.PPRequest = iPPRequest EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL PrepaidRequest THEN
      DO:
         DELETE PrepaidRequest.
      END.
   END.
END.


FUNCTION fRemoveMnpProcesses RETURN LOGICAL (INPUT pcMnpSeq AS CHARACTER):
   DEFINE VARIABLE iMnpSeq AS INTEGER NO-UNDO. 
   iMnpSeq = INTEGER(pcMnpSeq) NO-ERROR.
   IF NOT ERROR-STATUS:ERROR THEN
   DO:
       FIND MnpProcess WHERE MnpProcess.MnpSeq = iMnpSeq EXCLUSIVE-LOCK NO-ERROR.

       IF AVAIL MnpProcess THEN
       DO:
          FOR EACH MnpMessage WHERE MnpMessage.MnpSeq = iMnpSeq EXCLUSIVE-LOCK:
              DELETE MnpMessage.
          END.
          DELETE MnpProcess.
       END.
   END.
END.



FUNCTION fProcessKeys RETURN LOGICAL:
   FOR EACH ttKeyField: 
      CASE ttKeyField.cFieldName:
         WHEN "MobSub.MsSeq" THEN fRemoveMobSub(ttKeyField.cFieldValue).
         WHEN "Customer.CustNum" THEN fRemoveCustomer(ttKeyField.cFieldValue).
         WHEN "Invoice.InvNum" THEN fRemoveInvoice(ttKeyField.cFieldValue).
         WHEN "MSISDN.CLI"  THEN fRemoveMSISDN(ttKeyField.cFieldValue).
         WHEN "Order.OrderId" THEN fRemoveOrder(ttKeyField.cFieldValue).
         WHEN "PrepaidRequest.PPRequest" THEN fRemovePrepaidRequest(ttKeyField.cFieldValue).
         WHEN "MnpProcess.MnpSeq" THEN fRemoveMnpProcesses(ttKeyField.cFieldValue).
      END.
   END.
END.



INPUT STREAM sIds FROM VALUE(cIdentifierFileName). 
REPEAT:
   IMPORT STREAM sIds UNFORMATTED cLine.
   cLine = TRIM(cLine).
   IF cLine eq "" THEN NEXT.
   fAddKeyFields(cLine).
END.
INPUT STREAM sIds CLOSE.

fProcessKeys().
