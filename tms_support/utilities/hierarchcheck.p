DEFINE INPUT PARAMETER piFunction AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER pcFunctionParams AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcOutputDir AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcMsSeqFile AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER iDispInterval AS INTEGER NO-UNDO. 

/* Functionality options (piFunction value options)
   1 = lacking, meaning no records for table
   2 = record counts for specific mobsub
   3 = search mobsubs and orders having ICC in the ICC file
       given as FunctionParams
   4 = search mobsubs having ICC in the ICC file
       given as FunctionParams

   pcFunctionParams contains "allmobsub" means all mobsub record gone through,
                    msseq file does not have meaning
   pcFunctionParams contains ICC list file as last entry in , separated list
                    with functions 3 and 4.
*/


DEFINE VARIABLE piMsSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE cValue AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lFound AS LOGICAL NO-UNDO. 
DEFINE VARIABLE iCount AS INTEGER NO-UNDO. 

DEFINE STREAM sMsSeq.


DEFINE TEMP-TABLE ttCounts
  FIELD cTable AS CHARACTER
  FIELD iCount AS INTEGER
  INDEX idxTable cTable iCount.

DEFINE TEMP-TABLE ttList
  FIELD cValue AS CHARACTER
  INDEX idxValue cValue.

FUNCTION fInitCounts RETURN LOGICAL:
   EMPTY TEMP-TABLE ttList.
   RETURN TRUE.
END.


DEFINE STREAM sList.
FUNCTION fAddList RETURN LOGICAL:
   DEFINE VARIABLE iFuncParamEntries AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcValue AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cValueFile AS CHARACTER NO-UNDO. 
   iFuncParamEntries = NUM-ENTRIES(pcFunctionParams).
   cValueFile = ENTRY(iFuncParamEntries,pcFunctionParams).
   EMPTY TEMP-TABLE ttList.
   IF SEARCH(cValueFile) NE ? THEN
   DO:
      INPUT STREAM sList FROM VALUE(cValueFile).
      REPEAT:
         IMPORT STREAM sList UNFORMATTED lcValue.
         IF NOT CAN-FIND(ttList WHERE ttList.cValue = lcValue) THEN
         DO:
            CREATE ttList.
            ttList.cValue = lcValue.
         END.
      END.
      INPUT STREAM sList CLOSE.
   END.
   ELSE
   DO:
      DEFINE VARIABLE cPrefix AS CHARACTER NO-UNDO. 
      cPrefix = "Value".
      IF piFunction EQ 3 OR piFunction EQ 4 THEN cPrefix = "ICC".
      MESSAGE cPrefix " File " cValueFile " does not exist" VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   RETURN TRUE.
END.


FUNCTION fCheckValueInList RETURN LOGICAL (INPUT pcValue AS CHARACTER):
   FIND ttList WHERE ttList.cValue = pcValue NO-ERROR.
   IF AVAIL ttList THEN lFound = TRUE.
   RETURN TRUE.
END.


FUNCTION fIncrementCount RETURN LOGICAL (INPUT pcTable AS CHARACTER):
   IF NOT CAN-FIND(ttCounts WHERE ttCounts.cTable = pcTable) THEN
   DO:
      CREATE ttCounts.
      ASSIGN ttCounts.cTable = pcTable
             ttCounts.iCount = 0.
   END.

   FIND ttCounts WHERE ttCounts.cTable = pcTable.
   ttCounts.iCount = ttCounts.iCount + 1.
   RETURN TRUE.
END.


DEFINE STREAM sFile.

FUNCTION fEnsureEmptyFile RETURN LOGICAL (INPUT pcFile AS CHARACTER):
   IF SEARCH(pcFile) EQ ? THEN
   DO:
      OUTPUT STREAM sFile TO VALUE(pcFile).

      OUTPUT STREAM sFile CLOSE.
      RETURN TRUE.
   END.
   RETURN FALSE.
END.

FUNCTION fAddFileLine RETURN LOGICAL 
   (INPUT pcFile AS CHARACTER, INPUT pcLine AS CHARACTER):
   fEnsureEmptyFile(pcFile).
   OUTPUT STREAM sFile to VALUE(pcFile) APPEND.
      PUT STREAM sFile UNFORMATTED pcLine SKIP.
   OUTPUT STREAM sFile CLOSE.
   RETURN TRUE.
END.


FUNCTION fPerformCounts RETURN LOGICAL 
   (INPUT pcFilePrefix AS CHARACTER,
    INPUT pcFilePostFix AS CHARACTER):

   FOR EACH ttCounts:
      IF piFunction = 1 AND ttCounts.iCount = 0 THEN
         fAddFileLine(pcOutputDir + "/" + pcFilePrefix + ttCounts.cTable + pcFilePostFix,
            STRING(MobSub.MsSeq)).
      ELSE IF piFunction = 2 THEN
         fAddFileLine(pcOutputDir + "/" + pcFilePrefix + ttCounts.cTable + pcFilePostFix,
         STRING(MobSub.MsSeq) + "," + STRING(ttCounts.iCount)).
   END.
   RETURN TRUE.
END.


FUNCTION fPerformFunction RETURN LOGICAL:
   DEFINE VARIABLE cFilePrefix AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cFilePostFix AS CHARACTER NO-UNDO. 
   cFilePrefix = "".
   cFilePostFix = "".
   CASE piFunction:
      WHEN 1 THEN cFilePrefix = "lacking".
      WHEN 2 THEN cFilePostFix = "counts".
      WHEN 3 THEN cFilePrefix = "mobsubs_or_orders_with_one_of_sim.txt".
      WHEN 4 THEN cFilePrefix = "mobsubs_with_one_of_sim.txt".

   END.
   IF piFunction = 1 OR piFunction = 2 THEN
      fPerformCounts(cFilePrefix, cFilePostFix).
   IF piFunction = 3 OR piFunction = 4 THEN
      IF lFound THEN
        fAddFileLine(pcOutputDir + "/" + cFilePrefix,
           STRING(Mobsub.MsSeq)).

   RETURN TRUE.
END.



FUNCTION fCheckCLIRelated RETURN LOGICAL (INPUT pcCLI AS CHARACTER):
   FOR EACH MSISDN WHERE MSISDN.CLI = pcCLI NO-LOCK:
      fIncrementCount("MSISDN").
   END.

   FOR EACH MSISDNNumber WHERE MSISDNNumber.CLI = pcCLI NO-LOCK:
      fIncrementCount("MSISDNNumber").
   END.

   FOR EACH CallAlarm WHERE CallAlarm.CLI = pcCLI NO-LOCK:
      fIncrementCount("CallAlarm").
   END.

   FOR EACH PrepCDR NO-LOCK WHERE PrepCDR.CLI = pcCLI:
      fIncrementCount("PrepCDR").
   END.

   FOR EACH MobCDR NO-LOCK WHERE MobCDR.CLI = pcCLI:
      fIncrementCount("MobCDR").
   END.

   RETURN TRUE.
END.


FUNCTION fCheckICCRelated RETURN LOGICAL (INPUT pcICC AS CHARACTER):
   FOR EACH SIM WHERE SIM.ICC = pcICC NO-LOCK:
      fIncrementCount("SIM").
   END.

   FOR EACH IMSI WHERE IMSI.ICC = pcICC AND IMSI.IMSI = Mobsub.IMSI NO-LOCK:
      fIncrementCount("IMSI").
   END.

   RETURN TRUE.
END.



FUNCTION fCheckInvoiceRelated RETURN LOGICAL (INPUT piInvNum AS INTEGER):
   FOR EACH InvASub WHERE InvASub.InvNum = piInvNum NO-LOCK:
      fIncrementCount("InvASub").
   END.

   FOR EACH InvRow WHERE InvRow.InvNum = piInvNum NO-LOCK:
      fIncrementCount("InvRow").
   END.

   FIND Invoice WHERE Invoice.InvNum = piInvNum NO-LOCK NO-ERROR.
   IF AVAIL Invoice THEN
      fIncrementCount("Invoice").
   
   RETURN TRUE.
END.


FUNCTION fCheckOrderRelated RETURN LOGICAL:
   FOR EACH OrderAccessory WHERE OrderAccessory.OrderId = Order.OrderId NO-LOCK:
      fIncrementCount("OrderAccessory").
   END.

   FOR EACH OrderCustomer WHERE OrderCustomer.OrderId = Order.OrderId NO-LOCK:
      fIncrementCount("OrderCustomer").
   END.

   FOR EACH OrderDelivery WHERE OrderDelivery.OrderId = Order.OrderId NO-LOCK:
      fIncrementCount("OrderDelivery").
   END.

   FOR EACH OrderTopup WHERE OrderTopup.OrderId = Order.OrderId NO-LOCK:
      fIncrementCount("OrderTopup").
   END.

   FOR EACH OrderTimeStamp WHERE OrderTimeStamp.OrderId = Order.OrderId NO-LOCK:
      fIncrementCount("OrderTimeStamp").
   END.

   FOR EACH OrderPayment WHERE OrderPayment.OrderId = Order.OrderId NO-LOCK:
      fIncrementCount("OrderPayment").
   END.

   FOR EACH MnpProcess WHERE MnpProcess.OrderId = Order.OrderId NO-LOCK:
      FOR EACH MnpMessage WHERE MnpMessage.MnpSeq = MnpProcess.MnpSeq NO-LOCK:
          fIncrementCount("MnpMessage").
      END.
      fIncrementCount("MnpProcess").
   END.

   FOR EACH Memo WHERE Memo.Hosttable = "order" AND Memo.KeyValue = STRING(Order.OrderId) 
       NO-LOCK:
      fIncrementCount("Memo").
   END.

   fCheckInvoiceRelated(Order.InvNum).
   
   fIncrementCount("Order").

   RETURN TRUE.
END.

FUNCTION fCheckCustomerRelated RETURN LOGICAL (INPUT piCustNum AS INTEGER):
   FOR EACH Limit WHERE Limit.CustNum = piCustNum NO-LOCK:
      fIncrementCount("Limit").
   END.

   FOR EACH FaTime WHERE FaTime.CustNum = piCustNum NO-LOCK:
      fIncrementCount("FaTime").
   END.

   FOR EACH Invoice WHERE Invoice.CustNum = piCustNum NO-LOCK:
      fCheckInvoiceRelated(Invoice.InvNum).
   END.

   FIND Customer WHERE Customer.CustNum = piCustNum NO-LOCK NO-ERROR.
   IF AVAIL Customer THEN
      fIncrementCount("Customer").

   RETURN TRUE.
END.


FUNCTION fCheckMsSeqRelated RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   FOR EACH PrepaidRequest WHERE PrepaidRequest.MsSeq = piMsSeq NO-LOCK:
      fIncrementCount("PrepaidRequest").
   END.
   
   FOR EACH DCCLI WHERE DCCLI.MsSeq = piMsSeq NO-LOCK:
      fIncrementCount("DCCLI").
   END.

   FOR EACH DCCounter WHERE DCCounter.MsSeq = piMsSeq NO-LOCK:
      fIncrementCount("DCCounter").
   END.

   FOR EACH MsRequest WHERE MsRequest.MsSeq = piMsSeq NO-LOCK:
      fIncrementCount("MsRequest").
   END.

   FOR EACH MsOwner WHERE MsOwner.MsSeq = piMsSeq NO-LOCK:
      fIncrementCount("MsOwner").
   END.

   FOR EACH MnpSub WHERE MnpSub.MsSeq = piMsSeq NO-LOCK:
      fIncrementCount("MnpSub").
   END.

   FOR EACH SubSer WHERE SubSer.MsSeq = piMsSeq NO-LOCK:
      fIncrementCount("SubSer").
   END.

   FOR EACH SubSerPara WHERE SubSerPara.MsSeq = piMsSeq NO-LOCK:
      fIncrementCount("SubSerPara").
   END.

   RETURN TRUE.
END.


FUNCTION fHandleMobsub RETURN LOGICAL:
   lFound = FALSE.
   IF piFunction NE 3 AND piFunction NE 4 THEN 
   DO:
      fInitCounts().
      fCheckCustomerRelated(Mobsub.CustNum).
      fCheckCustomerRelated(Mobsub.AgrCust).
      fCheckCustomerRelated(Mobsub.InvCust).
   END.
   FOR EACH Order WHERE Order.MsSeq = Mobsub.MsSeq NO-LOCK:
       IF piFunction NE 3 AND piFunction NE 4 THEN 
       DO:
          fCheckCLIRelated(Order.CLI).
          fCheckOrderRelated().
          fCheckICCRelated(Order.ICC).
       END.
       ELSE IF piFunction EQ 3 THEN
          fCheckValueInList(Order.ICC).
   END.
   IF piFunction NE 3 AND piFunction NE 4 THEN 
   DO:
      fCheckICCRelated(Mobsub.ICC).
      fCheckCLIRelated(Mobsub.CLI).
      fCheckMsSeqRelated(MobSub.MsSeq).
   END.
   ELSE fCheckValueInList(MobSub.ICC).

   fPerformFunction().
   RETURN TRUE.
END.

IF piFunction = 3 OR piFunction = 4 THEN
   IF NOT fAddList() THEN RETURN.

iCount = 0.

IF INDEX(pcFunctionParams, "allmobsub") EQ 0 THEN
DO:
   INPUT STREAM sMsSeq FROM VALUE (pcMsSeqFile).
   REPEAT:
      IMPORT STREAM sMsSeq UNFORMATTED cValue.
      piMsSeq = INTEGER(cValue) NO-ERROR.
         
      IF NOT ERROR-STATUS:ERROR AND piMsSeq > 0 THEN
      DO:
         FIND Mobsub WHERE Mobsub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
         IF AVAIL Mobsub THEN
            fHandleMobsub().
      END.
      iCount = iCount + 1.
      IF iDispInterval > 0 THEN
          IF iCount MOD iDispInterval EQ 0 THEN DISP iCount.
   END.
   INPUT STREAM sMsSeq CLOSE.
END.
ELSE
DO:
   FOR EACH Mobsub NO-LOCK:
      fHandleMobsub().
      iCount = iCount + 1.
      IF iDispInterval > 0 THEN
          IF iCount MOD iDispInterval EQ 0 THEN DISP iCount.
   END.
END.



