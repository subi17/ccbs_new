{/home/harrim/utilities/ttinpmobsub.i}
DEFINE INPUT PARAMETER pcOutputDir        AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER piBeginMsSeq       AS INTEGER  NO-UNDO. 
DEFINE INPUT PARAMETER piEndMsSeq         AS INTEGER  NO-UNDO. 
DEFINE INPUT PARAMETER piWantedMsSeqCount AS INTEGER  NO-UNDO. 
DEFINE INPUT PARAMETER pcLogFile          AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcValueFileName    AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER piDispInterval     AS INTEGER  NO-UNDO. 
DEFINE INPUT PARAMETER pcValueFixNameList AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER TABLE FOR ttinpmobsub.

DEFINE VARIABLE iCountMobSub         AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lDisplay             AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE iOrderId             AS INTEGER   NO-UNDO. 
DEFINE VARIABLE cValueFixName        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iMsFixName           AS INTEGER NO-UNDO. 
DEFINE VARIABLE iMsFixNameRound      AS INTEGER NO-UNDO. 
DEFINE VARIABLE iMsFixNameCount      AS INTEGER NO-UNDO. 
DEFINE VARIABLE cValue               AS CHARACTER NO-UNDO. 


IF piDispInterval EQ 0 THEN lDisplay = FALSE. ELSE lDisplay = TRUE.
iCountMobSub       = 0.


DEFINE VARIABLE iCountDaysForMobCDRMax AS INTEGER NO-UNDO. 
DEFINE VARIABLE iCountDaysForMobCDRMin AS INTEGER NO-UNDO. 

DEFINE VARIABLE iCountDaysForPrepCDRMax AS INTEGER NO-UNDO. 
DEFINE VARIABLE iCountDaysForPrepCDRMin AS INTEGER NO-UNDO. 

ASSIGN iCountDaysForMobCDRMax = 90
       iCountDaysForPrepCDRMax = 90
       iCountDaysForMobCDRMin = 0
       iCountDaysForPrepCDRMin = 0.


DEFINE VARIABLE iMobSubFix         AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iCustFix           AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iInvRowFix         AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iInvASubFix        AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iInvoiceFix        AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iInvSeqFix         AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iCallAlarmFix      AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iMemoFix           AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iMnpProcessFix     AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iMnpMessageFix     AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iMnpSubFix         AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iMSISDNFix         AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iMSISDNNumberFix   AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iMsOwnerFix        AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iSIMFix            AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iOrderFix          AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iOrderAccessoryFix AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iOrderCustomerFix  AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iOrderDeliveryFix  AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iOrderPaymentFix   AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iOrderTimeStampFix AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iOrderTopupFix     AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iMsRequestFix      AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iDCCLIFix          AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iDCCounterFix      AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iFaTimeFix         AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iLimitFix          AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iIMSIFix           AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iPrepaidRequestFix AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iSubSerFix         AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iSubserParaFix     AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iMobCDRFix         AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iMCDRDtlFix        AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iPrepCDRFix        AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iCOTargFix         AS INTEGER INITIAL 1 NO-UNDO. 
DEFINE VARIABLE iTMCounterFix      AS INTEGER INITIAL 1 NO-UNDO. 

DEFINE TEMP-TABLE ttKeys
   FIELD cKeyName AS CHARACTER
   FIELD cValue   AS CHARACTER
   INDEX idxKey cKeyName cValue.

FUNCTION fAddkey RETURN LOGICAL (INPUT pcKeyName AS CHARACTER, INPUT pcValue AS CHARACTER):
   IF NOT CAN-FIND(ttKeys WHERE ttKeys.cKeyName = pcKeyName AND ttKeys.cValue = pcValue) THEN
   DO:
       CREATE ttKeys.
       ttKeys.cKeyName = pcKeyName.
       ttKeys.cValue = pcValue.
   END.
   RETURN FALSE.
END.

FUNCTION fValueExists RETURN LOGICAL (INPUT pcKeyName AS CHARACTER, INPUT pcValue AS CHARACTER):
   IF CAN-FIND(ttKeys WHERE ttKeys.cKeyName = pcKeyName AND ttKeys.cValue = pcValue) THEN
      RETURN TRUE.
   RETURN FALSE.
END.


FUNCTION fGetCharacterOption RETURN CHARACTER (INPUT pcOptionName AS CHARACTER):
   IF CAN-FIND(ttinpmobsub WHERE ttinpmobsub.coption EQ pcOptionName) THEN
   DO: 
      FIND ttinpmobsub WHERE ttinpmobsub.coption EQ pcOptionName.
      RETURN ttinpmobsub.cvalue.
   END.
   RETURN ?.
END.

FUNCTION fInitFixNums RETURN LOGICAL:
   ASSIGN
      iCustFix           = 1
      iInvRowFix         = 1
      iInvASubFix        = 1  
      iInvoiceFix        = 1  
      iInvSeqFix         = 1
      iCallAlarmFix      = 1  
      iMemoFix           = 1  
      iMnpProcessFix     = 1  
      iMnpMessageFix     = 1  
      iMnpSubFix         = 1  
      iMSISDNFix         = 1  
      iMSISDNNumberFix   = 1  
      iMsOwnerFix        = 1  
      iSIMFix            = 1  
      iOrderFix          = 1  
      iOrderAccessoryFix = 1  
      iOrderCustomerFix  = 1  
      iOrderDeliveryFix  = 1  
      iOrderPaymentFix   = 1  
      iOrderTimeStampFix = 1  
      iOrderTopupFix     = 1  
      iMsRequestFix      = 1  
      iDCCLIFix          = 1  
      iDCCounterFix      = 1  
      iFaTimeFix         = 1  
      iLimitFix          = 1  
      iIMSIFix           = 1  
      iPrepaidRequestFix = 1  
      iSubSerFix         = 1  
      iSubserParaFix     = 1 
      iMobCDRFix         = 1
      iMCDRDtlFix        = 1
      iPrepCDRFix        = 1
      iCOTargFix         = 1
      iTMCounterFix      = 1.

   RETURN TRUE.
END.


DEFINE STREAM sLog.
FUNCTION fDumpFixture RETURN LOGICAL (INPUT lhBuffer AS HANDLE, INPUT piFixNum AS INTEGER):

   DEFINE VARIABLE cRoundName AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcFile AS CHARACTER NO-UNDO. 
   lcFile = pcOutputDir + LC(lhBuffer:NAME) + ".yaml".

   DEFINE VARIABLE cFieldsTaken AS CHARACTER NO-UNDO. 
   cFieldsTaken = fGetCharacterOption(lhBuffer:NAME + "fields").

   FILE-INFO:FILE-NAME = lcFile.

   IF iMsFixNameRound > 1 THEN cRoundName = STRING(iMsFixNameRound).
   ELSE cRoundName = "".
   
   IF NOT FILE-INFO:FILE-TYPE EQ ? AND 
      NOT FILE-INFO:FILE-TYPE EQ "FRW" THEN 
      MESSAGE lcFile " status:" FILE-INFO:FILE-TYPE SKIP
         "NO WRITE ACCESS" VIEW-AS ALERT-BOX.

   OUTPUT STREAM sLog TO VALUE(lcFile) APPEND.
   IF lhBuffer:NAME EQ "mobsub" THEN
     PUT STREAM sLog UNFORMATTED cValueFixName cRoundName ":" SKIP.
   ELSE
     PUT STREAM sLog UNFORMATTED 
        lhBuffer:NAME piFixNum "For" cValueFixName cRoundName ":" SKIP.

   DEFINE VARIABLE i AS INTEGER NO-UNDO. 
   DO i = 1 TO lhBuffer:NUM-FIELDS:
     IF cFieldsTaken NE ? THEN
        IF LOOKUP(lhBuffer:BUFFER-FIELD(i):NAME,cFieldsTaken) = 0 THEN NEXT.

      /* skip extent fields; do not output in fixtures */
      IF lhBuffer:BUFFER-FIELD(i):EXTENT > 0 THEN NEXT.
      IF lhBuffer:BUFFER-FIELD(i):BUFFER-VALUE EQ 
         lhBuffer:BUFFER-FIELD(i):INITIAL THEN NEXT.
     
      DEFINE VARIABLE deValue AS DECIMAL FORMAT "9999999999999.99999" NO-UNDO. 
      IF lhBuffer:BUFFER-FIELD(i):FORMAT EQ "99999999.99999" THEN
      DO: 
         deValue = lhBuffer:BUFFER-FIELD(i):BUFFER-VALUE.
         
      END.
      ELSE
         IF lhBuffer:BUFFER-FIELD(i):BUFFER-VALUE EQ 
            lhBuffer:BUFFER-FIELD(i):INITIAL THEN NEXT. 

      PUT STREAM sLog UNFORMATTED 
         "   " lhBuffer:BUFFER-FIELD(i):NAME + ":".

      IF lhBuffer:BUFFER-FIELD(i):DATA-TYPE EQ "DECIMAL" THEN
      DO:
         deValue = lhBuffer:BUFFER-FIELD(i):BUFFER-VALUE.

         IF lhBuffer:BUFFER-FIELD(i):FORMAT EQ "99999999.99999"  THEN
            PUT STREAM sLog UNFORMATTED  deValue SKIP.
         ELSE
            PUT STREAM sLog UNFORMATTED 
               lhBuffer:BUFFER-FIELD(i):BUFFER-VALUE SKIP.
      END.
      ELSE IF lhBuffer:BUFFER-FIELD(i):DATA-TYPE EQ "CHARACTER" THEN 
         PUT STREAM sLog UNFORMATTED 
            '"' lhBuffer:BUFFER-FIELD(i):BUFFER-VALUE '"' SKIP.
      ELSE 
      DO:
         PUT STREAM sLog UNFORMATTED 
            lhBuffer:BUFFER-FIELD(i):BUFFER-VALUE SKIP.
      END.

   END.
      
   PUT STREAM sLog UNFORMATTED " " SKIP.
   OUTPUT STREAM sLog CLOSE.

   RETURN TRUE.
END.



FUNCTION fAddCustomers RETURN LOGICAL (INPUT piCustNum AS INTEGER):
  FOR EACH Customer WHERE Customer.Brand   = "1" AND
                          Customer.CustNum = piCustNum NO-LOCK:
      fDumpFixture(BUFFER Customer:HANDLE, iCustFix).
      iCustFix = iCustFix + 1.
  END.
END.


FUNCTION fAddInvRows RETURN LOGICAL (INPUT piInvNum AS INTEGER):
  FOR EACH InvRow WHERE InvRow.InvNum = piInvNum NO-LOCK:
     fDumpFixture(BUFFER InvRow:HANDLE, iInvRowFix).
     iInvRowFix = iInvRowFix + 1.
  END.

  RETURN TRUE.
END.

FUNCTION fAddInvASub RETURN LOGICAL (iNPUT piInvNum AS INTEGER):
   FOR EACH InvASub WHERE InvASub.InvNum = piInvNum NO-LOCK:
      fDumpFixture(BUFFER InvASub:HANDLE, iInvASubFix).
      iInvASubFix = iInvASubFix + 1.
   END.

   RETURN TRUE.
END.


FUNCTION fAddInvoices RETURN LOGICAL (INPUT piCustNum AS INTEGER):
  DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

  
  FOR EACH Invoice WHERE Invoice.Brand = "1" AND
                         Invoice.CustNum = piCustNum NO-LOCK:

      IF fValueExists("Invoice.InvNum", STRING(Invoice.InvNum)) THEN NEXT.

      fDumpFixture(BUFFER Invoice:HANDLE, iInvoiceFix).
      fAddInvRows(Invoice.InvNum).
      fAddInvASub(Invoice.InvNum).
      iInvoiceFix = iInvoiceFix + 1.

      fAddKey("Invoice.InvNum", STRING(Invoice.InvNum)).
  END.
  
END.

FUNCTION fAddOrderInvoices RETURN LOGICAL (INPUT piInvNum AS INTEGER):
  IF fValueExists("Invoice.InvNum", STRING(piInvNum)) THEN RETURN FALSE.

  FOR EACH Invoice WHERE Invoice.Brand = "1" AND
                         Invoice.InvNum = piInvNum NO-LOCK:
      fDumpFixture(BUFFER Invoice:HANDLE, iInvoiceFix).
      fAddInvRows(Invoice.InvNum).
      fAddInvASub(Invoice.InvNum).
      iInvoiceFix = iInvoiceFix + 1.

  END.
  fAddKey("Invoice.InvNum", STRING(piInvNum)).
END.


FUNCTION fAddMsRequests RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
  FOR EACH MsRequest WHERE MsRequest.MsSeq = piMsSeq NO-LOCK:
      fDumpFixture(BUFFER MsRequest:HANDLE, iMsRequestFix).
      iMsRequestFix = iMsRequestFix + 1.
  END.
END.

FUNCTION fAddDCCounter RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   FOR EACH DCCounter WHERE DCCounter.MsSeq = piMsSeq NO-LOCK:
      fDumpFixture(BUFFER DCCounter:HANDLE, iDCCounterFix).
      iDCCounterFix = iDCCounterFix + 1.
   END.
END.

FUNCTION fAddDCCLI RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   FOR EACH DCCLI WHERE DCCLI.MsSeq = piMsSeq NO-LOCK:
      fDumpFixture(BUFFER DCCLI:HANDLE, iDCCLIFix).
      iDCCLIFix = iDCCLIFix + 1.
   END.

   RETURN TRUE.
END.


FUNCTION fAddFaTimes RETURN LOGICAL (INPUT piCustNum AS INTEGER):
   FOR EACH FaTime WHERE FaTime.CustNum = piCustNum NO-LOCK:
      fDumpFixture(BUFFER FaTime:HANDLE, iFaTimeFix).
      iFaTimeFix = iFaTimeFix + 1.
   END.

   RETURN TRUE.
END.



FUNCTION fAddLimits RETURN LOGICAL (INPUT piCustNum AS INTEGER):
   FOR EACH Limit WHERE Limit.CustNum = piCustNum NO-LOCK:
      fDumpFixture(BUFFER Limit:HANDLE, iLimitFix).
      iLimitFix = iLimitFix + 1.
   END.
   
   RETURN TRUE.
END.


FUNCTION fAddInvSeqs RETURN LOGICAL (INPUT piCustNum AS INTEGER):
    IF AVAIL Mobsub THEN
    DO:
        FOR EACH InvSeq WHERE InvSeq.MsSeq = Mobsub.MsSeq AND
            InvSeq.CustNum = piCustNum NO-LOCK:
           fDumpFixture(BUFFER InvSeq:HANDLE, iInvSeqFix).
           iInvSeqFix = iInvSeqFix + 1.
        END.
    END.
    ELSE
    DO:
        FOR EACH InvSeq WHERE 
            InvSeq.CustNum = piCustNum NO-LOCK:
           fDumpFixture(BUFFER InvSeq:HANDLE, iInvSeqFix).
           iInvSeqFix = iInvSeqFix + 1.
        END.
    END.
    RETURN TRUE.
END.

FUNCTION fAddCustomerRecords RETURN LOGICAL (INPUT piCustNum AS INTEGER):
   IF fValueExists("Customer.CustNum", STRING(piCustNum)) THEN RETURN FALSE.

   fAddCustomers(piCustNum).
   fAddInvoices(piCustNum).
   fAddFaTimes(piCustNum).
   fAddLimits(piCustNum).
   fAddInvSeqs(piCustNum).
   fAddKey("Customer.CustNum", STRING(piCustNum)).

   RETURN TRUE.
END.


FUNCTION fAddMnpProcesses RETURN LOGICAL:
   FOR EACH MnpProcess WHERE MnpProcess.OrderId = iOrderId NO-LOCK:
      fDumpFixture(BUFFER MnpProcess:HANDLE, iMnpProcessFix).
      iMnpProcessFix = iMnpProcessFix + 1.
      FOR EACH MnpMessage WHERE MnpMessage.MnpSeq = MnpProcess.MnpSeq NO-LOCK:
          fDumpFixture(BUFFER MnpMessage:HANDLE, iMnpMessageFix).
          iMnpMessageFix = iMnpMessageFix + 1.
      END.
   END.

   RETURN TRUE.
END.


FUNCTION fAddPrepaidRequests RETURN LOGICAL:
   FOR EACH PrepaidRequest WHERE PrepaidRequest.Brand = "1" 
      AND PrepaidRequest.MsSeq = MobSub.MsSeq NO-LOCK:
       fDumpFixture(BUFFER PrepaidRequest:HANDLE, iPrepaidRequestFix).
       iPrepaidRequestFix = iPrepaidRequestFix + 1.
   END.
END.


FUNCTION fAddIMSIRecords RETURN LOGICAL:
   FOR EACH IMSI WHERE IMSI.ICC = SIM.ICC AND Mobsub.IMSI = IMSI.IMSI NO-LOCK:
      fDumpFixture(BUFFER IMSI:HANDLE, iIMSIFix).
      iIMSIFix = iIMSIFIx + 1.
   END.

   RETURN TRUE.
END.


FUNCTION fAddICCRelatedRecords RETURN LOGICAL (INPUT pcICC AS CHARACTER):
   FOR EACH SIM WHERE SIM.Brand = "1" AND SIM.ICC = Order.ICC NO-LOCK:
      IF NOT fValueExists("SIM.ICC", SIM.ICC) THEN
      DO:
         fDumpFixture(BUFFER SIM:HANDLE, iSIMFix).
         iSIMFix = iSIMFix + 1.
         fAddIMSIRecords().
         fAddKey("SIM.ICC", SIM.ICC).
      END.
   END.

   /* add counter for fixture names when value field is 
      not counting subscriptions */
   DEFINE VARIABLE cValueField AS CHARACTER NO-UNDO. 
   IF cValueField NE "Mobsub.MsSeq" THEN
     iCountMobsub = iCountMobsub + 1.

   RETURN TRUE.
END.


FUNCTION fAddMCDRDtl2s RETURN LOGICAL (INPUT pdaDateSt AS DATE,
   INPUT piDtlSeq AS INTEGER):
   FOR EACH MCDRDtl2 WHERE MCDRDtl2.DateSt = pdaDateSt AND
       MCDRDtl2.DtlSeq = piDtlSeq NO-LOCK:

       fDumpFixture(BUFFER MCDRDtl2:HANDLE, iMCDRDtlFix).
       iMCDRDtlFix = iMCDRDtlFix + 1.
   END.
   RETURN TRUE.
END.


FUNCTION fAddMSISDNRelatedRecords RETURN LOGICAL (INPUT pcCLI AS CHARACTER):
   IF fValueExists("MSISDN.CLI", pcCLI) THEN RETURN FALSE.
   FOR EACH MSISDN WHERE MSISDN.Brand = "1" AND MSISDN.CLI = pcCLI NO-LOCK:
      fDumpFixture(BUFFER MSISDN:HANDLE, iMSISDNFix).
      iMSISDNFix = iMSISDNFix + 1.
   END.

   FOR EACH MSISDNNumber WHERE MSISDNNumber.CLI = pcCLI NO-LOCK:
      fDumpFixture(BUFFER MSISDNNumber:HANDLE, iMSISDNNumberFix).
      iMSISDNNumberFix = iMSISDNNumberFix + 1.
   END.

   FOR EACH CallAlarm WHERE CallAlarm.CLI = pcCLI NO-LOCK:
      fDumpFixture(BUFFER CallAlarm:HANDLE, iCallAlarmFix).
      iCallAlarmFix = iCallAlarmFix + 1.
   END.
 

   DEFINE VARIABLE cValueField AS CHARACTER NO-UNDO. 
   cValueField = fGetCharacterOption("ValueField").
   IF cValueField EQ "Mobsub.MsSeq" THEN
   DO:
      DEFINE VARIABLE dtMobsubAct AS DATE NO-UNDO.
      dtMobsubAct = Mobsub.ActivationDate.

      FOR EACH MobCDR NO-LOCK WHERE 
        MobCDR.CLI = pcCLI AND 
        MobCDR.DateST >= dtMobsubAct - iCountDaysForMobCDRMax AND
        MobCDR.DateST <= dtMobsubAct - iCountDaysForMobCDRMin:

         fDumpFixture(BUFFER MobCDR:HANDLE, iMobCDRFix).
         iMobCDRFix = iMobCDRFix + 1.
         fAddMCDRDtl2s(MobCDR.DateSt, MobCDR.DtlSeq).
      END.


      FOR EACH PrepCDR NO-LOCK WHERE 
          PrepCDR.CLI = pcCLI AND 
          PrepCDR.DateST >= dtMobsubAct - iCountDaysForPrepCDRMax AND
          PrepCDR.DateST <= dtMobsubAct - iCountDaysForPrepCDRMin:

         fDumpFixture(BUFFER PrepCDR:HANDLE, iPrepCDRFix).
         iPrepCDRFix = iPrepCDRFix + 1.
      END.
   END.
   ELSE
   /* add counter for fixture names when value field is 
      not counting subscriptions */
     iCountMobsub = iCountMobsub + 1.

   fAddKey("MSISDN.CLI", pcCLI).

   RETURN TRUE.
END.



FUNCTION fAddOrderRecords RETURN LOGICAL:
   IF fValueExists("Order.OrderId", STRING(Order.OrderId)) THEN
      RETURN FALSE.

   fDumpFixture(BUFFER Order:HANDLE, iOrderFix).
   iOrderFix = iOrderFix + 1.
   FOR EACH OrderCustomer WHERE OrderCustomer.Brand = "1" AND 
                                OrderCustomer.OrderId = iOrderId NO-LOCK:
      fDumpFixture(BUFFER OrderCustomer:HANDLE, iOrderCustomerFix).
      iOrderCustomerFix = iOrderCustomerFix + 1.
   END.

   FOR EACH OrderTopup WHERE OrderTopup.Brand = "1" AND 
                             OrderTopup.OrderId = iOrderId NO-LOCK:
      fDumpFixture(BUFFER OrderTopup:HANDLE, iOrderTopupFix).
      iOrderTopupFix = iOrderTopupFix + 1.
   END.

   FOR EACH OrderAccessory WHERE OrderAccessory.Brand = "1" AND 
                                 OrderAccessory.OrderId = iOrderId NO-LOCK:
      fDumpFixture(BUFFER OrderAccessory:HANDLE, iOrderAccessoryFix).
      iOrderAccessoryFix = iOrderAccessoryFix + 1.
   END.


   FOR EACH OrderPayment WHERE OrderPayment.Brand = "1" AND 
                               OrderPayment.OrderId = iOrderId NO-LOCK:
      fDumpFixture(BUFFER OrderPayment:HANDLE, iOrderPaymentFix).
      iOrderPaymentFix = iOrderPaymentFix + 1.
   END.

   FOR EACH OrderDelivery WHERE OrderDelivery.Brand = "1" AND 
                                OrderDelivery.OrderId = iOrderId NO-LOCK:
      fDumpFixture(BUFFER OrderDelivery:HANDLE, iOrderDeliveryFix).
      iOrderDeliveryFix = iOrderDeliveryFix + 1.
   END.

   FOR EACH OrderTimeStamp WHERE OrderTimeStamp.Brand = "1" AND 
                                 OrderTimeStamp.OrderId = iOrderId NO-LOCK:
      fDumpFixture(BUFFER OrderTimeStamp:HANDLE, iOrderTimeStampFix).
      iOrderTimeStampFix = iOrderTimeStampFix + 1.
   END.

   FOR EACH Memo WHERE Memo.Brand = "1" AND Memo.HostTable = "Order" AND 
                       Memo.KeyValue = STRING(iOrderId) NO-LOCK:
      fDumpFixture(BUFFER Memo:HANDLE, iMemoFix).
      iMemoFix = iMemoFix + 1.
   END.


   fAddOrderInvoices(Order.InvNum).

   /* Order SIM */
   fAddICCRelatedRecords(Order.ICC).

   /* Subscription SIM */
   fAddICCRelatedRecords(MobSub.ICC).

   /* Order MSISDN */
   fAddMSISDNRelatedRecords(Order.CLI).

   /* Subscription MSISDN */
   fAddMSISDNRelatedRecords(Mobsub.CLI).
   fAddKey("Order.OrderId", STRING(Order.OrderId)).

   RETURN TRUE.
END.

FUNCTION fAddMsOwners RETURN LOGICAL:
   FOR EACH MsOwner WHERE MsOwner.MsSeq = Mobsub.MsSeq NO-LOCK:
      fDumpFixture(BUFFER MsOwner:HANDLE, iMsOwnerFix).
      iMsOwnerFix = iMsOwnerFix + 1.
   END.
   RETURN TRUE.
END.


FUNCTION fAddMnpSub RETURN LOGICAL:
   FOR EACH MnpSub WHERE MnpSub.MsSeq = Mobsub.MsSeq NO-LOCK:
      fDumpFixture(BUFFER MnpSub:HANDLE, iMnpSubFix).
      iMnpSubFix = iMnpSubFix + 1.
   END.

   RETURN TRUE.
END.



FUNCTION fAddSubsers RETURN LOGICAL:
   FOR EACH SubSer WHERE SubSer.MsSeq = Mobsub.MsSeq NO-LOCK:
      fDumpFixture(BUFFER SubSer:HANDLE, iSubSerFix).
      iSubSerFix = iSubSerFix + 1.
   END.

   FOR EACH SubSerPara WHERE SubSerPara.MsSeq = Mobsub.MsSeq NO-LOCK:
      fDumpFixture(BUFFER SubSerPara:HANDLE, iSubSerParaFix).
      iSubSerParaFix = iSubSerParaFix + 1.
   END.

   RETURN TRUE.
END.


FUNCTION fAddTMCounters RETURN LOGICAL:
   FOR EACH TMCounter WHERE TMCounter.MsSeq = Mobsub.MsSeq NO-LOCK:
      fDumpFixture(BUFFER TMCounter:HANDLE, iTMCounterFix).
      iTMCounterFix = iTMCounterFix + 1.
   END.

   RETURN TRUE.
END.

FUNCTION fAddCOTargs RETURN LOGICAL:
   FOR EACH COTarg WHERE COTarg.Brand = "1" AND COTarg.TargType = "M" AND
      COTarg.COTarg = STRING(Mobsub.MsSeq) NO-LOCK:
      fDumpFixture(BUFFER COTarg:HANDLE, iCOTargFix).
      iCOTargFix = iCOTargFix + 1.
   END.

   RETURN TRUE.
END.




FUNCTION fExportMobsubAndRelated RETURN LOGICAL 
   (OUTPUT plLeave AS LOGICAL, OUTPUT pcRejectReason AS CHARACTER):
   plLeave = FALSE.

   FIND FIRST Order WHERE Order.MsSeq = MobSub.MsSeq 
              USE-INDEX MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL Order THEN 
   DO:
      pcRejectReason = "no order was available.".
      RETURN FALSE.
   END.
   ELSE
   DO:
      iOrderId = Order.OrderId.
      IF iOrderId = 0 THEN 
      DO:
         pcRejectReason = "orderid was 0.".
         RETURN FALSE.
      END.
   END. 


   fDumpFixture(BUFFER Mobsub:HANDLE, iMobsubFix).
   iMobsubFix = iMobsubFix + 1.

   fAddMsOwners().
   fAddSubsers().
   fAddCOTargs().
   fAddCustomerRecords(MobSub.CustNum).
   fAddCustomerRecords(MobSub.AgrCust).
   fAddCustomerRecords(MobSub.InvCust).

   fAddMnpSub().
   fAddMnpProcesses().
   fAddPrepaidRequests().
   IF AVAIL Order THEN
      fAddOrderRecords().
   fAddMsRequests(MobSub.MsSeq).
   fAddDCCLI(MobSub.MsSeq).
   fAddDCCounter(MobSub.MsSeq).
   fAddTMCounters().

   iCountMobSub = iCountMobSub + 1.
   IF iCountMobSub >= piWantedMsSeqCount THEN
      plLeave = TRUE.
   IF lDisplay THEN
   IF iCountMobSub MOD piDispInterval = 0 THEN
      DISP iCountMobSub.

   RETURN TRUE.
END.

DEFINE STREAM sLogF.
DEFINE STREAM sValueF.

FUNCTION fLog RETURN LOGICAL (INPUT pcMsg AS CHARACTER):
   IF pcLogFile NE "" THEN
      PUT STREAM sLogF UNFORMATTED pcMsg SKIP.
   RETURN TRUE.
END.



FUNCTION fDumpMobsubAndRelated RETURN LOGICAL:
   DEFINE VARIABLE iMsSeq AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cRejectReason AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lLeave AS LOGICAL NO-UNDO. 

   iMsSeq = INTEGER(cValue) NO-ERROR.
   IF NOT ERROR-STATUS:ERROR THEN
   DO:
      FIND Mobsub WHERE Mobsub.MsSeq = iMsSeq NO-LOCK NO-ERROR.

      IF NOT AVAIL Mobsub THEN
      DO:
         fLog("Mobsub was not found with MsSeq " + STRING(iMsSeq)).
      END.
      ELSE
      DO:       
         IF NOT fExportMobsubAndRelated(
                OUTPUT lLeave, OUTPUT cRejectReason) THEN
         DO:
            fLog("Mobsub with MsSeq " + STRING(Mobsub.MsSeq) + 
                 "was rejected, because " + cRejectReason).
         END.
      END.
   END.
   ELSE
   DO:
      fLog("Input row " + cValue + 
           " did not contain integer suitable for MsSeq").
   END.

   RETURN TRUE.
END.


FUNCTION fDumpMsSeqRange RETURN LOGICAL:
   DEFINE VARIABLE lLeave AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE cRejectReason AS CHARACTER NO-UNDO. 

   GatherMobsubs:
   FOR EACH MobSub where MobSub.MsSeq >= piBeginMsSeq AND 
                         MobSub.MsSeq <= piEndMsSeq  NO-LOCK:
      IF NOT fExportMobsubAndRelated(
         OUTPUT lLeave, OUTPUT cRejectReason) THEN
      DO:
         fLog("Mobsub with MsSeq " + STRING(Mobsub.MsSeq) + 
              "was rejected, because " + cRejectReason).
      END.
      IF lLeave THEN LEAVE GatherMobsubs.
   END.

   RETURN TRUE.
END.


FUNCTION fMainLoop RETURN LOGICAL:
   DEFINE VARIABLE iMsSeq AS INTEGER NO-UNDO.
   DEFINE VARIABLE cValueField AS CHARACTER NO-UNDO. 
  
   cValueField = fGetCharacterOption("ValueField").
   IF cValueField EQ ? THEN cValueField = "MobSub.MsSeq".

   IF pcLogFile NE "" THEN
      OUTPUT STREAM sLogF TO VALUE(pcLogFile).

   iMsFixNameCount = NUM-ENTRIES(pcValueFixNameList).
   iMsFixNameRound = 1.

   IF pcValueFileName NE "" THEN
   DO:
      INPUT STREAM sValueF FROM VALUE(pcValueFileName).
      REPEAT:
         IMPORT STREAM sValueF UNFORMATTED cValue.
         iMsFixNameRound = TRUNCATE(iCountMobsub / iMsFixNameCount, 0).
         iMsFixName = iCountMobsub + 1 - iMsFixNameRound * iMsFixNameCount.
         cValueFixName = ENTRY(iMsFixName, pcValueFixNameList).
         fInitFixNums().
         CASE cValueField:
            WHEN "MobSub.MsSeq" THEN fDumpMobSubAndRelated().
            WHEN "MSISDN.CLI"   THEN fAddMSISDNRelatedRecords(cValue).
            WHEN "SIM.ICC"      THEN fAddICCRelatedRecords(cValue).
            WHEN "MsRequest.MsRequest" THEN
            DO:
               fDumpFixture(BUFFER MsRequest:HANDLE, iMsRequestFix).
               iMsRequestFix = iMsRequestFix + 1.
            END.
         END.
      END.
      INPUT STREAM sValueF CLOSE.
   END.
   ELSE
   DO:
      fDumpMsSeqRange().
   END.
  
   IF pcLogFile NE "" THEN
      OUTPUT STREAM sLogF CLOSE.

   RETURN TRUE.
END.

fMainLoop().


