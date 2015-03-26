DEFINE VARIABLE lMustFindInvoice     AS LOGICAL NO-UNDO.
DEFINE VARIABLE lMustFindInvRow      AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lMustFindMnpProcess  AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lMustFindMnpMessage  AS LOGICAL NO-UNDO. 

DEFINE VARIABLE lDeniedInvoice       AS LOGICAL NO-UNDO.
DEFINE VARIABLE lDeniedInvRow        AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lDeniedMnpProcess    AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lDeniedMnpMessage    AS LOGICAL NO-UNDO. 

DEFINE VARIABLE cOutputDir           AS CHARACTER NO-UNDO. 

DEFINE VARIABLE iCountMobSub         AS INTEGER NO-UNDO. 
DEFINE VARIABLE iWantedMobSubCount   AS INTEGER NO-UNDO. 

DEFINE VARIABLE iBeginMsSeq          AS INTEGER NO-UNDO. 
DEFINE VARIABLE iEndMsSeq            AS INTEGER NO-UNDO. 
DEFINE VARIABLE iDispInterval        AS INTEGER NO-UNDO. 
DEFINE VARIABLE lMustBeDextraCapable AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lRequiredDCCLIValid  AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lDeniedDCCLIValid    AS LOGICAL NO-UNDO. 

DEFINE VARIABLE iSepAscii            AS INTEGER NO-UNDO. 
iSepAscii = 255.

DEFINE VARIABLE lUseHandle           AS LOGICAL NO-UNDO. 
lUseHandle = TRUE.


DEFINE VARIABLE lDisplay             AS LOGICAL NO-UNDO. 
lDisplay = FALSE.

DEFINE VARIABLE rRowId               AS ROWID NO-UNDO.

lDeniedDCCLIValid = FALSE.
lRequiredDCCLIValid = FALSE.

IF lDeniedDCCLIValid AND lRequiredDCCLIValid THEN
   MESSAGE "DCCLI cannot be required and denied to be valid.".

lMustBeDextraCapable = FALSE.
iDispInterval = 100000.

&GLOBAL-DEFINE lcSep |

/* These variables effect the way the records are selected */
/* iBeginMsSeq = 971164. Required all four */
/* iBeginMsSeq = 975700. Denied MnpProcess and MnpMessage */
/* iBeginMsSeq = 976200. Denied Invoice */
/* Required Invoice and denied Invrow not found easily, MsSeq >= 976230 */
/* Required MnpProcess and denied MnpMessage not found easily, 
   MsSeq >= 976230 */

DEFINE VARIABLE iCountOrder                   AS INTEGER NO-UNDO. 
DEFINE VARIABLE lOrderWithoutMobSubProcessing AS LOGICAL NO-UNDO. 
DEFINE VARIABLE iBeginOrderId                 AS INTEGER NO-UNDO. 
DEFINE VARIABLE iEndOrderId                   AS INTEGER NO-UNDO.
DEFINE VARIABLE iWantedCountOrder             AS INTEGER NO-UNDO. 

lOrderWithoutMobsubProcessing = FALSE.
iBeginOrderId     = 1170000.
iEndOrderId       = 2000001.
iWantedCountOrder = 20000.

iBeginMsSeq        = 802418.
iEndMsSeq          = 802418.
iCountMobSub       = 0.
iWantedMobSubCount = 1.
cOutputDir = "/home/anttis/dynexp/".

lMustFindInvoice    = FALSE.
lMustFindInvRow     = FALSE.
lMustFindMnpProcess = FALSE.
lMustFindMnpMessage = FALSE.

lDeniedInvoice    = FALSE.
lDeniedInvRow     = FALSE.
lDeniedMnpProcess = FALSE.
lDeniedMnpMessage = FALSE. 

DEFINE STREAM sLog.
OUTPUT STREAM sLog TO VALUE("/home/anttis/dynexp/expmobsub_longdump.log").


/* End of the variables for records selection */

/* Check disallowed options */

/* Same required and denied */
IF lMustFindInvoice AND lDeniedInvoice THEN
   MESSAGE "Invoice cannot be required and denied".
IF lMustFindInvRow AND lDeniedInvRow THEN
   MESSAGE "InvRow cannot be required and denied".
IF lMustFindMnpProcess AND lDeniedMnpProcess THEN
   MESSAGE "MnpProcess cannot be required and denied".
IF lMustFindMnpMessage AND lDeniedMnpMessage THEN
   MESSAGE "MnpMessage cannot be required and denied".

/* Table dependency impossibility */
IF lMustFindInvRow AND lDeniedInvoice THEN
   MESSAGE "Cannot deny Invoice when requiring InvRow".
IF lMustFindMnpMessage AND lDeniedMnpProcess THEN
   MESSAGE "Cannot deny MnpProcess when requiring MnpMessage".


DEFINE STREAM sMobSub.
DEFINE STREAM sCustomer.
DEFINE STREAM sInvoice.
DEFINE STREAM sInvRow.
DEFINE STREAM sMnpProcess.
DEFINE STREAM sMnpMessage.


DEFINE STREAM sOrder.
DEFINE STREAM sOrderCust.
DEFINE STREAM sOrderTopup.
DEFINE STREAM sOrderAcc.
DEFINE STREAM sOrderPay.
DEFINE STREAM sOrderMemo.
DEFINE STREAM sOrderSIM.
DEFINE STREAM sOrdMSISDN.

DEFINE STREAM sPrepReq.
DEFINE STREAM sMsRequest.
DEFINE STREAM sDCCLI.
DEFINE STREAM sDCCounter.
DEFINE STREAM sMsOwner.
DEFINE STREAM sIMSI.

DEFINE VARIABLE iOrderId AS INTEGER NO-UNDO. 


DEFINE TEMP-TABLE ttKeyField
   FIELD cFieldName AS CHARACTER  
   FIELD cFieldValue AS CHARACTER 
   INDEX idxField IS PRIMARY UNIQUE cFieldName cFieldValue.


FUNCTION fKeyExists RETURN LOGICAL
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


DEFINE BUFFER bufSIM FOR SIM.

DEFINE VARIABLE lcMnpTime AS CHARACTER NO-UNDO. 
DEFINE VARIABLE classTS   AS timedate       NO-UNDO.

DEFINE BUFFER bufOrder  FOR Order.
DEFINE BUFFER bufOrder2 FOR Order.

FUNCTION fIsDextraCapable RETURN LOGICAL (INPUT piOrderId AS INTEGER):
   classTS = NEW timedate().

   FIND bufOrder WHERE bufOrder.Brand = "1" AND 
                       bufOrder.Orderid = piOrderId NO-LOCK
   NO-ERROR.
   IF AVAIL bufOrder THEN
   DO:
      FOR EACH SIM WHERE SIM.Brand = "1" AND SIM.SIMStat = 20,
         FIRST bufOrder2 NO-LOCK WHERE
               bufOrder2.MsSeq = SIM.MsSeq:

         FIND FIRST bufSIM WHERE
              RECID(bufSIM) = RECID(SIM)
         NO-LOCK NO-ERROR NO-WAIT.

         IF ERROR-STATUS:ERROR OR LOCKED(bufSIM) THEN RETURN FALSE.

         IF LOOKUP(STRING(bufOrder2.MNPStatus),"5,8") > 0 THEN RETURN FALSE.

         lcMNPTime = "".
         FIND FIRST MNPProcess WHERE 
            MNPProcess.OrderID = bufOrder2.OrderId AND
            (MNPProcess.StatusCode EQ 5 OR 
            MNPProcess.StatusCode EQ 6) NO-LOCK NO-ERROR.
         IF AVAIL MNPProcess THEN 
         DO:
            /* Is MNP Transfer time known ? */
            FIND FIRST MNPSub WHERE
               MNPSub.MNPSeq = MNPProcess.MNPSeq AND
               MNPSub.MsSeq  = bufOrder2.MsSeq NO-LOCK NO-ERROR.

            IF AVAIL MNPSub THEN 
            DO:
               IF MNPSub.PortingTime NE 0 THEN 
               DO:
                  ASSIGN
                     lcMNPTime = classTS:TS2HMS(MNPSub.PortingTime)
                     lcMNPTime = REPLACE(lcMNPTime," ","")
                     lcMNPTime = REPLACE(lcMNPTime,".","")
                     lcMNPTime = REPLACE(lcMNPTime,":","")
                     lcMNPTime = SUBSTR(lcMNPTime,5,4) + lcMNPTime.
                  SUBSTR(lcMNPTime,9,4) = "".
               END.
               ELSE RETURN FALSE.
            END.
         END.

         FIND FIRST Invoice WHERE
                    Invoice.InvNum = Order.InvNum
         NO-LOCK NO-ERROR.

         IF NOT AVAILABLE(Invoice) THEN RETURN FALSE.

         RETURN TRUE.
      END.
   END.
   RETURN FALSE.
END.


DEFINE VARIABLE cMobSubFileName         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCustomerFileName       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cInvoiceFilename        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cInvRowFileName         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cMnpProcessFileName     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cMnpMessageFileName     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderFilename          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderSIMFileName       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cPrepaidRequestFilename AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cMsRequestFileName      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderMSISDNFileName    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderCustomerFileName  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cDCCLIFileName          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cDCCounterFileName      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderTopupFileName     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderAccessoryFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderPaymentFileName   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderMemoFileName      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cMsOwnerFileName        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cIMSIFileName           AS CHARACTER NO-UNDO. 

DEFINE VARIABLE cMustIncludeInFileName  AS CHARACTER NO-UNDO. 
define variable cdeniedinfilename       as character no-undo. 

cMobSubFileName = 
   cOutputDir + "mobsubs" + STRING(iBeginMsSeq) + "_" 
   + STRING(iEndMsSeq) + "_" + STRING(iWantedMobSubCount).

IF lMustFindInvoice THEN
   cMustIncludeInFileName = "_Invoice".
IF lMustFindInvRow THEN
   cMustIncludeInFileName = cMustIncludeInFilename + "_InvRow".
IF lMustFindMnpProcess THEN
   cMustIncludeInFileName = cMustIncludeInFilename + "_MnpProcess".
IF lMustFindMnpMessage THEN
   cMustIncludeInFileName = cMustIncludeInFilename + "_MnpMessage".

IF lRequiredDCCLIValid THEN
   cMustIncludeInFileName = cMustIncludeInFileName + "_DCCLI_Valid".
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
IF lDeniedDCCLIValid THEN
   cDeniedInFileName = cDeniedInFileName + "_DCCLI_Valid".

IF cDeniedInFileName <> "" THEN 
   cDeniedInFileName = cDeniedInFilename + "_records_denied".


cMobSubFileName = cMobSubFileName + cMustIncludeInFileName + cDeniedInFileName.
IF iSepASCII > 0 THEN
   cMobSubFileName = cMobSubFileName + "_sep" + STRING(iSepAscii).
ELSE
   cMobSubFileName = cMobSubFileName + "_sep" + STRING(ASC('{&lcSep}')).

IF lUseHandle THEN
   cMobSubFileName = cMobSubFileName + "_use_handle".

IF lOrderWithoutMobSubProcessing THEN
   cMobSubFileName = cMobSubFileName + "_Order_Without_MobSub".

IF lMustBeDextraCapable AND lOrderWithoutMobSubProcessing THEN
   cMobSubFileName = cMobSubFileName + "_Dextra_capable".

DEFINE VARIABLE cPathDir            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iPathDir            AS INTEGER   NO-UNDO. 
DEFINE VARIABLE iNumPathEntries     AS INTEGER   NO-UNDO. 
DEFINE VARIABLE cMobSubFileNamePart AS CHARACTER NO-UNDO. 

iNumPathEntries = NUM-ENTRIES(cMobSubFileName, "/").

cPathDir = "".
REPEAT iPathDir = 1 TO iNumPathEntries - 1:
   cPathDir = cPathDir + ENTRY(iPathDir, cMobSubFileName, "/") + "/".
END.
cMobSubFileNamePart = ENTRY(iNumPathEntries, cMobSubFileName, "/").


cCustomerFileName = cPathDir + 
    REPLACE(cMobSubFileNamePart, "mobsubs", "customers").
cInvoiceFileName = cPathDir + 
    REPLACE(cMobSubFileNamePart, "mobsubs", "invoices").
cInvRowFileName = cPathDir + 
    REPLACE(cMobSubFileNamePart, "mobsubs", "invrows").
cMnpProcessFileName = cPathDir +
    REPLACE(cMobSubFileNamePart, "mobsubs", "mnpprocesses").
cMnpMessageFileName = cPathDir +
    REPLACE(cMobSubFileNamePart, "mobsubs", "mnpmessages").

cOrderFilename = cPathDir + 
    REPLACE(cMobSubFileNamePart, "mobsubs", "orders").
cOrderCustomerFileName = cPathDir + 
    REPLACE(cMobSubFileNamePart, "mobsubs", "ordercustomers").
cOrderAccessoryFileName = cPathDir + 
   REPLACE(cMobSubFileNamePart, "mobsubs", "orderacessories").
cOrderTopupFileName = cPathDir + 
    REPLACE(cMobSubFileNamePart, "mobsubs", "ordertopups").
cOrderPaymentFileName = cPathDir + 
    REPLACE(cMobSubFileNamePart, "mobsubs", "orderpayments").
cOrderMemoFileName = cPathDir + 
    REPLACE(cMobSubFileNamePart, "mobsubs", "ordermemos").
cOrderSIMFilename = cPathDir + 
   REPLACE(cMobSubFileNamePart, "mobsubs", "orderSIMs").
cOrderMSISDNFilename = cPathDir + 
   REPLACE(cMobSubFileNamePart, "mobsubs", "orderMSISDNs").

cPrepaidRequestFileName = cPathDir + 
   REPLACE(cMobSubFileNamePart, "mobsubs", "PrepaidRequests").
cMsRequestFileName = cPathDir + 
   REPLACE(cMobSubFileNamePart, "mobsubs", "MsRequests").
cDCCLIFileName = cPathDir + 
   REPLACE(cMobSubFileNamePart, "mobsubs", "DCCLI").
cDCCounterFileName = cPathDir + 
   REPLACE(cMobSubFileNamePart, "mobsubs", "DCCounter"). 
cMsOwnerFileName = cPathDir + 
   REPLACE(cMobSubFileNamePart, "mobsubs", "MSOwner").
cIMSIFileName = cPathDir + 
   REPLACE(cMobSubFileNamePart, "mobsubs", "IMSI").


IF lOrderWithoutMobSubProcessing THEN
   cMobSubFileName = cMobSubFileName + "_Order_Without_MobSub".

IF lMustBeDextraCapable AND lOrderWithoutMobSubProcessing THEN
   cMobSubFileName = cMobSubFileName + "_Dextra_capable".

iNumPathEntries = NUM-ENTRIES(cMobSubFileName, "/").

cPathDir = "".
IF iNumPathEntries > 0 THEN
DO:
   REPEAT iPathDir = 1 TO iNumPathEntries - 1:
      cPathDir = cPathDir + ENTRY(iPathDir, cMobSubFileName, "/") + "/".
   END.
   cMobSubFileNamePart = ENTRY(iNumPathEntries, cMobSubFileName, "/").
END.


OUTPUT STREAM sMobSub     TO VALUE(cMobSubFileName).
OUTPUT STREAM sCustomer   TO VALUE(cCustomerFileName).

OUTPUT STREAM sInvoice    TO VALUE(cInvoiceFileName).
OUTPUT STREAM sInvRow     TO VALUE(cInvRowFileName).

OUTPUT STREAM sMnpProcess TO VALUE(cMnpProcessFileName).
OUTPUT STREAM sMnpMessage TO VALUE(cMnpMessageFileName).


OUTPUT STREAM sOrder      TO VALUE(cOrderFilename).
OUTPUT STREAM sOrderCust  TO VALUE(cOrderCustomerFileName).
OUTPUT STREAM sOrderTopup TO VALUE(cOrderTopupFileName).
OUTPUT STREAM sOrderAcc   TO VALUE(cOrderAccessoryFilename).
OUTPUT STREAM sOrderPay   TO VALUE(cOrderPaymentFileName).

OUTPUT STREAM sOrderMemo  TO VALUE(cOrderMemoFileName).
OUTPUT STREAM sOrderSIM   TO VALUE(cOrderSIMFileName).
OUTPUT STREAM sOrdMSISDN  TO VALUE(cOrderMSISDNFileName).
OUTPUT STREAM sPrepReq    TO VALUE(cPrepaidRequestFileName).
OUTPUT STREAM sMsRequest  TO VALUE(cMsRequestFileName).
OUTPUT STREAM sDCCLI      TO VALUE(cDCCLIFileName).
OUTPUT STREAM sDCCounter  TO VALUE(cDCCounterFileName).
OUTPUT STREAM sMsOwner    TO VALUE(cMsOwnerFileName).
OUTPUT STREAM sIMSI       TO VALUE(cIMSIFileName).


DEFINE BUFFER xCustomer   FOR Customer.
DEFINE BUFFER xInvoice    FOR Invoice.
DEFINE BUFFER xInvRow     FOR InvRow.
DEFINE BUFFER xMnpProcess FOR MnpProcess.
DEFINE BUFFER xMnpMessage FOR MnpMessage.



FUNCTION fGetRecordData RETURN CHARACTER (
   INPUT        pcTableName AS CHARACTER,
   INPUT-OUTPUT phRecHandle AS HANDLE, 
   INPUT        piRowId     AS ROWID):

   DEFINE VARIABLE iNumFields  AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE hField      AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE iExtent     AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE cFieldName  AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cFieldValue AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE iField      AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE cRecData    AS CHARACTER NO-UNDO. 

   phRecHandle:FIND-BY-ROWID(piRowId).
   IF NOT ERROR-STATUS:ERROR THEN
   DO:
      iNumFields = phRecHandle:NUM-FIELDS.
      REPEAT iField = 1 to iNumFields:
        hField = phRecHandle:BUFFER-FIELD(iField).
        cFieldname = hField:NAME.
        IF hField:EXTENT > 1 THEN
        DO:
          REPEAT iExtent = 1 TO hField:EXTENT:
            cFieldValue = hField:BUFFER-VALUE(iExtent).
            cRecData = cRecData + CHR(iSepAscii) + cFieldName + 
               STRING(iExtent) + CHR(iSepAscii).
            IF cFieldValue = ? THEN
               cRecData = cRecData + "?".
            ELSE
               cRecData = cRecData + cFieldValue.
          END.
        END.
        ELSE
        DO:
            cFieldValue = hField:BUFFER-VALUE.
            cRecData = cRecData + CHR(iSepAscii) + cFieldName + CHR(iSepAscii).
            IF cFieldValue = ? THEN
               cRecData = cRecData + "?".
            ELSE
               cRecData = cRecData + cFieldValue.
        END.
      END.
   END.
   ELSE
      PUT STREAM sLog UNFORMATTED 
         "Unable to get buffer with table " + pcTableName SKIP.
   RETURN cRecData.
END.



FUNCTION fAddCustomers RETURN LOGICAL (INPUT piCustNum AS INTEGER):

  DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

  FOR EACH Customer WHERE Customer.Brand   = "1" AND
                          Customer.CustNum = piCustNum NO-LOCK:
      IF NOT fKeyExists("Customer.CustNum", STRING(Customer.CustNum)) THEN 
      DO:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER Customer:HANDLE.
            cRecData = fGetRecordData("Customer", INPUT-OUTPUT hTableHandle, 
                       ROWID(Customer)).
            PUT STREAm sCustomer UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sCustomer DELIMITER "{&lcSep}" Customer. 
      END.
  END.
END.


FUNCTION fAddInvRows RETURN LOGICAL (INPUT piInvNum AS INTEGER):
  DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

  FOR EACH InvRow WHERE InvRow.InvNum = piInvNum NO-LOCK:
     IF lUseHandle THEN
     DO:
        hTableHandle = BUFFER InvRow:HANDLE.
        cRecData = fGetRecordData("InvRow", INPUT-OUTPUT hTableHandle, 
           ROWID(InvRow)).
        PUT STREAM sInvRow UNFORMATTED cRecData SKIP.
     END.
     ELSE
         EXPORT STREAM sInvRow DELIMITER "{&lcSep}" InvRow.
  END.
END.

FUNCTION fAddInvoices RETURN LOGICAL (INPUT piCustNum AS INTEGER):
  DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

  FOR EACH Invoice WHERE Invoice.Brand = "1" AND
                         Invoice.CustNum = piCustNum NO-LOCK:
      IF NOT fKeyExists("Invoice.InvNum", STRING(Invoice.InvNum)) THEN 
      DO:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER Invoice:HANDLE.
            cRecData = fGetRecordData("Invoice", INPUT-OUTPUT hTableHandle, 
               ROWID(Invoice)).
            PUT STREAM sInvoice UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sInvoice DELIMITER "{&lcSep}" Invoice.
      END.
      fAddInvRows(Invoice.InvNum).
  END.
END.

FUNCTION fAddOrderInvoices RETURN LOGICAL (INPUT piInvNum AS INTEGER):
  DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

  FOR EACH Invoice WHERE Invoice.Brand = "1" AND
                         Invoice.InvNum = piInvNum NO-LOCK:
      IF NOT fKeyExists("Invoice.InvNum", STRING(Invoice.InvNum)) THEN 
      DO:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER Invoice:HANDLE.
            cRecData = fGetRecordData("Invoice", INPUT-OUTPUT hTableHandle,
               ROWID(Invoice)).
            PUT STREAM sInvoice UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sInvoice DELIMITER "{&lcSep}" Invoice.
      END.
      fAddInvRows(Invoice.InvNum).
  END.
END.


FUNCTION fAddMsRequests RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
  DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

  FOR EACH MsRequest WHERE MsRequest.MsSeq = piMsSeq NO-LOCK:
     IF NOT fKeyExists("MsRequest.MsRequest", STRING(MsRequest.MsRequest)) THEN
     DO:
        IF lUseHandle THEN
        DO:
           hTableHandle = BUFFER MsRequest:HANDLE.
           cRecData = fGetRecordData("MsRequest", INPUT-OUTPUT hTableHandle, 
              ROWID(MsRequest)).
           PUT STREAM sMsRequest UNFORMATTED cRecData SKIP.
        END.
        ELSE
           EXPORT STREAM sMsRequest DELIMITER "{&lcSep}" MsRequest.
     END.
  END.
END.

FUNCTION fAddDCCounter RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   FOR EACH DCCounter WHERE DCCounter.MsSeq = piMsSeq NO-LOCK:
      IF lUseHandle THEN
      DO:
         hTableHandle = BUFFER DCCounter:HANDLE.
         cRecData = fGetRecordData("DCCounter", INPUT-OUTPUT hTableHandle, 
            ROWID(DCCounter)).
         PUT STREAM sDCCounter UNFORMATTED cRecData SKIP.
      END.
      ELSE
         EXPORT STREAM sDCCounter DELIMITER "{&lcSep}" DCCounter.
   END.
END.

FUNCTION fAddDCCLI RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   FOR EACH DCCLI WHERE DCCLI.MsSeq = piMsSeq NO-LOCK:
      IF lUseHandle THEN
      DO:
         hTableHandle = BUFFER DCCLI:HANDLE.
         cRecData = fGetRecordData("DCCLI", INPUT-OUTPUT hTableHandle, 
            ROWID(DCCLI)).
         PUT STREAM sDCCLI UNFORMATTED cRecData SKIP.
      END.
      ELSE
         EXPORT STREAM sDCCLI DELIMITER "{&lcSep}" DCCLI. 
   END.
END.



FUNCTION fAddCustomerRecords RETURN LOGICAL (INPUT piCustNum AS INTEGER):
   fAddCustomers(piCustNum).
   fAddInvoices(piCustNum).

   RETURN TRUE.
END.

FUNCTION fCustomerRecordsExist RETURN LOGICAL (INPUT piCustNum AS INTEGER):
    FIND FIRST xCustomer WHERE xCustomer.Brand = "1" AND 
                             xCustomer.CustNum = piCustNum NO-LOCK NO-ERROR.
    IF NOT AVAIL xCustomer THEN RETURN FALSE.

    DEFINE VARIABLE lInvoiceFound AS LOGICAL NO-UNDO. 
    DEFINE VARIABLE lInvRowFound  AS LOGICAL NO-UNDO. 

    lInvoiceFound = FALSE.
    lInvRowFound = FALSE.
    LoopInvoices:
    FOR EACH xInvoice WHERE xInvoice.Brand = "1" AND 
                             xInvoice.CustNum = piCustNum NO-LOCK:

         lInvoiceFound = TRUE.
         IF NOT lMustFindInvRow AND NOT lDeniedInvRow THEN LEAVE LoopInvoices.
         FIND FIRST xInvRow WHERE xInvRow.InvNum = xInvoice.InvNum 
             NO-LOCK NO-ERROR.
         IF AVAIL xInvRow THEN
         DO:
            lInvRowFound = TRUE.
            LEAVE LoopInvoices.
         END.
    END.

    IF lMustFindInvoice AND NOT lInvoiceFound THEN RETURN FALSE.
    IF lMustFindInvRow  AND NOT lInvRowFound  THEN RETURN FALSE.
    IF lDeniedInvoice   AND     lInvoiceFound THEN RETURN FALSE.
    IF lDeniedInvRow    AND     lInvRowFound  THEN RETURN FALSE.

    FIND FIRST xMnpProcess WHERE xMnpProcess.OrderId = iOrderId 
               NO-LOCK NO-ERROR.

    IF NOT AVAIL xMnpProcess AND lMustFindMnpProcess THEN 
        RETURN FALSE.
    IF AVAIL xMnpProcess AND lDeniedMnpProcess THEN
        RETURN FALSE.


    IF AVAIL xMnpProcess AND (lMustFindMnpMessage OR lDeniedMnpMessage) THEN
    DO:
       DEFINE VARIABLE lMnpMessageFound AS LOGICAL NO-UNDO. 
       lMnpMessageFound = FALSE.
       LoopMnpProcess:
       FOR EACH xMnpProcess WHERE xMnpProcess.OrderId = iOrderId NO-LOCK.
          FIND FIRST MnpMessage WHERE MnpMessage.MnpSeq = xMnpProcess.MnpSeq 
              NO-LOCK NO-ERROR.
          IF AVAIL MnpMessage THEN 
          DO:
              lMnpMessageFound = TRUE.
              LEAVE LoopMnpProcess.
          END.
       END.
       IF lMustFindMnpMessage AND NOT lMnpMessageFound THEN RETURN FALSE.
       IF lDeniedMnpMessage   AND     lMnpMessageFound THEN RETURN FALSE.
    END.

    RETURN TRUE.
END.


FUNCTION fAddMnpProcesses RETURN LOGICAL:
    DEFINE VARIABLE cRecdata     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

    FOR EACH MnpProcess WHERE MnpProcess.OrderId = iOrderId NO-LOCK:
       fKeyExists("MnpProcess.MnpSeq", STRING(MnpProcess.MnpSeq)).
       IF lUseHandle THEN
       DO:
          hTableHandle = BUFFER MnpProcess:HANDLE.
          cRecData = fGetRecordData("MnpProcess", INPUT-OUTPUT hTableHandle, 
             ROWID(MnpProcess)).
          PUT STREAM sMnpProcess UNFORMATTED cRecData SKIP.
       END.
       ELSE
          EXPORT STREAM sMnpProcess DELIMITER "{&lcSep}" MnpProcess.
       
       FOR EACH MnpMessage WHERE MnpMessage.MnpSeq = MnpProcess.MnpSeq NO-LOCK:
          IF lUseHandle THEN
          DO:
             hTableHandle = BUFFER MnpMessage:HANDLE.
             cRecData = fGetRecordData("MnpMessage", INPUT-OUTPUT hTableHandle, 
                ROWID(MnpMessage)).
             PUT STREAM sMnpMessage UNFORMATTED cRecData SKIP.
          END.
          ELSE
              EXPORT STREAM sMnpMessage DELIMITER "{&lcSep}" MnpMessage.
       END.
    END.
END.


FUNCTION fAddPrepaidRequests RETURN LOGICAL:
   DEFINE VARIABLE cRecData AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE NO-UNDO. 
   FOR EACH PrepaidRequest WHERE PrepaidRequest.Brand = "1" 
      AND PrepaidRequest.MsSeq = MobSub.MsSeq NO-LOCK:

       fKeyExists("PrepaidRequest.PPRequest", 
          STRING(PrepaidRequest.PPRequest)).
       IF lUseHandle THEN
       DO:
           hTableHandle = BUFFER PrepaidRequest:HANDLE.
           cRecData = fGetRecordData("PrepaidRequest", 
              INPUT-OUTPUT hTableHandle, ROWID(PrepaidRequest)).
           PUT STREAM sPrepReq UNFORMATTED cRecData SKIP.
       END.
       ELSE
          EXPORT STREAM sPrepReq DELIMITER "{&lcSep}" PrepaidRequest.
   END.
END.


FUNCTION fAddIMSIRecords RETURN LOGICAL:
   DEFINE VARIABLE cRecDataIMSI     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandleIMSI AS HANDLE    NO-UNDO. 

   FOR EACH IMSI WHERE IMSI.ICC = SIM.ICC AND Mobsub.IMSI = IMSI.IMSI NO-LOCK:
      IF lUseHandle THEN
      DO:
         hTableHandleIMSI = BUFFER IMSI:HANDLE.
         cRecDataIMSI = fGetRecordData("IMSI", INPUT-OUTPUT hTableHandleIMSI, 
            ROWID(IMSI)).
         PUT STREAM sIMSI UNFORMATTED cRecDataIMSI SKIP.
      END.
      ELSE
         EXPORT STREAM sIMSI DELIMITER "{&lcSep}" IMSI.
   END.

   RETURN TRUE.
END.



FUNCTION fAddOrderRecords RETURN LOGICAL:
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   fKeyExists("Order.OrderId", STRING(Order.OrderId)).
   IF lUseHandle THEN
   DO:
      hTableHandle = BUFFER Order:HANDLE.
      cRecData = fGetRecordData("Order", INPUT-OUTPUT hTableHandle, 
         ROWID(Order)).
      PUT STREAM sOrder UNFORMATTED cRecData SKIP.
   END.
   ELSE
      EXPORT STREAM sOrder DELIMITER "{&lcSep}" Order.

   FOR EACH OrderCustomer WHERE OrderCustomer.Brand = "1" AND 
                                OrderCustomer.OrderId = iOrderId NO-LOCK:
      IF lUseHandle THEN
      DO:
          hTableHandle = BUFFER OrderCustomer:HANDLE.
          cRecData = fGetRecordData("OrderCustomer", 
            INPUT-OUTPUT hTableHandle, ROWID(OrderCustomer)).
          PUT STREAM sOrderCust UNFORMATTED cRecData SKIP.
      END.
      ELSE
         EXPORT STREAM sOrderCust DELIMITER "{&lcSep}" OrderCustomer.
   END.

   FOR EACH OrderTopup WHERE OrderTopup.Brand = "1" AND 
                             OrderTopup.OrderId = iOrderId NO-LOCK:
      IF lUseHandle THEN
      DO:
         hTableHandle = BUFFER OrderTopup:HANDLE.
         cRecData = fGetRecordData("OrderTopup", INPUT-OUTPUT hTableHandle, 
            ROWID(OrderTopup)).
         PUT STREAM sOrderTopup UNFORMATTED cRecData SKIP.
      END.
      ELSE
          EXPORT STREAM sOrderTopup DELIMITER "{&lcSep}" OrderTopup.
   END.

   FOR EACH OrderAccessory WHERE OrderAccessory.Brand = "1" AND 
                                 OrderAccessory.OrderId = iOrderId NO-LOCK:
      IF lUseHandle THEN
      DO:
         hTableHandle = BUFFER OrderAccessory:HANDLE.
         cRecData = fGetRecordData("OrderAccessory", 
            INPUT-OUTPUT hTableHandle, ROWID(OrderAccessory)).
         PUT STREAM sOrderAcc UNFORMATTED cRecData SKIP.
      END.
      ELSE
         EXPORT STREAM sOrderAcc DELIMITER "{&lcSep}" OrderAccessory.
   END.


   FOR EACH OrderPayment WHERE OrderPayment.Brand = "1" AND 
                               OrderPayment.OrderId = iOrderId NO-LOCK:
      IF lUseHandle THEN
      DO:
         hTableHandle = BUFFER OrderPayment:HANDLE.
         cRecData = fGetRecordData("OrderPayment", INPUT-OUTPUT hTableHandle, 
            ROWID(OrderPayment)).
         PUT STREAM sOrderPay UNFORMATTED cRecData SKIP.
      END.
      ELSE
          EXPORT STREAM sOrderPay DELIMITER "{&lcSep}" OrderPayment.
   END.

   FOR EACH Memo WHERE Memo.Brand = "1" AND Memo.HostTable = "Order" AND 
                       Memo.KeyValue = STRING(iOrderId) NO-LOCK:
      IF lUseHandle THEN
      DO:
         hTableHandle = BUFFER Memo:HANDLE.
         cRecData = fGetRecordData("Memo", INPUT-OUTPUT hTableHandle, 
            ROWID(Memo)).
         PUT STREAM sOrderMemo UNFORMATTED cRecData SKIP.
      END.
      ELSE
          EXPORT STREAM sOrderMemo DELIMITER "{&lcSep}" Memo.
   END.

   /* Order SIM */
   FOR EACH SIM WHERE SIM.Brand = "1" AND SIM.ICC = Order.ICC NO-LOCK:
      fKeyExists("SIM.ICC", SIM.ICC).
      IF lUseHandle THEN
      DO:
         hTableHandle = BUFFER SIM:HANDLE.
         cRecData = fGetRecordData("SIM", INPUT-OUTPUT hTableHandle, 
            ROWID(SIM)).
         PUT STREAM sOrderSIM UNFORMATTED cRecData SKIP.
      END.
      ELSE
         EXPORT STREAM sOrderSIM DELIMITER "{&lcSep}" SIM.
      fAddIMSIRecords().
   END.

   /* Subscription SIM */
   FOR EACH SIM WHERE SIM.Brand = "1" AND SIM.ICC = Mobsub.ICC NO-LOCK:
      fKeyExists("SIM.ICC", SIM.ICC).
      IF lUseHandle THEN
      DO:
         hTableHandle = BUFFER SIM:HANDLE.
         cRecData = fGetRecordData("SIM", INPUT-OUTPUT hTableHandle, 
            ROWID(SIM)).
         PUT STREAM sOrderSIM UNFORMATTED cRecData SKIP.
      END.
      ELSE
         EXPORT STREAM sOrderSIM DELIMITER "{&lcSep}" SIM.
      fAddIMSIRecords().
   END.

   /* Order MSISDN */
   FOR EACH MSISDN WHERE MSISDN.Brand = "1" AND MSISDN.CLI = Order.CLI 
      NO-LOCK:
      fKeyExists("MSISDN.CLI", MSISDN.CLI).

      IF lUseHandle THEN
      DO:
         hTableHandle = BUFFER MSISDN:HANDLE.
         cRecData = fGetRecordData("MSISDN", INPUT-OUTPUT hTableHandle, 
            ROWID(MSISDN)).
         PUT STREAM sOrdMSISDN UNFORMATTED cRecData SKIP.
      END.
      ELSE
          EXPORT STREAM sOrdMSISDN DELIMITER "{&lcSep}" MSISDN.
   END.

   /* Subscription MSISDN */
   FOR EACH MSISDN WHERE MSISDN.Brand = "1" AND MSISDN.CLI = MobSub.CLI 
      NO-LOCK:
      fKeyExists("MSISDN.CLI", MSISDN.CLI).

      IF lUseHandle THEN
      DO:
         hTableHandle = BUFFER MSISDN:HANDLE.
         cRecData = fGetRecordData("MSISDN", INPUT-OUTPUT hTableHandle, 
            ROWID(MSISDN)).
         PUT STREAM sOrdMSISDN UNFORMATTED cRecData SKIP.
      END.
      ELSE
          EXPORT STREAM sOrdMSISDN DELIMITER "{&lcSep}" MSISDN.
   END.
END.

FUNCTION fAddMsOwners RETURN LOGICAL:
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   FOR EACH MsOwner WHERE MsOwner.MsSeq = Mobsub.MsSeq NO-LOCK:

      IF lUseHandle THEN
      DO:
         hTableHandle = BUFFER MsOwner:HANDLE.
         cRecData = fGetRecordData("MsOwner", INPUT-OUTPUT hTableHandle, 
            ROWID(MsOwner)).
         PUT STREAM sMsOwner UNFORMATTED cRecData SKIP.
      END.
      ELSE
         EXPORT STREAM sMsOwner DELIMITER "{&lcSep}" MsOwner.

  END.
  RETURN TRUE.
END.

FUNCTION fGetOnlyOrderRelatedRecords RETURN LOGICAL 
   (INPUT plNoMobSubOrders AS LOGICAL):

   iCountOrder = 0.
   GatherOrders:
   FOR EACH Order WHERE Order.Brand = "1" AND Order.OrderId >= iBeginOrderId
                AND Order.OrderId <= iEndOrderId NO-LOCK:
       
       FIND FIRST MobSub WHERE MobSub.MsSeq = Order.MsSeq NO-LOCK
                  NO-ERROR.
       IF (NOT AVAIL Mobsub AND plNoMobSubOrders) OR NOT plNoMobSubOrders THEN
       DO:
          IF lMustBeDextraCapable AND 
              NOT fIsDextraCapable(Order.OrderId) 
          THEN NEXT GatherOrders.

          fAddOrderRecords().
          fAddMnpProcesses().
          fAddOrderInvoices(Order.InvNum).
          fAddCustomerRecords(Order.CustNum).
          iCountOrder = iCountOrder + 1.

          IF iCountOrder >= iWantedCountOrder THEN
             LEAVE GatherOrders.
          IF iCountOrder MOD iDispInterval = 0 THEN
             DISP iCountOrder.
       END.
   END.
   RETURN TRUE.
END.

DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 


IF lOrderWithoutMobsubProcessing OR lMustBeDextraCapable THEN
DO:
   fGetOnlyOrderRelatedRecords(lOrderWithoutMobsubProcessing).
END.
ELSE
DO:
   GatherMobSubs:
   FOR EACH MobSub where MobSub.MsSeq >= iBeginMsSeq AND 
                         MobSub.MsSeq <= iEndMsSeq   NO-LOCK:
        FIND FIRST Order WHERE Order.MsSeq = MobSub.MsSeq 
                   USE-INDEX MsSeq NO-LOCK NO-ERROR.
        IF NOT AVAIL Order THEN NEXT.

        iOrderId = Order.OrderId.
        IF iOrderId = 0 THEN NEXT.

        IF NOT fCustomerRecordsExist(MobSub.CustNum) THEN NEXT.
        IF NOT fCustomerRecordsExist(MobSub.AgrCust) THEN NEXT.
        IF NOT fCustomerRecordsExist(MobSub.InvCust) THEN NEXT.
        IF lRequiredDCCLIValid OR lDeniedDCCLIValid THEN
        DO:
           DEFINE VARIABLE lDCCLIFound AS LOGICAL NO-UNDO. 
           lDCCLIFound = FALSE.
           DCCLILoop:
           FOR EACH DCCLI WHERE DCCLI.MsSeq = MobSub.MsSeq NO-LOCK:
              IF DCCLI.ValidTo >= TODAY THEN
              DO:
                 lDCCLIFound = TRUE.
                 LEAVE DCCLILoop.
              END.
           END.
           IF NOT lDCCLIFound AND lRequiredDCCLIValid THEN NEXT GatherMobSubs.
           IF     lDCCLIFound AND lDeniedDCCLIValid   THEN NEXT GatherMobSubs.
        END.
        
        fKeyExists("MobSub.MsSeq", STRING(MobSub.MsSeq)).

        IF lUseHandle THEN
        DO:
             hTableHandle = BUFFER MobSub:HANDLE.
             cRecData = fGetRecordData("MobSub", INPUT-OUTPUT hTableHandle, 
                ROWID(MobSub)).
             PUT STREAM sMobSub UNFORMATTED cRecData SKIP.
        END.
        ELSE
            EXPORT STREAM sMobSub DELIMITER "{&lcSep}" MobSub. 

        fAddMsOwners().
        fAddCustomerRecords(MobSub.CustNum).
        fAddCustomerRecords(MobSub.AgrCust).
        fAddCustomerRecords(MobSub.InvCust).

        fAddMnpProcesses().
        fAddOrderRecords().
        fAddMsRequests(MobSub.MsSeq).
        fAddDCCLI(MobSub.MsSeq).
        fAddDCCounter(MobSub.MsSeq).

        iCountMobSub = iCountMobSub + 1.
        IF iCountMobSub >= iWantedMobSubCount THEN
           LEAVE GatherMobsubs.
        IF lDisplay THEN
        IF iCountMobSub MOD iDispInterval = 0 THEN
            DISP iCountMobSub.
   END.
END.

OUTPUT STREAM sMobSub     CLOSE.
OUTPUT STREAM sCustomer   CLOSE.
OUTPUT STREAM sInvoice    CLOSE.
OUTPUT STREAM sInvRow     CLOSE.
OUTPUT STREAM sMnpProcess CLOSE.
OUTPUT STREAM sMnpMessage CLOSE.

OUTPUT STREAM sOrder      CLOSE.
OUTPUT STREAM sOrderCust  CLOSE.
OUTPUT STREAM sOrderTopup CLOSE.
OUTPUT STREAM sOrderAcc   CLOSE.
OUTPUT STREAM sOrderPay   CLOSE.
OUTPUT STREAM sOrderMemo  CLOSE.
OUTPUT STREAM sOrderSIM   CLOSE.
OUTPUT STREAM sOrdMSISDN  CLOSE.
OUTPUT STREAM sPrepReq    CLOSE.
OUTPUT STREAM sMsOwner    CLOSE.
OUTPUT STREAM sIMSI       CLOSE.

DEFINE STREAM sIdent.
DEFINE VARIABLE cIdentFile AS CHARACTER NO-UNDO.

cIdentFile = cPathDir + 
   REPLACE(cMobSubFileNamePart, "mobsubs", "indentifiers").

OUTPUT STREAM sIdent TO VALUE(cIdentFile).
  FOR EACH ttKeyField BREAK BY cFieldName:
     IF FIRST-OF(ttKeyField.cFieldName) THEN
     DO:
         PUT STREAM sIdent UNFORMATTED SKIP(2) ttKeyField.cFieldName SKIP.
     END.
     PUT STREAM sIdent UNFORMATTED ttKeyField.cFieldValue SKIP.
  END.
OUTPUT STREAM sIdent CLOSE.
