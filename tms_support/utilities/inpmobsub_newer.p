DEFINE VARIABLE lImportMobsub AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lImportCustomer AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lImportInvoice AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lImportInvRow AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lImportMnpProcess AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lImportMnpMessage AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lImportOrder  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lImportOrderCustomer  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lImportOrderTopup  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lImportOrderAccessory  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lImportOrderPayment  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lImportOrderMemo  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lImportOrderSIM  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lImportOrderMSISDN  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lImportPrepaidRequest  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lImportMsRequest AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lImportDCCLI AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lImportDCCounter AS LOGICAL NO-UNDO. 

DEFINE VARIABLE lcIdentifiersNotAccFileList AS CHARACTER NO-UNDO. 
lcIdentifiersNotAccFileList = "".



/* Set options, which tables of the possible ones to import */

/* Common default setting */
lImportMobsub         = TRUE. 
lImportCustomer       = lImportMobSub.
lImportInvoice        = lImportMobSub.
lImportInvRow         = lImportMobSub.
lImportMnpProcess     = lImportMobSub.
lImportMnpMessage     = lImportMobSub.
lImportOrder          = lImportMobSub.
lImportOrderCustomer  = lImportMobSub.
lImportOrderTopup     = lImportMobSub.
lImportOrderAccessory = lImportMobSub.
lImportOrderPayment   = lImportMobSub.
lImportOrderMemo      = lImportMobSub.
lImportOrderSIM       = lImportMobSub.
lImportOrderMSISDN    = lImportMobSub.
lImportPrepaidRequest = lImportMobSub.


/* Options for skipping records table spesifically
   that exist both in the database and in the file. 
   Value TRUE means no change in database, value FALSE means
   overwriting the database record */
DEFINE VARIABLE lSkipExistingMobsub AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lSkipExistingCustomer AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lSkipExistingInvoice AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lSkipExistingInvRow AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lSkipExistingMnpProcess AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lSkipExistingMnpMessage AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lSkipExistingOrder AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSkipExistingOrderCustomer AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSkipExistingOrderTopup AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lSkipExistingOrderAcc  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSkipExistingOrderPay  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSkipExistingOrderMemo  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSkipExistingOrderSIM  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSkipExistingOrderMSISDN  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSkipExistingPrepaidRequest  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSkipExistingMsRequest AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lSkipExistingDCCLI AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lSkipExistingDCCounter AS LOGICAL NO-UNDO. 

lSkipExistingMobsub         = FALSE.
lSkipExistingCustomer       = lSkipExistingMobsub.
lSkipExistingInvoice        = lSkipExistingMobsub.
lSkipExistingInvRow         = lSkipExistingMobsub.
lSkipExistingMnpProcess     = lSkipExistingMobsub.
lSkipExistingMnpMessage     = lSkipExistingMobsub.
lSkipExistingOrder          = lSkipExistingMobsub.
lSkipExistingOrderCustomer  = lSkipExistingMobsub.
lSkipExistingOrderTopup     = lSkipExistingMobsub.
lSkipExistingOrderAcc       = lSkipExistingMobsub.
lSkipExistingOrderPay       = lSkipExistingMobsub.
lSkipExistingOrderMemo      = lSkipExistingMobsub.
lSkipExistingOrderSIM       = lSkipExistingMobsub.
lSkipExistingOrderMSISDN    = lSkipExistingMobsub.
lSkipExistingPrepaidRequest = lSkipExistingMobSub. 
lSkipExistingMsRequest      = lSkipExistingMobSub.
lSkipExistingDCCLI          = lSkipExistingMobSub.
lSkipExistingDCCounter      = lSkipExistingMobSub.
/* You may choose some of these to act differently */

/* Here you can set some table to act differently by requiring
   or demanding records */

/* Table by table hierachical handling for big tables 
   starting from mobsub */

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


/* Set the following run parameters the same as with exporting
   to form correct filenames for the imported files, expect
   the cInputDir to the directory where export files are located */

iBeginMsSeq = 1170000.

/* Set these also to the same as with export */
iEndMsSeq   = 2000001.
iCountMobSub = 0.
iWantedMobSubCount = 20000.

/* The directory where export files exist */
cInputDir = "/home/harrim/renewaldata/".

/* The directory for the log files */

DEFINE VARIABLE cLogDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cLogFileBase AS CHARACTER NO-UNDO. 

cLogDir = "/home/harrim/renewaldata/log1170000v2/".

/* Required and denied options for 4 tables: Invoice, InvRow, MnpProcess,
   MnpMessage; set all to the same as with the export */
lMustFindInvoice = FALSE.
lMustFindInvRow = FALSE.
lMustFindMnpProcess = FALSE.
lMustFindMnpMessage = FALSE.

lDeniedInvoice    = FALSE.
lDeniedInvRow     = FALSE.
lDeniedMnpProcess = FALSE.
lDeniedMnpMessage = FALSE. 
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


/* Create log about completely new records */
DEFINE VARIABLE lGenerateCreateNewLog AS LOGICAL NO-UNDO. 
lGenerateCreateNewLog = TRUE.


DEFINE VARIABLE cMobSubFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCustomerFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cInvoiceFilename AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cInvRowFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cMnpProcessFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cMnpMessageFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderFilename AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderCustomerFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderTopupFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderAccessoryFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderPaymentFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderMemoFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderSIMFilename AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrderMSISDNFilename AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cPrepaidRequestFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cMsRequestFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cDCCLIFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cDCCounterFileName AS CHARACTER NO-UNDO. 

DEFINE VARIABLE cLogFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cSkipReplLogFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCrLogFileName AS CHARACTER NO-UNDO. 

/* Form the name base for imported files based on 
   the export options */
cMobSubFileName = 
   cInputDir + "mobsubs" + STRING(iBeginMsSeq) + "_" 
   + STRING(iEndMsSeq) + "_" + STRING(iWantedMobSubCount).

cLogFileBase = 
   cLogDir + "crl" + STRING(iBeginMsSeq) + "_" 
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

/* Final imported filename base for all imported files*/
cMobSubFileName = cMobSubFileName + 
                  cMustIncludeInFileName + cDeniedInFileName.
cLogFileName = cLogFileBase + 
               cMustIncludeInFileName + cDeniedInFileName.

/* Names of files based on msseq, count and 
   required/denied options to be imported, based on the filename based
   on cMobSubFileName. */

cCustomerFileName = REPLACE(cMobSubFileName, "mobsubs", "customers").
cInvoiceFileName = REPLACE(cMobSubFileName, "mobsubs", "invoices").
cInvRowFileName = REPLACE(cMobSubFileName, "mobsubs", "invrows").
cMnpProcessFileName = REPLACE(cMobSubFileName, "mobsubs", "mnpprocesses").
cMnpMessageFileName = REPLACE(cMobSubFileName, "mobsubs", "mnpmessages").

cOrderFilename = REPLACE(cMobSubFileName, "mobsubs", "orders").
cOrderCustomerFileName = REPLACE(cMobSubFileName, "mobsubs", "ordercustomers").
cOrderAccessoryFileName = 
   REPLACE(cMobSubFileName, "mobsubs", "orderacessories").
cOrderTopupFileName = REPLACE(cMobSubFileName, "mobsubs", "ordertopups").
cOrderPaymentFileName = REPLACE(cMobSubFileName, "mobsubs", "orderpayments").
cOrderMemoFileName = REPLACE(cMobSubFileName, "mobsubs", "ordermemos").
cOrderSIMFilename = REPLACE(cMobSubFileName, "mobsubs", "orderSIMs").
cOrderMSISDNFilename = REPLACE(cMobSubFileName, "mobsubs", "orderMSISDNs").
cPrepaidRequestFileName = 
   REPLACE(cMobSubFileName, "mobsubs", "PrepaidRequests").
cMsRequestFileName = 
   REPLACE(cMsRequestFileName, "mobsubs", "MsRequests").

/* Generated log file names */
cSkipReplLogFileName = REPLACE(cLogFileName, "crl", "log_skip_replace").
cCrLogFileName = REPLACE(cLogFileName, "crl", "log_created_new").

/* Check about required file existince */
DEFINE VARIABLE lNotFoundFile AS LOGICAL NO-UNDO. 
lNotFoundFile = FALSE.

IF lImportMobSub AND SEARCH(cMobSubFilename) = ? THEN
DO:
   MESSAGE "MobSub file " cMobSubFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportCustomer AND SEARCH(cCustomerFilename) = ? THEN
DO:
   MESSAGE "Customer file " cCustomerFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportInvoice AND SEARCH(cInvoiceFilename) = ? THEN
DO:
   MESSAGE "Invoice file " cInvoiceFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportInvRow AND SEARCH(cInvRowFilename) = ? THEN
DO:
   MESSAGE "Invrow file " cInvRowFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportMnpProcess AND SEARCH(cMnpProcessFilename) = ? THEN
DO:
   MESSAGE "MnpProcess file " cMnpProcessFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportMnpMessage AND SEARCH(cMnpMessageFilename) = ? THEN
DO:
   MESSAGE "MnpMessage file " cMnpMessageFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportOrder AND SEARCH(cOrderFilename) = ? THEN
DO:
   MESSAGE "Order file " cOrderFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportOrderCustomer AND SEARCH(cOrderCustomerFilename) = ? THEN
DO:
   MESSAGE "OrderCustomer file " cOrderCustomerFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportOrderAccessory AND SEARCH(cOrderAccessoryFilename) = ? THEN
DO:
   MESSAGE "OrderAccessory file " cOrderAccessoryFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportOrderTopup AND SEARCH(cOrderTopupFilename) = ? THEN
DO:
   MESSAGE "OrderTopup file " cOrderTopupFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportOrderPayment AND SEARCH(cOrderPaymentFilename) = ? THEN
DO:
   MESSAGE "OrderPayment file " cOrderPaymentFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportOrderMemo AND SEARCH(cOrderMemoFilename) = ? THEN
DO:
   MESSAGE "Order Memo file " cOrderMemoFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportOrderSIM AND SEARCH(cOrderSIMFilename) = ? THEN
DO:
   MESSAGE "Order SIM file " cOrderSIMFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportOrderMSISDN AND SEARCH(cOrderMSISDNFilename) = ? THEN
DO:
   MESSAGE "Order MSISDN file " cOrderMSISDNFileName " not found".
   lNotFoundFile = TRUE.
END.
IF lImportPrepaidRequest AND SEARCH(cPrepaidRequestFileName) = ? THEN
DO:
   MESSAGE "PrepaidRequest file " cPrepaidRequestFileName " not found". 
   lNotFoundFile = TRUE.
END.
IF lImportMsRequest AND SEARCH(cMsRequestFileName) = ? THEN
DO:
   MESSAGE "MsRequest file " cMsRequestFileName " not found.".
   lNotFoundFile = TRUE.
END.
IF lImportDCCLI AND SEARCH(cDCCLIFileName) = ? THEN
DO:
   MESSAGE "DCCLI file " cDCCLIFileName " not found.".
   lNotFoundFile = TRUE.
END.
IF lImportDCCounter AND SEARCH(cDCCounterFileName) = ? THEN
DO:
   MESSAGE "DCCounter file " cDCCounterFileName " not found.".
   lNotFoundFile = TRUE.
END.

DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO. 
lContinue = TRUE.


DEFINE TEMP-TABLE ttKeyField
   FIELD cFieldName AS CHARACTER  
   FIELD cFieldValue AS CHARACTER 
   INDEX idxField IS PRIMARY UNIQUE cFieldName cFieldValue.

DEFINE STREAM sIds.

DEFINE VARIABLE cLine AS CHARACTER NO-UNDO. 


FUNCTION fReadid RETURN LOGICAL:
   IMPORT STREAM sIds UNFORMATTED cLine.
   cLine = TRIM(cLine).
   IF cLine eq "" THEN RETURN FALSE.
   RETURN TRUE.
END.

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

FUNCTION fGetIdsFromFile RETURN LOGICAL (
   INPUT pcIdentifierFileName AS CHARACTER):
   INPUT STREAM sIds FROM VALUE(pcIdentifierFileName). 
   REPEAT:
      IMPORT STREAM sIds UNFORMATTED cLine.
      cLine = TRIM(cLine).
      IF cLine eq "" THEN NEXT.
      fAddKeyFields(cLine).
   END.
   INPUT STREAM sIds CLOSE.
END.

DEFINE VARIABLE cDelimIdFiles AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iCountIdFiles AS INTEGER NO-UNDO. 
DEFINE VARIABLE iIdFile AS INTEGER NO-UNDO. 

cDelimIdFiles = "|".
iCountIdFiles = NUM-ENTRIES(lcIdentifiersNotAccFileList, cDelimIdFiles).

REPEAT iIdFile = 1 TO iCountIdFiles:
   fGetIdsFromFile(ENTRY(iIdFile, lcIdentifiersNotAccFileList, cDelimIdFiles)).
END.

IF lNotFoundFile THEN
   MESSAGE "Some import files did not exist. Do you wish to continue ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 
   "Check the file situation" UPDATE lContinue.

IF lContinue THEN
   RUN pDoImport.
ELSE 
   MESSAGE "Import canceled." VIEW-AS ALERT-BOX.


/* Streams for subscription, invoice or mnp related 
   related tables */
DEFINE STREAM sMobSub.
DEFINE STREAM sCustomer.
DEFINE STREAM sInvoice.
DEFINE STREAM sInvRow.
DEFINE STREAM sMnpProcess.
DEFINE STREAM sMnpMessage.
DEFINE STREAM sMsRequest.
DEFINE STREAM sDCCLI.
DEFINE STREAM sDCCounter.

/* Streams for order related tables */
DEFINE STREAM sOrder.
DEFINE STREAM sOrderCust.
DEFINE STREAM sOrderTopup.
DEFINE STREAM sOrderAcc.
DEFINE STREAM sOrderPay.
DEFINE STREAM sOrderMemo.
DEFINE STREAM sOrderSIM.
DEFINE STREAM sOrdMSISDN.

/* streams for other tables */
DEFINE STREAM sPrepReq.

/* Define log streams */
DEFINE STREAM sLog.
DEFINE STREAM sSkipRepl.
DEFINE STREAM sCrLog.

/* Temp tables to enable the skip/replace actions,
   not directly acting on the database */
DEFINE TEMP-TABLE ttMobSub LIKE MobSub. 
DEFINE TEMP-TABLE ttDCCLI LIKE DCCLI.
DEFINE TEMP-TABLE ttDCCounter LIKE DCCounter.
DEFINE TEMP-TABLE ttMsRequest LIKE MsRequest.
DEFINE TEMP-TABLE ttCustomer LIKE Customer. 
DEFINE TEMP-TABLE ttInvoice LIKE Invoice. 
DEFINE TEMP-TABLE ttInvRow LIKE InvRow.
DEFINE TEMP-TABLE ttMnpProcess LIKE MnpProcess.
DEFINE TEMP-TABLE ttMnpMessage LIKE MnpMessage.
DEFINE TEMP-TABLE ttOrder LIKE Order.
DEFINE TEMP-TABLE ttOrderCustomer LIKE OrderCustomer. 
DEFINE TEMP-TABLE ttOrderTopup LIKE OrderTopup.
DEFINE TEMP-TABLE ttOrderAccessory LIKE OrderAccessory.
DEFINE TEMP-TABLE ttOrderPayment LIKE OrderPayment.
DEFINE TEMP-TABLE ttMemo LIKE Memo.
DEFINE TEMP-TABLE ttSIM LIKE SIM.
DEFINE TEMP-TABLE ttMSISDN LIKE MSISDN.
DEFINE TEMP-TABLE ttPrepaidRequest LIKE PrepaidRequest.


/* Function to create entries to log files;
   simplifies the locations in the real import logic. */
FUNCTION fLogErrors RETURN LOGICAL:
   DEFINE VARIABLE iError AS INTEGER NO-UNDO. 
   DO iError = 1 TO ERROR-STATUS:NUM-MESSAGES:
      PUT STREAM sLog UNFORMATTED 
         ERROR-STATUS:GET-MESSAGE(iError) SKIP.
   END.
END.

FUNCTION fLog RETURN LOGICAL (INPUT pcMsg AS CHARACTER):
   PUT STREAM sSkipRepl UNFORMATTED pcMsg SKIP.
END.

FUNCTION fCrLog RETURN LOGICAL (INPUT pcMsg AS CHARACTER):
   IF lGenerateCreateNewLog THEN 
      PUT STREAM sCrLog UNFORMATTED pcMsg SKIP.
END.



FUNCTION fIsZeroLength RETURN LOGICAL (INPUT pcFileName AS CHARACTER):
   FILE-INFO:FILE-NAME = pcFileName.
   IF FILE-INFO:FILE-SIZE = 0 THEN RETURN TRUE.
   RETURN FALSE.
END.


FUNCTION fDisplayPhase RETURN LOGICAL (INPUT pcText AS CHARACTER):
   DISP pcText FORMAT "X(40)".
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


PROCEDURE pDoImport:

   /* Open log files */
   OUTPUT STREAM sLog TO VALUE(cLogFileName).
   OUTPUT STREAm sSkipRepl TO VALUE(cSkipReplLogFileName).
   IF lGenerateCreateNewLog THEN
      OUTPUT STREAM sCrLog TO VALUE(cCrLogFileName).

   /* 
      Code sections for each table handled 
   */

   /* Mobsubs */
   IF lImportMobSub AND NOT fIsZeroLength(cMobSubFileName) THEN
   DO:
      fDisplayPhase("Importing mobsubs").
      INPUT STREAM sMobSub FROM VALUE(cMobSubFileName).
      LoopMobSubs:
      REPEAT:
         CREATE ttMobSub.
         IMPORT STREAM sMobSub DELIMITER "|" ttMobSub NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopMobsubs.
         END.

         IF NOT fKeyExists("MobSub.MsSeq", STRING(ttMobSub.MsSeq)) THEN
         DO:
            FIND MobSub WHERE MobSub.MsSeq = ttMobSub.MsSeq 
                 EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL MobSub THEN
            DO:
               IF NOT lSkipExistingMobSub THEN
               DO:
                  fLog("Replace MobSub with MsSeq " + STRING(MobSub.MsSeq)). 
                  DELETE MobSub. 

                  CREATE MobSub.
                  BUFFER-COPY ttMobSub TO MobSub.
               END.
               ELSE
               DO:
                 fLog("Skipping existing MobSub with MsSeq " + 
                      STRING(MobSub.MsSeq)). 
               END.
            END.
            ELSE
            DO:
               DEFINE BUFFER xMobSub FOR Mobsub.
               IF (NOT CAN-FIND(xMobSub WHERE xMobSub.Brand = "1" AND
                                            xMobSub.CLI = ttMobSub.CLI)) 
                  AND
                  
                  (NOT CAN-FIND(xMobSub WHERE (xMobSub.CLI = ttMobSub.CLI))) 
                  
                  AND
                  
                  (NOT CAN-FIND(xMobSub WHERE (MobSub.Brand = "1") AND
                                             (MobSUb.CustNum = 
                                             ttMobSub.CustNum) AND
                                             (xMobSub.BillTarget = 
                                             ttMobSub.BillTarget) AND
                                             (xMobSub.CLI = ttMobSub.CLI))) 
                  
                  AND
                  
                  (NOT CAN-FIND(xMobSub WHERE (xMobSub.CustNum = 
                                               ttMobSub.CustNum)
                                         AND (xMobSub.BillTarget = 
                                             ttMobSub.BillTarget)
                                         AND (MobSub.CLI = ttMobSub.CLI)))
               THEN
               DO:
                  CREATE xMobSub.
                  BUFFER-COPY ttMobSub TO xMobSub.
                  fCrLog("Created new MobSub with MsSeq " + 
                          STRING(xMobSub.MsSeq)).
               END.
               ELSE
               DO:
                  fCrLog("Mobsub with MsSeq " + STRING(ttMobSub.MsSeq) +
                         " and CLI = " + STRING(ttMobSub.CLI) +
                         " could not be created because of unique " +
                         " index clash; probably because changes in " + 
                         " target database after last import.").
               END.
            END.
         END.
         EMPTY TEMP-TABLE ttMobSub.
      END.
      INPUT STREAM sMobSub CLOSE.
   END.

   /* DCCLI */
   IF lImportDCCLI AND NOT fIsZeroLength(cDCCLIFileName) THEN
   DO:
      fDisplayPhase("Importing DCCLI").
      INPUT STREAM sDCCLI FROM VALUE(cDCCLIFileName).
      LoopDCCLI:
      REPEAT:
         CREATE ttDCCLI.
         IMPORT STREAM sDCCLI DELIMITER "|" ttDCCLI NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopDCCLI.
         END.


         IF NOT fKeyExists("MobSub.MsSeq", STRING(MobSub.MsSeq)) THEN
         DO:
            FIND DCCLI WHERE DCCLI.MsSeq = ttDCCLI.MsSeq AND 
                             DCCLI.ValidTo = ttDCCLi.ValidTo 
                             EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL DCCLI THEN
            DO:
               IF NOT lSkipExistingDCCLI THEN
               DO:
                  fLog("Replace DCCLI with DCCLI.MsSeq " + 
                     STRING(DCCLI.MsSeq) + " and ValidTo " + 
                     STRING(DCCLI.ValidTo)).
                  DELETE DCCLI. 
                  CREATE DCCLI.
                  BUFFER-COPY ttDCCLI TO DCCLI.
               END.
               ELSE
               DO:
                  fLog("Skipping existing DCCLI with DCCLI.MsSeq " + 
                     STRING(DCCLI.MsSeq) + 
                     " and ValidTo " + STRING(DCCLI.ValidTo)).
               END.
            END.
            ELSE
            DO:
               CREATE DCCLI.
               BUFFER-COPY ttDCCLI TO DCCLI.
               fCrLog("Created new DCCLI with MsSeq " + 
                  STRING(DCCLI.MsSeq) + " and ValidTo " + STRING(DCCLI.ValidTo)).
            END.
         END.
         EMPTY TEMP-TABLE ttDCCLI.
      END.
      INPUT STREAM sDCCLI CLOSE.
   END.


   /* DCCounter */
   IF lImportDCCounter AND NOT fIsZeroLength(cDCCounterFileName) THEN
   DO:
      fDisplayPhase("Importing DCCounter").
      INPUT STREAM sDCCounter FROM VALUE(cDCCounterFileName).
      LoopDCCounter:
      REPEAT:
         CREATE ttDCCounter.
         IMPORT STREAM sDCCounter DELIMITER "|" ttDCCounter NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopDCCounter.
         END.

         IF NOT fKeyExists("MobSub.MsSeq", STRING(ttDCCounter.MsSeq)) THEN
         DO:
            FIND DCCounter WHERE DCCounter.MsSeq = ttDCCounter.MsSeq AND 
                             DCCounter.DCDate = ttDCCounter.DCDate AND
                             DCCounter.DCTarget = ttDCCounter.DCTarget AND
                             DCCounter.DCEvent = ttDCCounter.DCEvent AND
                             DCCounter.BillCode = ttDCCounter.BillCode 
                             EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL DCCounter THEN
            DO:
               IF NOT lSkipExistingDCCounter THEN
               DO:
                  fLog("Replace DCCounter with DCCounter.MsSeq " + 
                     STRING(DCCounter.MsSeq) + " DCDate " + 
                     STRING(DCCounter.DCDate) + 
                     " DCTarget " + STRING(DCCounter.DCTarget) + " DCEvent " + 
                     STRING(DCCounter.DCEvent) + " BillCode " + 
                     STRING(DCCounter.BillCode)).
                  DELETE DCCounter. 
                  CREATE DCCounter.
                  BUFFER-COPY ttDCCounter TO DCCounter.
               END.
               ELSE
               DO:
                  fLog("Skipping existing DCCounter with DCCounter.MsSeq " + 
                     STRING(DCCounter.MsSeq) + " DCDate " + 
                     STRING(DCCounter.DCDate) + 
                     " DCTarget " + STRING(DCCounter.DCTarget) + " DCEvent " + 
                     STRING(DCCounter.DCEvent) + " BillCode " + 
                     STRING(DCCounter.BillCode)).
               END.
            END.
            ELSE
            DO:
               CREATE DCCounter.
               BUFFER-COPY ttDCCounter TO DCCounter.
               fCrLog("Created new DCCounter with MsSeq " + 
                  STRING(DCCounter.MsSeq) + " DCDate " + 
                  STRING(DCCounter.DCDate) + 
                  " DCTarget " + STRING(DCCounter.DCTarget) + " DCEvent " + 
                  STRING(DCCounter.DCEvent) + " BillCode " + 
                  STRING(DCCounter.BillCode)).
            END.
         END.
         EMPTY TEMP-TABLE ttDCCounter.
      END.
      INPUT STREAM sDCCounter CLOSE.
   END.

   /* MsRequest */
   IF lImportMsRequest AND NOT fIsZeroLength(cMsRequestFileName) THEN
   DO:
      fDisplayPhase("Importing MsRequest").
      INPUT STREAM sMsRequest FROM VALUE(cMsRequestFileName).
      LoopMsRequest:
      REPEAT:
         CREATE ttMsRequest.
         IMPORT STREAM sMsRequest DELIMITER "|" ttMsRequest NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopMsRequest.
         END.
         FIND MsRequest WHERE MsRequest.MsRequest = 
                              ttMsRequest.MsRequest EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL MsRequest THEN
         DO:
            IF NOT lSkipExistingMsRequest THEN
            DO:
               fLog("Replace MsRequest with MsRequest " + 
                  STRING(MsRequest.MsRequest)). 
               DELETE MsRequest. 
               CREATE MsRequest.
               BUFFER-COPY ttMsRequest TO MsRequest.
            END.
            ELSE
            DO:
              fLog("Skipping existing MsRequest with MsSeq " + 
                    STRING(MsRequest.MsRequest)). 
            END.
         END.
         ELSE
         DO:
            CREATE MsRequest.
            BUFFER-COPY ttMsRequest TO MsRequest.
            fCrLog("Created new MsRequest with MsRequest " + 
               STRING(MsRequest.MsRequest)).
         END.
         EMPTY TEMP-TABLE ttMsRequest.
      END.
      INPUT STREAM sMsRequest CLOSE.
   END.

   /* Customers */
   IF lImportCustomer AND NOT fIsZeroLength(cCustomerFileName) THEN
   DO:
      fDisplayPhase("Importing Customer").
      INPUT STREAM sCustomer FROM VALUE(cCustomerFileName).
      LoopCustomers:
      REPEAT:
         CREATE ttCustomer.
         IMPORT STREAM sCustomer DELIMITER "|" ttCustomer NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopCustomers.
         END.

         IF NOT fKeyExists("Customer.CustNum", STRING(Customer.CustNum)) THEN
         DO:

            FIND Customer WHERE Customer.CustNum = 
                                ttCustomer.CustNum EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL Customer THEN
            DO:
               IF NOT lSkipExistingCustomer THEN
               DO:
                  fLog("Replace Customer with CustNum " + 
                       STRING(Customer.CustNum)). 
                  DELETE Customer. 
                  CREATE Customer.
                  BUFFER-COPY ttCustomer TO Customer.
               END.
               ELSE
               DO:
                 fLog("Skipping existing Customer with CustNum " + 
                      STRING(Customer.CustNum)). 
               END.
            END.
            ELSE
            DO:
               CREATE Customer.
               BUFFER-COPY ttCustomer TO Customer.
               fCrLog("Created new Customer with CustNum " + 
                      STRING(Customer.CustNum)).
            END.
            END.
         EMPTY TEMP-TABLE ttCustomer.
      END.
      INPUT STREAM sCustomer CLOSE.
   END.

   /* Invoices */
   IF lImportInvoice AND NOT fIsZeroLength(cInvoiceFileName) THEN
   DO:
      fDisplayPhase("Importing Invoice").
      INPUT STREAM sInvoice FROM VALUE(cInvoiceFileName).
      LoopInvoices:
      REPEAT:
         CREATE ttInvoice.
         IMPORT STREAM sInvoice DELIMITER "|" ttInvoice NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopInvoices.
         END.

         IF NOT fKeyExists("Invoice.InvNum", STRING(Invoice.InvNum)) THEN
         DO:
            FIND Invoice WHERE Invoice.InvNum = 
                                ttInvoice.InvNum EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL Invoice THEN
            DO:
               IF NOT lSkipExistingInvoice THEN
               DO:
                  fLog("Replace Invoice with InvNum " + 
                       STRING(Invoice.InvNum)). 
                  DELETE Invoice. 
                  CREATE Invoice.
                  BUFFER-COPY ttInvoice TO Invoice.
               END.
               ELSE
               DO:
                 fLog("Skipping existing Invoice with InvNum " + 
                      STRING(Invoice.InvNum)). 
               END.
            END.
            ELSE
            DO:
               CREATE Invoice.
               BUFFER-COPY ttInvoice TO Invoice.
               fCrLog("Created new Invoice with InvNum " + 
                      STRING(Invoice.InvNum)).
            END.
         END.
         EMPTY TEMP-TABLE ttInvoice.
      END.
      INPUT STREAM sInvoice CLOSE.
   END.

   /* InvRows */
   IF lImportInvRow AND NOT fIsZeroLength(cInvRowFileName) THEN
   DO:
      fDisplayPhase("Importing InvRow").
      INPUT STREAM sInvRow FROM VALUE(cInvRowFileName).
      LoopInvRows:
      REPEAT:
         CREATE ttInvRow.
         IMPORT STREAM sInvRow DELIMITER "|" ttInvRow NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopInvRows.
         END.

         IF NOT fKeyExists("Invoice.InvNum", STRING(ttInvRow.InvNum)) THEN
         DO:
            FIND FIRST InvRow WHERE InvRow.InvNum = ttInvRow.InvNum AND
                                    InvRow.InvRowNum = ttInvRow.InvRowNum 
                                    EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL InvRow THEN
            DO:
               IF NOT lSkipExistingInvRow THEN
               DO:
                  fLog("Replace InvRow with InvNum " + 
                       STRING(InvRow.InvNum) + 
                       " and InvRowNum = " + STRING(InvRow.InvRowNum)).
                  DELETE InvRow. 
                  CREATE InvRow.
                  BUFFER-COPY ttInvRow TO InvRow.
               END.
               ELSE
               DO:
                 fLog("Skipping existing InvRow with InvNum " + 
                      STRING(Invoice.InvNum) + 
                      " and InvRowNum = " + STRING(InvRow.InvRowNum)).
               END.
            END.
            ELSE
            DO:
               CREATE InvRow.
               BUFFER-COPY ttInvRow TO InvRow.
               fCrLog("Created new InvRow with InvNum " + 
                      STRING(InvRow.InvNum) + 
                      " and InvRowNum = " + STRING(InvRow.InvRowNum)).
            END.
         END.
         EMPTY TEMP-TABLE ttInvRow.
      END.
      INPUT STREAM sInvRow CLOSE.
   END.

   /* MnpProcess */
   IF lImportMnpProcess AND NOT fIsZeroLength(cMnpProcessFileName) THEN
   DO:
      fDisplayPhase("Importing MnpProcess").
      INPUT STREAM sMnpProcess FROM VALUE(cMnpProcessFileName).
      LoopMnpProcess:
      REPEAT:
         CREATE ttMnpProcess.
         IMPORT STREAM sMnpProcess DELIMITER "|" ttMnpProcess NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopMnpProcess.
         END.

         IF NOT fKeyExists("MnpProcess.MnpSeq", STRING(MnpProcess.MnpSeq))
         THEN
         DO:
            FIND FIRST MnpProcess WHERE MnpProcess.MnpSeq = ttMnpProcess.MnpSeq
                                    EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL MnpProcess THEN
            DO:
               IF NOT lSkipExistingMnpProcess THEN
               DO:
                  fLog("Replace MnpProcess with MnpSeq " + 
                       STRING(MnpProcess.MnpSeq)).
                  DELETE MnpProcess. 
                  CREATE MnpProcess.
                  BUFFER-COPY ttMnpProcess TO MnpProcess.
               END.
               ELSE
               DO:
                 fLog("Skipping existing MnpProcess with MnpSeq " + 
                      STRING(MnpProcess.MnpSeq)). 
               END.
            END.
            ELSE
            DO:
               CREATE MnpProcess.
               BUFFER-COPY ttMnpProcess TO MnpProcess.
               fCrLog("Created new MnpProcess with MnpSeq " + 
                       STRING(MnpProcess.MnpSeq)).
            END.
         END.
         EMPTY TEMP-TABLE ttMnpProcess.
      END.
      INPUT STREAM sMnpProcess CLOSE.
   END.

   /* MnpMessage */
   IF lImportMnpMessage AND NOT fIsZeroLength(cMnpMessageFileName) THEN
   DO:
      fDisplayPhase("Importing MnpMessage").
      INPUT STREAM sMnpMessage FROM VALUE(cMnpMessageFileName).
      LoopMnpMessage:
      REPEAT:
         CREATE ttMnpMessage.
         IMPORT STREAM sMnpMessage DELIMITER "|" ttMnpMessage NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopMnpMessage.
         END.

         IF NOT fKeyExists("MnpProcess.MnpSeq", STRING(ttMnpMessage.MnpSeq)) 
         THEN
         DO:
            FIND FIRST MnpMessage WHERE MnpMessage.MnpSeq = ttMnpMessage.MnpSeq
                 AND MnpMessage.CreatedTs = ttMnpMessage.Createdts
                 EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL MnpMessage THEN
            DO:
               IF NOT lSkipExistingMnpMessage THEN
               DO:
                  fLog("Replace MnpMessage with MnpSeq " + 
                       STRING(MnpMessage.MnpSeq) + " and CreatedTs = " + 
                       STRING(MnpMessage.CreatedTs)).

                  DELETE MnpMessage. 
                  CREATE MnpMessage.
                  BUFFER-COPY ttMnpMessage TO MnpMessage.
               END.
               ELSE
               DO:
                 fLog("Skipping existing MnpMessage with MnpSeq " + 
                      STRING(MnpMessage.MnpSeq) + " and CreatedTs = " + 
                      STRING(MnpMessage.CreatedTs)).
               END.
            END.
            ELSE
            DO:
               CREATE MnpMessage.
               BUFFER-COPY ttMnpMessage TO MnpMessage.
               fCrLog("Created new MnpMessage with MnpSeq " + 
                       STRING(MnpMessage.MnpSeq) + " and CreatedTs = " + 
                      STRING(MnpMessage.CreatedTs)).
            END.
         END.
         EMPTY TEMP-TABLE ttMnpMessage.
      END.
      INPUT STREAM sMnpMessage CLOSE.
   END.

   /* Order */
   IF lImportOrder AND NOT fIsZeroLength(cOrderFileName) THEN
   DO:
      fDisplayPhase("Importing Order").
      INPUT STREAM sOrder FROM VALUE(cOrderFilename).
      LoopOrder:
      REPEAT:
         CREATE ttOrder.
         IMPORT STREAM sOrder DELIMITER "|" ttOrder NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopOrder.
         END.

         IF NOT fKeyExists("Order.OrderId", STRING(ttOrder.OrderId)) THEN
         DO:
            FIND FIRST Order WHERE Order.OrderId = ttOrder.OrderId
                 EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL Order THEN
            DO:
               IF NOT lSkipExistingOrder THEN
               DO:
                  fLog("Replace Order with OrderId " + 
                       STRING(Order.OrderId)).

                  DELETE Order. 
                  CREATE Order.
                  BUFFER-COPY ttOrder TO Order.
               END.
               ELSE
               DO:
                 fLog("Skipping existing Order with OrderId " + 
                      STRING(Order.OrderId)).
               END.
            END.
            ELSE
            DO:
               CREATE Order.
               BUFFER-COPY ttOrder TO Order.
               fCrLog("Created new Order with OrderId " + 
                       STRING(Order.OrderId)).
            END.
         END.
         EMPTY TEMP-TABLE ttOrder.
      END.
      INPUT STREAM sOrder CLOSE.
   END.

   /* OrderCustomer */
   IF lImportOrderCustomer AND NOT fIsZeroLength(cOrderCustomerFileName) THEN
   DO:
      fDisplayPhase("Importing OrderCustomer").
      INPUT STREAM sOrderCust FROM VALUE(cOrderCustomerFileName).
      LoopOrderCustomer:
      REPEAT:
         CREATE ttOrderCustomer.
         IMPORT STREAM sOrderCust DELIMITER "|" ttOrderCustomer NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopOrderCustomer.
         END.

         IF NOT fKeyExists("Order.OrderId", 
                STRING(ttOrderCustomer.OrderId)) THEN
         DO:

         FIND FIRST OrderCustomer WHERE 
             OrderCustomer.Brand = "1" AND
             OrderCustomer.OrderId = ttOrderCustomer.OrderId AND
             OrderCustomer.RowType = ttOrderCustomer.RowType 
              EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL OrderCustomer THEN
         DO:
            IF NOT lSkipExistingOrderCustomer THEN
            DO:
               fLog("Replace OrderCustomer with OrderId " + 
                    STRING(OrderCustomer.OrderId) + " and RowType = " 
                    + STRING(OrderCustomer.RowType)).
               DELETE OrderCustomer. 
               CREATE OrderCustomer.
               BUFFER-COPY ttOrderCustomer TO OrderCustomer.
            END.
            ELSE
            DO:
              fLog("Skipping existing OrderCustomer with OrderId " + 
                   STRING(OrderCustomer.OrderId) + 
                   " and RowType = " + STRING(OrderCustomer.RowType)).
            END.
         END.
         ELSE
         DO:
            CREATE OrderCustomer.
            BUFFER-COPY ttOrderCustomer TO OrderCustomer.
            fCrLog("Created new OrderCustomer with OrderId " + 
                    STRING(Order.OrderId)  + 
                   " and RowType = " + STRING(OrderCustomer.RowType)).
         END.
         END.
         EMPTY TEMP-TABLE ttOrderCustomer.
      END.
      INPUT STREAM sOrderCust CLOSE.
   END.

   /* OrderTopup */
   IF lImportOrderTopup AND NOT fIsZeroLength(cOrderTopupFileName) THEN
   DO:
      fDisplayPhase("Importing OrderTopup").
      INPUT STREAM sOrderTopup FROM VALUE(cOrderTopupFileName).
      LoopOrderTopup:
      REPEAT:
         CREATE ttOrderTopup.
         IMPORT STREAM sOrderTopup DELIMITER "|" ttOrderTopup NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopOrderTopup.
         END.

         IF NOT fKeyExists("Order.OrderId", 
                 STRING(ttOrderTopup.OrderId)) THEN
         DO:
            FIND FIRST OrderTopup WHERE 
                OrderTopup.Brand = "1" AND
                OrderTopup.OrderId = ttOrderTopup.OrderId 
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL OrderTopup THEN
            DO:
               IF NOT lSkipExistingOrderTopup THEN
               DO:
                  fLog("Replace OrderTopup with OrderId " + 
                       STRING(OrderTopup.OrderId)).
                  DELETE OrderTopup. 
                  CREATE OrderTopup.
                  BUFFER-COPY ttOrderTopup TO OrderTopup.
               END.
               ELSE
               DO:
                 fLog("Skipping existing OrderTopup with OrderId " + 
                      STRING(OrderTopup.OrderId)). 
               END.
            END.
            ELSE
            DO:
               CREATE OrderTopup.
               BUFFER-COPY ttOrderTopup TO OrderTopup.
               fCrLog("Created new OrderTopup with OrderId " + 
                       STRING(Order.OrderId)). 
            END.
         END.
         EMPTY TEMP-TABLE ttOrderTopup.
      END.
      INPUT STREAM sOrderTopup CLOSE.
   END.

   /* OrderAccessory */
   IF lImportOrderAccessory AND NOT fIsZeroLength(cOrderAccessoryFileName) THEN
   DO:
      fDisplayPhase("Importing OrderAccessory").
      INPUT STREAM sOrderAcc FROM VALUE(cOrderAccessoryFilename).
      LoopOrderAccessory:
      REPEAT:
         CREATE ttOrderAccessory.
         IMPORT STREAM sOrderAcc DELIMITER "|" ttOrderAccessory NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopOrderAccessory.
         END.


         IF NOT fKeyExists("Order.OrderId", 
                STRING(ttOrderAccessory.OrderId)) THEN
         DO:

            FIND FIRST OrderAccessory WHERE 
                OrderAccessory.Brand = "1" AND
                OrderAccessory.OrderId = ttOrderAccessory.OrderId 
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL OrderAccessory THEN
            DO:
               IF NOT lSkipExistingOrderAcc THEN
               DO:
                  fLog("Replace OrderAccessory with OrderId " + 
                       STRING(OrderAccessory.OrderId)).
                  DELETE OrderAccessory. 
                  CREATE OrderAccessory.
                  BUFFER-COPY ttOrderAccessory TO OrderAccessory.
               END.
               ELSE
               DO:
                 fLog("Skipping existing OrderAccessory with OrderId " + 
                      STRING(OrderAccessory.OrderId)). 
               END.
            END.
            ELSE
            DO:
               CREATE OrderAccessory.
               BUFFER-COPY ttOrderAccessory TO OrderAccessory.

               fCrLog("Created new OrderAccessory with OrderId " + 
                       STRING(OrderAccessory.OrderId)). 
            END.
         END.
         EMPTY TEMP-TABLE ttOrderAccessory.
      END.
      INPUT STREAM sOrderAcc CLOSE.
   END.

   /* OrderPayment */
   IF lImportOrderPayment AND NOT fIsZeroLength(cOrderPaymentFileName) THEN
   DO:
      fDisplayPhase("Importing OrderPayment").
      INPUT STREAM sOrderPay FROM VALUE(cOrderPaymentFileName).
      LoopOrderPayment:
      REPEAT:
         CREATE ttOrderPayment.
         IMPORT STREAM sOrderPay DELIMITER "|" ttOrderPayment NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopOrderPayment.
         END.

         IF NOT fKeyExists("Order.OrderId", 
                STRING(ttOrderPayment.OrderId)) THEN
         DO:
            FIND FIRST OrderPayment WHERE 
                OrderPayment.Brand = "1" AND
                OrderPayment.OrderId = ttOrderPayment.OrderId 
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL OrderPayment THEN
            DO:
               IF NOT lSkipExistingOrderPay THEN
               DO:
                  fLog("Replace OrderPayment with OrderId " + 
                       STRING(OrderPayment.OrderId)).
                  DELETE OrderPayment. 
                  CREATE OrderPayment.
                  BUFFER-COPY ttOrderPayment TO OrderPayment.
               END.
               ELSE
               DO:
                 fLog("Skipping existing OrderPayment with OrderId " + 
                      STRING(OrderPayment.OrderId)). 
               END.
            END.
            ELSE
            DO:
               CREATE OrderPayment.
               BUFFER-COPY ttOrderPayment TO OrderPayment.
               fCrLog("Created new OrderPayment with OrderId " + 
                       STRING(OrderPayment.OrderId)). 
            END.
         END.
         EMPTY TEMP-TABLE ttOrderPayment.
      END.
      INPUT STREAM sOrderPay CLOSE.
   END.

   /* Order Memo */
   IF lImportOrderMemo AND NOT fIsZeroLength(cOrderMemoFileName) THEN
   DO:
      fDisplayPhase("Importing Memo for order").
      INPUT STREAM sOrderMemo FROM VALUE(cOrderMemoFileName).
      LoopOrderMemo:
      REPEAT:
         CREATE ttMemo.
         IMPORT STREAM sOrderMemo DELIMITER "|" ttMemo NO-ERROR.
         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopOrderMemo.
         END.

         IF NOT fKeyExists("Memo.MemoSeq", STRING(ttMemo.MemoSeq)) THEN
         DO:
            FIND FIRST Memo WHERE 
                Memo.MemoSeq = ttMemo.MemoSeq 
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL Memo  THEN
            DO:
               IF NOT lSkipExistingOrderMemo THEN
               DO:
                  fLog("Replace Memo with MemoSeq " + 
                       STRING(Memo.MemoSeq)).

                  DELETE Memo. 
                  CREATE Memo.
                  BUFFER-COPY ttMemo TO Memo.
               END.
               ELSE
               DO:
                 fLog("Skipping existing Memo with MemoSeq " + 
                      STRING(Memo.MemoSeq)). 
               END.
            END.
            ELSE
            DO:
               CREATE Memo.
               BUFFER-COPY ttMemo TO Memo.
               fCrLog("Created new Memo for Order with MemoSeq " + 
                       STRING(Memo.MemoSeq)). 
            END.
         END.
         EMPTY TEMP-TABLE ttMemo.
      END.
      INPUT STREAM sOrderMemo CLOSE.
   END.

   /* Order SIM */
   IF lImportOrderSIM AND NOT fIsZeroLength(cOrderSIMFileName) THEN
   DO:
      fDisplayPhase("Importing SIM for Order").
      INPUT STREAM sOrderSIM FROM VALUE(cOrderSIMFileName).
      LoopOrderSIM:
      REPEAT:
         CREATE ttSIM.
         IMPORT STREAM sOrderSIM DELIMITER "|" ttSIM NO-ERROR.

         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopOrderSIM.
         END.

         IF NOT fKeyExists("SIM.ICC", ttSIM.ICC) THEN
         DO:
            FIND FIRST SIM WHERE 
                SIM.ICC = ttSIM.ICC 
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL SIM  THEN
            DO:
               IF NOT lSkipExistingOrderSIM THEN
               DO:
                  fLog("Replace SIM with ICC " + SIM.ICC).

                  DELETE SIM. 
                  CREATE SIM.
                  BUFFER-COPY ttSIM TO SIM.
               END.
               ELSE
               DO:
                 fLog("Skipping existing SIM with ICC " + SIM.ICC).
               END.
            END.
            ELSE
            DO:
               CREATE SIM.
               BUFFER-COPY ttSIM TO SIM.
               fCrLog("Created new SIM for Order with ICC " + 
                       STRING(SIM.ICC)). 
            END.
         END.
         EMPTY TEMP-TABLE ttSIM.
      END.
      OUTPUT STREAM sOrderSIM CLOSE.
   END.

   /* Order MSISDN */
   IF lImportOrderMSISDN AND NOT fIsZeroLength(cOrderMSISDNFileName) THEN
   DO:
      fDisplayPhase("Importing MSISDN for Order").
      INPUT STREAM sOrdMSISDN FROM VALUE(cOrderMSISDNFileName).
      LoopOrderMSISDN:
      REPEAT:
         CREATE ttMSISDN.
         IMPORT STREAM sOrderSIM DELIMITER "|" ttMSISDN NO-ERROR.

         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopOrderMSISDN.
         END.

         IF NOT fKeyExists("MSISDN.CLI", ttMSISDN.CLI) THEN
         DO:
            FIND FIRST MSISDN WHERE 
                MSISDN.Brand = "1" AND 
                MSISDN.CLI = ttMSISDN.CLI AND
                MSISDN.ValidFrom = ttMSISDN.ValidFrom 
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL MSISDN  THEN
            DO:
               IF NOT lSkipExistingOrderMSISDN THEN
               DO:
                  fLog("Replace MSISDN with CLI " + MSISDN.CLI +
                       " and ValidFrom = " + STRING(MSISDN.ValidFrom)).
                  DELETE MSISDN. 
                  CREATE MSISDN.
                  BUFFER-COPY ttMSISDN TO MSISDN.
               END.
               ELSE
               DO:
                 fLog("Skipping existing MSISDN with CLI " + MSISDN.CLI +  
                      " and ValidFrom = " + STRING(MSISDN.ValidFrom)).
               END.
            END.
            ELSE
            DO:
               CREATE MSISDN.
               BUFFER-COPY ttMSISDN TO MSISDN.
               fCrLog("Created new MSISDN with CLI " + 
                      MSISDN.CLI +
                      " and ValidFrom = " + STRING(MSISDN.ValidFrom)).
            END.
         END.
         EMPTY TEMP-TABLE ttMSISDN.
      END.
      INPUT STREAM sOrdMSISDN CLOSE.
   END.

   /* Prepaid requests */
   IF lImportPrepaidRequest AND NOT fIsZeroLength(cPrepaidRequestFileName) THEN
   DO:
      fDisplayPhase("Importing PrepaidRequest").
      INPUT STREAM sPrepReq FROM VALUE(cPrepaidRequestFileName).
      LoopPrepaidRequest:
      REPEAT:
         CREATE ttPrepaidRequest.
         IMPORT STREAM sPrepReq DELIMITER "|" ttPrepaidRequest NO-ERROR.

         IF ERROR-STATUS:ERROR THEN
         DO:
            fLogErrors().
            UNDO LoopPrepaidRequest.
         END.

         FIND FIRST PrepaidRequest WHERE 
             PrepaidRequest.Brand = "1" AND 
             PrepaidRequest.PPRequest = ttPrepaidRequest.PPRequest
             EXCLUSIVE-LOCK NO-ERROR.
         IF AVAIL PrepaidRequest  THEN
         DO:
            IF NOT lSkipExistingPrepaidRequest THEN
            DO:
               fLog("Replace PrepaidRequest with PPRequest " + 
                    STRING(PrepaidRequest.PPRequest)).
               DELETE PrepaidRequest. 
               CREATE PrepaidRequest.
               BUFFER-COPY ttPrepaidRequest TO PrepaidRequest.
            END.
            ELSE
            DO:
              fLog("Skipping existing PrepaidRequest with PPRequest " + 
                    STRING(PrepaidRequest.PPRequest)).  
            END.
         END.
         ELSE
         DO:
            CREATE PrepaidRequest.
            BUFFER-COPY ttPrepaidRequest TO PrepaidRequest.
            fCrLog("Created new PrepaidRequest with PPRequest " + 
                   STRING(PrepaidRequest.PPRequest)).
         END.
         EMPTY TEMP-TABLE ttPrepaidRequest.
      END.
      INPUT STREAM sPrepReq CLOSE.
   END.

   /* Close log files */
   OUTPUT STREAM sLog CLOSE.
   OUTPUT STREAM sSkipRepl CLOSE.
   IF lGenerateCreateNewLog THEN
      OUTPUT STREAM sCrLog CLOSE.

END.
