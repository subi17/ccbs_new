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

DEFINE VARIABLE lSkipExisting AS LOGICAL NO-UNDO. 
lSkipExisting = FALSE.

/* Set the following run parameters the same as with exporting
   to form correct filenames for the imported files, expect
   the cInputDir to the directory where export files are located */

iBeginMsSeq = 976200.


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

DEFINE VARIABLE cLogFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cSkipReplLogFileName AS CHARACTER NO-UNDO. 



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

cCustomerFileName = REPLACE(cMobSubFileName, "mobsubs", "customers").
cInvoiceFileName = REPLACE(cMobSubFileName, "mobsubs", "invoices").
cInvRowFileName = REPLACE(cMobSubFileName, "mobsubs", "invrows").
cMnpProcessFileName = REPLACE(cMobSubFileName, "mobsubs", "mnpprocesses").
cMnpMessageFileName = REPLACE(cMobSubFileName, "mobsubs", "mnpmessages").

cOrderFilename = REPLACE(cMobSubFileName, "mobsubs", "orders").
cOrderCustomerFileName = REPLACE(cMobSubFileName, "mobsubs", "ordercustomers").
cOrderAccessoryFileName = REPLACE(cMobSubFileName, "mobsubs", "orderacessories").
cOrderTopupFileName = REPLACE(cMobSubFileName, "mobsubs", "ordertopups").
cOrderPaymentFileName = REPLACE(cMobSubFileName, "mobsubs", "orderpayments").
cOrderMemoFileName = REPLACE(cMobSubFileName, "mobsubs", "ordermemos").
cOrderSIMFilename = REPLACE(cMobSubFileName, "mobsubs", "orderSIMs").
cOrderMSISDNFilename = REPLACE(cMobSubFileName, "mobsubs", "orderMSISDNs").
cLogFileName = REPLACE(cMobSubFileName, "mobsubs", "log").
cSkipReplLogFileName = REPLACE(cMobSubFileName, "mobsubs", "skip_replace").

cPrepaidRequestFileName = REPLACE(cMobSubFileName, "mobsubs", "PrepaidRequests").


IF SEARCH(cMobSubFilename) = ? THEN
   MESSAGE "MobSub file " cMobSubFileName " not found".
IF SEARCH(cCustomerFilename) = ? THEN
   MESSAGE "Customer file " cCustomerFileName " not found".
IF SEARCH(cInvoiceFilename) = ? THEN
   MESSAGE "Invoice file " cInvoiceFileName " not found".
IF SEARCH(cInvRowFilename) = ? THEN
   MESSAGE "Invrow file " cInvRowFileName " not found".
IF SEARCH(cMnpProcessFilename) = ? THEN
   MESSAGE "MnpProcess file " cMnpProcessFileName " not found".
IF SEARCH(cMnpMessageFilename) = ? THEN
   MESSAGE "MnpMessage file " cMnpMessageFileName " not found".

IF SEARCH(cOrderFilename) = ? THEN
   MESSAGE "Order file " cOrderFileName " not found".
IF SEARCH(cOrderCustomerFilename) = ? THEN
   MESSAGE "OrderCustomer file " cOrderCustomerFileName " not found".
IF SEARCH(cOrderAccessoryFilename) = ? THEN
   MESSAGE "OrderAccessory file " cOrderAccessoryFileName " not found".
IF SEARCH(cOrderTopupFilename) = ? THEN
   MESSAGE "OrderTopup file " cOrderTopupFileName " not found".
IF SEARCH(cOrderPaymentFilename) = ? THEN
   MESSAGE "OrderPayment file " cOrderPaymentFileName " not found".
IF SEARCH(cOrderMemoFilename) = ? THEN
   MESSAGE "Order Memo file " cOrderMemoFileName " not found".
IF SEARCH(cOrderSIMFilename) = ? THEN
   MESSAGE "Order SIM file " cOrderSIMFileName " not found".
IF SEARCH(cOrderMSISDNFilename) = ? THEN
   MESSAGE "Order MSISDN file " cOrderMSISDNFileName " not found".
IF SEARCH(cPrepaidRequestFileName) = ? THEN
   MESSAGE "PrepaidRequest file " cPrepaidRequestFileName " not found". 


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


DEFINE STREAM sLog.
DEFINE STREAM sSkipRepl.

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

OUTPUT STREAM sLog TO VALUE(cLogFileName).
OUTPUT STREAm sSkipRepl TO VALUE(cSkipReplLogFileName).

DEFINE BUFFER xMobSub FOR MobSub.
/* Mobsubs */
INPUT STREAM sMobSub FROM VALUE(cMobSubFileName).
LoopMobSubs:
REPEAT:
   CREATE MobSub.
   IMPORT STREAM sMobSub DELIMITER "|" MobSub NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
   DO:
      fLogErrors().
      UNDO LoopMobsubs.
   END.
   FIND xMobSub WHERE xMobSub.MsSeq = MobSub.MsSeq EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL xMobSub THEN
   DO:
      IF NOT lSkipExisting THEN
      DO:
         fLog("Replace MobSub with MsSeq " + STRING(MobSub.MsSeq)). 
         DELETE xMobSub. 
      END.
      ELSE
      DO:
        fLog("Skipping existing MobSub with MsSeq " + STRING(MobSub.MsSeq)). 
        DELETE MobSub.
      END.
   END.
END.
INPUT STREAM sMobSub CLOSE.

OUTPUT STREAM sLog CLOSE.

OUTPUT STREAM sSkipRepl CLOSE.
