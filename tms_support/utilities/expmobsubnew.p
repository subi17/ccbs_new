{utilities/ttinpmobsub.i}
DEFINE INPUT PARAMETER pcOutputDir        AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER piBeginMsSeq       AS INTEGER  NO-UNDO. 
DEFINE INPUT PARAMETER piEndMsSeq         AS INTEGER  NO-UNDO. 
DEFINE INPUT PARAMETER piWantedMsSeqCount AS INTEGER  NO-UNDO. 
DEFINE INPUT PARAMETER pcLogFile          AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcValueFileName    AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER piDispInterval     AS INTEGER  NO-UNDO. 
DEFINE INPUT PARAMETER TABLE FOR ttinpmobsub.

DEFINE VARIABLE iCountMobSub         AS INTEGER   NO-UNDO. 
DEFINE VARIABLE iSepAscii            AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lUseHandle           AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE lDisplay             AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE iOrderId             AS INTEGER   NO-UNDO. 
DEFINE VARIABLE cBeginRecTag         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cEndRecTag           AS CHARACTER NO-UNDO. 

DEFINE BUFFER xCustomer   FOR Customer.
DEFINE BUFFER xInvoice    FOR Invoice.
DEFINE BUFFER xInvRow     FOR InvRow.
DEFINE BUFFER xMnpProcess FOR MnpProcess. 
DEFINE BUFFER xMnpMessage FOR MnpMessage.

DEFINE TEMP-TABLE ttKeyField
   FIELD cFieldName AS CHARACTER  
   FIELD cFieldValue AS CHARACTER 
   INDEX idxField IS PRIMARY UNIQUE cFieldName cFieldValue.


cBeginRecTag = "<DynRecBegin>".
cEndRecTag   = "<DynRecEnd>".

&GLOBAL-DEFINE lcSep |

iSepAscii = 255.
lUseHandle = TRUE.
IF piDispInterval EQ 0 THEN lDisplay = FALSE. ELSE lDisplay = TRUE.
iCountMobSub       = 0.

DEFINE VARIABLE cMobSubFileName         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cIdentFile AS CHARACTER NO-UNDO.


FUNCTION fGetCharacterOption RETURN CHARACTER (INPUT pcOptionName AS CHARACTER):
   IF CAN-FIND(FIRST ttinpmobsub WHERE ttinpmobsub.coption EQ pcOptionName) THEN
   DO:
      FIND FIRST ttinpmobsub WHERE ttinpmobsub.coption EQ pcOptionName.
      RETURN ttinpmobsub.cvalue.
   END.
   RETURN ?.
END.

FUNCTION fAddOrUpdateOption RETURN LOGICAL (INPUT pcOption AS CHARACTER,
   INPUT pcOptionValue AS CHARACTER):
   FIND FIRST ttinpmobsub WHERE ttinpmobsub.cOption = pcOption NO-ERROR.
   IF AVAIL ttinpmobsub THEN
   DO:
      ttinpmobsub.cvalue = pcOptionValue.
      RETURN FALSE.
   END.
   ELSE
   DO:
      CREATE ttinpmobsub.
      ttinpmobsub.coption = pcOption.
      ttinpmobsub.cvalue = pcOptionValue.
      RETURN TRUE.
   END.
   RETURN TRUE.
END.


FUNCTION fGetLogicalOption RETURN LOGICAL (INPUT pcOptionName AS CHARACTER):
   DEFINE VARIABLE cOptionValue AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cDefOptionValue AS CHARACTER NO-UNDO. 
   cOptionValue = fGetCharacterOption(pcOptionName).
   IF cOptionValue NE ? THEN
   DO:
      RETURN LOGICAL(cOptionValue).
   END.
   ELSE
   DO:
      IF pcOptionName BEGINS "Export" THEN
      DO:
         cDefOptionValue = fGetCharacterOption("DefaultExport").
         IF cDefOptionValue EQ ? THEN 
            RETURN TRUE.
      END.
      IF pcOptionName BEGINS "MustFind" THEN
      DO:
         cDefOptionValue = fGetCharacterOption("DefaultMustFind").
         IF cDefOptionValue EQ ? THEN 
            RETURN FALSE.
      END.
      IF pcOptionName BEGINS "Denied" THEN
      DO:
         cDefOptionValue = fGetCharacterOption("DefaultDenied").
         IF cDefOptionValue EQ ? THEN 
            RETURN FALSE.
      END.
      IF pcOptionName BEGINS "Required" THEN
      DO:
         cDefOptionValue = fGetCharacterOption("DefaultRequired").
         IF cDefOptionValue EQ ? THEN 
            RETURN FALSE.
      END.
      RETURN LOGICAL(cDefOptionValue).
   END.
 
   RETURN FALSE.
END.

FUNCTION fGetIntOption RETURN INTEGER (INPUT pcOptionName AS CHARACTER):
   DEFINE VARIABLE cOptionValue AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE iOptionValue AS INTEGER NO-UNDO. 
   cOptionValue = fGetCharacterOption(pcOptionName).
   IF cOptionValue NE ? THEN
   DO:
      iOptionValue = INTEGER(cOptionValue) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO:
         IF pcOptionName BEGINS "mobcdr" OR
            pcOptionName BEGINS "prepcdr" THEN
         DO:
            IF INDEX(pcOptionName, "max") > 0 THEN
               iOptionValue = 90. /* maximum default is 90 days */
            ELSE 
               iOptionValue = 0. /* minimum default is 0 days */
         END.
      END.
   END.
   ELSE
   DO:
      IF pcOptionName BEGINS "mobcdr" OR
         pcOptionName BEGINS "prepcdr" THEN
      DO:
         IF INDEX(pcOptionName, "max") > 0 THEN
            iOptionValue = 90. /* maximum default is 90 days */
         ELSE 
            iOptionValue = 0. /* minimum default is 0 days */
      END.
   END.

   RETURN iOptionValue.
END.


DEFINE FRAME framemsg WITH WIDTH 180.
FUNCTION fMsg RETURN LOGICAL (INPUT pcMsg AS CHARACTER):
   DISP pcMsg FORMAT "X(178)" WITH FRAME framemsg.
END.


FUNCTION fDeniedOptionExists RETURN LOGICAL 
   (INPUT pcDeniedOptionList AS CHARACTER):
   
   DEFINE VARIABLE iOption AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iOptionCount AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lOptionFound AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE cOption AS CHARACTER NO-UNDO. 

   lOptionFound = FALSE.
   iOptionCount = NUM-ENTRIES(pcDeniedOptionList).

   DeniedOptionLoop:
   REPEAT iOption = 1 TO iOptionCount:
      cOption = ENTRY(iOption, pcDeniedOptionList).
      IF fGetCharacterOption(cOption) NE ? THEN
      DO:
        lOptionFound = TRUE.
        LEAVE DeniedOptionLoop.
      END.
   END.

   RETURN lOptionFound.
END.

/* End of the variables for records selection */
FUNCTION fCheckSelectionOptionErrors RETURN LOGICAL:
   DEFINE VARIABLE lErrorComb AS LOGICAL NO-UNDO. 
   lErrorComb = FALSE.

   IF pcValueFileName NE "" AND
      fDeniedOptionExists(
         "MustFindInvoice,DeniedInvoice,MustFindMnpProcess,DeniedMnpProcess," +
         "MustFindMnpMessage,DeniedMnpMessage,MustFindInvRow,DeniedInvRow," + 
         "MustFindInvoice,DeniedInvoice,DeniedDCCLIValid,RequiredDCCLIValid") 
   THEN
   DO:
      fMsg("Denied option used when using value file").
      lErrorComb = TRUE.
      RETURN lErrorComb.
   END.

   /* Same required and denied */
   IF fGetLogicalOption("MustFindInvoice") AND 
      fGetLogicalOption("DeniedInvoice") THEN
   DO:
      fMsg("Invoice cannot be required and denied").
      lErrorComb = TRUE.
   END.

   IF fGetLogicalOption("MustFindInvRow") AND 
      fGetLogicalOption("DeniedInvRow") THEN
   DO:
      fMsg("InvRow cannot be required and denied").
      lErrorComb = TRUE.
   END.

   IF fGetLogicalOption("MustFindMnpProcess") AND 
      fGetLogicalOption("DeniedMnpProcess") THEN
   DO:
      fMsg("MnpProcess cannot be required and denied").
      lErrorComb = TRUE.
   END.

   IF fGetLogicalOption("MustFindMnpMessage") AND 
      fGetLogicalOption("DeniedMnpMessage") THEN
   DO:
      fMsg("MnpMessage cannot be required and denied").
      lErrorComb = TRUE.
   END.


   IF fGetLogicalOption("MustFindInvRow") AND 
      fGetLogicalOption("DeniedInvoice") THEN
   DO:
      fMsg("Cannot deny Invoice when requiring InvRow").
      lErrorComb = TRUE.
   END.
   IF fGetLogicalOption("MustFindMnpMessage") AND 
      fGetLogicalOption("DeniedMnpProcess") THEN
   DO:
      fMsg("Cannot deny MnpProcess when requiring MnpMessage").
      lErrorComb = TRUE.
   END.

   IF fGetLogicalOption("DeniedDCCLIValid") AND 
      fGetLogicalOption("RequiredDCCLIValid") THEN
   DO:
      fMsg("DCCLI cannot be required and denied to be valid.").
      lErrorComb = TRUE.
   END.

   RETURN lErrorComb.
END.

FUNCTION fGetMsSeqFileNameExtension RETURN CHARACTER:
   DEFINE VARIABLE cRetVal AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE iNumPathEntries     AS INTEGER   NO-UNDO. 

   iNumPathEntries = NUM-ENTRIES(pcValueFileName, "/").
   cRetVal = ENTRY(iNumPathEntries, pcValueFileName, "/").
   cRetVal = REPLACE(cRetVal, ".", "").
   RETURN cRetVal.
END.


FUNCTION fAddOptionsToFileNames RETURN LOGICAL:
   DEFINE VARIABLE cMustIncludeInFileName        AS CHARACTER NO-UNDO. 
   define variable cdeniedinfilename             as character no-undo. 

   IF pcValueFileName EQ "" THEN
   DO:
      /* Must find and denied */
      IF fGetLogicalOption("MustFindInvoice") THEN
         cMustIncludeInFileName = "_Invoice".
      IF fGetLogicalOption("MustFindInvRow") THEN
         cMustIncludeInFileName = cMustIncludeInFilename + "_InvRow".
      IF fGetLogicalOption("MustFindMnpProcess") THEN
         cMustIncludeInFileName = cMustIncludeInFilename + "_MnpProcess".
      IF fGetLogicalOption("MustFindMnpMessage") THEN
         cMustIncludeInFileName = cMustIncludeInFilename + "_MnpMessage".
      IF fGetLogicalOption("RequiredDCCLIValid") THEN
         cMustIncludeInFileName = cMustIncludeInFileName + "_DCCLI_Valid".
      IF cMustIncludeInFileName <> "" THEN 
         cMustIncludeInFileName = cMustIncludeInFilename + "_records_required".

      IF fGetLogicalOption("DeniedInvoice") THEN
         cDeniedInFileName = "_Invoice".
      IF fGetLogicalOption("DeniedInvRow") THEN
         cDeniedInFileName = cDeniedInFilename + "_InvRow".
      IF fGetLogicalOption("DeniedMnpProcess") THEN
         cDeniedInFileName = cDeniedInFilename + "_MnpProcess".
      IF fGetLogicalOption("DeniedMnpMessage") THEN
         cDeniedInFileName = cDeniedInFilename + "_MnpMessage".
      IF fGetLogicalOption("DeniedDCCLIValid") THEN
         cDeniedInFileName = cDeniedInFileName + "_DCCLI_Valid".

      IF cDeniedInFileName <> "" THEN 
         cDeniedInFileName = cDeniedInFilename + "_records_denied".

      cMobSubFileName = 
         pcOutputDir + "mobsubs" + STRING(piBeginMsSeq) + "_" 
         + STRING(piEndMsSeq) + "_" + STRING(piWantedMsSeqCount).

      cMobSubFileName = cMobSubFileName + cMustIncludeInFileName + 
                        cDeniedInFileName.
      IF iSepASCII > 0 THEN
         cMobSubFileName = cMobSubFileName + "_sep" + STRING(iSepAscii).
      ELSE
         cMobSubFileName = cMobSubFileName + "_sep" + STRING(ASC('{&lcSep}')).

      IF lUseHandle THEN
         cMobSubFileName = cMobSubFileName + "_use_handle".
   END.
   ELSE
   DO:
      cMobSubFileName = pcOutputDir + "mobsubs" + "_" +
         fGetMsSeqFileNameExtension(). 
   END.

   RETURN TRUE.
END.


FUNCTION fSetDefExpFileToParams RETURN LOGICAL (INPUT pcTable AS CHARACTER,
   INPUT pcFileName AS CHARACTER):
   IF NOT CAN-FIND(ttInpMobsub WHERE ttInpMobsub.cOption = 
            "expfile" + pcTable) THEN
   DO:
      CREATE ttinpmobsub.
      ttinpmobsub.coption = "expfile" + pcTable.
      ttinpmobsub.cvalue = pcFileName.
   END.
   RETURN TRUE.
END.


FUNCTION fFormOtherFileNamesFromMobSub RETURN LOGICAL:
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

   fSetDefExpFileToParams("MobSub", cPathDir + cMobSubFileNamePart).
   fSetDefExpFileToParams("Customer",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "customers")).
   fSetDefExpFileToParams("Invoice",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "invoices")).
   fSetDefExpFileToParams("InvRow",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "invrows")).
   fSetDefExpFileToParams("InvASub",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "invasubs")).
   fSetDefExpFileToParams("InvSeq",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "invseqs")).
   fSetDefExpFileToParams("MnpProcess",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "mnpprocesses")).
   fSetDefExpFileToParams("MnpMessage",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "mnpmessages")).
   fSetDefExpFileToParams("MnpSub", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "mnpsubs")).
   fSetDefExpFileToParams("SubSer", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "subsers")).
   fSetDefExpFileToParams("SubSerPara", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "subserparas")).
   fSetDefExpFileToParams("Limit", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "limits")).
   fSetDefExpFileToParams("Order", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "orders")).
   fSetDefExpFileToParams("OrderCustomer", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "ordercustomers")).
   fSetDefExpFileToParams("OrderAccessory", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "orderacessories")).
   fSetDefExpFileToParams("OrderTopup", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "ordertopups")).
   fSetDefExpFileToParams("OrderPayment", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "orderpayments")).
   fSetDefExpFileToParams("Memo", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "ordermemos")).
   fSetDefExpFileToParams("SIM", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "orderSIMs")).
   fSetDefExpFileToParams("MSISDN", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "orderMSISDNs")).
   fSetDefExpFileToParams("OrderDelivery", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "orderdeliveries")).
   fSetDefExpFileToParams("OrderTimeStamp", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "ordertimestamps")).
   fSetDefExpFileToParams("PrepaidRequest", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "PrepaidRequests")).
   fSetDefExpFileToParams("MsRequest", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "MsRequests")).
   fSetDefExpFileToParams("DCCLI", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "DCCLI")).
   fSetDefExpFileToParams("DCCounter", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "DCCounter")). 
   fSetDefExpFileToParams("MsOwner", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "MSOwner")).
   fSetDefExpFileToParams("IMSI", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "IMSI")).
   fSetDefExpFileToParams("FaTime", 
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "FaTime")).
   fSetDefExpFileToParams("CallAlarm",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "CallAlarm")).
   fSetDefExpFileToParams("MSISDNNumber",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "MSISDNNumber")).
   fSetDefExpFileToParams("MobCDR",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "mobcdrs")).
   fSetDefExpFileToParams("MCDRDtl2",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "mcdrdtl2s")).
   fSetDefExpFileToParams("PrepCDR",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "prepcdrs")).
   fSetDefExpFileToParams("COTarg",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "cotargs")).
   fSetDefExpFileToParams("TMCounter",
      cPathDir + REPLACE(cMobSubFileNamePart, "mobsubs", "TMCounters")).


   cIdentFile = cPathDir + 
       REPLACE(cMobSubFileNamePart, "mobsubs", "indentifiers").


   RETURN TRUE.
END.


DEFINE STREAM sMobSub.

DEFINE STREAM sCustomer.
DEFINE STREAM sInvoice.
DEFINE STREAM sInvRow.
DEFINE STREAM sInvASub.
DEFINE STREAM sInvSeq.
DEFINE STREAM sMnpProcess.
DEFINE STREAM sMnpMessage.
DEFINE STREAM sMnpSub.

DEFINE STREAM sSubSerPara.
DEFINE STREAM sSubSer.
DEFINE STREAM sLimit.

DEFINE STREAM sOrder.
DEFINE STREAM sOrderCust.
DEFINE STREAM sOrderTopup.
DEFINE STREAM sOrderAcc.
DEFINE STREAM sOrderPay.
DEFINE STREAM sOrderMemo.
DEFINE STREAM sOrderSIM.
DEFINE STREAM sOrdMSISDN.
DEFINE STREAM sOrderDeliv.
DEFINE STREAM sOrderTS.

DEFINE STREAM sMSISDNNumb.
DEFINE STREAM sCallAlarm.

DEFINE STREAM sPrepReq.
DEFINE STREAM sMsRequest.
DEFINE STREAM sDCCLI.
DEFINE STREAM sDCCounter.
DEFINE STREAM sMsOwner.
DEFINE STREAM sIMSI.

DEFINE STREAM sFaTime.
DEFINE STREAM sPrepCdr.
DEFINE STREAM sMobCDR.
DEFINE STREAM sMCDRDtl.
DEFINE STREAM sCOTarg.

DEFINE STREAM sTMCounter.

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


FUNCTION fOpenStreams RETURN LOGICAL:
   IF fGetLogicalOption("ExportMobSub") THEN 
      OUTPUT STREAM sMobSub     TO VALUE(
         fGetCharacterOption("expfileMobSub")).
   IF fGetLogicalOption("ExportCustomer") THEN
      OUTPUT STREAM sCustomer   TO VALUE(
         fGetCharacterOption("expfileCustomer")).
   IF fGetLogicalOption("ExportInvoice") THEN
      OUTPUT STREAM sInvoice    TO VALUE(
         fGetCharacterOption("expfileInvoice")).
   IF fGetLogicalOption("ExportInvRow") THEN
      OUTPUT STREAM sInvRow     TO VALUE(
         fGetCharacterOption("expfileInvRow")).
   IF fGetLogicalOption("ExportInvASub") THEN
      OUTPUT STREAM sInvASub    TO VALUE(
         fGetCharacterOption("expfileInvASub")).
   IF fGetLogicalOption("ExportInvSeq") THEN
      OUTPUT STREAM sInvSeq     TO VALUE(
         fGetCharacterOption("expfileInvSeq")).
   IF fGetLogicalOption("ExportMnpProcess") THEN
      OUTPUT STREAM sMnpProcess TO VALUE(
         fGetCharacterOption("expfileMnpProcess")).
   IF fGetLogicalOption("ExportMnpMessage") THEN
      OUTPUT STREAM sMnpMessage TO VALUE(
         fGetCharacterOption("expfileMnpMessage")).
   IF fGetLogicalOption("ExportMnpSub") THEN
      OUTPUT STREAM sMnpSub     TO VALUE(
         fGetCharacterOption("expfileMnpSub")).
   IF fGetLogicalOption("ExportSubSerPara") THEN
      OUTPUT STREAM sSubSerPara TO VALUE(
         fGetCharacterOption("expfileSubSerPara")).
   IF fGetLogicalOption("ExportSubSer") THEN
      OUTPUT STREAM sSubSer     TO VALUE(
         fGetCharacterOption("expfileSubSer")).
   IF fGetLogicalOption("ExportLimit") THEN
      OUTPUT STREAM sLimit      TO VALUE(
         fGetCharacterOption("expfileLimit")).

   IF fGetLogicalOption("ExportOrder") THEN
      OUTPUT STREAM sOrder      TO VALUE(
         fGetCharacterOption("expfileorder")).
   IF fGetLogicalOption("ExportOrderCustomer") THEN
      OUTPUT STREAM sOrderCust  TO VALUE(
         fGetCharacterOption("expfileOrderCustomer")).
   IF fGetLogicalOption("ExportOrderTopup") THEN
      OUTPUT STREAM sOrderTopup TO VALUE(
         fGetCharacterOption("expfileOrderTopup")).
   IF fGetLogicalOption("ExportOrderAccessory") THEN
      OUTPUT STREAM sOrderAcc   TO VALUE(
         fGetCharacterOption("expfileOrderAccessory")).
   IF fGetLogicalOption("ExportOrderPayment") THEN
      OUTPUT STREAM sOrderPay   TO VALUE(
         fGetCharacterOption("expfileOrderPayment")).

   IF fGetLogicalOption("ExportMemo") THEN
      OUTPUT STREAM sOrderMemo  TO VALUE(
         fGetCharacterOption("expfileMemo")).
   IF fGetLogicalOption("ExportSIM") THEN
      OUTPUT STREAM sOrderSIM   TO VALUE(
         fGetCharacterOption("expfileSIM")).
   IF fGetLogicalOption("ExportMSISDN") THEN
      OUTPUT STREAM sOrdMSISDN  TO VALUE(
         fGetCharacterOption("expfileMSISDN")).
   IF fGetLogicalOption("ExportOrderDelivery") THEN
      OUTPUT STREAM sOrderDeliv TO VALUE(
         fGetCharacterOption("expfileOrderDelivery")).
   IF fGetLogicalOption("ExportOrderTimeStamp") THEN
      OUTPUT STREAM sOrderTS    TO VALUE(
         fGetCharacterOption("expfileOrderTimeStamp")).
   IF fGetLogicalOption("ExportPrepaidRequest") THEN
      OUTPUT STREAM sPrepReq    TO VALUE(
         fGetCharacterOption("expfilePrepaidRequest")). 
   IF fGetLogicalOption("ExportMsRequest") THEN
      OUTPUT STREAM sMsRequest  TO VALUE(
         fGetCharacterOption("expfileMsRequest")).
   IF fGetLogicalOption("ExportDCCLI") THEN
      OUTPUT STREAM sDCCLI      TO VALUE(
         fGetCharacterOption("expfileDCCLI")).
   IF fGetLogicalOption("ExportDCCounter") THEN
      OUTPUT STREAM sDCCounter  TO VALUE(
         fGetCharacterOption("expfileDCCounter")).
   IF fGetLogicalOption("ExportMsOwner") THEN
      OUTPUT STREAM sMsOwner    TO VALUE(
         fGetCharacterOption("expfileMsOwner")).
   IF fGetLogicalOption("ExportIMSI") THEN
      OUTPUT STREAM sIMSI       TO VALUE(
         fGetCharacterOption("expfileIMSI")).

   IF fGetLogicalOption("ExportFaTime") THEN
      OUTPUT STREAM sFaTime     TO VALUE(
         fGetCharacterOption("expfileFaTime")).
   IF fGetLogicalOption("ExportCallAlarm") THEN
      OUTPUT STREAM sCallAlarm  TO VAlUE(
         fGetCharacterOption("expfileCallAlarm")).
   IF fGetLogicalOption("ExportMSISDNNumber") THEN
      OUTPUT STREAM sMSISDNNumb TO VALUE(
         fGetCharacterOption("expfileMSISDNNumber")).

   IF fGetLogicalOption("ExportMobCDR") THEN
      OUTPUT STREAM sMobCDR TO VALUE(
         fGetCharacterOption("expfileMobCDR")).
   IF fGetLogicalOption("ExportPrepCDR") THEN
      OUTPUT STREAM sPrepCDR TO VALUE(
         fGetCharacterOption("expfilePrepCDR")).
   IF fGetLogicalOption("ExportCOTarg") THEN
      OUTPUT STREAM sCoTarg TO VALUE(
         fGetCharacterOption("expfileCOTarg")).
   IF fGetLogicalOption("ExportMCDRDtl2") THEN
      OUTPUT STREAM sMCDRDtl TO VALUE(
      fGetCharacterOption("expfileMCDRDtl2")).
   IF fGetLogicalOption("ExportTMCounter") THEN
      OUTPUT STREAM sTMCounter TO VALUE(
      fGetCharacterOption("expfileTMCounter")).

   RETURN TRUE.
END.


FUNCTION fCloseStreams RETURN LOGICAL:
   IF fGetLogicalOption("ExportMobSub") THEN 
      OUTPUT STREAM sMobSub     CLOSE.
   IF fGetLogicalOption("ExportCustomer") THEN
      OUTPUT STREAM sCustomer   CLOSE.
   IF fGetLogicalOption("ExportInvoice") THEN
      OUTPUT STREAM sInvoice    CLOSE.
   IF fGetLogicalOption("ExportInvRow") THEN
      OUTPUT STREAM sInvRow     CLOSE.
   IF fGetLogicalOption("ExportInvSeq") THEN
      OUTPUT STREAM sInvSeq     CLOSE.
   IF fGetLogicalOption("ExportInvASub") THEN
      OUTPUT STREAM sInvASub    CLOSE.

   IF fGetLogicalOption("ExportmMnpProcess") THEN
      OUTPUT STREAM sMnpProcess CLOSE.
   IF fGetLogicalOption("ExportMnpMessage") THEN
      OUTPUT STREAM sMnpMessage CLOSE.
   IF fGetLogicalOption("ExportMnpSub") THEN
      OUTPUT STREAM sMnpSub     CLOSE.

   IF fGetLogicalOption("ExportSubSerPara") THEN
      OUTPUT STREAM sSubSerPara CLOSE.
   IF fGetLogicalOption("ExportSubSer") THEN
      OUTPUT STREAM sSubSer     CLOSE.
   IF fGetLogicalOption("ExportLimit") THEN
      OUTPUT STREAM sLimit      CLOSE.

   IF fGetLogicalOption("ExportOrder") THEN
      OUTPUT STREAM sOrder      CLOSE.
   IF fGetLogicalOption("ExportOrderCustomer") THEN
      OUTPUT STREAM sOrderCust  CLOSE.
   IF fGetLogicalOption("ExportOrderTopup") THEN
      OUTPUT STREAM sOrderTopup CLOSE.
   IF fGetLogicalOption("ExportOrderAccessory") THEN
      OUTPUT STREAM sOrderAcc   CLOSE.
   IF fGetLogicalOption("ExportOrderPayment") THEN
      OUTPUT STREAM sOrderPay   CLOSE.
   IF fGetLogicalOption("ExportMemo") THEN
      OUTPUT STREAM sOrderMemo  CLOSE.
   IF fGetLogicalOption("ExportSIM") THEN
      OUTPUT STREAM sOrderSIM   CLOSE.
   IF fGetLogicalOption("ExportMSISDN") THEN
      OUTPUT STREAM sOrdMSISDN  CLOSE.
   IF fGetLogicalOption("ExportOrderDelivery") THEN
      OUTPUT STREAM sOrderDeliv CLOSE.
   IF fGetLogicalOption("ExportOrderTimeStamp") THEN
      OUTPUT STREAM sOrderTS    CLOSE.
   IF fGetLogicalOption("ExportPrepaidRequest") THEN
      OUTPUT STREAM sPrepReq    CLOSE.
   IF fGetLogicalOption("ExportMsRequest") THEN
      OUTPUT STREAM sMsRequest  CLOSE.
   IF fGetLogicalOption("ExportDCCLI") THEN
      OUTPUT STREAM sDCCLI      CLOSE.
   IF fGetLogicalOption("ExportDCCounter") THEN
      OUTPUT STREAM sDCCounter  CLOSE.

   IF fGetLogicalOption("ExportMsOwner") THEN
      OUTPUT STREAM sMsOwner    CLOSE.
   IF fGetLogicalOption("ExportIMSI") THEN
      OUTPUT STREAM sIMSI       CLOSE.

   IF fGetLogicalOption("ExportFaTime") THEN
      OUTPUT STREAM sFaTime     CLOSE.
   IF fGetLogicalOption("ExportCallAlarm") THEN
      OUTPUT STREAM sCallAlarm  CLOSE.
   IF fGetLogicalOption("ExportMSISDNNumber") THEN
      OUTPUT STREAM sMSISDNNumb CLOSE.

   IF fGetLogicalOption("ExportMobCDR") THEN
      OUTPUT STREAM sMobCDR CLOSE.
   IF fGetLogicalOption("ExportPrepCDR") THEN
      OUTPUT STREAM sPrepCDR CLOSE.
   IF fGetLogicalOption("ExportCOTarg") THEN
      OUTPUT STREAM sCOTarg CLOSE.
   IF fGetLogicalOption("ExportMCDRDtl2") THEN
      OUTPUT STREAM sMCDRDtl CLOSE.
   IF fGetLogicalOption("ExportTMCounter") THEN
      OUTPUT STREAM sTMCounter CLOSE.

   RETURN TRUE.
END.

FUNCTION fGetDelimiterDesc RETURN CHARACTER:
   RETURN cBeginRecTag + CHR(10) + 
          cEndRecTag + CHR(10) + 
          CHR(iSepAscii) + CHR(10).
END.


FUNCTION fAddDelimiterDescs RETURN LOGICAL:
   DEFINE VARIABLE cDelimiterDesc AS CHARACTER NO-UNDO. 
   cDelimiterDesc = fGetDelimiterDesc().
  
   IF fGetLogicalOption("ExportMobSub") THEN 
      PUT STREAM sMobSub     UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportCustomer") THEN
      PUT STREAM sCustomer   UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportInvoice") THEN
      PUT STREAM sInvoice    UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportInvRow") THEN
      PUT STREAM sInvRow     UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportInvASub") THEN
      PUT STREAM sInvASub    UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportInvSeq") THEN
      PUT STREAM sInvSeq     UNFORMATTED cDelimiterDesc.

   IF fGetLogicalOption("ExportMnpProcess") THEN
      PUT STREAM sMnpProcess UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportMnpMessage") THEN
      PUT STREAM sMnpMessage UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportMnpSub") THEN
      PUT STREAM sMnpSub     UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportSubSerPara") THEN
      PUT STREAM sSubSerPara UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportSubSer") THEN
      PUT STREAM sSubSer     UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportLimit") THEN
      PUT STREAM sLimit      UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportOrder") THEN
      PUT STREAM sOrder      UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportOrderCustomer") THEN
      PUT STREAM sOrderCust  UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportOrderTopup") THEN
      PUT STREAM sOrderTopup UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportOrderAccessory") THEN
      PUT STREAM sOrderAcc   UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportOrderPayment") THEN
      PUT STREAM sOrderPay   UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportMemo") THEN
      PUT STREAM sOrderMemo  UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportSIM") THEN
      PUT STREAM sOrderSIM   UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportMSISDN") THEN
      PUT STREAM sOrdMSISDN  UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportOrderDelivery") THEN
      PUT STREAM sOrderDeliv UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportOrderTimeStamp") THEN
      PUT STREAM sOrderTS    UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportPrepaidRequest") THEN
      PUT STREAM sPrepReq    UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportMsRequest") THEN
      PUT STREAM sMsRequest  UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportDCCLI") THEN
      PUT STREAM sDCCLI      UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportDCCounter") THEN
      PUT STREAM sDCCounter  UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportMsOwner") THEN
      PUT STREAM sMsOwner    UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportIMSI") THEN
      PUT STREAM sIMSI       UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportFaTime") THEN
      PUT STREAM sFaTime     UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportCallAlarm") THEN
      PUT STREAM sCallAlarm  UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportMSISDNNumber") THEN
      PUT STREAM sMSISDNNumb UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportMobCDR") THEN
      PUT STREAM sMobCDR UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportPrepCDR") THEN
      PUT STREAM sPrepCDR UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportCOTarg") THEN
      PUT STREAM sCOTarg UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportMCDRDtl2") THEN
      PUT STREAM sMCDRDtl UNFORMATTED cDelimiterDesc.
   IF fGetLogicalOption("ExportTMCounter") THEN
      PUT STREAM sTMCounter UNFORMATTED cDelimiterDesc.

   RETURN TRUE.
END.


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
   DEFINE VARIABLE cFieldsTaken AS CHARACTER NO-UNDO. 
   cFieldsTaken = fGetCharacterOption(pcTableName + "fields").

   cRecData = cBeginRecTag.
   iNumFields = phRecHandle:NUM-FIELDS.
   REPEAT iField = 1 to iNumFields:
     hField = phRecHandle:BUFFER-FIELD(iField).
     cFieldname = hField:NAME.
     IF cFieldsTaken NE ? THEN
        IF LOOKUP(cFieldName,cFieldsTaken) = 0 THEN NEXT.

     IF hField:EXTENT > 1 THEN
     DO:
       REPEAT iExtent = 1 TO hField:EXTENT:
         cFieldValue = hField:BUFFER-VALUE(iExtent).
         cRecData = cRecData + CHR(iSepAscii) + cFieldName + "," + 
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
   cRecData = cRecData + cEndRecTag.
   RETURN cRecData.
END.



FUNCTION fAddCustomers RETURN LOGICAL (INPUT piCustNum AS INTEGER):
  DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

  IF fGetLogicalOption("ExportCustomer") THEN
  DO:
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
END.


FUNCTION fAddInvRows RETURN LOGICAL (INPUT piInvNum AS INTEGER):
  DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

  IF fGetLogicalOption("ExportInvRow") THEN
  DO:
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

  RETURN TRUE.
END.

FUNCTION fAddInvASub RETURN LOGICAL (iNPUT piInvNum AS INTEGER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportInvASub") THEN
   DO:
      FOR EACH InvASub WHERE InvASub.InvNum = piInvNum NO-LOCK:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER InvASub:HANDLE.
            cRecData = fGetRecordData("InvASub", INPUT-OUTPUT hTableHandle, 
               ROWID(InvASub)).
            PUT STREAM sInvASub UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sInvASub DELIMITER "{&lcSep}" InvASub.
      END.
   END.

   RETURN TRUE.
END.


FUNCTION fAddInvoices RETURN LOGICAL (INPUT piCustNum AS INTEGER):
  DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 


  IF fGetLogicalOption("ExportInvoice") THEN
  DO:
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
         fAddInvASub(Invoice.InvNum).
     END.
  END.
END.

FUNCTION fAddOrderInvoices RETURN LOGICAL (INPUT piInvNum AS INTEGER):
  DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 
  
  IF fGetLogicalOption("ExportInvoice") THEN
  DO:
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
         fAddInvASub(Invoice.InvNum).
     END.
  END.
END.


FUNCTION fExportMsRequest RETURN LOGICAL:
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 
   IF lUseHandle THEN
   DO:
      hTableHandle = BUFFER MsRequest:HANDLE.
      cRecData = fGetRecordData("MsRequest", INPUT-OUTPUT hTableHandle, 
          ROWID(MsRequest)).
      PUT STREAM sMsRequest UNFORMATTED cRecData SKIP.
   END.
   ELSE
      EXPORT STREAM sMsRequest DELIMITER "{&lcSep}" MsRequest.
   RETURN TRUE.
END.




FUNCTION fAddMsRequests RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
  DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 
  
  IF fGetLogicalOption("ExportMsRequest") THEN
  DO:
     FOR EACH MsRequest WHERE MsRequest.MsSeq = piMsSeq NO-LOCK:
        IF NOT fKeyExists("MsRequest.MsRequest", STRING(MsRequest.MsRequest)) THEN
        DO:
           fExportMsRequest().
        END.
     END.
  END.
END.

FUNCTION fAddDCCounter RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportDCCounter") THEN
   DO:
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
END.

FUNCTION fAddDCCLI RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportDCCLI") THEN
   DO:
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

   RETURN TRUE.
END.


FUNCTION fAddFaTimes RETURN LOGICAL (INPUT piCustNum AS INTEGER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportFaTime") THEN
   DO:
      FOR EACH FaTime WHERE FaTime.CustNum = piCustNum NO-LOCK:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER FaTime:HANDLE.
            cRecData = fGetRecordData("FaTime", INPUT-OUTPUT hTableHandle, 
               ROWID(FaTime)).
            PUT STREAM sFaTime UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sFaTime DELIMITER "{&lcSep}" FaTime. 
      END.
   END.

   RETURN TRUE.
END.



FUNCTION fAddLimits RETURN LOGICAL (INPUT piCustNum AS INTEGER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportLimit") THEN
   DO:
      FOR EACH Limit WHERE Limit.CustNum = piCustNum NO-LOCK:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER Limit:HANDLE.
            cRecData = fGetRecordData("Limit", INPUT-OUTPUT hTableHandle, 
               ROWID(Limit)).
            PUT STREAM sLimit UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sLimit DELIMITER "{&lcSep}" Limit.
      END.
   END.
   
   RETURN TRUE.
END.


FUNCTION fAddInvSeqs RETURN LOGICAL (INPUT piCustNum AS INTEGER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportInvSeq") THEN
   DO:
      IF AVAIL MobSub THEN
      DO:
         FOR EACH InvSeq WHERE 
             InvSeq.MsSeq = Mobsub.MsSeq AND
             InvSeq.CustNum = piCustNum NO-LOCK:
            IF lUseHandle THEN
            DO:
               hTableHandle = BUFFER InvSeq:HANDLE.
               cRecData = fGetRecordData("InvSeq", INPUT-OUTPUT hTableHandle, 
                  ROWID(InvSeq)).
               PUT STREAM sInvSeq UNFORMATTED cRecData SKIP.
            END.
            ELSE
               EXPORT STREAM sInvSeq DELIMITER "{&lcSep}" InvSeq.
         END.
      END.
      ELSE
      DO:
         FOR EACH InvSeq WHERE 
             InvSeq.CustNum = piCustNum NO-LOCK:

            IF lUseHandle THEN
            DO:
               hTableHandle = BUFFER InvSeq:HANDLE.
               cRecData = fGetRecordData("InvSeq", INPUT-OUTPUT hTableHandle, 
                  ROWID(InvSeq)).
               PUT STREAM sInvSeq UNFORMATTED cRecData SKIP.
            END.
            ELSE
               EXPORT STREAM sInvSeq DELIMITER "{&lcSep}" InvSeq.
         END.
      END.
   END.
   RETURN TRUE.

END.

FUNCTION fAddCustomerRecords RETURN LOGICAL (INPUT piCustNum AS INTEGER):
   fAddCustomers(piCustNum).
   fAddInvoices(piCustNum).
   fAddFaTimes(piCustNum).
   fAddLimits(piCustNum).
   fAddInvSeqs(piCustNum).

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
         IF NOT fGetLogicalOption("MustFindInvRow") AND 
            NOT fGetLogicalOption("DeniedInvRow") THEN LEAVE LoopInvoices.
         FIND FIRST xInvRow WHERE xInvRow.InvNum = xInvoice.InvNum 
             NO-LOCK NO-ERROR.
         IF AVAIL xInvRow THEN
         DO:
            lInvRowFound = TRUE.
            LEAVE LoopInvoices.
         END.
    END.

    IF fGetLogicalOption("MustFindInvoice") AND NOT lInvoiceFound THEN 
       RETURN FALSE.
    IF fGetLogicalOption("MustFindInvRow")  AND NOT lInvRowFound  THEN 
       RETURN FALSE.
    IF fGetLogicalOption("DeniedInvoice")   AND     lInvoiceFound THEN 
       RETURN FALSE.
    IF fGetLogicalOption("DeniedInvRow")    AND     lInvRowFound  THEN 
       RETURN FALSE.

    FIND FIRST xMnpProcess WHERE xMnpProcess.OrderId = iOrderId 
               NO-LOCK NO-ERROR.

    IF NOT AVAIL xMnpProcess AND fGetLogicalOption("MustFindMnpProcess") THEN 
        RETURN FALSE.
    IF AVAIL xMnpProcess AND fGetLogicalOption("DeniedMnpProcess") THEN
        RETURN FALSE.


    IF AVAIL xMnpProcess AND 
       (fGetLogicalOption("MustFindMnpMessage") OR 
        fGetLogicalOption("DeniedMnpMessage")) THEN
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
       IF fGetLogicalOption("MustFindMnpMessage") AND 
          NOT lMnpMessageFound THEN RETURN FALSE.
       IF fGetLogicalOption("DeniedMnpMessage")   AND     
          lMnpMessageFound THEN RETURN FALSE.
    END.

    RETURN TRUE.
END.


FUNCTION fAddMnpProcesses RETURN LOGICAL:
    DEFINE VARIABLE cRecdata     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

    
    FOR EACH MnpProcess WHERE MnpProcess.OrderId = iOrderId NO-LOCK:
       fKeyExists("MnpProcess.MnpSeq", STRING(MnpProcess.MnpSeq)).
       
       IF fGetLogicalOption("ExportMnpProcess") THEN
       DO:
          IF lUseHandle THEN
          DO:
             hTableHandle = BUFFER MnpProcess:HANDLE.
             cRecData = fGetRecordData("MnpProcess", INPUT-OUTPUT hTableHandle, 
                ROWID(MnpProcess)).
             PUT STREAM sMnpProcess UNFORMATTED cRecData SKIP.
          END.
          ELSE
             EXPORT STREAM sMnpProcess DELIMITER "{&lcSep}" MnpProcess.
       END.
      
       IF fGetLogicalOption("ExportMnpMessage") THEN
       DO:
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
END.


FUNCTION fAddPrepaidRequests RETURN LOGICAL:
   DEFINE VARIABLE cRecData AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE NO-UNDO. 

   IF fGetLogicalOption("ExportPrepaidRequest") THEN
   DO:
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
END.


FUNCTION fAddIMSIRecords RETURN LOGICAL:
   DEFINE VARIABLE cRecDataIMSI     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandleIMSI AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportIMSI") THEN
   DO:
      IF AVAIL Mobsub THEN
      DO:
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
      END.
      ELSE
      DO:
         FOR EACH IMSI WHERE IMSI.ICC = SIM.ICC NO-LOCK:
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
      END.
   END.

   RETURN TRUE.
END.


FUNCTION fAddMCDRDtl2 RETURN LOGICAL 
   (INPUT pdaDateSt AS DATE, 
    INPUT piDtlSeq AS INTEGER):

   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 
   IF fGetLogicalOption("ExportMCDRDtl2") THEN
   DO:
      FOR EACH MCDRDtl2 NO-LOCK 
         WHERE MCDRDtl2.DateSt = pdaDateSt AND
               MCDRDtl2.DtlSeq = piDtlSeq:
        
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER MCDRDtl2:HANDLE.
            cRecData = fGetRecordData("MCDRDtl2", INPUT-OUTPUT hTableHandle, 
               ROWID(MCDRDtl2)).
            PUT STREAM sMCDRDtl UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sMCDRDtl DELIMITER "{&lcSep}" MCDRDtl2.
      END.
   END.
   RETURN TRUE.
END. 

FUNCTION fAddMobCDRs RETURN LOGICAL (INPUT pcCLI AS CHARACTER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE iCountDaysForMobCDRMax AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iCountDaysForMobCDRMin AS INTEGER NO-UNDO. 
   DEFINE VARIABLE dtMobsubAct AS DATE NO-UNDO. 
   iCountDaysForMobCDRMax = fGetIntOption("mobcdrdaysmax").
   iCountDaysForMobCDRMin = fGetIntOption("mobcdrdaysmin").

   IF NOT AVAIL Mobsub THEN
      FIND Mobsub WHERE Mobsub.CLI = pcCLI NO-LOCK NO-ERROR.
   IF AVAIL Mobsub THEN
      dtMobsubAct = Mobsub.ActivationDate.
   ELSE
      dtMobsubAct = TODAY.

      FOR EACH MobCDR NO-LOCK WHERE 
         MobCDR.CLI = pcCLI AND 
         MobCDR.DateST >= dtMobsubAct + iCountDaysForMobCDRMin AND
         MobCDR.DateST <= dtMobsubAct + iCountDaysForMobCDRMax:

         IF fGetLogicalOption("ExportMobCDR") THEN
         DO:
            IF lUseHandle THEN
            DO:
               hTableHandle = BUFFER MobCDR:HANDLE.
               cRecData = fGetRecordData("MobCDR", INPUT-OUTPUT hTableHandle, 
                  ROWID(MobCDR)).
               PUT STREAM sMobCDR UNFORMATTED cRecData SKIP.
            END.
            ELSE
               EXPORT STREAM sMobCDR DELIMITER "{&lcSep}" MobCDR.
         END.
         fAddMCDRDtl2(MobCDR.DateSt, MobCDR.DtlSeq).
      END.

   RETURN TRUE.
END.

FUNCTION fAddPrepCDRs RETURN LOGICAL (INPUT pcCLI AS CHARACTER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE iCountDaysForPrepCDRMax AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iCountDaysForPrepCDRMin AS INTEGER NO-UNDO. 
   DEFINE VARIABLE dtMobsubAct AS DATE NO-UNDO. 

   iCountDaysForPrepCDRMax = fGetIntOption("prepcdrdaysmax").
   iCountDaysForPrepCDRMin = fGetIntOption("prepcdrdaysmin").


   IF NOT AVAIL Mobsub THEN
      FIND Mobsub WHERE Mobsub.CLI = pcCLI NO-LOCK NO-ERROR.
   IF AVAIL Mobsub THEN
      dtMobsubAct = Mobsub.ActivationDate.
   ELSE
      dtMobsubAct = TODAY.

   IF fGetLogicalOption("ExportPrepCDR") THEN
   DO:
     FOR EACH PrepCDR NO-LOCK WHERE 
         PrepCDR.CLI = pcCLI AND 
         PrepCDR.DateST >= dtMobsubAct + iCountDaysForPrepCDRMin AND
         PrepCDR.DateST <= dtMobsubAct + iCountDaysForPrepCDRMax:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER PrepCDR:HANDLE.
            cRecData = fGetRecordData("PrepCDR", INPUT-OUTPUT hTableHandle, 
               ROWID(PrepCDR)).
            PUT STREAM sPrepCDR UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sPrepCDR DELIMITER "{&lcSep}" PrepCDR.
      END. 
   END.

   RETURN TRUE.
END.


FUNCTION fAddICCRelatedRecords RETURN LOGICAL (INPUT pcICC AS CHARACTER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportSIM") THEN
   DO:
      FOR EACH SIM WHERE SIM.Brand = "1" AND SIM.ICC = pcICC NO-LOCK:
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
   END.

   RETURN TRUE.
END.


FUNCTION fAddMSISDNRelatedRecords RETURN LOGICAL (INPUT pcCLI AS CHARACTER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportMSISDN") THEN
   DO:
      FOR EACH MSISDN WHERE MSISDN.Brand = "1" AND MSISDN.CLI = pcCLI 
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

   IF fGetLogicalOption("ExportMSISDNNumber") THEN
   DO:
      FOR EACH MSISDNNumber WHERE MSISDNNumber.CLI = pcCLI NO-LOCK:
         fKeyExists("MSISDN.CLI", MSISDNNumber.CLI).

         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER MSISDNNumber:HANDLE.
            cRecData = fGetRecordData("MSISDNNumber", INPUT-OUTPUT hTableHandle, 
               ROWID(MSISDNNumber)).
            PUT STREAM sMSISDNNumb UNFORMATTED cRecData SKIP.
         END.
         ELSE
             EXPORT STREAM sMSISDNNumb DELIMITER "{&lcSep}" MSISDNNumber.
      END.
   END.

   IF fGetLogicalOption("ExportCallAlarm") THEN
   DO:
      FOR EACH CallAlarm WHERE CallAlarm.CLI = pcCLI NO-LOCK:
         fKeyExists("MSISDN.CLI", CallAlarm.CLI).

         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER CallAlarm:HANDLE.
            cRecData = fGetRecordData("CallAlarm", INPUT-OUTPUT hTableHandle, 
               ROWID(CallAlarm)).
            PUT STREAM sCallAlarm UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sCallAlarm DELIMITER "{&lcSep}" CallAlarm.
      END.
   END.
   fAddPrepCDRs(pcCLI).
   fAddMobCDRs(pcCLI).

   RETURN TRUE.
END.



FUNCTION fAddOrderRecords RETURN LOGICAL:
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   fKeyExists("Order.OrderId", STRING(Order.OrderId)).

   IF fGetLogicalOption("ExportOrder") THEN
   DO:
      IF lUseHandle THEN
      DO:
         hTableHandle = BUFFER Order:HANDLE.
         cRecData = fGetRecordData("Order", INPUT-OUTPUT hTableHandle, 
            ROWID(Order)).
         PUT STREAM sOrder UNFORMATTED cRecData SKIP.
      END.
      ELSE
         EXPORT STREAM sOrder DELIMITER "{&lcSep}" Order.
   END.

   IF fGetLogicalOption("ExportOrderCustomer") THEN
   DO:
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
   END.

   IF fGetLogicalOption("ExportOrderTopup") THEN
   DO:
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
   END.

   IF fGetLogicalOption("ExportOrderAccessory") THEN
   DO:
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
   END.

   IF fGetLogicalOption("ExportOrderPayment") THEN
   DO:
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
   END.

   IF fGetLogicalOption("ExportOrderDelivery") THEN
   DO:
      FOR EACH OrderDelivery WHERE OrderDelivery.Brand = "1" AND 
                                   OrderDelivery.OrderId = iOrderId NO-LOCK:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER OrderDelivery:HANDLE.
            cRecData = fGetRecordData("OrderDelivery", INPUT-OUTPUT hTableHandle, 
               ROWID(OrderDelivery)).
            PUT STREAM sOrderDeliv UNFORMATTED cRecData SKIP.
         END.
         ELSE
             EXPORT STREAM sOrderDeliv DELIMITER "{&lcSep}" OrderDelivery.
      END.
   END.

   IF fGetLogicalOption("ExportOrderTimeStamp") THEN
   DO:
      FOR EACH OrderTimeStamp WHERE OrderTimeStamp.Brand = "1" AND 
                                    OrderTimeStamp.OrderId = iOrderId NO-LOCK:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER OrderTimeStamp:HANDLE.
            cRecData = fGetRecordData("OrderTimeStamp", INPUT-OUTPUT hTableHandle, 
               ROWID(OrderTimeStamp)).
            PUT STREAM sOrderTS UNFORMATTED cRecData SKIP.
         END.
         ELSE
             EXPORT STREAM sOrderTS DELIMITER "{&lcSep}" OrderTimeStamp.
      END.
   END.

   
   IF fGetLogicalOption("ExportMemo") THEN
   DO:
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

   RETURN TRUE.
END.

FUNCTION fAddMsOwners RETURN LOGICAL:
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportMsOwner") THEN
   DO:
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
  END.
  RETURN TRUE.
END.


FUNCTION fAddMnpSub RETURN LOGICAL:
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportMnpSub") THEN
   DO:
      FOR EACH MnpSub WHERE MnpSub.MsSeq = Mobsub.MsSeq NO-LOCK:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER MnpSub:HANDLE.
            cRecData = fGetRecordData("MnpSub", INPUT-OUTPUT hTableHandle, 
               ROWID(MnpSub)).
            PUT STREAM sMnpSub UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sMnpSub DELIMITER "{&lcSep}" MnpSub.
      END.
   END.

   RETURN TRUE.
END.





FUNCTION fAddSubsers RETURN LOGICAL:
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportSubSer") THEN
   DO:
      FOR EACH SubSer WHERE SubSer.MsSeq = Mobsub.MsSeq NO-LOCK:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER SubSer:HANDLE.
            cRecData = fGetRecordData("SubSer", INPUT-OUTPUT hTableHandle, 
               ROWID(SubSer)).
            PUT STREAM sSubSer UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sSubSer DELIMITER "{&lcSep}" SubSer.
      END.
   END.

   IF fGetLogicalOption("ExportSubSerPara") THEN
   DO:
      FOR EACH SubSerPara WHERE SubSerPara.MsSeq = Mobsub.MsSeq NO-LOCK:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER SubSerPara:HANDLE.
            cRecData = fGetRecordData("SubSerPara", INPUT-OUTPUT hTableHandle, 
               ROWID(SubSerPara)).
            PUT STREAM sSubSerPara UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sSubSerPara DELIMITER "{&lcSep}" SubSerPara.
      END.
   END.

   RETURN TRUE.
END.


FUNCTION fAddCOTarg RETURN LOGICAL (INPUT piMsSeq AS INTEGER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportCOTarg") THEN
   DO:
      FOR EACH COTarg WHERE COTarg.Brand = "1" AND COTarg.TargType = "M" AND 
             COTarg.COTarg = STRING(piMsSeq) NO-LOCK:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER CoTarg:HANDLE.
               cRecData = fGetRecordData("CoTarg", INPUT-OUTPUT hTableHandle, 
                  ROWID(COTarg)).
               PUT STREAM sCOTarg UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sCOTarg DELIMITER "{&lcSep}" COTarg.
      END.
   END.

   RETURN TRUE.
END.


DEFINE STREAM sIdent.
FUNCTION fOutputIdentifiers RETURN LOGICAL:

   OUTPUT STREAM sIdent TO VALUE(cIdentFile).
     FOR EACH ttKeyField BREAK BY cFieldName:
        IF FIRST-OF(ttKeyField.cFieldName) THEN
        DO:
            PUT STREAM sIdent UNFORMATTED SKIP(2) ttKeyField.cFieldName SKIP.
        END.
        PUT STREAM sIdent UNFORMATTED ttKeyField.cFieldValue SKIP.
     END.
   OUTPUT STREAM sIdent CLOSE.
   RETURN TRUE.
END.

FUNCTION fAddTMCounters RETURN LOGICAL:
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 

   IF fGetLogicalOption("ExportTMCounter") THEN
   DO:
      FOR EACH TMCounter NO-LOCK WHERE TMCounter.MsSeq = MobSub.MsSeq:
         IF lUseHandle THEN
         DO:
            hTableHandle = BUFFER TMCounter:HANDLE.
               cRecData = fGetRecordData("TMCounter", INPUT-OUTPUT hTableHandle, 
                  ROWID(TMCounter)).
               PUT STREAM sTMCounter UNFORMATTED cRecData SKIP.
         END.
         ELSE
            EXPORT STREAM sTMCounter DELIMITER "{&lcSep}" TMCounter.
      END.
   END.
   RETURN TRUE.
END.

FUNCTION fExportMobsubAndRelated RETURN LOGICAL 
   (OUTPUT plLeave AS LOGICAL, OUTPUT pcRejectReason AS CHARACTER):
   DEFINE VARIABLE cRecData     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hTableHandle AS HANDLE    NO-UNDO. 
   plLeave = FALSE.

   FIND FIRST Order WHERE Order.MsSeq = MobSub.MsSeq 
              USE-INDEX MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL Order THEN 
   DO:
      pcRejectReason = "no order was available.".
      RETURN FALSE.
   END.

   iOrderId = Order.OrderId.
   IF iOrderId = 0 THEN RETURN FALSE.

   IF NOT fCustomerRecordsExist(MobSub.CustNum) THEN 
   DO:
      pcRejectReason = "customer records for custnum " + 
         STRING(Mobsub.CustNum) + " did not exist.".
      RETURN FALSE.
   END.
   IF NOT fCustomerRecordsExist(MobSub.AgrCust) THEN 
   DO:
      pcRejectReason = "agreement customer records for custnum " + 
         STRING(Mobsub.AgrCust) + " did not exist.".
      RETURN FALSE.
   END.
   IF NOT fCustomerRecordsExist(MobSub.InvCust) THEN 
   DO:
      pcRejectReason = "invoice customer records for custnum " + 
         STRING(Mobsub.InvCust) + " did not exist.".
      RETURN FALSE.
   END.
   IF fGetLogicalOption("RequiredDCCLIValid") OR 
      fGetLogicalOption("DeniedDCCLIValid") THEN
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
      IF NOT lDCCLIFound AND 
         fGetLogicalOption("RequiredDCCLIValid") THEN 
      DO:
         pcRejectReason = 
            "DCCLI did not exist or was not valid although required.".
         RETURN FALSE.
      END.
      IF     lDCCLIFound AND 
         fGetLogicalOption("DeniedDCCLIValid")   THEN 
      DO:   
         pcRejectReason = "there existed valid DCCLI although denied.".
         RETURN FALSE.
      END.
   END.
  
   fKeyExists("MobSub.MsSeq", STRING(MobSub.MsSeq)).


   IF fGetLogicalOption("ExportMobSub") THEN 
   DO:
      IF lUseHandle THEN
      DO:
          hTableHandle = BUFFER MobSub:HANDLE.
          cRecData = fGetRecordData("MobSub", INPUT-OUTPUT hTableHandle, 
             ROWID(MobSub)).
          PUT STREAM sMobSub UNFORMATTED cRecData SKIP.
      END.
      ELSE
         EXPORT STREAM sMobSub DELIMITER "{&lcSep}" MobSub. 
   END.

   fAddMsOwners().
   fAddSubsers().
   fAddCustomerRecords(MobSub.CustNum).
   fAddCustomerRecords(MobSub.AgrCust).
   fAddCustomerRecords(MobSub.InvCust).

   fAddMnpSub().
   fAddMnpProcesses().
   fAddPrepaidRequests().
   fAddOrderRecords().
   fAddMsRequests(MobSub.MsSeq).
   fAddDCCLI(MobSub.MsSeq).
   fAddDCCounter(MobSub.MsSeq).
   fAddCOTarg(Mobsub.MsSeq).
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


FUNCTION fLoopMobsubs RETURN LOGICAL:
   DEFINE VARIABLE lLeave AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE cRejectReason AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cMsSeq AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE iMsSeq AS INTEGER NO-UNDO.

   REPEAT:
      IMPORT STREAM sValueF UNFORMATTED cMsSeq.
      iMsSeq = INTEGER(cMsSeq) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
      DO:
         FIND Mobsub WHERE Mobsub.MsSeq = iMsSeq NO-LOCK NO-ERROR.
         IF NOT AVAIL Mobsub THEN
         DO:
            fLog("Mobsub was not found with MsSeq " + STRING(iMsSeq)).
         END.
         ELSE
         DO:       
            IF fExportMobsubAndRelated(
                   OUTPUT lLeave, OUTPUT cRejectReason) THEN
            DO:
               fLog("Mobsub with MsSeq " + STRING(Mobsub.MsSeq) + 
                    "was rejected, because " + cRejectReason).
            END.
         END.
      END.
      ELSE
      DO:
         fLog("Input row " + cMsSeq + 
              " did not contain integer suitable for MsSeq").
      END.
   END.
   RETURN TRUE.
END.


FUNCTION fLoopCLIs RETURN LOGICAL:
   DEFINE VARIABLE cCLI AS CHARACTER NO-UNDO. 
   REPEAT:
      IMPORT STREAM sValueF UNFORMATTED cCLI.
      fAddMSISDNRelatedRecords(cCLI).
   END.
   RETURN TRUE.
END.


FUNCTION fLoopICCs RETURN LOGICAL:
   DEFINE VARIABLE cICC AS CHARACTER NO-UNDO. 

   REPEAT:
      IMPORT STREAM sValueF UNFORMATTED cICC.
      fAddICCRelatedRecords(cICC).
   END.
   RETURN TRUE.
END.


FUNCTION fLoopMsRequests RETURN LOGICAL:
   DEFINE VARIABLE cMsRequest AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE iMsRequest AS INTEGER NO-UNDO.

   REPEAT:
      IMPORT STREAM sValueF UNFORMATTED cMsRequest.
      iMsRequest = INTEGER(cMsRequest) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
      DO:
         FIND MsRequest WHERE MsRequest.MsRequest = iMsRequest NO-LOCK NO-ERROR.
         IF NOT AVAIL MsRequest THEN
         DO:
            fLog("MsRequest was not found with MsRequest " + STRING(iMsRequest)).
         END.
         ELSE
         DO:      
            fExportMsRequest().
         END.
      END.
   END.
END.


FUNCTION fRemoveExportsUnsuitableForSIMAndICC RETURN LOGICAL:
   fAddOrUpdateOption("ExportMobsub", "FALSE").
   fAddOrUpdateOption("ExportCustomer", "FALSE").
   fAddOrUpdateOption("ExportMsOwner", "FALSE").
   fAddOrUpdateOption("ExportMemo", "FALSE").
   fAddOrUpdateOption("ExportLimit", "FALSE").
   fAddOrUpdateOption("ExportPrepaidRequest", "FALSE").
   fAddOrUpdateOption("ExportOrder", "FALSE").
   fAddOrUpdateOption("ExportOrderCustomer", "FALSE").
   fAddOrUpdateOption("ExportOrderDelivery", "FALSE").
   fAddOrUpdateOption("ExportOrderAccessory", "FALSE").
   fAddOrUpdateOption("ExportOrderTimeStamp", "FALSE").
   fAddOrUpdateOption("ExportOrderPayment", "FALSE").
   fAddOrUpdateOption("ExportOrderTopup", "FALSE").
   fAddOrUpdateOption("ExportSubSer", "FALSE").
   fAddOrUpdateOption("ExportSubSerPara", "FALSE").
   fAddOrUpdateOption("ExportDCCLI", "FALSE").
   fAddOrUpdateOption("ExportDCCounter", "FALSE").
   fAddOrUpdateOption("ExportMsRequest", "FALSE").
   fAddOrUpdateOption("ExportCOTarg", "FALSE").
   fAddOrUpdateOption("ExportMnpProcess", "FALSE").
   fAddOrUpdateOption("ExportMnpMessage", "FALSE").
   fAddOrUpdateOption("ExportMnpSub", "FALSE").
   fAddOrUpdateOption("ExportInvoice", "FALSE").
   fAddOrUpdateOption("ExportInvRow", "FALSE").
   fAddOrUpdateOption("ExportInvASub", "FALSE").
   fAddOrUpdateOption("ExportFaTime", "FALSE").

   RETURN TRUE.
END.

FUNCTION fRemoveUnsuitableExports RETURN LOGICAL:
    DEFINE VARIABLE cValueField AS CHARACTER NO-UNDO. 
      cValueField = fGetCharacterOption("ValueField").
      IF cValueField EQ ? THEN cValueField = "MobSub.MsSeq".
      CASE cValueField:
         WHEN "MSISDN.CLI"   THEN
         do:
            fRemoveExportsUnsuitableForSIMAndICC().
            fAddOrUpdateOption("ExportSIM", "FALSE").
            fAddOrUpdateOption("ExportIMSI", "FALSE").
         end.
         WHEN "SIM.ICC"      THEN 
         do:
            fRemoveExportsUnsuitableForSIMAndICC().
            fAddOrUpdateOption("ExportMSISDN", "FALSE").
            fAddOrUpdateOption("ExportMSISDNNumber", "FALSE").
            fAddOrUpdateOption("ExportCallAlarm", "FALSE").
            fAddOrUpdateOption("ExportPrepCDR", "FALSE").
            fAddOrUpdateOption("ExportMobCDR", "FALSE").
         end.
         WHEN "MsRequest.MsRequest" THEN 
         do:
            fRemoveExportsUnsuitableForSIMAndICC().
            fAddOrUpdateOption("ExportSIM", "FALSE").
            fAddOrUpdateOption("ExportIMSI", "FALSE").
            fAddOrUpdateOption("ExportMSISDN", "FALSE").
            fAddOrUpdateOption("ExportMSISDNNumber", "FALSE").
            fAddOrUpdateOption("ExportCallAlarm", "FALSE").
            fAddOrUpdateOption("ExportPrepCDR", "FALSE").
            fAddOrUpdateOption("ExportMobCDR", "FALSE").
            fAddOrUpdateOption("ExportMsRequest", "TRUE").
         end.
     END.

   RETURN TRUE.
END.


FUNCTION fMainLoop RETURN LOGICAL:
   DEFINE VARIABLE lLeave AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE cRejectReason AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cMsSeq AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE iMsSeq AS INTEGER NO-UNDO.
   DEFINE VARIABLE cValueField AS CHARACTER NO-UNDO. 

   IF pcLogFile NE "" THEN
      OUTPUT STREAM sLogF TO VALUE(pcLogFile).

   IF pcValueFileName NE "" THEN
   DO:
      INPUT STREAM sValueF FROM VALUE(pcValueFileName).
      cValueField = fGetCharacterOption("ValueField").
      IF cValueField EQ ? THEN cValueField = "MobSub.MsSeq".
      CASE cValueField:
         WHEN "MSISDN.CLI"   THEN fLoopCLIs().
         WHEN "MobSub.MsSeq" THEN fLoopMobsubs().
         WHEN "SIM.ICC"      THEN fLoopICCs().
         WHEN "MsRequest.MsRequest" THEN fLoopMsRequests().
      END.
      INPUT STREAM sValueF CLOSE.
   END.
   ELSE
   DO:
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
   END.
  
   IF pcLogFile NE "" THEN
      OUTPUT STREAM sLogF CLOSE.

   RETURN TRUE.
END.

fRemoveUnsuitableExports().
IF fCheckSelectionOptionErrors() THEN RETURN.
fAddOptionsToFileNames(). 
fFormOtherFileNamesFromMobSub(). 
fOpenStreams().
fAddDelimiterDescs().
fMainLoop().
fCloseStreams().
fOutputIdentifiers(). 


