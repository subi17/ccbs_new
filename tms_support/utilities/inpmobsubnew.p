{/home/harrim/utilities/ttinpmobsub.i}
DEFINE INPUT PARAMETER pcInputDir         AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER piBeginMsSeq       AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER piEndMsSeq         AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER piWantedMsSeqCount AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER pcLogFile          AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcMsSeqFileName    AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER TABLE FOR ttinpmobsub.

DEFINE STREAM sData.
DEFINE STREAM sLog.

DEFINE VARIABLE cBeginRecTag        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cEndRecTag          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cFieldSep           AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iSepASCII           AS INTEGER NO-UNDO. 
DEFINE VARIABLE cMobSubFileNamePart AS CHARACTER NO-UNDO. 

DEFINE TEMP-TABLE ttFieldNames
   FIELD cFieldName AS CHARACTER
   INDEX idxFieldName cFieldName.

cFieldSep = CHR(255).
iSepASCII = 255.


FUNCTION fGetCharacterOption RETURN CHARACTER (INPUT pcOptionName AS CHARACTER):
   IF CAN-FIND(ttinpmobsub WHERE ttinpmobsub.coption EQ pcOptionName) THEN
   DO:
      FIND FIRST ttinpmobsub WHERE ttinpmobsub.coption EQ pcOptionName.
      RETURN ttinpmobsub.cvalue.
   END.
   RETURN ?.
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
      /* if option not mentioned, use default value */
      IF pcOptionName BEGINS "Import" THEN 
      DO:
         cDefOptionValue = fGetCharacterOption("DefaultImport").
         IF cDefOptionValue EQ ? THEN 
         do:
            RETURN TRUE.
         end.
         ELSE 
         do:
            RETURN LOGICAL(cDefOptionValue).
         end.
      END.

      IF pcOptionName BEGINS "SkipExisting" THEN
      DO:
         cDefOptionValue = fGetCharacterOption("DefaultSkipExisting").
         IF cDefOptionValue EQ ? THEN 
            RETURN TRUE.
         ELSE 
            RETURN LOGICAL(cOptionValue).
      END.

      IF pcOptionName BEGINS "oldformat" THEN
      DO:
         cDefOptionValue = fGetCharacterOption("DefaultOldFormat").
         IF cDefOptionValue EQ ? THEN 
            RETURN FALSE.
         ELSE 
            RETURN LOGICAL(cOptionValue).
      END.
   END.
 
   RETURN FALSE.
END.



FUNCTION fSetDefImpFileToParams RETURN LOGICAL (INPUT pcTable AS CHARACTER,
   INPUT pcFileName AS CHARACTER):
   IF NOT CAN-FIND(ttInpMobsub WHERE ttInpMobsub.cOption = 
            "impfile" + pcTable) THEN
   DO:
      CREATE ttinpmobsub.
      ttinpmobsub.coption = "impfile" + pcTable.
      ttinpmobsub.cvalue = pcFileName.
   END.
   RETURN TRUE.
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



FUNCTION fCheckSelectionOptionErrors RETURN LOGICAL:
   DEFINE VARIABLE lErrorComb AS LOGICAL NO-UNDO. 
   lErrorComb = FALSE.

   IF pcMsSeqFileName NE "" AND
      fDeniedOptionExists(
         "MustFindInvoice,DeniedInvoice,MustFindMnpProcess,DeniedMnpProcess," +
         "MustFindMnpMessage,DeniedMnpMessage,MustFindInvRow,DeniedInvRow," + 
         "MustFindInvoice,DeniedInvoice,DeniedDCCLIValid,RequiredDCCLIValid") 
   THEN
   DO:
      fMsg("Denied option used when using msseq file").
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

   /* Table dependency impossibility */
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

   RETURN lErrorComb.
END.


FUNCTION fGetMsSeqFileNameExtension RETURN CHARACTER:
   DEFINE VARIABLE cRetVal AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE iNumPathEntries     AS INTEGER   NO-UNDO. 

   iNumPathEntries = NUM-ENTRIES(pcMsSeqFileName, "/").
   cRetVal = ENTRY(iNumPathEntries, pcMsSeqFileName, "/").
   cRetVal = REPLACE(cRetVal, ".", "").
   RETURN cRetVal.
END.


FUNCTION fAddOptionsToFileNames RETURN LOGICAL:
   DEFINE VARIABLE cMustIncludeInFileName        AS CHARACTER NO-UNDO. 
   define variable cdeniedinfilename             as character no-undo. 

   IF pcMsSeqFileName EQ "" THEN
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

      /* Final imported filename base for all imported files*/
      cMobSubFileNamePart = cMobSubFileNamePart + 
                            cMustIncludeInFileName + cDeniedInFileName.

      cMobSubFileNamePart = cMobSubFileNamePart + "_sep" + STRING(iSepAscii).
      cMobSubFileNamePart = cMobSubFileNamePart + "_use_handle".
   END.
   ELSE
      cMobSubFileNamePart = cMobSubFileNamePart + "_" + 
         fGetMsSeqFileNameExtension(). 

   RETURN TRUE.
END.



FUNCTION fGetDelimetersFromFile RETURN LOGICAL:
   IMPORT STREAM sData UNFORMATTED cBeginRecTag.
   IMPORT STREAM sData UNFORMATTED cEndRecTag.
   IMPORT STREAM sData UNFORMATTED cFieldSep.
   cFieldSep = SUBSTRING(cFieldSep,1,1).
   RETURN TRUE.
END.

FUNCTION fFormOtherFileNamesFromMobSub RETURN LOGICAL:
   fSetDefImpFileToParams("MobSub",
      pcInputDir + cMobSubFileNamePart).
   fSetDefImpFileToParams("Customer",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "customers")).
   fSetDefImpFileToParams("Invoice",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "invoices")).
   fSetDefImpFileToParams("InvRow",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "invrows")).
   fSetDefImpFileToParams("InvSeq",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "invseqs")).
   fSetDefImpFileToParams("MnpProcess",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "mnpprocesses")).
   fSetDefImpFileToParams("MnpMessage",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "mnpmessages")).
   fSetDefImpFileToParams("Order",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "orders")).
   fSetDefImpFileToParams("OrderCustomer",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "ordercustomers")).
   fSetDefImpFileToParams("OrderAccessory",
      pcInputDir +  REPLACE(cMobSubFileNamePart, "mobsubs", "orderacessories")).
   fSetDefImpFileToParams("OrderTopup",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "ordertopups")).
   fSetDefImpFileToParams("OrderPayment",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "orderpayments")).
   fSetDefImpFileToParams("Memo",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "ordermemos")).
   fSetDefImpFileToParams("SIM",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "orderSIMs")).
   fSetDefImpFileToParams("MSISDN",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "orderMSISDNs")).
   fSetDefImpFileToParams("PrepaidRequest",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "PrepaidRequests")).
   fSetDefImpFileToParams("MsRequest",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "MsRequests")).
   fSetDefImpFileToParams("DCCounter",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "DCCounter")).
   fSetDefImpFileToParams("DCCLI",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "DCCLI")).
   fSetDefImpFileToParams("MsOwner",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "MSOwner")).

   fSetDefImpFileToParams("SubSer",
      pcInputDir +  REPLACE(cMobSubFileNamePart, "mobsubs", "subsers")).
   fSetDefImpFileToParams("SubSerPara",
      pcInputDir +  REPLACE(cMobSubFileNamePart, "mobsubs", "subserparas")).

   fSetDefImpFileToParams("IMSI",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "IMSI")).
   fSetDefImpFileToParams("FaTime",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "FaTime")).

   fSetDefImpFileToParams("CallAlarm",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "CallAlarm")).
   fSetDefImpFileToParams("MSISDNNumber",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "MSISDNNumber")).

   fSetDefImpFileToParams("OrderDelivery",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "orderdeliveries")).
   fSetDefImpFileToParams("OrderTimeStamp",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "ordertimestamps")).

   fSetDefImpFileToParams("Limit",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "limits")).
   fSetDefImpFileToParams("MnpSub",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "mnpsubs")).
   fSetDefImpFileToParams("InvASub",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "invasubs")).

   fSetDefImpFileToParams("MobCDR",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "mobcdrs")).
   fSetDefImpFileToParams("MCDRDtl2",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "mcdrdtl2s")).

   fSetDefImpFileToParams("PrepCDR",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "prepcdrs")).

   fSetDefImpFileToParams("COTarg",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "cotargs")).
   fSetDefImpFileToParams("TMCounter",
      pcInputDir + REPLACE(cMobSubFileNamePart, "mobsubs", "TMCounters")).

   RETURN TRUE.
END.


FUNCTION fCheckImportFile RETURN LOGICAL 
  (INPUT pcTableName AS CHARACTER, INPUT plImportFile AS LOGICAL,
   INPUT pcImportFileName AS CHARACTER):

  IF plImportFile THEN
  DO:
     IF SEARCH(pcImportFileName) EQ ? THEN
     DO:
        MESSAGE "Import canceled because import file " 
                pcImportFileName 
                " for table " 
                pcTableName " did not exist." 
                VIEW-AS ALERT-BOX.
        RETURN FALSE.
     END.
  END.
  RETURN TRUE.
END.


FUNCTION fCheckImportFileExistence RETURN LOGICAL:
   IF NOT fCheckImportFile("Mobsub", 
             fGetLogicalOption("ImportMobsub"),
             fGetCharacterOption("impfileMobSub"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("Customer", 
             fGetLogicalOption("ImportCustomer"), 
             fGetCharacterOption("impfileCustomer"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("Invoice", 
             fGetLogicalOption("ImportInvoice"), 
             fGetCharacterOption("impfileInvoice"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("InvRow", 
             fGetLogicalOption("ImportInvRow"), 
             fGetCharacterOption("impfileInvRow"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("InvSeq", 
             fGetLogicalOption("ImportInvSeq"), 
             fGetCharacterOption("impfileInvSeq"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("MnpProcess", 
             fGetLogicalOption("ImportMnpProcess"), 
             fGetCharacterOption("impfileMnpProcess"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("MnpMessage", 
             fGetLogicalOption("ImportMnpMessage"), 
             fGetCharacterOption("impfileMnpMessage"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("Order", 
             fGetLogicalOption("ImportOrder"), 
             fGetCharacterOption("impfileOrder"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("OrderCustomer", 
             fGetLogicalOption("ImportOrderCustomer"), 
             fGetCharacterOption("impfileOrderCustomer"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("OrderAccessory", 
             fGetLogicalOption("ImportOrderAccessory"), 
             fGetCharacterOption("impfileOrderAccessory"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("OrderTopup", 
             fGetLogicalOption("ImportOrderTopup"), 
             fGetCharacterOption("impfileOrderTopup"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("OrderPayment", 
             fGetLogicalOption("ImportOrderPayment"), 
             fGetCharacterOption("impfileOrderPayment"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("Memo", 
             fGetLogicalOption("ImportMemo"), 
             fGetCharacterOption("impfileMemo"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("SIM", 
             fGetLogicalOption("ImportSIM"), 
             fGetCharacterOption("impfileSIM"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("MSISDN", 
             fGetLogicalOption("ImportMSISDN"), 
             fGetCharacterOption("impfileMSISDN"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("PrepaidRequest", 
             fGetLogicalOption("ImportPrepaidRequest"), 
             fGetCharacterOption("impfilePrepaidRequest"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("MsRequest", 
             fGetLogicalOption("ImportMsRequest"), 
             fGetCharacterOption("impfileMsRequest"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("DCCLI", 
             fGetLogicalOption("ImportDCCLI"), 
             fGetCharacterOption("impfileDCCLI"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("DCCounter", 
             fGetLogicalOption("ImportDCCounter"), 
             fGetCharacterOption("impfileDCCounter"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("MsOwner", 
             fGetLogicalOption("ImportMsOwner"), 
             fGetCharacterOption("impfileMsOwner"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("SubSer", 
             fGetLogicalOption("ImportSubSer"), 
             fGetCharacterOption("impfileSubSer"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("SubSerPara", 
             fGetLogicalOption("ImportSubSerPara"), 
             fGetCharacterOption("impfileSubSerPara"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("InvASub", 
             fGetLogicalOption("ImportInvASub"), 
             fGetCharacterOption("impfileInvASub"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("MnpSub", 
             fGetLogicalOption("ImportMnpSub"), 
             fGetCharacterOption("impfileMnpSub"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("Limit", 
             fGetLogicalOption("ImportLimit"), 
             fGetCharacterOption("impfileLimit"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("OrderDelivery", 
             fGetLogicalOption("ImportOrderDelivery"), 
             fGetCharacterOption("impfileOrderDelivery"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("OrderTimeStamp", 
             fGetLogicalOption("ImportOrderTimeStamp"), 
             fGetCharacterOption("impfileOrderTimeStamp"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("IMSI", 
             fGetLogicalOption("ImportIMSI"), 
             fGetCharacterOption("impfileIMSI"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("FaTime", 
             fGetLogicalOption("ImportFaTime"), 
             fGetCharacterOption("impfileFaTime"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("CallAlarm", 
             fGetLogicalOption("ImportCallAlarm"), 
             fGetCharacterOption("impfileCallAlarm"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("MSISDNNumber", 
             fGetLogicalOption("ImportMSISDNNumber"), 
             fGetCharacterOption("impfileMSISDNNumber"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("MobCDR", 
             fGetLogicalOption("ImportMobCDR"), 
             fGetCharacterOption("impfileMobCDR"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("MCDRDtl2", 
             fGetLogicalOption("ImportMCDRDtl2"), 
             fGetCharacterOption("impfileMCDRDtl2"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("PrepCDR", 
             fGetLogicalOption("ImportPrepCDR"), 
             fGetCharacterOption("impfilePrepCDR"))
             THEN RETURN FALSE. 
   IF NOT fCheckImportFile("COTarg", 
             fGetLogicalOption("ImportCOTarg"), 
             fGetCharacterOption("impfileCOTarg"))
             THEN RETURN FALSE.
   IF NOT fCheckImportFile("TMCounter", 
             fGetLogicalOption("ImportTMCounter"), 
             fGetCharacterOption("impfileTMCounter"))
             THEN RETURN FALSE.

   RETURN TRUE.
END.


FUNCTION fGetReplaceBuffer RETURN HANDLE 
    (INPUT pcTableName AS CHARACTER, INPUT pcCondition AS CHARACTER):
   DEFINE VARIABLE hRetBuf AS HANDLE NO-UNDO. 
   DEFINE VARIABLE cPredicate AS CHARACTER NO-UNDO. 

   cPredicate = "FOR EACH " + pcTableName + 
                " WHERE " + pcCondition + " EXCLUSIVE-LOCK".

   DEFINE VARIABLE hTblBuf AS HANDLE NO-UNDO. 
   DEFINE VARIABLE hQuery AS HANDLE NO-UNDO. 
   CREATE BUFFER hTblBuf FOR TABLE pcTableName.
   CREATE QUERY hQuery.
   hQuery:SET-BUFFERS(hTblBuf).
   DEFINE VARIABLE lAns AS LOGICAL NO-UNDO. 
   lAns = hQuery:QUERY-PREPARE(cPredicate) NO-ERROR.
   IF lAns THEN
   DO:
      hQuery:QUERY-OPEN().
      hQuery:GET-FIRST() NO-ERROR.
      IF NOT ERROR-STATUS:ERROR AND VALID-HANDLE(hTblBuf) THEN
      DO:
         hRetBuf = hTblBuf.
      END.
      hQuery:QUERY-CLOSE().
   END.

   IF VALID-HANDLE(hQuery) THEN
      DELETE OBJECT hQuery.

   RETURN hRetBuf.
END.


FUNCTION fGetConditionFromFieldList RETURN CHARACTER 
   (INPUT pcUniqFieldList AS CHARACTER,
    INPUT phTempTableBufOfNewRec AS HANDLE):

   DEFINE VARIABLE iNumUniqFields AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iField AS INTEGER NO-UNDO.
   DEFINE VARIABLE hField AS HANDLE NO-UNDO. 
   DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cCondition AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cNewValue AS CHARACTER NO-UNDO. 

   iNumUniqFields = NUM-ENTRIES(pcUniqFieldList).
   REPEAT iField = 1 TO iNumUniqFields:
      cFieldName = ENTRY(iField, pcUniqFieldList).
      hField = phTemptableBufOfNewRec:BUFFER-FIELD(cFieldName).
      cNewValue = STRING(hField:BUFFER-VALUE()).
      cCondition = cCondition + " " + cFieldName + " = ".
      IF hField:DATA-TYPE EQ "character" THEN
         cCondition = cCondition + '"' + cNewValue + '"'.
      ELSE
         cCondition = cCondition + cNewValue.
      IF iField < iNumUniqFields THEN
         cCondition = cCondition + " AND ".
   END.
 
   RETURN cCondition.
END.

FUNCTION fCheckOneUniquenessFieldList RETURN LOGICAL 
   (INPUT pcTableName AS CHARACTER,
    INPUT pcUniqFieldList AS CHARACTER,
    INPUT phTemptableOfNewRec AS HANDLE,
    OUTPUT pcCondition AS CHARACTER):

   DEFINE VARIABLE cPredicate AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lQueryAns AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE hQuery AS HANDLE NO-UNDO. 
   DEFINE VARIABLE hTblBuf AS HANDLE NO-UNDO. 
   DEFINE VARIABLE lNotExists AS LOGICAL NO-UNDO. 
   lNotExists = FALSE.

   pcCondition = fGetConditionFromFieldList(pcUniqFieldList,
      phTemptableOfNewRec).
   cPredicate = "FOR EACH " + pcTableName + 
                " WHERE " + pcCondition + " NO-LOCK".

   CREATE BUFFER hTblBuf FOR TABLE pcTableName.
   CREATE QUERY hQuery.
   hQuery:SET-BUFFERS(hTblBuf).
   lQueryAns = hQuery:QUERY-PREPARE(cPredicate) NO-ERROR.
   IF lQueryAns THEN
   DO:
      hQuery:QUERY-OPEN().
      hQuery:GET-FIRST().
      IF hQuery:QUERY-OFF-END THEN lNotExists = TRUE.
      hQuery:QUERY-CLOSE().
   END.
   
   IF VALID-HANDLE(hQuery) THEN
      DELETE OBJECT hQuery.

   IF VALID-HANDLE(hTblBuf) THEN
      DELETE OBJECT hTblBuf.

   RETURN lNotExists.
END.


FUNCTION fAddToCharList RETURN LOGICAL (INPUT-OUTPUT pcList AS CHARACTER,
   INPUT pcNewEntry AS CHARACTER):

   IF pcList = "" THEN pcList = pcNewEntry.
   ELSE pcList = pcList + "," + pcNewEntry.

   RETURN TRUE.
END.


FUNCTION fNotExists RETURN LOGICAL (
   INPUT pcTableName AS CHARACTER,
   INPUT pcUniqFieldLists AS CHARACTER,
   INPUT phTemptableBufOfNewRec AS HANDLE,
   OUTPUT pcExistConditions AS CHARACTER,
   OUTPUT pcNotExistsConditions AS CHARACTER,
   OUTPUT pcImportantCondition AS CHARACTER,
   OUTPUT phoReplacedBuffer AS HANDLE):

   DEFINE VARIABLE iNumOfUniqFieldLists AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iUniqFieldList AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cUniqFieldList AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cCurrCondition AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cFirstCondition AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lRetVal AS LOGICAL NO-UNDO. 
   iNumOfUniqFieldLists = NUM-ENTRIES(pcUniqFieldLists, ";").
   ASSIGN 
      pcExistConditions = ""
      pcNotExistsConditions = ""
      pcImportantCondition = "".

   lRetVal = TRUE.
   /* Check uniqueness conditions and return if some of them fail,
      replace buffer is the first buffer with some unique condition */
   REPEAT iUniqFieldList = 1 TO iNumOfUniqFieldLists:
      cUniqFieldList = ENTRY(iUniqFieldList, pcUniqFieldLists, ";").
      IF NOT fCheckOneUniquenessFieldList(pcTableName, cUniqFieldList,
         phTemptableBufOfNewRec, OUTPUT cCurrCondition) THEN
      DO:
         pcImportantCondition = cCurrCondition.
         phoReplacedBuffer = fGetReplaceBuffer(pcTableName, 
            pcImportantCondition).
         lRetVal = FALSE.
         fAddToCharList(pcExistConditions, cCurrCondition).
      END.
      ELSE 
      DO:   
         fAddToCharList(pcNotExistsConditions, cCurrCondition).
         IF iUniqFieldList EQ 1 THEN
            pcImportantCondition = cCurrCondition.
      END.
   END.
   RETURN lRetVal.
END.


FUNCTION fSetValue RETURN LOGICAL
  (INPUT phField AS HANDLE, INPUT pcValue AS CHARACTER):
  CASE phField:DATA-TYPE:
      WHEN "character" THEN phField:BUFFER-VALUE = pcValue.
      WHEN "integer" THEN phField:BUFFER-VALUE = INTEGER(pcValue).
      WHEN "logical" THEN phField:BUFFER-VALUE = LOGICAL(pcValue).
      WHEN "decimal" THEN phField:BUFFER-VALUE = DECIMAL( 
         pcValue ).
      WHEN "date"    THEN phField:BUFFER-VALUE = DATE(pcValue).
  END.
END.


FUNCTION fSetExtentValue RETURN LOGICAL
  (INPUT phField AS HANDLE, INPUT piExtent AS INTEGER, 
   INPUT pcValue AS CHARACTER):
  CASE phField:DATA-TYPE:
      WHEN "character" THEN phField:BUFFER-VALUE(piExtent) = pcValue.
      WHEN "integer" THEN phField:BUFFER-VALUE(piExtent) = INTEGER(pcValue).
      WHEN "logical" THEN phField:BUFFER-VALUE(piExtent) = LOGICAL(pcValue).
      WHEN "decimal" THEN phField:BUFFER-VALUE(piExtent) =  DECIMAL( 
          pcValue  ).
      WHEN "date"    THEN phField:BUFFER-VALUE(piExtent) = DATE(pcValue).
  END.
END.


FUNCTION fFieldExistsInDb RETURN LOGICAL (INPUT pcFieldName AS CHARACTER):
    IF CAN-FIND(ttFieldNames WHERE ttFieldNames.cFieldName = pcFieldName) 
       THEN RETURN TRUE.
    RETURN FALSE.
END.


FUNCTION fInputFields RETURN LOGICAL
  (INPUT phTable AS HANDLE, INPUT pcRecordData AS CHARACTER):
  
  DEFINE VARIABLE iField      AS INTEGER   NO-UNDO. 
  DEFINE VARIABLE hField      AS HANDLE    NO-UNDO. 
  DEFINE VARIABLE cFieldName  AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE cFieldValue AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE iExtent     AS INTEGER   NO-UNDO. 
  DEFINE VARIABLE iEntry      AS INTEGER   NO-UNDO.

  IF pcRecordData BEGINS cFieldSep THEN 
     pcRecordData = SUBSTRING(pcRecordData, 2).

  REPEAT iEntry = 1 TO NUM-ENTRIES(pcRecordData, cFieldSep) - 1 BY 2:
     cFieldName = ENTRY(iEntry, pcRecordData, cFieldSep).
     cFieldValue = ENTRY(iEntry + 1, pcRecordData, cFieldSep).
     IF INDEX(cFieldName, ",") > 0 THEN
     DO:
        iExtent = INTEGER(ENTRY(2, cFieldName, ",")).
        cFieldName = TRIM(ENTRY(1, cFieldName, ",")).
        IF fFieldExistsInDb(cFieldName) THEN
        DO:
           hField = phTable:BUFFER-FIELD(cFieldName).
           fSetExtentValue(hField, iExtent, cFieldValue).
        END.
     END.
     ELSE 
     DO:
        IF fFieldExistsInDb(cFieldName) THEN
        DO:
           hField = phTable:BUFFER-FIELD(cFieldName).
           fSetValue(hField, cFieldValue).
        END.
     END.
  END.

  RETURN TRUE.
END.


FUNCTION fLog RETURN LOGICAL (INPUT pcMsg AS CHARACTER):
   IF pcLogFile NE "" THEN 
      PUT STREAM sLog UNFORMATTED pcMsg SKIP.
   RETURN TRUE.
END.


FUNCTION fLogValues RETURN LOGICAL (INPUT phTT AS HANDLE):
   IF NOT fGetLogicalOption("LogValues") THEN RETURN FALSE.

   DEFINE VARIABLE iNumFields AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iField AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iExtent AS INTEGER NO-UNDO. 
   DEFINE VARIABLE hField AS HANDLE NO-UNDO. 
   iNumFields = phTT:NUM-FIELDS.
   REPEAT iField = 1 TO iNumFields:
      hField = phTT:BUFFER-FIELD(iField).
      IF hField:EXTENT <= 1 THEN
         PUT STREAM sLog UNFORMATTED 
            hField:NAME " = " hField:BUFFER-VALUE SKIP.
      ELSE
         REPEAT iExtent = 1 TO hField:EXTENT:
            PUT STREAM sLog UNFORMATTED
               hField:NAME ",extent " iExtent " = " 
               hField:BUFFER-VALUE(iExtent) SKIP.
         END.
   END.
   RETURN TRUE.
END.


FUNCTION fGetOneRecordText RETURN CHARACTER:
   DEFINE VARIABLE lInRecord AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE lFirstRow AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE cRecordData AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cLine AS CHARACTER NO-UNDO. 

   cRecordData = "".
   lFirstRow = TRUE.
   lInRecord = TRUE.
   DO WHILE lInRecord:
      IMPORT STREAM sDATA UNFORMATTED cLine.
      IF lFirstRow THEN
      DO:
         /* Check the record begin is at its place */
         IF SUBSTRING(cLine, 1, LENGTH(cBeginRecTag)) NE cBeginRecTag THEN
         DO:
            /* DUMP FILE FORMAT ERROR */
         END.
         ELSE
         DO:
            /* Take the rest but the begin rec mark to the record data */
            cRecordData = SUBSTRING(cLine, LENGTH(cBeginRecTag) + 1). 
         END.
       END.
       ELSE
          cRecordData = cRecordData + cLine.

       /* Check record ending: now record data ends with that if it is so */
       IF SUBSTRING(cRecordData, LENGTH(cRecordData) - LENGTH(cEndRecTag) + 1, 
                    LENGTH(cEndRecTag)) = cEndRecTag THEN
       DO:
          /* Remove the record end mark from the record data */
          cRecordData = SUBSTRING(cRecordData, 1, 
              LENGTH(cRecordData) - LENGTH(cEndRecTag)).
          lInRecord = FALSE. /* Record ends: out of loop */
       END.
       ELSE /* Record continues and data contains a row break to be added back
               to the record data */
          cRecordData = cRecordData + CHR(10).
       lFirstRow = FALSE.
   END.
   RETURN cRecordData.
END.


FUNCTION fGetFieldNames RETURN LOGICAL (INPUT hTableBuf AS HANDLE):
   DEFINE VARIABLE iNumFields AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iField AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcFieldName AS CHARACTER NO-UNDO. 
   iNumFields = hTableBuf:NUM-FIELDS.
   REPEAT iField = 1 TO iNumFields:
       lcFieldName = hTableBuf:BUFFER-FIELD(iField):NAME.
       IF NOT CAN-FIND(ttFieldNames WHERE 
           ttFieldNames.cFieldName = lcFieldName) THEN
       DO:
           CREATE ttFieldNames.
           ASSIGN ttFieldNames.cFieldName = lcFieldName.
       END.
   END.
END.


FUNCTION fImportTable RETURN LOGICAL
   (INPUT pcFileName AS CHARACTER, 
    INPUT plImportTable AS LOGICAL,
    INPUT plSkipExisting AS LOGICAL,
    INPUT pcTableName AS CHARACTER, 
    INPUT pcUniqueFieldList AS CHARACTER,
    INPUT plOldFormat AS LOGICAL):

   MESSAGE "tablename " pcTableName VIEW-AS ALERT-BOX.
   IF NOT plImportTable THEN RETURN FALSE.
   MESSAGE "importing option TRUE" VIEW-AS ALERT-BOX.

   DEFINE VARIABLE httTable AS HANDLE NO-UNDO. 
   DEFINE VARIABLE httTableBuf AS HANDLE NO-UNDO. 
   DEFINE VARIABLE hBuf     AS HANDLE NO-UNDO. 
   DEFINE VARIABLE cRecData AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hReplacedBuf AS HANDLE NO-UNDO. 
   DEFINE VARIABLE cCondition AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cExistsConditions AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cNotExistsConditions AS CHARACTER NO-UNDO. 


   CREATE TEMP-TABLE httTable.
   httTable:CREATE-LIKE(pcTableName).
   httTable:TEMP-TABLE-PREPARE("tt" + pcTableName).
   httTableBuf = httTable:DEFAULT-BUFFER-HANDLE. 

   IF fGetLogicalOption("ShowTableNames") THEN 
      MESSAGE pcTableName VIEW-AS ALERT-BOX.
   INPUT STREAM sData FROM VALUE(pcFileName).
   IF NOT plOldFormat THEN
      fGetDelimetersFromFile().

   LoopRecords:
   REPEAT:
      httTableBuf:BUFFER-CREATE().
      fGetFieldNames(httTableBuf).

      IF plOldFormat THEN
         IMPORT STREAM sdata UNFORMATTED cRecData.
      ELSE
      DO:
         cRecData = fGetOneRecordText().
         IF cRecData eq ? THEN LEAVE LoopRecords.
      END.

      fInputFields(httTableBuf, cRecData).
      DO TRANSACTION:
         IF fNotExists(pcTableName, pcUniqueFieldList, httTableBuf, 
            OUTPUT cExistsConditions, OUTPUT cNotExistsConditions,
            OUTPUT cCondition, OUTPUT hReplacedBuf ) THEN
         DO:
            IF NOT fGetLogicalOption("Test") AND 
               NOT fGetLogicalOption("Delete") THEN
            DO:
               CREATE BUFFER hBuf FOR TABLE pcTableName.
               hBuf:BUFFER-COPY(httTableBuf).
               hBuf:BUFFER-RELEASE().
               DELETE OBJECT hBuf.
            END.
            IF NOT fGetLogicalOption("Delete") THEN
            DO:
               fLog("Created new " + pcTableName + " with " + 
                     cCondition).
            END.
            ELSE
               fLog("Not found " + pcTableName + " with " +
                    cCondition + " to delete.").
            fLogValues(httTableBuf).
         END.
         ELSE
         DO:
            IF cExistsConditions NE "" AND cNotExistsConditions NE "" THEN
            DO:
               fLog("!!!!! There is conflict between uniqueness conditions:").
               fLog("Record exists with conditions " + cExistsConditions + 
                    ", but not with conditions " + cNotExistsConditions).
               fLog("This is because unique fields have changed between" + 
                    " dumps and dumps are not totally suitable to each other").
            END.
            ELSE
            DO:
               IF NOT plSkipExisting THEN
               DO:
                  IF NOT fGetLogicalOption("Test") THEN
                  DO:
                     IF VALID-HANDLE(hReplacedBuf) THEN
                     DO:
                        IF fGetLogicalOption("Delete") THEN
                        DO:
                           hReplacedBuf:BUFFER-DELETE().
                           fLog("Deleted " + pcTableName + " with " + 
                                cCondition).
                        END.
                        ELSE
                        DO:
                           hReplacedBuf:BUFFER-COPY(httTableBuf).        
                        END.
                     END.
                     ELSE
                        fLog("!!! Tryed to replace existing " + 
                           pcTableName + " with " + cCondition + 
                           ", but the record was locked.").
                  END.
                  IF VALID-HANDLE(hReplacedBuf) THEN
                  DO:
                     hReplacedBuf:BUFFER-RELEASE().
                     DELETE OBJECT hReplacedBuf.
                  END.
                  IF NOT fGetLogicalOption("Delete") THEN
                     fLog("Replaced existing " + pcTableName + " with " + 
                          cCondition).
                  fLogValues(httTableBuf).
               END.
               ELSE
               DO:
                  IF VALID-HANDLE(hReplacedBuf) THEN
                  DO:
                     hReplacedBuf:BUFFER-RELEASE().
                     DELETE OBJECT hReplacedBuf.
                  END.
                  fLog("Skipped existing " + pcTableName + " with " + 
                       cCondition).
                  fLogValues(httTableBuf).
               END.
            END.
         END.
      END.
      httTableBuf:BUFFER-DELETE().
   END.

   INPUT STREAM sData CLOSE.
   EMPTY TEMP-TABLE ttFieldNames.
   RETURN TRUE.
END.

IF fCheckSelectionOptionErrors() THEN RETURN.

cMobSubFileNamePart = 
   "mobsubs".
IF pcMsSeqFileName EQ "" THEN
   cMobSubFileNamePart = cMobSubFileNamePart
   + STRING(piBeginMsSeq) + "_" 
   + STRING(piEndMsSeq) + "_" + STRING(piWantedMsSeqCount).

fAddOptionsToFileNames(). 
fFormOtherFileNamesFromMobSub(). 

IF NOT fCheckImportFileExistence() THEN RETURN.

OUTPUT STREAM sLog TO VALUE(pcLogFile).

fImportTable(fGetCharacterOption("impfileMobSub"), 
             fGetLogicalOption("ImportMobsub"), 
             fGetLogicalOption("SkipExistingMobSub"), 
             "MobSub", 
             "MsSeq;Custnum,BillTarget,CLI;CLI", 
             fGetLogicalOption("OldFormatMobsub")).
fImportTable(fGetCharacterOption("impfileMsOwner"), 
             fGetLogicalOption("ImportMsOwner"), 
             fGetLogicalOption("SkipExistingMsOwner"),
             "MsOwner", 
             "Brand,CLI,TsEnd", 
             fGetLogicalOption("OldFormatMsOwner")).
fImportTable(fGetCharacterOption("impfileDCCLI"), 
             fGetLogicalOption("ImportDCCLI"), 
             fGetLogicalOption("SkipExistingDCCLI"),
             "DCCLI", 
             "DCEvent,MsSeq,ValidTo", 
             fGetLogicalOption("OldFormarDCCLI")).
fImportTable(fGetCharacterOption("impfileDCCounter"), 
             fGetLogicalOption("ImportDCCounter"), 
             fGetLogicalOption("SkipExistingDCCounter"),
             "DCCounter", 
             "MsSeq,DCDate,DCTarget,DCEvent", 
             fGetLogicalOption("OldFormatDCCounter")).
fImportTable(fGetCharacterOption("impfileMsRequest"), 
             fGetLogicalOption("ImportMsRequest"), 
             fGetLogicalOption("SkipExistingMsRequest"),
             "MsRequest", 
             "MsRequest", 
             fGetLogicalOption("OldFormatMsRequest")).
fImportTable(fGetCharacterOption("impfileCustomer"), 
             fGetLogicalOption("ImportCustomer"), 
             fGetLogicalOption("SkipExistingCustomer"),
             "Customer", 
             "CustNum", 
             fGetLogicalOption("OldFormatCustomer")).
fImportTable(fGetCharacterOption("impfileInvoice"), 
             fGetLogicalOption("ImportInvoice"), 
             fGetLogicalOption("SkipExistingInvoice"),
             "Invoice", 
             "InvNum", 
             fGetLogicalOption("OldFormatInvoice")).
fImportTable(fGetCharacterOption("impfileInvRow"), 
             fGetLogicalOption("ImportInvRow"), 
             fGetLogicalOption("SkipExistingInvRow"),
             "InvRow", 
             "InvNum,InvRowNum", 
             fGetLogicalOption("OldFormatInvRow")).

fImportTable(fGetCharacterOption("impfileInvSeq"), 
             fGetLogicalOption("ImportInvSeq"), 
             fGetLogicalOption("SkipExistingInvSeq"),
             "InvSeq", 
             "InvSeq", 
             fGetLogicalOption("OldFormatInvSeq")).
             
fImportTable(fGetCharacterOption("impfileMnpProcess"), 
             fGetLogicalOption("ImportMnpProcess"), 
             fGetLogicalOption("SkipExistingMnpProcess"),
             "MnpProcess", 
             "MNPSeq", 
             fGetLogicalOption("OldFormatMnpProcess")).
fImportTable(fGetCharacterOption("impfileMnpMessage"), 
             fGetLogicalOption("ImportMnpMessage"), 
             fGetLogicalOption("SkipExistingMnpMessage"),
             "MnpMessage", 
             "MnpSeq,Sender,StatusCode", 
             fGetLogicalOption("OldFormatMnpMessage")).
fImportTable(fGetCharacterOption("impfileSubSer"), 
             fGetLogicalOption("ImportSubSer"), 
             fGetLogicalOption("SkipExistingSubSer"),
             "SubSer", 
             "MsSeq,ServCom,SSDate", 
             fGetLogicalOption("OldFormatSubSer")).
fImportTable(fGetCharacterOption("impfileSubSerPara"), 
             fGetLogicalOption("ImportSubSerPara"), 
             fGetLogicalOption("SkipExistingSubSerPara"),
             "SubSerPara", 
             "MsSeq,ServCom,ParaName,SSDate", 
             fGetLogicalOption("OldFormatSubSerPara")).
fImportTable(fGetCharacterOption("impfileOrder"), 
             fGetLogicalOption("ImportOrder"), 
             fGetLogicalOption("SkipExistingOrder"),
             "Order", 
             "Brand,OrderId", 
             fGetLogicalOption("OldFormatOrder")).
fImportTable(fGetCharacterOption("impfileOrderCustomer"), 
             fGetLogicalOption("ImportOrderCustomer"), 
             fGetLogicalOption("SkipExistingOrderCustomer"),
             "OrderCustomer", 
             "Brand,OrderId,RowType", 
             fGetLogicalOption("OldFormatOrderCustomer")).
fImportTable(fGetCharacterOption("impfileOrderTopup"), 
             fGetLogicalOption("ImportOrderTopup"), 
             fGetLogicalOption("SkipExistingOrderTopup"),
             "OrderTopup", 
             "Brand,OrderId", 
             fGetLogicalOption("OldFormatOrderTopup")).
fImportTable(fGetCharacterOption("impfileOrderAccessory"), 
             fGetLogicalOption("ImportOrderAccessory"), 
             fGetLogicalOption("SkipExistingOrderAccessory"),
             "OrderAccessory", 
             "Brand,OrderId", 
             fGetLogicalOption("OldFormatOrderAccessory")).
fImportTable(fGetCharacterOption("impfileOrderPayment"), 
             fGetLogicalOption("ImportOrderPayment"), 
             fGetLogicalOption("SkipExistingOrderPayment"), 
             "OrderPayment", 
             "Brand,OrderId", 
             fGetLogicalOption("OldFormatOrderPayment")).
 
fImportTable(fGetCharacterOption("impfileSIM"), 
             fGetLogicalOption("ImportSIM"), 
             fGetLogicalOption("SkipExistingSIM"),
             "SIM", 
             "ICC", 
             fGetLogicalOption("OldFormatSIM")).
fImportTable(fGetCharacterOption("impfileMemo"), 
             fGetLogicalOption("ImportMemo"), 
             fGetLogicalOption("SkipExistingMemo"),
             "Memo", 
             "MemoSeq", 
             fGetLogicalOption("OldFormatMemo")).
fImportTable(fGetCharacterOption("impfileMSISDN"), 
             fGetLogicalOption("ImportMSISDN"), 
             fGetLogicalOption("SkipExistingMSISDN"),
             "MSISDN", 
             "Brand,CLI,ValidFrom", 
             fGetLogicalOption("OldFormatMSISDN")).
fImportTable(fGetCharacterOption("impfilePrepaidRequest"), 
             fGetLogicalOption("ImportPrepaidRequest"), 
             fGetLogicalOption("SkipExistingPrepaidRequest"),
             "PrepaidRequest", 
             "Brand,PPRequest", 
             fGetLogicalOption("OldFormatPrepaidRequest")). 

fImportTable(fGetCharacterOption("impfileInvASub"), 
             fGetLogicalOption("ImportInvASub"), 
             fGetLogicalOption("SkipExistingInvASub"),
             "InvASub", 
             "InvNum,CLI,CCN,BillCode", 
             fGetLogicalOption("OldFormatInvASub")). 
fImportTable(fGetCharacterOption("impfileMnpSub"), 
             fGetLogicalOption("ImportMnpSub"), 
             fGetLogicalOption("SkipExistingMnpSub"),
             "MnpSub", 
             "MnpSeq", 
             fGetLogicalOption("OldFormatMnpSub")).
fImportTable(fGetCharacterOption("impfileLimit"), 
             fGetLogicalOption("ImportLimit"), 
             fGetLogicalOption("SkipExistingLimit"),
             "Limit", 
             "MsSeq,LimitType,TMRuleSeq,ToDate,CustNum", 
             fGetLogicalOption("OldFormatLimit")).

fImportTable(fGetCharacterOption("impfileOrderDelivery"), 
             fGetLogicalOption("ImportOrderDelivery"),
             fGetLogicalOption("SkipExistingOrderDelivery"),
             "OrderDelivery", 
             "Brand,OrderId,LOTimeStamp", 
             fGetLogicalOption("OldFormatOrderDelivery")).
fImportTable(fGetCharacterOption("impfileOrderTimeStamp"), 
             fGetLogicalOption("ImportOrderTimeStamp"),
             fGetLogicalOption("SkipExistingOrderTimeStamp"),
             "OrderTimeStamp", 
             "Brand,OrderId,RowType", 
             fGetLogicalOption("OldFormatOrderTimeStamp")).
fImportTable(fGetCharacterOption("impfileIMSI"), 
             fGetLogicalOption("ImportIMSI"),
             fGetLogicalOption("SkipExistingIMSI"),
             "IMSI",
             "IMSI", 
             fGetLogicalOption("OldFormatIMSI")).
fImportTable(fGetCharacterOption("impfileFaTime"), 
             fGetLogicalOption("ImportFaTime"),
             fGetLogicalOption("SkipExistingFaTime"),
             "FaTime", 
             "Brand,FatNum", 
             fGetLogicalOption("OldFormatFaTime")).
fImportTable(fGetCharacterOption("impfileCallAlarm"), 
             fGetLogicalOption("ImportCallAlarm"),
             fGetLogicalOption("SkipExistingCallAlarm"),
             "CallAlarm", 
             "Brand,CLI,ActStamp,DeliMsg", 
             fGetLogicalOption("OldFormatCallAlarm")).
fImportTable(fGetCharacterOption("impfileMSISDNNumber"), 
             fGetLogicalOption("ImportMSISDNNumber"),
             fGetLogicalOption("SkipExistingMSISDNNumber"), 
             "MSISDNNumber", 
             "CLI", 
             fGetLogicalOption("OldFormatMSISDNNumber")).
fImportTable(fGetCharacterOption("impfileMobCDR"), 
             fGetLogicalOption("ImportMobCDR"),
             fGetLogicalOption("SkipExistingMobCDR"), 
             "MobCDR", 
             "CLI,DateSt,TimeStart", 
             fGetLogicalOption("OldFormatMobCDR")).
fImportTable(fGetCharacterOption("impfileMCDRDtl2"), 
             fGetLogicalOption("ImportMCDRDtl2"),
             fGetLogicalOption("SkipExistingMCDRDtl2"), 
             "MCDRDtl2", 
             "DateSt,DtlSeq", 
             fGetLogicalOption("OldFormatMCDRDtl2")).
fImportTable(fGetCharacterOption("impfilePrepCDR"), 
             fGetLogicalOption("ImportPrepCDR"),
             fGetLogicalOption("SkipExistingPrepCDR"), 
             "PrepCDR", 
             "CLI,DateSt,TimeStart", 
             fGetLogicalOption("OldFormatPrepCDR")).

fImportTable(fGetCharacterOption("impfileCOTarg"), 
             fGetLogicalOption("ImportCOTarg"),
             fGetLogicalOption("SkipExistingCOTarg"), 
             "COTarg", 
             "COTargId", 
             fGetLogicalOption("OldFormatCOTarg")).
fImportTable(fGetCharacterOption("impfileTMCounter"), 
             fGetLogicalOption("ImportTMCounter"),
             fGetLogicalOption("SkipExistingTMCounter"), 
             "TMCounter", 
             "MsSeq,TMRuleSeq,ToDate", 
             fGetLogicalOption("OldFormatTMCounter")).


OUTPUT STREAM sLog CLOSE.
