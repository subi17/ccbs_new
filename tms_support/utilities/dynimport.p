DEFINE INPUT PARAMETER pcDir AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcTableName AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER piDispInterval AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER plEmptyTableFirst AS LOGICAL NO-UNDO. 
/* DEFINE INPUT PARAMETER plReplace AS LOGICAL NO-UNDO. 
 FALSE means skip the existing; future development idea;
 now clean the whole table before creating new records */

DEFINE VARIABLE cXMLFile AS CHARACTER NO-UNDO. 

DEFINE VARIABLE hTable AS HANDLE NO-UNDO. 

DEFINE VARIABLE cFile AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lImportXML AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lCreateDynTT AS LOGICAL NO-UNDO. 


DEFINE VARIABLE cBeginRecTag AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cEndRecTag AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cFieldSep AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lInRecord AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lRecordDataReady AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cRecordData AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iBeginTag AS INTEGER NO-UNDO. 
DEFINE VARIABLE iEndTag   AS INTEGER NO-UNDO. 


cFile = pcDir + "/dynexport_" + pcTableName + ".txt".
cXMLFile = pcDir + "/dynexport_" + pcTableName + ".xml".


lImportXML = FALSE.
lCreateDynTT = FALSE.

IF lImportXML THEN 
  lCreateDynTT = TRUE.

FUNCTION fEmptyTable RETURN LOGICAL:
   DEFINE VARIABLE hQuery AS HANDLE NO-UNDO. 
   CREATE QUERY hQuery.
   hQuery:SET-BUFFERS(hTable).
   hQuery:QUERY-PREPARE("FOR EACH " + pcTableName + " EXCLUSIVE-LOCK").
   DO TRANSACTION:
      hQuery:QUERY-OPEN.
      hQuery:GET-FIRST().

      loop:
      REPEAT:
        IF hQuery:QUERY-OFF-END THEN LEAVE loop.
        hTable:BUFFER-DELETE().
        hQuery:GET-NEXT().
      END.
      hQuery:QUERY-CLOSE().
            
   END.
   
END.


CREATE BUFFER hTable FOR TABLE pcTableName.

IF plEmptyTableFirst THEN
  fEmptyTable().


FUNCTION fSetValue RETURN LOGICAL
  (INPUT phField AS HANDLE, INPUT pcValue AS CHARACTER):

  CASE phField:DATA-TYPE:
      WHEN "character" THEN phField:BUFFER-VALUE = pcValue.
      WHEN "integer" THEN phField:BUFFER-VALUE = INTEGER(pcValue).
      WHEN "logical" THEN phField:BUFFER-VALUE = LOGICAL(pcValue).
      WHEN "decimal" THEN phField:BUFFER-VALUE = DECIMAL(pcValue).
      WHEN "date"    THEN phField:BUFFER-VALUE = DATE(pcValue).
  END.
END.


FUNCTION fSetExtentValue RETURN LOGICAL
  (INPUT phField AS HANDLE, INPUT piExtent AS INTEGER, INPUT pcValue AS CHARACTER):

  CASE phField:DATA-TYPE:
      WHEN "character" THEN phField:BUFFER-VALUE(piExtent) = pcValue.
      WHEN "integer" THEN phField:BUFFER-VALUE(piExtent) = INTEGER(pcValue).
      WHEN "logical" THEN phField:BUFFER-VALUE(piExtent) = LOGICAL(pcValue).
      WHEN "decimal" THEN phField:BUFFER-VALUE(piExtent) = DECIMAL(pcValue).
      WHEN "date"    THEN phField:BUFFER-VALUE(piExtent) = DATE(pcValue).
  END.
END.

DEFINE TEMP-TABLE ttFieldNames
   FIELD cFieldName AS CHARACTER
   INDEX idxFieldName cFieldName.

DEFINE TEMP-TABLE ttLackingFieldNames
   FIELD cFieldName AS CHARACTER
   INDEX idxFName cFieldName.

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




FUNCTION fAddLackingFieldName RETURN LOGICAL
   (INPUT pcNewLackingField AS CHARACTER):
   IF NOT CAN-FIND(ttLackingFieldNames WHERE 
       ttLackingFieldNames.cFieldName = pcNewLackingField) THEN
   DO:
      CREATE ttLackingFieldNames.
      ASSIGN ttLackingFieldNames.cFieldName = pcNewLackingField.
   END.
END.


FUNCTION fFieldExistsInDb RETURN LOGICAL (INPUT pcFieldName AS CHARACTER,
     INPUT plFirst AS LOGICAL):
    IF CAN-FIND(ttFieldNames WHERE ttFieldNames.cFieldName = pcFieldName) 
       THEN RETURN TRUE.
    IF plFirst THEN fAddLackingFieldName(pcFieldName). 
    RETURN FALSE.
END.

DEFINE STREAM sData.

FUNCTION fInputFields RETURN LOGICAL
  (INPUT pcRecordData AS CHARACTER, INPUT plFirst AS LOGICAL):
  DEFINE VARIABLE iField AS INTEGER NO-UNDO. 
  DEFINE VARIABLE hField AS HANDLE NO-UNDO. 
  DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE cFieldValue AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE iExtent AS INTEGER NO-UNDO. 
  DEFINE VARIABLE iEntry AS INTEGER NO-UNDO.

  REPEAT iEntry = 1 TO NUM-ENTRIES(pcRecordData, cFieldSep) - 1 BY 2:
     cFieldName = ENTRY(iEntry, pcRecordData, cFieldSep).
     cFieldValue = ENTRY(iEntry + 1, pcRecordData, cFieldSep).

     IF plEmptyTableFirst THEN
     DO:
       IF INDEX(cFieldName, ",") > 0 THEN
       DO:
          iExtent = INTEGER(ENTRY(2, cFieldName, ",")).
          cFieldName = TRIM(ENTRY(1, cFieldName, ",")).
          IF fFieldExistsInDb(cFieldName, plFirst) THEN
          DO:
             hField = hTable:BUFFER-FIELD(cFieldName).
             fSetExtentValue(hField, iExtent, cFieldValue).
          END.
       END.
       ELSE 
       DO:
          IF fFieldExistsInDb(cFieldName, plFirst) THEN
          DO:
             hField = hTable:BUFFER-FIELD(cFieldName).
             fSetValue(hField, cFieldValue).
          END.
       END.
     END.
  END.
END.




DEFINE VARIABLE iCount AS INTEGER NO-UNDO. 
iCount = 0.



IF NOT lImportXML THEN 
    INPUT STREAM sData FROM VALUE(cFile).

DEFINE VARIABLE cLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lFirst AS LOGICAL NO-UNDO. 
lFirst = TRUE.



IF NOT lImportXML THEN
DO:
   IMPORT STREAM sData UNFORMATTED cBeginRecTag.
   IMPORT STREAM sData UNFORMATTED cEndRecTag.
   IMPORT STREAM sData UNFORMATTED cFieldSep.
   cFieldSep = SUBSTRING(cFieldSep,1,1).
END.

lInRecord = FALSE.

DEFINE VARIABLE iRow AS INTEGER NO-UNDO. 

fGetFieldNames(hTable).


FUNCTION fGetOneRecordText RETURN CHARACTER:
   DEFINE VARIABLE lInRecord AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE lFirstRow AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE cRecordData AS CHARACTER NO-UNDO.

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


DEFINE VARIABLE cOneRecordData AS CHARACTER NO-UNDO. 

DEFINE STREAM sRecords.
OUTPUT STREAM sRecords TO VALUE ("records.txt").

FUNCTION fOutputRecord RETURN LOGICAL (INPUT piCount AS INTEGER, INPUT pcRecordData AS CHARACTER):
   PUT STREAM sRecords UNFORMATTED "-------------------------------------------------" SKIP.
   PUT STREAM sRecords UNFORMATTED "Record : " piCount SKIP.
   PUT STREAM sRecords UNFORMATTED "-------------------------------------------------" SKIP.

   PUT STREAM sRecords UNFORMATTED pcRecordData SKIP.

   PUT STREAM sRecords UNFORMATTED "-------------------------------------------------" SKIP.
   RETURN TRUE.
END.


loop:
REPEAT:
  /* Just create the record, because the record certainly does not exist. */

  DO TRANSACTION:
     cOneRecordData = fGetOneRecordText().
     IF cOneRecordData eq ? THEN LEAVE loop.
     hTable:BUFFER-CREATE().
     fInputFields(cOneRecordData, lFirst). 
     lFirst = FALSE.
  END.
  iCount = iCount + 1.

  IF piDispInterval > 0 THEN
    IF iCount MOD piDispInterval = 0 THEN  DISP iCount.
  iRow = iRow + 1.
END.

OUTPUT STREAM sRecords CLOSE.


DELETE OBJECT hTable.

DEFINE STREAM sLackFields.
FIND FIRST ttLackingFieldNames NO-LOCK NO-ERROR.
IF AVAIL ttLackingFieldNames THEN
DO:
   OUTPUT STREAM sLackFields TO VALUE(pcDir + "/" + pcTableName + 
        "_lacked_fields.txt").
   FOR EACH ttLackingFieldNames NO-LOCK:
      PUT STREAM sLackFields UNFORMATTED ttLackingFieldNames.cFieldName SKIP.
   END.

   OUTPUT STREAM sLackFields CLOSE.


END.
