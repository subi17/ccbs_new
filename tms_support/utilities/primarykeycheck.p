DEFINE VARIABLE lDispTableNames AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lSkipSysBeginningTbl AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lSkip_beginningTbl AS LOGICAL NO-UNDO. 

lSkipSysBeginningTbl = TRUE.
lSkip_beginningTbl = TRUE.

DEFINE VARIABLE cTableListFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cTable AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cIndexFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lEventLogExistsCheck AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cFileNotProcTbl AS CHARACTER NO-UNDO. 
lEventLogExistsCheck = TRUE.

cTableListFile = "".
cIndexFile = "/home/harrim/indexes_sel.txt".
cFileNotProcTbl = "/home/harrim/not_poc_tbl.txt".

DEFINE TEMP-TABLE ttTables
   FIELD cTableName AS CHARACTER
   INDEX idxTable cTableName.

DEFINE STREAM sIndex.


DEFINE STREAM sNotProcTbl.
IF cFileNotProcTbl NE "" THEN
   OUTPUT STREAM sNotProcTbl TO VALUE (cFileNotProcTbl).

FUNCTION fAddTable RETURN LOGICAL (INPUT pcTable AS CHARACTER):
   IF NOT CAN-FIND(FIRST ttTables WHERE ttTables.cTableName = pcTable) THEN
   DO:
      CREATE ttTables.
      ttTables.cTableName = pcTable.
   END.
   RETURN TRUE.
END.

FUNCTION fCheckTable RETURN LOGICAL (INPUT pcTable AS CHARACTER):
   IF lEventLogExistsCheck THEN

   DO:
      IF NOT(CAN-FIND(FIRST EventLog WHERE 
          EventLog.TableName = pcTable)) THEN RETURN FALSE.
   END.
   IF cTableListFile EQ "" THEN RETURN TRUE.
   IF NOT CAN-FIND(FIRST ttTables WHERE ttTables.cTableName = pcTable) THEN
      RETURN FALSE.
   RETURN TRUE.
END.


DEFINE STREAM sTable.
IF cTableListFile NE "" THEN
DO:
  INPUT STREAM sTable FROM VALUE(cTableListFile).
  REPEAT:
     IMPORT STREAM sTable UNFORMATTED cTable.
     fAddTable(cTable).
  END.
  INPUT STREAM sTable CLOSE.
END.

OUTPUT STREAM sIndex TO VALUE(cIndexFile).
DEFINE FRAME X.

FUNCTION fParseIndexInfToReadable RETURN LOGICAL 
     (INPUT  pcIndexInf    AS CHARACTER,
      OUTPUT pcIndexName   AS CHARACTER,
      OUTPUT pcIndexFields AS CHARACTER,
      OUTPUT plUnique      AS LOGICAL  ,
      OUTPUT plPrimary     AS LOGICAL):
     
   DEFINE VARIABLE iEntry        AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE cFieldName    AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lAsc          AS LOGICAL   NO-UNDO. 
   DEFINE VARIABLE iNumEntries   AS INTEGER   NO-UNDO. 

   plUnique = FALSE.
   plPrimary = FALSE.
   iNumEntries = NUM-ENTRIES(pcIndexInf).

   DEFINE VARIABLE cIndexName AS CHARACTER NO-UNDO. 
   pcIndexName = ENTRY(1, pcIndexInf).
   IF ENTRY(2, pcIndexInf) EQ "1" THEN plUnique  = TRUE.
   IF ENTRY(3, pcIndexInf) EQ "1" THEN plPrimary = TRUE.

   DEFINE VARIABLE cIndexFields AS CHARACTER NO-UNDO.  
   REPEAT iEntry = 5 TO iNumEntries - 1 BY 2:
     cFieldName = ENTRY(iEntry, pcIndexInf).
     lAsc = FALSE.

     IF ENTRY(iEntry + 1, pcIndexInf) = "0" THEN lAsc = TRUE.

     pcIndexFields = pcIndexFields + cFieldName.
     IF lAsc THEN 
        pcIndexFields = pcIndexFields + " (ASC) ".
     ELSE
        pcIndexFields = pcIndexFields + " (DESC) ".
   END.


   RETURN TRUE.    
END.


FUNCTION fCheckIndexOfTableBuf RETURN LOGICAL 
   (INPUT phBuf AS HANDLE):

   DEFINE VARIABLE iOfIndex     AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cIndexname   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cIndexInf    AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cIndexFields AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lUnique      AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE lPrimary     AS LOGICAL NO-UNDO. 

   iOfIndex = 1.
   LoopIndexes:
   REPEAT:
      cIndexInf = phBuf:INDEX-INFORMATION(iOfIndex).
      IF cIndexInf EQ ? THEN LEAVE LoopIndexes.
      fParseIndexInfToReadable(cIndexInf,
         OUTPUT cIndexName, OUTPUT cIndexFields, 
         OUTPUT lUnique   , OUTPUT lPrimary).

      IF lPrimary THEN
      DO:
         PUT STREAM sIndex UNFORMATTED "Primary index: " cIndexName
           ", Fields: " cIndexFields SKIP.
      END.
      IF lUnique THEN
      DO:
         PUT STREAM sIndex UNFORMATTED "Unique index: " cIndexName
           ", Fields: " cIndexFields SKIP.
      END.
      iOfIndex = iOfIndex + 1.
   END.

   RETURN TRUE.
END.


FUNCTION fProcessTable RETURN LOGICAL
   (INPUT pcDbName AS CHARACTER, INPUT pcTableName AS CHARACTER):
   DEFINE VARIABLE hBuf AS HANDLE NO-UNDO. 

   IF pcTableName BEGINS "SYS" AND lSkipSysBeginningTbl THEN RETURN TRUE.
   IF pcTableName BEGINS "_" AND lSkip_beginningTbl THEN RETURN TRUE.
   IF NOT fCheckTable(pcTableName) THEN 
   DO:
      IF cFileNotProcTbl NE "" THEN
         PUT STREAM sNotProcTbl UNFORMATTED pcTableName SKIP.
      RETURN TRUE.
   END.

   CREATE BUFFER hBuf FOR TABLE pcTableName.
   PUT STREAM sIndex UNFORMATTED "Table: " pcTableName SKIP.
   PUT STREAM sIndex UNFORMATTED "--------------------------------------" SKIP.
   fCheckIndexOfTableBuf(hBuf).
   PUT STREAM sIndex UNFORMATTED SKIP(2).

   IF lDispTableNames THEN
      DISP pcTableName FORMAT "X(40)" WITH FRAME A 30 down.
      down 1 with FRAME a.
END.


FUNCTION fHandleAllTables RETURN LOGICAL:
   FOR EACH common._FILE NO-LOCK:
      fProcessTable("common", common._FILE._File-name).
   END.
   
   FOR EACH mobile._FILE NO-LOCK:
      fProcessTable("mobile", mobile._FILE._File-name).
   END.
   
   FOR EACH mcdr._FILE NO-LOCK:
      fProcessTable("mcdr", mcdr._FILE._File-name).
   END.
   
   FOR EACH mcdrdtl._FILE NO-LOCK:
      fProcessTable("mcdrdtl", mcdrdtl._FILE._File-name).
   END.
   
   FOR EACH prepcdr._FILE NO-LOCK:
      fProcessTable("prepcdr", prepcdr._FILE._File-name).
   END.
   
   FOR EACH roamcdr._FILE NO-LOCK:
      fProcessTable("roamcdr", roamcdr._FILE._File-name).
   END.

   FOR EACH ordercanal._FILE NO-LOCK:
      fProcessTable("ordercanal", ordercanal._FILE._File-name).
   END.

   RETURN TRUE.
END.

fHandleAllTables().

OUTPUT STREAM sIndex CLOSE.
IF cFileNotProcTbl NE "" THEN
   OUTPUT STREAM sNotProcTbl CLOSE.
