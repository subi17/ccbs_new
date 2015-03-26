DEFINE VARIABLE lGetCounts AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lGetFields AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lDispTableNames AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lDispCounts AS LOGICAL NO-UNDO.

DEFINE VARIABLE lSkipSysBeginningTbl AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lSkip_beginningTbl AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lHTML AS LOGICAL NO-UNDO.
DEFINE VARIABLE lLuna AS LOGICAL NO-UNDO. 

DEFINE VARIABLE iLimitCounts AS INTEGER NO-UNDO. 
DEFINE VARIABLE lGenerateTableList AS LOGICAL NO-UNDO. 

DEFINE VARIABLE lTableListInFile AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cTableListFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lUseNumResultAttrOfQuery AS LOGICAL NO-UNDO. 

DEFINE VARIABLE lUseAnchor AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lUseManyFiles AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lFilesByDatabase AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lFilesByAlpCat AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lFilesByDbAndAlpCat AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lBeginWithEndCatAcc AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cOutputDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cHeader AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cFieldsFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cTableListFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCountsFile AS CHARACTER NO-UNDO. 
DEF VAR lcRootDir AS CHAR NO-UNDO. 

lcRootDir = "/home/anttis/".

cHeader = "Hierarchical dump".



cFieldsFile = lcRootDir + "hierarchfields.txt". 
cOutputDir = lcRootDir + "tabledesc/".
lGetFields = TRUE.
lUseAnchor = TRUE.
lUseManyFiles = TRUE.
lFilesByDatabase = TRUE.
lFilesByAlpCat = TRUE.
lFilesByDbAndAlpCat = TRUE.
lBeginWithEndCatAcc = TRUE.

lUseNumResultAttrOfQuery = FALSE.

lTableListInFile = TRUE.
cTableListFileName = lcRootDir + "hierarchical_dump_tables.txt".

iLimitCounts = 1000.

lLuna = TRUE.
lHTML = FALSE.
lGetCounts = FALSE.
lGenerateTableList = TRUE.
lDispTableNames = TRUE.
lSkipSysBeginningTbl = TRUE.
lSkip_beginningTbl = TRUE.

IF lUseManyFiles AND lGetCounts THEN
DO:
   MESSAGE "Do not count records when creating multiple files"
     VIEW-AS ALERT-BOX.
   RETURN.
END.


DEFINE STREAM sTables.
DEFINE STREAM sFields.
DEFINE STREAM sCounts.

DEFINE TEMP-TABLE ttTables
   FIELD cDatabase AS CHARACTER
   FIELD cTable    AS CHARACTER 
   INDEX dbIdx     cDatabase cTable
   INDEX tableIdx  cTable cDatabase.

DEFINE TEMP-TABLE ttDatabases
   FIELD cDatabase AS CHARACTER
   INDEX dbIdx cDatabase.

DEFINE TEMP-TABLE ttAlphaCat
   FIELD cBeginLimit AS CHARACTER
   FIELD cEndLimit AS CHARACTER
   INDEX idxACat cBeginLimit cEndLimit.


DEFINE TEMP-TABLE ttLinkList
   FIELD cDbName AS CHARACTER
   FIELD cBeginLimit AS CHARACTER
   FIELD cEndLimit AS CHARACTER
   INDEX idxLinkList cDbName cBeginLimit cEndLimit.


DEFINE TEMP-TABLE ttTableLinks
   FIELD cTable AS CHARACTER 
   FIELD cLinkText AS CHARACTER
   INDEX idxTable cTable.


FUNCTION fAddLinks RETURN LOGICAL (INPUT pcDbName AS CHARACTER,
   INPUT pcBeginLimit AS CHARACTER, INPUT pcEndLimit AS CHARACTER):
    IF NOT CAN-FIND(ttLinkList WHERE ttLinkList.cDbName = pcDbName AND
         ttLinkList.cBeginLimit = pcBeginLimit AND
         ttLinkList.cEndLimit = pcEndLimit) THEN
    DO:
       CREATE ttLinkList.
       ASSIGN ttLinkList.cDbName = pcDbName
              ttLinkList.cBeginLimit = pcBeginLimit
              ttLinkList.cEndLimit = pcEndLimit.
       RETURN TRUE.
    END.
    RETURN FALSE.
END.




FUNCTION fAddTableLink RETURN LOGICAL (INPUT pcTableName AS CHARACTER,
   INPUT pcLinkText AS CHARACTER):
   IF pcTableName BEGINS "SYS" AND lSkipSysBeginningTbl THEN RETURN TRUE.
   IF pcTableName BEGINS "_" AND lSkip_beginningTbl THEN RETURN TRUE.
    IF NOT CAN-FIND(ttTableLinks WHERE ttTableLinks.cTable = pcTableName) THEN
    DO:
       CREATE ttTableLinks.
       ASSIGN ttTableLinks.cTable = pcTableName
              ttTableLinks.cLinkText = pcLinkText.
       RETURN TRUE.
    END.
    RETURN FALSE.
END.


FUNCTION fAddDatabaseToTT RETURN LOGICAL (INPUT pcDbName AS CHARACTER):
    IF NOT CAN-FIND(ttDatabases WHERE ttDatabases.cDatabase = pcDbName) THEN
    DO:
       CREATE ttDatabases.
       ttDatabases.cDatabase = pcDbName.
       RETURN TRUE.
    END.
    RETURN FALSE.
END.



FUNCTION fAddTableToTT RETURN LOGICAL (INPUT pcDbName AS CHARACTER,
    INPUT pcTableName AS CHARACTER):
   IF pcTableName BEGINS "SYS" AND lSkipSysBeginningTbl THEN RETURN TRUE.
   IF pcTableName BEGINS "_" AND lSkip_beginningTbl THEN RETURN TRUE.
   IF NOT CAN-FIND(ttTables WHERE ttTables.cDatabase = pcDbName AND
      ttTables.cTable = pcTableName) THEN
   DO:
      CREATE ttTables.
      ttTables.cDatabase = pcDbName.
      ttTables.cTable    = pcTableName.
      RETURN TRUE.
   END.
   RETURN FALSE.
END.


FUNCTION fAddAlphaCat RETURN LOGICAL 
   (INPUT pcBeginLimit AS CHARACTER, INPUT pcEndLimit AS CHARACTER) :
   
   IF NOT CAN-FIND(ttAlphaCat WHERE ttAlphaCat.cBeginLimit = pcBeginLimit AND
      ttAlphaCat.cEndLimit = pcEndLimit) THEN
   DO:
      CREATE ttAlphaCat.
      ttAlphaCat.cBeginLimit = pcBeginLimit.
      ttAlphaCat.cEndLimit   = pcEndLimit.
      RETURN TRUE.
   END.
   RETURN FALSE.
END.



IF cCountsFile EQ "" THEN
DO:
   cCountsFile = lcRootDir + "counts" + STRING(iLimitCounts).
   IF lUseNumResultAttrOfQuery THEN
      cCountsFile = cCountsFile + "_used_query_attr.txt".
   ELSE
      cCountsFile = cCountsFile + ".txt".
END.
IF cFieldsFile EQ "" THEN
   cFieldsFile = lcRootDir + "fields.txt".
IF cTableListFile EQ "" THEN
   cTableListFile = lcRootDir + "tables_with_counts.txt".

IF lGenerateTableList THEN
   OUTPUT STREAM sTables TO VALUE(cTableListFile).
IF lGetFields AND NOT lUseManyFiles THEN
   OUTPUT STREAM sFields TO VALUE(cFieldsFile).
IF lGetCounts THEN
   OUTPUT STREAM sCounts TO VALUE(cCountsFile).


DEFINE FRAME X.
   

FUNCTION fParseIndexInfToReadable RETURN CHARACTER 
     (INPUT pcIndexInf AS CHARACTER, INPUT pcBeginString AS CHARACTER,
      INPUT pcColumnSep AS CHARACTER, INPUT pcEndString AS CHARACTER):
     DEFINE VARIABLE cRetIndexInfo AS CHARACTER NO-UNDO. 
     DEFINE VARIABLE lUnique AS LOGICAL NO-UNDO. 
     DEFINE VARIABLE lPrimary AS LOGICAL NO-UNDO.
     DEFINE VARIABLE lWordIndex AS LOGICAL NO-UNDO. 
     DEFINE VARIABLE iEntry AS INTEGER NO-UNDO. 
     DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO. 
     DEFINE VARIABLE lAsc AS LOGICAL NO-UNDO. 
     DEFINE VARIABLE iNumEntries AS INTEGER NO-UNDO. 

     lUnique = FALSE.
     lPrimary = FALSE.
     lWordIndex = FALSE.

     cRetIndexInfo = "".
     iNumEntries = NUM-ENTRIES(pcIndexInf).

      /* Formatted as |Index name|Fields|Properties|" */

     /* Index name */
     cRetIndexInfo = pcBeginString + ENTRY(1, pcIndexInf) + pcColumnSep.
     IF ENTRY(2, pcIndexInf) EQ "1" THEN lUnique = TRUE.
     IF ENTRY(3, pcIndexInf) EQ "1" THEN lPrimary = TRUE.
     IF ENTRY(4, pcIndexInf) EQ "1" THEN lWordIndex = TRUE.

    
     REPEAT iEntry = 5 TO iNumEntries - 1 BY 2:
        cFieldName = ENTRY(iEntry, pcIndexInf).
        lAsc = FALSE.
        IF ENTRY(iEntry + 1, pcIndexInf) = "0" THEN lAsc = TRUE.

        IF iEntry > 5 THEN cRetIndexInfo = cRetIndexInfo + ",".
        cRetIndexInfo = cRetIndexInfo + cFieldName.
        IF lAsc THEN 
           cRetIndexInfo = cRetIndexInfo + " (ASC)".
        ELSE
           cRetIndexInfo = cRetIndexInfo + " (DESC)".
     END.

     DEFINE VARIABLE lFirstPropertyExists AS LOGICAL NO-UNDO. 
     lFirstPropertyExists = FALSE.
     cRetIndexInfo = cRetIndexInfo + pcColumnSep.
     IF lUnique THEN 
     DO:
        cRetIndexInfo = cRetIndexInfo + "Unique".
        lFirstPropertyExists = TRUE.
     END.

     IF lPrimary THEN 
     DO:
        IF lFirstPropertyExists THEN
           cRetIndexInfo = cRetIndexInfo + ", ".
        cRetIndexInfo = cRetIndexInfo + "Primary".
        lFirstPropertyExists = TRUE.
     END.

     IF lWordIndex THEN 
     DO:
        IF lFirstPropertyExists THEN
           cRetIndexInfo = cRetIndexInfo + ", ".
        cRetIndexInfo = cRetIndexInfo + "Word".
        lFirstPropertyExists = TRUE.
     END.
    
     cRetIndexInfo = cRetIndexInfo + pcEndString.       
     RETURN cRetIndexInfo.
END.


FUNCTION fGetHTMLDefinition RETURN LOGICAL (INPUT phBuf AS HANDLE):

   DEFINE VARIABLE iNumFields AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iField AS INTEGER NO-UNDO. 
   DEFINE VARIABLE hField AS HANDLE NO-UNDO. 
   iNumFields = phBuf:NUM-FIELDS.
   PUT STREAM sFields "<table class='fielddesc'>" SKIP.
   PUT STREAM sFields UNFORMATTED "<tr><th class='fielddeschdr'>Field name</th>" 
                                      "<th class='fielddeschdr'>Field type</th>"
                                      "<th class='fielddeschdr'>Extent</th>"
                                      "<th class='fielddeschdr'>Format</th>"
                                      "<th class='fielddeschdr'>Help</th>"
                                      "<th class='fielddeschdr'>Label</th>"
                                      "<th class='fielddeschdr'>Column label</th></tr>".
   REPEAT iField = 1 TO iNumFields:
      hField = phBuf:BUFFER-FIELD(iField).
      PUT STREAM sFields UNFORMATTED "<tr><td class='fieldname'>" hField:NAME "</td><td class='datatype'>" hField:DATA-TYPE 
                                          "</td><td class='extent'>" hField:EXTENT 
                                          "</td><td class='format'>" hField:FORMAT 
                                          "</td><td class='help'>" hField:HELP
                                         "</td><td class='label'>" hField:LABEL 
                                          "</td><td class='columnlabel'>" hField:COLUMN-LABEL 
                                          "</td><tr>"SKIP.
   END.
   PUT STREAM sFields UNFORMATTED "</table>" SKIP(2).

   PUT STREAM sFields "<table class='fielddesc'>" SKIP.
   PUT STREAM sFields UNFORMATTED "<tr><th class='fielddeschdr'>Index name</th>" 
                                      "<th class='fielddeschdr'>Fields</th>"
                                      "<th class='fielddeschdr'>Properties</th></tr>" SKIP.

   DEFINE VARIABLE iOfIndex AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cIndexInf AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cReadableIndexInf AS CHARACTER NO-UNDO. 
   iOfIndex = 1.
   LoopIndexes:
   REPEAT:
      cIndexInf = phBuf:INDEX-INFORMATION(iOfIndex).
      IF cIndexInf EQ ? THEN LEAVE LoopIndexes.
      cReadableIndexInf = fParseIndexInfToReadable(cIndexInf,
         "<tr><td class='fieldname'>", "</td><td class='fieldname'>", "</td></tr>").
      PUT STREAM sFields UNFORMATTED cReadableIndexInf SKIP.
      iOfIndex = iOfIndex + 1.
   END.
   PUT STREAM sFields UNFORMATTED "</table>" SKIP(2).

   RETURN TRUE.
END.


FUNCTION fGetTextDefinition RETURN LOGICAL (INPUT phBuf AS HANDLE):

   DEFINE VARIABLE iNumFields AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iField AS INTEGER NO-UNDO. 
   DEFINE VARIABLE hField AS HANDLE NO-UNDO. 
   iNumFields = phBuf:NUM-FIELDS.
   PUT STREAM sFields UNFORMATTED 
      "Field name | Field type | Extent | Format | Description | Help | "
      "Label | Column label".
   REPEAT iField = 1 TO iNumFields:
      hField = phBuf:BUFFER-FIELD(iField).
      PUT STREAM sFields UNFORMATTED hfield:NAME " | " 
          hField:DATA-TYPE " | " hField:EXTENT 
          " | " hField:FORMAT " | " 
          " | " hField:HELP " | "
          hField:LABEL " | "  hField:COLUMN-LABEL SKIP.
   END.

   PUT STREAM sFields UNFORMATTED 
       "Index name | Fields | Properties" SKIP.

   DEFINE VARIABLE iOfIndex AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cIndexInf AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cReadableIndexInf AS CHARACTER NO-UNDO. 
   iOfIndex = 1.
   LoopIndexes:
   REPEAT:
      cIndexInf = phBuf:INDEX-INFORMATION(iOfIndex).
      IF cIndexInf EQ ? THEN LEAVE LoopIndexes.
      cReadableIndexInf = fParseIndexInfToReadable(cIndexInf,
         "", " | ", "").
      PUT STREAM sFields UNFORMATTED cReadableIndexInf SKIP.
      iOfIndex = iOfIndex + 1.
   END.

   RETURN TRUE.

END.





FUNCTION fGetWikiDefinition RETURN LOGICAL (INPUT phBuf AS HANDLE):
   DEFINE VARIABLE iNumFields AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iField AS INTEGER NO-UNDO. 
   DEFINE VARIABLE hField AS HANDLE NO-UNDO. 

   iNumFields = phBuf:NUM-FIELDS.
   PUT STREAM sFields UNFORMATTED SKIP(1) 
       "||Field name||Field type||Extent||Format||"
       "Help||Label||Column label||" SKIP.

   REPEAT iField = 1 TO iNumFields:
      hField = phBuf:BUFFER-FIELD(iField).
      PUT STREAM sFields UNFORMATTED 
          "| " hField:NAME " | " hField:DATA-TYPE " | " hField:EXTENT 
          " | " hField:FORMAT " | " hField:HELP " | " hField:LABEL 
          " | " hField:COLUMN-LABEL " |" SKIP.
   END.

   PUT STREAM sFields UNFORMATTED SKIP(1) 
       "||Index name||Fields||Properties||" SKIP.

   DEFINE VARIABLE iOfIndex AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cIndexInf AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE cReadableIndexInf AS CHARACTER NO-UNDO. 
   iOfIndex = 1.
   LoopIndexes:
   REPEAT:
      cIndexInf = phBuf:INDEX-INFORMATION(iOfIndex).
      IF cIndexInf EQ ? THEN LEAVE LoopIndexes.
      cReadableIndexInf = fParseIndexInfToReadable(cIndexInf,
         "| ", " | ", " |").

      PUT STREAM sFields UNFORMATTED cReadableIndexInf SKIP.
      iOfIndex = iOfIndex + 1.
   END.
   PUT STREAM sFields UNFORMATTED SKIP(1).

   RETURN TRUE.
END.

DEFINE FRAME A.

FUNCTION fProcessTable RETURN LOGICAL(INPUT pcDbName AS CHARACTER, INPUT pcTableName AS CHARACTER):
   DEFINE VARIABLE hBuf AS HANDLE NO-UNDO. 
   DEFINE VARIABLE hQuery AS HANDLE NO-UNDO. 
   DEFINE VARIABLE cQuery AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lCountLimitExceeded AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE cAnchorText AS CHARACTER NO-UNDO. 
   cAnchorText = "".
   IF lUseAnchor THEN
      cAnchorText = "\{anchor:" + pcTableName + "\} ".

   lCountLimitExceeded = FALSE.

   IF pcTableName BEGINS "SYS" AND lSkipSysBeginningTbl THEN RETURN TRUE.
   IF pcTableName BEGINS "_" AND lSkip_beginningTbl THEN RETURN TRUE.

   IF lGenerateTableList THEN 
      PUT STREAM sTables UNFORMATTED pcTableName SKIP.
   IF lGetFields OR lGetCounts THEN
   DO:
      IF lGetFields THEN
      DO:
         DEFINE VARIABLE cDbDesc AS CHARACTER NO-UNDO. 
         IF lhtml THEN
         DO:
           IF pcDbName EQ "" THEN cDbDesc = "". 
           ELSE cDbDesc = " in database" + pcDbName.
           PUT STREAM sFields UNFORMATTED "<h1>" pcTableName 
              cDbDesc "</h1>" SKIP.
         END.
         ELSE IF NOT lLuna THEN
         DO:
           IF pcDbName EQ "" THEN cDbDesc = "". 
           ELSE cDbDesc = " in database" + pcDbName.
           PUT STREAM sFields UNFORMATTED 
              pcTableName cDbDesc ":" SKIP.
         END.
         ELSE 
         DO:
           IF pcDbName EQ "" THEN cDbDesc = "". 
           ELSE cDbDesc = pcDbName + ".".

           PUT STREAM sFields UNFORMATTED 
              "h2. Table " cAnchorText cDbDesc pcTableName SKIP.
         END.
      END.

      CREATE BUFFER hBuf FOR TABLE pcTableName.


      IF lGetFields THEN
      DO:
         IF lHTML THEN         
            fGetHTMLDefinition(hBuf).
         ELSE IF NOT lLuna THEN 
            fGetTextDefinition(hBuf).
         ELSE 
            fGetWikiDefinition(hBuf).
      END.


      IF NOT ERROR-STATUS:ERROR OR NOT lGetCounts THEN
      DO:
         cQuery = "FOR EACH " + pcTableName + " NO-LOCK.".
         CREATE QUERY hQuery.
         hQuery:SET-BUFFERS(hBuf).
         hQuery:QUERY-PREPARE(cQuery).
         hQuery:QUERY-OPEN().

         DEFINE VARIABLE iCount AS INTEGER NO-UNDO. 
         iCount = 0.
         LoopBufs:
         REPEAT: 
            hQuery:GET-NEXT().
            IF hQuery:QUERY-OFF-END THEN LEAVE LoopBufs.
           
            iCount = iCount + 1.
            IF iCount = 1 THEN 
            DO:
               IF lUseNumResultAttrOfQuery THEN
               DO:
                 iCount = hQuery:NUM-RESULTS.
                 LEAVE LoopBufs.
               END.
            END.
            IF iCount > iLimitCounts THEN 
            DO:
               lCountLimitExceeded = TRUE.
               LEAVE LoopBufs.
            END.
         END.
         hQuery:QUERY-CLOSE().
         IF lGetCounts THEN
         DO:
            IF NOT lCountLimitExceeded THEN
               PUT STREAM sCounts UNFORMATTED pcTableName " : " iCount " records." SKIP.
            ELSE 
               PUT STREAM sCounts UNFORMATTED pcTableName " : count limit " 
                   iLimitCounts " exceeded." SKIP.
            IF lDispCounts THEN DISP iCount with frame x.
         END.
      END.
   END.
   
   IF lDispTableNames THEN
      DISP pcTableName FORMAT "X(40)" WITH FRAME A 30 down.
      down 1 with FRAME a.
END.

DEFINE STREAM sTablesF.

FUNCTION fAddDatabaseDescription RETURN LOGICAL (INPUT pcDbName AS CHARACTER):
   IF lGetFields THEN
   DO:
     IF lLuna  THEN
     DO:
         PUT STREAM sFields UNFORMATTED 
            "h1. Database \{anchor:" pcDbName "\} " pcDbName 
            ": tables, fields and indexes" SKIP(2).      
         RETURN TRUE.
     END.
   END.
   RETURN FALSE.
END.


FUNCTION fAddDatabaseLink RETURN LOGICAL (INPUT pcD AS CHARACTER):
    PUT STREAM sFields UNFORMATTED 
       "Database [#" pcD "] " pcD SKIP(1).
    RETURN TRUE.
END.


FUNCTION fCreateTableLink RETURN LOGICAL (INPUT pcTableName AS CHARACTER):
   IF pcTableName BEGINS "SYS" AND lSkipSysBeginningTbl THEN RETURN TRUE.
   IF pcTableName BEGINS "_" AND lSkip_beginningTbl THEN RETURN TRUE.
   PUT STREAM sFields UNFORMATTED "[#" pcTableName "]" SKIP.
END.



FUNCTION fDoTableFunctionality RETURN LOGICAL
   (INPUT piFunctionality AS INTEGER, 
    INPUT pcDbName AS CHARACTER,
    INPUT pcTableName AS CHARACTER):

   CASE piFunctionality:
     WHEN 1 THEN
        fProcessTable(pcDbName, pcTableName).
     WHEN 2 THEN
        fCreateTableLink(pcTableName).
     WHEN 3 THEN 
        fAddTableToTT(pcDbName, pcTableName). 
   END.
   RETURN TRUE.
END.

FUNCTION fDoDatabaseFunctionality RETURN LOGICAL 
   (INPUT piFunctionality AS INTEGER, INPUT pcDbName AS CHARACTER):
   CASE piFunctionality:
     WHEN 1 THEN
        fAddDatabaseDescription(pcDbName).
     WHEN 2 THEN 
        fAddDatabaseLink(pcDbName). 
     WHEN 3 THEN 
        fAddDatabaseToTT(pcDbName).
   END.
   RETURN TRUE.
END.


FUNCTION fAddEmptyRows RETURN LOGICAL (INPUT piFunctionality AS INTEGER):
   IF piFunctionality EQ 2 THEN
      PUT STREAM sFields UNFORMATTED SKIP(1).
   RETURN TRUE.
END.

FUNCTION fHandleAllTables RETURN LOGICAL (INPUT piFunctionality AS INTEGER):
   fDoDatabaseFunctionality(piFunctionality, "common").
   FOR EACH common._FILE NO-LOCK:
      fDoTableFunctionality(piFunctionality, "common", common._FILE._File-name).
   END.
   fAddEmptyRows(piFunctionality).

   fDoDatabaseFunctionality(piFunctionality, "mobile").
   FOR EACH mobile._FILE NO-LOCK:
      fDoTableFunctionality(piFunctionality, "mobile", mobile._FILE._File-name).
   END.
   fAddEmptyRows(piFunctionality).

   fDoDatabaseFunctionality(piFunctionality, "mcdr").
   FOR EACH mcdr._FILE NO-LOCK:
      fDoTableFunctionality(piFunctionality, "mcdr", mcdr._FILE._File-name).
   END.
   fAddEmptyRows(piFunctionality).

   fDoDatabaseFunctionality(piFunctionality, "mcdrdtl").
   FOR EACH mcdrdtl._FILE NO-LOCK:
      fDoTableFunctionality(piFunctionality, "mcdrdtl", mcdrdtl._FILE._File-name).
   END.
   fAddEmptyRows(piFunctionality).

   fDoDatabaseFunctionality(piFunctionality, "prepcdr").
   FOR EACH prepcdr._FILE NO-LOCK:
      fDoTableFunctionality(piFunctionality, "prepcdr", prepcdr._FILE._File-name).
   END.
   fAddEmptyRows(piFunctionality).

   fDoDatabaseFunctionality(piFunctionality, "roamcdr").
   FOR EACH roamcdr._FILE NO-LOCK:
      fDoTableFunctionality(piFunctionality, "roamcdr", roamcdr._FILE._File-name).
   END.
   fAddEmptyRows(piFunctionality).
   
   fDoDatabaseFunctionality(piFunctionality, "fraudcdr").
   FOR EACH fraudcdr._FILE NO-LOCK:
      fDoTableFunctionality(piFunctionality, "fraudcdr", fraudcdr._FILE._File-name).
   END.
   fAddEmptyRows(piFunctionality).
   
   fDoDatabaseFunctionality(piFunctionality, "reratelog").
   FOR EACH reratelog._FILE NO-LOCK:
      fDoTableFunctionality(piFunctionality, "reratelog", reratelog._FILE._File-name).
   END.
   fAddEmptyRows(piFunctionality).

   fDoDatabaseFunctionality(piFunctionality, "ordercanal").
   FOR EACH ordercanal._FILE NO-LOCK:
      fDoTableFunctionality(piFunctionality, "ordercanal", ordercanal._FILE._File-name).
   END.
   fAddEmptyRows(piFunctionality).
   

   RETURN TRUE.
END.


FUNCTION fHandleTablesByDatabases RETURN LOGICAL:
    FOR EACH ttTables USE-INDEX dbIdx BREAK BY cDatabase.
       IF FIRST-OF(ttTables.cDatabase) THEN
       DO:
          OUTPUT STREAM sFields TO VALUE(cOutputDir + ttTables.cDatabase + ".txt").
          PUT STREAM sFields UNFORMATTED "\{toc\}" SKIP.
       END.
       
       fProcessTable(ttTables.cDatabase, ttTables.cTable).
     
       IF LAST-OF(ttTables.cDatabase) THEN
          OUTPUT STREAM sFields CLOSE.
    END.
END.


FUNCTION fAddAlpCats RETURN LOGICAL:
   
   fAddAlphaCat("A","E").
   fAddAlphaCat("F","H").
   fAddAlphaCat("I","M").
   fAddAlphaCat("N","R").
   fAddAlphaCat("S","Z").
   RETURN TRUE.
END.

FUNCTION fDispTTTables RETURN LOGICAL:
    FOR EACH ttTables:
       DISP ttTables.
    END.
END.


FUNCTION fTableIsInCategory RETURN LOGICAL (INPUT pcBeginLimit AS CHARACTER,
     INPUT pcEndLimit AS CHARACTER):
    IF ttTables.cTable >= pcBeginLimit AND 
       (ttTables.cTable <= pcEndLimit OR
        (lBeginWithEndCatAcc AND 
         (ttTables.cTable BEGINS pcEndLimit))) THEN RETURN TRUE.
    RETURN FALSE.
END.

FUNCTION fHandleTablesByAlpCat RETURN LOGICAL (INPUT pcDb AS CHARACTER):
    DEFINE VARIABLE cDbFileAdd AS CHARACTER NO-UNDO. 
    cDbFileAdd = "".
    IF pcDb NE "" THEN cDbFileAdd = pcDb + "_".
    
    FOR EACH ttAlphaCat:
       OUTPUT STREAM sFields TO VALUE (cOutputDir + cDbFileAdd + "alphacat" + 
          "_" + ttAlphaCat.cBeginLimit + "_" + ttAlphaCat.cEndLimit + 
          ".txt").
       IF lLuna THEN
          PUT STREAM sFields UNFORMATTED "\{toc\}" SKIP.

       FOR EACH ttTables:
          IF (pcDb EQ "") OR (ttTables.cDatabase EQ pcDB) THEN
          DO:
             IF fTableIsInCategory(ttAlphaCat.cBeginLimit, ttAlphaCat.cEndLimit) THEN 
             DO:
                 fProcessTable(ttTables.cDatabase, ttTables.cTable).
              END.
          END.
       END.

       OUTPUT STREAM sFields CLOSE.
    END.
    RETURN TRUE.
END.


FUNCTION fhandletablesbydbandalpcat RETURN LOGICAL:
   FOR EACH ttDatabases:
      fHandleTablesByAlpCat(ttDatabases.cDatabase).
   END.
   RETURN TRUE.
END.


FUNCTION fGatherDbRelatedLinks RETURN LOGICAL (INPUT pcDbName AS CHARACTER):
  FOR EACH ttTables WHERE ttTables.cdatabase = pcDbName:
     fAddTableLink(ttTables.ctable, 
         "[" + ttTables.ctable + "|" + pcDbName + " database#" +
         ttTables.ctable + "]").
  END.
  RETURN TRUE.
END.


FUNCTION fGatherDbAndCategoryRelatedLinks RETURN LOGICAL
   (INPUT pcDbName AS CHARACTER, INPUT cBeginLimit AS CHARACTER, INPUT cEndLimit AS CHARACTER):
   FOR EACH ttTables WHERE ttTables.cdatabase = pcDbName:
       if fTableIsInCategory(cBeginLimit, cEndLimit) THEN
          fAddTableLink(ttTables.ctable, 
              "[" + ttTables.ctable + "|" + pcDbName + " database, from " 
              + cBeginLimit + " to " + cEndLimit + "#" 
              + ttTables.ctable + "]").
    END.
    RETURN TRUE.
END.

FUNCTION fGatherAlphabeticalLinks RETURN LOGICAL
   (INPUT cBeginLimit AS CHARACTER, INPUT cEndLimit AS CHARACTER):
   
   FOR EACH ttTables:
      if fTableIsInCategory(cBeginLimit, cEndLimit) THEN
      DO:
          IF cBeginLimit EQ "A" THEN
          DO:
             fAddTableLink(ttTables.ctable, 
                "[" + ttTables.ctable + "|All tables from " 
                + cBeginLimit + "-" + cEndLimit + "#" 
                + ttTables.ctable + "]").
          END.
          ELSE
             fAddTableLink(ttTables.ctable, 
                "[" + ttTables.ctable + "|All tables from " 
                + cBeginLimit + " to " + cEndLimit + "#" 
                + ttTables.ctable + "]").
      END.
   END.
   RETURN TRUE.
END.


FUNCTION fOutputLinks RETURN LOGICAL (INPUT pcFileSuffix AS CHARACTER):
   OUTPUT STREAM sFields TO VALUE (cOutputDir + "links" + pcFileSuffix + ".txt").
   FOR EACH ttTableLinks:
       PUT STREAM sFields UNFORMATTED ttTableLinks.cLinkText SKIP.
   END.
   OUTPUT STREAM sFields CLOSE.
END.


FUNCTION fCreateManyPageLinks RETURN LOGICAL (INPUT pcFileSuffix AS CHARACTER):
   FOR EACH ttLinkList:
      IF ttLinkList.cBeginLimit EQ "" THEN
         fGatherDbRelatedLinks(ttLinkList.cdbName).
      ELSE IF ttLinkList.cDbName EQ "" THEN
         fGatherAlphabeticalLinks(ttLinkList.cBeginLimit, ttLinkList.cEndLimit).
      ELSE
         fGatherDbAndCategoryRelatedLinks(ttLinkList.cdbname,
            ttLinkList.cBeginLimit, ttLinkList.cEndLimit).
   END.
   fOutputLinks(pcFileSuffix).
   RETURN TRUE.
END.


FUNCTION fAddDatabaseRelatedLinkList RETURN LOGICAL:
   EMPTY TEMP-TABLE ttLinkList.
   EMPTY TEMP-TABLE ttTableLinks.
   fAddLinks("common", "A", "I").
   fAddLinks("common", "H", "Z").
   fAddLinks("mcdr", "", "").
   fAddLinks("mcdrdtl", "", "").
   fAddLinks("mobile", "", "").
   fAddLinks("ordercanal", "", "").
   fAddLinks("prepcdr", "", "").
   fAddLinks("roamcdr", "", "").
   fAddLinks("fraudcdr", "", "").
   fAddLinks("reratelog", "", "").
   fCreateManyPageLinks("_dbrelated").
   RETURN TRUE.
END.


FUNCTION fAddAlphabeticalLinkList RETURN LOGICAL:
   EMPTY TEMP-TABLE ttLinkList.
   EMPTY TEMP-TABLE ttTableLinks.
   fAddLinks("", "A", "E").
   fAddLinks("", "F", "H").
   fAddLinks("", "I", "M").
   fAddLinks("", "N", "R").
   fAddLinks("", "S", "Z").
   fCreateManyPageLinks("_alphabetical").
   RETURN TRUE.

END.




IF NOT lUseManyFiles THEN
DO:
   IF NOT lTableListInFile THEN
   DO:
      IF lLuna THEN
      DO:
         IF lUseAnchor THEN
         DO:
            PUT STREAM sFields UNFORMATTED "h1. Databases and tables in CCBS" SKIP(1).
            fHandleAllTables(2). /* 2 = table link creation */
         END.
         ELSE
            PUT STREAM sFields UNFORMATTED "\{toc\}" SKIP.
      END.
      fHandleAllTables(1). /* 1 = normal processing */
   END.
   ELSE
   DO:
      INPUT STREAM sTablesF FROM VALUE(cTableListFileName).

      IF lLuna AND lGetFields THEN
      DO:
         PUT STREAM sFields UNFORMATTED "h1. Databases and tables in " cHeader SKIP(1).
         PUT STREAM sFields UNFORMATTED "\{toc\}" SKIP.
      END.
      DEFINE VARIABLE lcTableName AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE cDbName AS CHARACTER NO-UNDO. 
      REPEAT:
          IMPORT STREAM sTablesF UNFORMATTED lcTableName.
          lcTableName = TRIM(lcTableName).
          cDbName = "".
          IF INDEX(lcTableName, ".") > 0 THEN
          DO:
             DEFINE VARIABLE lcTemp AS CHARACTER NO-UNDO. 
             lcTemp = lcTableName.
             lcTableName = ENTRY(2, lcTemp, ".").
             cDbName = ENTRY(1, lcTemp, ".").
          END.
          fProcessTable(cDbName, lcTableName).
      END.

      INPUT STREAM sTablesF CLOSE.
   END.
END.
ELSE
DO:
   IF lLuna THEN
   DO:
      fHandleAllTables(3).
      fAddAlpCats().
      IF lFilesByDatabase THEN
         fHandleTablesByDatabases(). 
      IF lFilesByAlpCat THEN
         fHandleTablesByAlpCat("").
      IF lFilesByDbAndAlpCat THEN
         fhandletablesbydbandalpcat().
      fAddDatabaseRelatedLinkList().
      fAddAlphabeticalLinkList().
   END.
END.



IF lGetFields AND NOT lUseManyFiles THEN
   OUTPUT STREAM sFields CLOSE.
IF lGenerateTableList THEN
   OUTPUT STREAM sTables CLOSE.
IF lGetCounts THEN 
   OUTPUT STREAM sCounts CLOSE.
