DEFINE VARIABLE cTableName              AS CHARACTER   
    LABEL "Give tableName: " 
    FORMAT "X(60)"                                     NO-UNDO.
DEFINE VARIABLE cDescFile               AS CHARACTER   
    LABEL "Give description filename: " 
    FORMAT "X(60)"                                     NO-UNDO.
DEFINE VARIABLE iLimitForGettingForward AS INTEGER
    LABEL "Give Limit count of records checked" 
    FORMAT ">>>>9"                                     NO-UNDO.
DEFINE VARIABLE cEmptyFile              AS CHARACTER   
    LABEL "Give the file to list fields having empty name" 
    FORMAT "X(60)"                                     NO-UNDO.

DEFINE VARIABLE bh                      AS HANDLE      NO-UNDO.
DEFINE VARIABLE bf                      AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCount                  AS INTEGER     NO-UNDO. 
DEFINE VARIABLE lForward                AS LOGICAL     NO-UNDO.
DEFINE VARIABLE hTable                  AS HANDLE      NO-UNDO.
DEFINE VARIABLE cTempTableName          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iTotalFieldCount        AS INTEGER     NO-UNDO. 
DEFINE VARIABLE iNotEmptyFieldCount     AS INTEGER     NO-UNDO. 
DEFINE VARIABLE iEmptyFieldCount        AS INTEGER     NO-UNDO. 

ASSIGN iTotalFieldCount = 0
       iNotEmptyFieldCount = 0 
       iEmptyFieldCount = 0.

DEFINE TEMP-TABLE ttEmptyField
   FIELD fieldName AS CHARACTER
   INDEX fNameIdx IS PRIMARY UNIQUE fieldName.

cDescFile = "/home/harrim/".
cEmptyFile = "/home/harrim/emptyfile.txt".
UPDATE cTableName cDescFile iLimitForGettingForward cEmptyFile.
lForward = TRUE.
cTempTableName = "tt" + REPLACE(cTableName,".","").
CREATE TEMP-TABLE hTable.
hTable:CREATE-LIKE(cTableName).
hTable:TEMP-TABLE-PREPARE(cTempTableName).
bh = hTable:DEFAULT-BUFFER-HANDLE.   
bh:BUFFER-CREATE.

DEFINE STREAM sTtDesc.
OUTPUT STREAM sTtDesc TO VALUE(cDescFile).
PUT STREAM sTtDesc UNFORMATTED "Description for the table " + cTableName SKIP.
RUN create_field_descriptions(bh, cTempTableName, cTableName, cEmptyFile).
OUTPUT STREAM sTtDesc CLOSE.

FUNCTION getCharValue RETURN CHARACTER 
   (INPUT hTtField AS HANDLE, INPUT iExtent AS INTEGER):
   IF iExtent = 0 THEN DO:
      IF hTtField = ? THEN DO: RETURN ?. END.
      IF hTtField:BUFFER-VALUE = ? THEN RETURN ?. 
         ELSE RETURN STRING(hTtField:BUFFER-VALUE).
   END. ELSE DO:
      IF hTtField = ? THEN DO: RETURN ?. END.
      IF hTtField:BUFFER-VALUE(iExtent) = ? THEN RETURN ?.
         ELSE RETURN STRING(hTtField:BUFFER-VALUE(iExtent)).
   END.
END.


PROCEDURE create_field_descriptions:
   DEFINE INPUT PARAMETER phTtBuffer  AS HANDLE    NO-UNDO.
   DEFINE INPUT PARAMETER pcTempTable AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER pcTableName AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER pcEmptyFile AS CHARACTER NO-UNDO. 

   DEFINE VARIABLE hQueryDb          AS HANDLE    NO-UNDO.
   DEFINE VARIABLE hQueryTt          AS HANDLE    NO-UNDO.
   DEFINE VARIABLE hBufTt            AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE hQuery            AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE I                 AS INTEGER   NO-UNDO.
   DEFINE VARIABLE J                 AS INTEGER   NO-UNDO.
   DEFINE VARIABLE iTotalExtent      AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE cFieldName        AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hDbTableBuf       AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE cDbBufName        AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lFirstEndReached  AS LOGICAL   NO-UNDO. 
   DEFINE VARIABLE lFirstEndReached2 AS LOGICAL   NO-UNDO. 
   DEFINE VARIABLE hDbField          AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE hDbField1         AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE hTtField          AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lEmpty            AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE cCharValue        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lEmpty2           AS LOGICAL   NO-UNDO. 
   DEFINE VARIABLE cCharValue2       AS CHARACTER NO-UNDO.

   cDbBufName = "db" + REPLACE(cTableName, ".", "").
   CREATE BUFFER hDbTableBuf FOR TABLE pcTableName BUFFER-NAME cDbBufName.
   
   CREATE QUERY hQueryDb.
   hQueryDb:SET-BUFFERS(hDbTableBuf).

   DEFINE VARIABLE cDbQueryString AS CHARACTER NO-UNDO. 

   cDbQueryString = 'FOR EACH ' + cDbBufName + " NO-LOCK". 

   MESSAGE cDbQueryString VIEW-AS ALERT-BOX.

   hQueryDb:QUERY-PREPARE(cDbQueryString).
   hQueryDb:QUERY-OPEN().
   hQueryDb:GET-FIRST().

   CREATE QUERY hQueryTt.
   hQueryTt:SET-BUFFERS(phTtBuffer).

   DEFINE VARIABLE cTempTableQuery AS CHARACTER NO-UNDO. 

   cTempTableQuery = 'FOR EACH ' + pcTempTable + ' NO-LOCK.'.
   MESSAGE cTempTableQuery VIEW-AS ALERT-BOX.

   hQueryTt:QUERY-PREPARE(cTempTableQuery).
   hQueryTt:QUERY-OPEN().
   hQueryTt:GET-FIRST(). 

   iTotalFieldCount = phTtBuffer:NUM-FIELDS.
   DO I = 1 TO phTtBuffer:NUM-FIELDS:
       ASSIGN iCount = 0
              lFirstEndReached = FALSE
              lFirstEndReached2 = FALSE
              bf = phTtBuffer:BUFFER-FIELD(I)
              iTotalExtent = bf:EXTENT
              cFieldName = bf:NAME.

       DISP I iCount.
       hTtField = phTtBuffer:BUFFER-FIELD(bf:NAME).
       IF iTotalExtent = 0  THEN
       DO: 
          ASSIGN hDbField1 = hDbTableBuf:BUFFER-FIELD(I)
                 lEmpty = TRUE.
          DISP hDbField1:NAME.
          FindNotEmpty:
          DO WHILE lEmpty:
             ASSIGN hTtField = phTtBuffer:BUFFER-FIELD(bf:NAME)
                    hDbField1 = hDbTableBuf:BUFFER-FIELD(I).
             RUN getFieldDataToTt(
                 INPUT-OUTPUT hTtField, 
                 hQueryDb, hDbField1, cFieldName, 1, iTotalExtent).
             cCharValue = getCharValue(hTtField, 0).
             IF cCharValue = "" OR cCharValue = ? THEN
             DO:
                IF lForward THEN hQueryDb:GET-NEXT() NO-ERROR.
                   ELSE hQueryDb:GET-PREV() NO-ERROR.
                iCount = iCount + 1.
                DISP iCount.
                IF hQueryDb:QUERY-OFF-END THEN
                DO:
                   IF lForward THEN hQueryDb:GET-LAST() NO-ERROR. 
                      ELSE hQueryDb:GET-FIRST() NO-ERROR.
                   IF lForward THEN lForward = FALSE. ELSE lForward = TRUE.
                   ASSIGN hDbField1 = hDbTableBuf:BUFFER-FIELD(I)
                          hDbField = hDbTableBuf:BUFFER-FIELD(I).
                END.
                IF iCount = iLimitForGettingForward THEN
                DO:
                   ASSIGN hDbField1 = hDbTableBuf:BUFFER-FIELD(I)
                          hDbField = hDbTableBuf:BUFFER-FIELD(I).

                   MESSAGE iLimitForGettingForward 
                           " records gone through - skip field " 
                           hDbField:NAME "with empty value " 
                           VIEW-AS ALERT-BOX.

                   iEmptyFieldCount = iEmptyFieldCount + 1.

                   RUN addEmptyField(hDbField:NAME).
                   iCount = 0.                     
                   hQueryDb:GET-FIRST().
                   lForward = TRUE.
                   LEAVE findNotEmpty.
               END.
             END.
             ELSE
               lEmpty = FALSE.
          END.
       end.
       ELSE
       do:
          hTtField = phTtBuffer:BUFFER-FIELD(bf:NAME).
          DO J = 1 TO bf:EXTENT:
             DISP J.
             ASSIGN hDbField = hDbTableBuf:BUFFER-FIELD(I)
                    lEmpty2 = TRUE.
             DISP hDbField:NAME.
             RUN getFieldDataToTt(
                 INPUT-OUTPUT hTtField, 
                 hQueryDb, hDbField, cFieldName, J, iTotalExtent).
             FindNotEmpty2:
             DO WHILE lEmpty2:
                hDbField = hDbTableBuf:BUFFER-FIELD(I).
                RUN getFieldDataToTt(
                    INPUT-OUTPUT hTtField, hQueryDb, hDbField, cFieldName, J, iTotalExtent).
                cCharValue = getCharValue(hTtField, J).
                IF cCharValue2 = "" OR cCharValue2 = ? THEN
                DO:
                   IF lForward THEN hQueryDb:GET-NEXT() NO-ERROR. 
                      ELSE hQueryDb:GET-PREV() NO-ERROR.
                   iCount = iCount + 1.
                   DISP iCount.
                   IF hQueryDb:QUERY-OFF-END THEN
                   DO:
                      IF lForward THEN hQueryDb:GET-LAST() NO-ERROR.
                         ELSE hQueryDb:GET-FIRST() NO-ERROR.
                      IF lForward THEN lForward = FALSE. ELSE lForward = TRUE.
                      IF lFirstEndReached THEN LEAVE findNotEmpty2.
                      ASSIGN lFirstEndReached = TRUE
                             hDbField1 = hDbTableBuf:BUFFER-FIELD(I)
                             hDbField = hDbTableBuf:BUFFER-FIELD(I).
                   END.
                   IF iCount = iLimitForGettingForward THEN
                   DO:
                      ASSIGN hDbField1 = hDbTableBuf:BUFFER-FIELD(I)
                             hDbField = hDbTableBuf:BUFFER-FIELD(I).

                      MESSAGE iLimitForGettingForward 
                          " records gone through - skip field " hDbField:NAME 
                          " with empty value " VIEW-AS ALERT-BOX.
                      iEmptyFieldCount = iEmptyFieldCount + 1.
                      RUN addEmptyField(hDbField:NAME).
                      iCount = 0.                     
                      hQueryDb:GET-FIRST().
                      lForward = TRUE.
                      LEAVE findNotEmpty2.
                   END.
                END. 
                ELSE 
                   lEmpty2 = FALSE.
             END.
         END.
       end.
   END.

   iNotEmptyFieldCount = iTotalFieldCount - iEmptyFieldCount.
   RUN fillInFirstRowOfTempTable(phTtBuffer). 
   RUN produceEmptyFile(pcEmptyFile).
   MESSAGE "End of it" VIEW-AS ALERT-BOX.
END PROCEDURE.


PROCEDURE addEmptyField:
   DEFINE INPUT PARAMETER pcFieldName AS CHARACTER NO-UNDO. 
   IF NOT CAN-FIND(ttEmptyField WHERE fieldName = pcFieldName) THEN
   DO:
      CREATE ttEmptyField.
      ASSIGN ttEmptyField.fieldName = pcFieldName.
   END.
END.

DEFINE STREAM sEmptyFile.
PROCEDURE produceEmptyFile:
   DEFINE INPUT PARAMETER pcFilename AS CHARACTER NO-UNDO. 
   OUTPUT STREAM sEmptyFile TO VALUE (pcFileName). 

   PUT STREAM sEmptyFile UNFORMATTED 
      "Total field count : " iTotalFieldCount
      " filled field count : " iNotEmptyFieldCount
      " empty field count : " iEmptyFieldCount SKIP.

   PUT STREAM sEmptyFile UNFORMATTED 
      "Names of the fields that were left with empty values:" SKIP.
   FOR EACH ttEmptyField:
      PUT STREAM sEmptyFile UNFORMATTED ttEmptyField.fieldName SKIP.
   END.

   OUTPUT STREAM sEmptyFile CLOSE.
END.

PROCEDURE pReOpenQuery:
    DEFINE INPUT-OUTPUT PARAMETER phQuery AS HANDLE NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER phBuffer AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER pcTableName AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER pcTableBufName AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER iField AS INTEGER NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER phDbField1 AS HANDLE NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER phDbField2 AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER I AS INTEGER NO-UNDO. 
   
    DEFINE VARIABLE lcDbQueryString AS CHARACTER NO-UNDO. 
    lcDbQueryString = 'FOR EACH ' + pcTableBufName + ' NO-LOCK WHERE ' + 
                          pcTableBufName + '.Brand = "1" NO-LOCK'. 

    phQuery:QUERY-CLOSE().
    DELETE OBJECT phQuery.
    DELETE WIDGET phBuffer.
    CREATE BUFFER phBuffer FOR TABLE pcTableName BUFFER-NAME pcTableBufName.
    CREATE QUERY phQuery.
    phQuery:SET-BUFFERS(phBuffer).
    phQuery:QUERY-PREPARE(lcDbQueryString).
    phQuery:QUERY-OPEN(). 
    phQuery:GET-FIRST(). 
    phDbField1 = phBuffer:BUFFER-FIELD(I).
    phDbField2 = phBuffer:BUFFER-FIELD(I).
END.


PROCEDURE fillInFirstRowOfTempTable:
   DEFINE INPUT PARAMETER phBufferTt AS HANDLE NO-UNDO. 
   
   DEFINE VARIABLE hField AS HANDLE NO-UNDO. 
   DEFINE VARIABLE I AS INTEGER NO-UNDO. 
   DEFINE VARIABLE cField AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE J AS INTEGER NO-UNDO. 
   DEFINE VARIABLE iTotalExtent AS INTEGER NO-UNDO.
   DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO. 

   PUT STREAM sTtDesc UNFORMATTED 
      "Total field count : " iTotalFieldCount
      " filled field count : " iNotEmptyFieldCount
      " empty field count : " iEmptyFieldCount SKIP.

   DO I = 1 TO phBufferTt:NUM-FIELDS:
       ASSIGN hField = phBufferTt:BUFFER-FIELD(I).
       iTotalExtent = hField:EXTENT.
       cFieldName = hField:NAME.
       IF iTotalExtent = 0  THEN DO:
          RUN create_field(phBufferTt, hField, 0).
       END. ELSE DO:
          RUN create_field(phBufferTt, hField, iTotalExtent).
       END.
   END.
END.

PROCEDURE getFieldDataToTt:
   DEFINE INPUT-OUTPUT PARAMETER hTtField AS HANDLE NO-UNDO. 

   DEFINE INPUT PARAMETER hDbQuery AS HANDLE NO-UNDO. 
   DEFINE INPUT PARAMETER hDbFieldBuffer AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER cFieldName AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER iExtent AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER iTotalExtent AS INTEGER NO-UNDO.
   
   IF iTotalExtent = 0 THEN   
      hTtField:BUFFER-VALUE = hDbFieldBuffer:BUFFER-VALUE.
   ELSE
      hTtField:BUFFER-VALUE(iExtent) = hDbFieldBuffer:BUFFER-VALUE(iExtent).
END. 


FUNCTION getExampleContent RETURN CHARACTER 
   (INPUT bfTable AS HANDLE, INPUT cFieldName AS CHARACTER,
    INPUT extentCount AS INTEGER):

   DEFINE VARIABLE cFieldValue AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE hField AS HANDLE NO-UNDO. 
   IF extentCount = 0 THEN
      cFieldValue = bfTable:BUFFER-FIELD(cFieldName):BUFFER-VALUE.
   ELSE
   DO:
      DEFINE VARIABLE iExtent AS INTEGER NO-UNDO. 
      DEFINE VARIABLE lNotEmptyFound AS LOGICAL NO-UNDO.
      ExtentLoop:
      REPEAT iExtent = 1 TO extentCount:
        hField = bfTable:BUFFER-FIELD(cFieldName).
        cFieldValue = hField:BUFFER-VALUE(iExtent).
        IF cFieldValue <> ? AND cFieldValue <> "" THEN
        DO:
           IF hField:DATA-TYPE = "character" THEN
              LEAVE extentLoop.
           IF hField:DATA-TYPE = "integer" THEN
              IF cFieldValue <> "0" THEN LEAVE extentLoop.
           IF hField:DATA-TYPE = "decimal" THEN
              IF NOT cFieldValue BEGINS "0.0" THEN
                 LEAVE extentLoop.
        END.
      END.
   END.
   RETURN cFieldValue.
END.

PROCEDURE create_field:
    DEFINE INPUT PARAMETER bTable AS HANDLE   NO-UNDO.
    DEFINE INPUT  PARAMETER bf AS HANDLE      NO-UNDO.
    DEFINE INPUT PARAMETER iExtentCount AS INTEGER NO-UNDO.

    DEFINE VARIABLE h  AS HANDLE NO-UNDO.  
    DEFINE VARIABLE wf AS HANDLE NO-UNDO.
    DEFINE VARIABLE cExampleContent AS CHARACTER NO-UNDO. 
    cExampleContent = getExampleContent(bh, bf:NAME, iExtentCount).  

    IF cExampleContent <> "" THEN
    DO:
       PUT STREAM sTtDesc UNFORMATTED bf:NAME "|" bf:DATA-TYPE "|".
       IF iExtentCount > 0 THEN
          PUT STREAM sTtDesc UNFORMATTED "extent " iExtentCount "|".
       ELSE
          PUT STREAM sTtDesc UNFORMATTED "|".
       PUT STREAM sTtDesc UNFORMATTED bf:LABEL.
       PUT STREAM sTtDesc UNFORMATTED "|" cExampleContent SKIP.
    END.
    ELSE
       MESSAGE "Field " bf:NAME " left out as unused one" VIEW-AS ALERT-BOX.
END.



