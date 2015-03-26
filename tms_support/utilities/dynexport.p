DEFINE INPUT PARAMETER cDir AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER cTableName AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER iCountLimit AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER iDispInterval AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER plOutputFixture AS LOGICAL NO-UNDO. 

DEFINE VARIABLE cFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cXMLFile AS CHARACTER NO-UNDO. 

DEFINE VARIABLE hTable AS HANDLE NO-UNDO. 
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO. 

DEFINE VARIABLE lCreateXML AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lCreateDynTT AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lOutputTT AS LOGICAL NO-UNDO. 


DEFINE VARIABLE cBeginRecTag AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cEndRecTag AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cFieldSep AS CHARACTER NO-UNDO. 

DEFINE VARIABLE httTable AS HANDLE NO-UNDO. 
DEFINE VARIABLE httTableBuf AS HANDLE NO-UNDO. 

DEFINE VARIABLE cQuery AS CHARACTER NO-UNDO. 

cFile = cDir + "/dynexport_" + cTableName + ".txt".
cXMLFile = cDir + "/dynexport_" + cTableName + ".xml".
lCreateXML = FALSE.

cBeginRecTag = "<DynRecBegin>".
cEndRecTag   = "<DynRecEnd>".
cFieldSep    = CHR(255).

cQuery = "FOR EACH " + cTableName + " NO-LOCK".

lCreateDynTT = FALSE.
lOutputTT = FALSE.

IF lCreateXML THEN 
  lCreateDynTT = TRUE.


CREATE BUFFER hTable FOR TABLE cTableName.

IF lCreateDynTT THEN
DO:
   CREATE TEMP-TABLE httTable.
   httTable:CREATE-LIKE(cTableName).
   httTable:TEMP-TABLE-PREPARE("tt" + cTableName).
   httTableBuf = httTable:DEFAULT-BUFFER-HANDLE. 
END.


CREATE QUERY hQuery.
hQuery:ADD-BUFFER(hTable).
hQuery:QUERY-PREPARE(cQuery).
hQuery:QUERY-OPEN.


DEFINE VARIABLE iCount AS INTEGER NO-UNDO. 
iCount = 0.


DEFINE STREAM sData.
DEFINE STREAM sData2.

FUNCTION fDumpFixture RETURN LOGICAL (INPUT lhBuffer AS HANDLE):
   DEFINE VARIABLE liNum AS INTEGER NO-UNDO. 
   liNum = iCount + 1.
    
   PUT STREAM sData UNFORMATTED 
      "GeneratedFixture-" liNum ":" SKIP.

   DEFINE VARIABLE i AS INTEGER NO-UNDO. 
   DO i = 1 TO lhBuffer:NUM-FIELDS:

      /* skip extent fields; do not output in fixtures */
      IF lhBuffer:BUFFER-FIELD(i):EXTENT > 0 THEN NEXT.

      PUT STREAM sData UNFORMATTED 
         "   " lhBuffer:BUFFER-FIELD(i):NAME + ":".

      IF lhBuffer:BUFFER-FIELD(i):DATA-TYPE EQ "CHARACTER" THEN 
         PUT STREAM sData UNFORMATTED 
            '"' lhBuffer:BUFFER-FIELD(i):BUFFER-VALUE '"' SKIP.
      ELSE
         PUT STREAM sData UNFORMATTED 
            lhBuffer:BUFFER-FIELD(i):BUFFER-VALUE SKIP.

   END.
      
   PUT STREAM sData UNFORMATTED " " SKIP.

   RETURN TRUE.
END.




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






FUNCTION fOutputFields RETURN LOGICAL:
  DEFINE VARIABLE iField AS INTEGER NO-UNDO. 
  DEFINE VARIABLE hField AS HANDLE NO-UNDO. 
  DEFINE VARIABLE httField AS HANDLE NO-UNDO. 
  DEFINE VARIABLE iExtent AS INTEGER NO-UNDO. 

  PUT STREAM sData UNFORMATTED cBeginRecTag.

  REPEAT iField = 1 TO hTable:NUM-FIELDS:
     hField = hTable:BUFFER-FIELD(iField).
     IF hField:EXTENT <= 1 THEN 
     DO:
         PUT STREAM sData UNFORMATTED hField:NAME cFieldSep hField:BUFFER-VALUE cFieldSep.
     END.
     ELSE
     DO:
       REPEAT iExtent = 1 TO hField:EXTENT:
          PUT STREAM sData UNFORMATTED hField:NAME "," iExtent cFieldSep hField:BUFFER-VALUE(iExtent) cFieldSep.
       END.
     END.
     IF lCreateDynTT THEN
     DO:
      httField = httTableBuf:BUFFER-FIELD(hField:NAME).
      IF lOutputTT THEN 
         fSetValue(httField, STRING(hField:BUFFER-VALUE)).
     END.
  END.
  PUT STREAM sData UNFORMATTED cEndRecTag SKIP.
END.


IF NOT plOutputFixture THEN
DO:
   OUTPUT STREAM sData TO VALUE(cFile).
END.
ELSE
DO:
   cFile = cDir + "/" + cTableName + ".yaml".
   OUTPUT STREAM sData TO VALUE(cFile).
END.


IF lOutputTT THEN
   OUTPUT STREAM sData2 TO VALUE(cFile + ".tt").

IF NOT plOutputFixture THEN
DO:
   PUT STREAM sData UNFORMATTED cBeginRecTag SKIP.
   PUT STREAM sData UNFORMATTED cEndRecTag SKIP.
   PUT STREAM sData UNFORMATTED cFieldSep SKIP.
END.

hQuery:GET-FIRST().

loop:
REPEAT:
   IF hQuery:QUERY-OFF-END THEN LEAVE loop.
   IF lCreateDynTT THEN
   DO:
      httTableBuf:BUFFER-CREATE().
      httTableBuf:BUFFER-COPY(hTable). 
      IF lCreateXML THEN
          httTableBuf:WRITE-XML("FILE", cXMLFile, TRUE).  
   END.
   IF plOutputFixture THEN
   DO:
      fDumpFixture(hTable).
   END.
   ELSE
      fOutputFields().

   iCount = iCount + 1.
   IF iDispInterval > 0 THEN
      IF iCount MOD iDispInterval = 0 THEN  DISP iCount.
   IF iCountLimit > 0 THEN
      IF iCount >= iCountLimit THEN LEAVE loop.
  
   hQuery:GET-NEXT().
END.

OUTPUT STREAM sData CLOSE.
IF lOutputTT THEN 
   OUTPUT STREAM sData2 CLOSE.


DELETE OBJECT hQuery.
DELETE OBJECT hTable.

IF lCreateXML THEN
DO:
  DELETE OBJECT httTable.
END.




