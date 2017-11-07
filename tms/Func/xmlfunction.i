/* ----------------------------------------------------------------------
  MODULE .......: xmlfunction.i
  TASK .........: xml temp-table functions
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 25.05.05
  CHANGED ......: 02.06.05 tk pGetNodeValue
                  07.06.05 tk pShowSchema
                  13.06.05 tk do not use ttXML in pGetNodeValue (emptied ttXML)
                  09.11.06 kl fGetRPCNodeValue
                  
  Version ......: TeleF
  ---------------------------------------------------------------------- */

&IF "{&xmlfunction}" NE "YES" 
&THEN

&GLOBAL-DEFINE xmlfunction YES

DEFINE TEMP-TABLE ttXML NO-UNDO
   FIELD MNPMessage AS INTEGER
   FIELD ParentName AS CHARACTER FORMAT "X(20)"
   FIELD NodeName   AS CHARACTER FORMAT "X(20)"
   FIELD NodeValue  AS CHARACTER FORMAT "X(20)".
         
DEFINE TEMP-TABLE ttSendXML NO-UNDO
   FIELD MNPMessage AS INTEGER
   FIELD LevelNum   AS INTEGER 
   FIELD ParentName AS CHARACTER FORMAT "X(20)"
   FIELD NodeName   AS CHARACTER FORMAT "X(20)"
   FIELD NodeValue  AS CHARACTER FORMAT "X(20)".
         
DEFINE TEMP-TABLE ttXMLSchema NO-UNDO
   FIELD LevelNum AS INTEGER
   FIELD XML      AS CHARACTER FORMAT "X(78)".

DEFINE TEMP-TABLE ttNodeTable LIKE ttXML.

FUNCTION fGetNodeValue RETURNS CHARACTER
  (INPUT pcXMLMsg AS CHARACTER,
   INPUT pcNode   AS CHARACTER):

   DEFINE VARIABLE liIdx1   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liIdx2   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcReturn AS CHARACTER NO-UNDO INIT "N/A".
   
   ASSIGN
      liIdx1 = INDEX(pcXMLMsg,"<" + pcNode + ">")
      liIdx2 = INDEX(pcXMLMsg,"</" + pcNode + ">").

   IF liIdx1 NE 0 AND liIdx2 NE 0 THEN DO:
      
      liIdx1 = liIdx1 + LENGTH("<" + pcNode + ">").
   
      lcReturn = SUBSTR(pcXMLMsg,liIdx1,liIdx2 - liIdx1).

   END.

   RETURN lcReturn.

END FUNCTION.

FUNCTION fGetHashValue RETURNS CHARACTER
  (INPUT pcXMLMsg AS LONGCHAR,
   INPUT pcNode   AS CHARACTER):

   DEFINE VARIABLE liIdx1   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liIdx2   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcReturn AS CHARACTER NO-UNDO INIT "N/A".
   
   ASSIGN
      liIdx1 = INDEX(pcXMLMsg,pcNode).

   IF liIdx1 NE 0 THEN DO:
 
      ASSIGN
         liIdx2 = INDEX(pcXMLMsg,"</value>", liIdx1)
         liIdx1 = INDEX(pcXMLMsg,"<value>", liIdx1).
      
      IF liIdx1 NE 0 AND liIdx2 NE 0 THEN DO:
         liIdx1 = liIdx1 + LENGTH("<value>").
      
         lcReturn = SUBSTR(pcXMLMsg,liIdx1,liIdx2 - liIdx1).
      END.

   END.

   RETURN lcReturn.

END FUNCTION.

FUNCTION fGetRPCNodeValue RETURNS CHARACTER
  (INPUT pcXMLMsg AS CHARACTER,
   INPUT pcNode   AS CHARACTER):

   DEFINE VARIABLE liIdx1   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liIdx2   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liLoop   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcReturn AS CHARACTER NO-UNDO INIT "N/A".
   
   liIdx1 = INDEX(pcXMLMsg,"<name>" + pcNode + "</name>").

   IF liIdx1 NE 0 THEN DO:

      liIdx1 = liIdx1 + LENGTH("<name>") + LENGTH(pcNode) + LENGTH("</name>").
      
      DO liLoop = liIdx1 TO LENGTH(pcXMLMsg):
         IF SUBSTR(pcXMLMsg,liLoop,1) = ">" THEN liIdx2 = liIdx2 + 1.
         IF liIdx2 = 2 THEN LEAVE.
      END.

      ASSIGN
         liIdx1   = liLoop + 1
         lcReturn = "".

      DO liLoop = liIdx1 TO LENGTH(pcXMLMsg):
         IF SUBSTR(pcXMLMsg,liLoop,1) NE "<" THEN
            lcReturn = lcReturn + SUBSTR(pcXMLMsg,liLoop,1).
         ELSE LEAVE.
      END.

   END.

   RETURN lcReturn.

END FUNCTION.

PROCEDURE pMessage2Table:

   DEFINE INPUT PARAMETER hParent AS HANDLE  NO-UNDO.
   DEFINE INPUT PARAMETER lLevel  AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER lMNPNum AS INTEGER NO-UNDO.

   DEFINE VARIABLE iLoop    AS INTEGER NO-UNDO.
   DEFINE VARIABLE hNoderef AS HANDLE  NO-UNDO.
   DEFINE VARIABLE lGood    AS LOGICAL NO-UNDO.

   CREATE X-NODEREF hNoderef.
   REPEAT iLoop = lLevel TO hParent:NUM-CHILDREN:

      lGood = hParent:GET-CHILD(hNoderef,iLoop).

      IF NOT lGood THEN LEAVE.

      IF hNoderef:NAME = "#text" THEN DO:
         IF LENGTH(TRIM(hNoderef:NODE-VALUE)) > 0 THEN
            ttXML.NodeValue = hNoderef:NODE-VALUE.
      END.
      ELSE DO:
         CREATE ttXML.
         ttXML.MNPMessage = lMNPNum.
         ttXML.ParentName = hParent:NAME.
         ttXML.NodeName   = hNoderef:NAME.
      END.

      RUN pMessage2Table(hNoderef,1,lMNPNum).

   END.

   DELETE OBJECT hNoderef.

END PROCEDURE.


PROCEDURE pMessage2NodeTable:

   DEFINE INPUT PARAMETER hParent AS HANDLE  NO-UNDO.
   DEFINE INPUT PARAMETER lLevel  AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER lMNPNum AS INTEGER NO-UNDO.

   DEFINE VARIABLE iLoop    AS INTEGER NO-UNDO.
   DEFINE VARIABLE hNoderef AS HANDLE  NO-UNDO.
   DEFINE VARIABLE lGood    AS LOGICAL NO-UNDO.

   CREATE X-NODEREF hNoderef.
   REPEAT iLoop = lLevel TO hParent:NUM-CHILDREN:

      lGood = hParent:GET-CHILD(hNoderef,iLoop).

      IF NOT lGood THEN LEAVE.

      IF hNoderef:NAME = "#text" THEN DO:
         IF LENGTH(TRIM(hNoderef:NODE-VALUE)) > 0 THEN
            ttNodeTable.NodeValue = hNoderef:NODE-VALUE.
      END.
      ELSE DO:
         CREATE ttNodeTable.
         ttNodeTable.MNPMessage = lMNPNum.
         ttNodeTable.ParentName = hParent:NAME.
         ttNodeTable.NodeName   = hNoderef:NAME.
      END.

      RUN pMessage2NodeTable(hNoderef,1,lMNPNum).

   END.

   DELETE OBJECT hNoderef.

END PROCEDURE.

PROCEDURE pMessage2XMLTable:

   DEFINE INPUT PARAMETER hParent AS HANDLE  NO-UNDO.
   DEFINE INPUT PARAMETER lLevel  AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER lMNPNum AS INTEGER NO-UNDO.

   DEFINE VARIABLE iLoop    AS INTEGER NO-UNDO.
   DEFINE VARIABLE hNoderef AS HANDLE  NO-UNDO.
   DEFINE VARIABLE lGood    AS LOGICAL NO-UNDO.

   lMNPNum = lMNPNum + 1.

   CREATE X-NODEREF hNoderef.
   REPEAT iLoop = lLevel TO hParent:NUM-CHILDREN:

      lGood = hParent:GET-CHILD(hNoderef,iLoop).

      IF NOT lGood THEN LEAVE.

      IF iLoop = 1 THEN DO:
         CREATE ttXMLSchema.
         ttXMLSchema.LevelNum = lMNPNum.
         ttXMLSchema.XML      = "<" + hParent:NAME.
         IF hParent:ATTRIBUTE-NAMES > "" THEN 
            RUN pAttributes2XMLTable(hParent,INPUT-OUTPUT ttXMLSchema.XML).
            
         ttXMLSchema.XML = ttXMLSchema.XML + ">".
      END.

      IF hNoderef:NAME = "#text" THEN DO:
         IF LENGTH(TRIM(hNoderef:NODE-VALUE)) > 0 THEN
            ttXMLSchema.XML = ttXMLSchema.XML + hNoderef:NODE-VALUE.
      END.
      
      RUN pMessage2XMLTable(hNoderef,1,lMNPNum).

   END.

   IF hParent:NAME NE "#text" THEN DO:
      
      FIND LAST ttXMLSchema NO-LOCK.
      
      IF ttXMLSchema.LevelNum > lMNPNum THEN DO:
         CREATE ttXMLSchema.
         ttXMLSchema.LevelNum = lMNPNum.
         ttXMLSchema.XML      = "</" + hParent:NAME + ">".
   
      END.
      ELSE DO:
         IF INDEX(ttXMLSchema.XML,"<" + hParent:NAME + ">") > 0 THEN
            ttXMLSchema.XML = ttXMLSchema.XML + "</" + hParent:NAME + ">".
         ELSE DO:
            CREATE ttXMLSchema.
            ttXMLSchema.LevelNum = lMNPNum.
            ttXMLSchema.XML      = "<" + hParent:NAME.
            IF hParent:ATTRIBUTE-NAMES > "" THEN 
               RUN pAttributes2XMLTable(hParent,INPUT-OUTPUT ttXMLSchema.XML).
            ttXMLSchema.XML = ttXMLSchema.XML  + "/>".
         END.
      END.

   END.
   
   DELETE OBJECT hNoderef.

END PROCEDURE.

PROCEDURE pAttributes2XMLTable:

   DEFINE INPUT PARAMETER ihNode AS HANDLE NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ioNodeRow AS CHAR NO-UNDO.
   
   DEFINE VARIABLE liCount     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcAttribute AS CHARACTER NO-UNDO.
   
   DO liCount = 1 TO NUM-ENTRIES(ihNode:ATTRIBUTE-NAMES):
      lcAttribute = ENTRY(liCount,ihNode:ATTRIBUTE-NAMES).
      
      ioNodeRow = ioNodeRow + " " + 
                  lcAttribute + '="' +
                  ihNode:GET-ATTRIBUTE(lcAttribute) + '"'.
   END.
   
END PROCEDURE.

PROCEDURE pGetNodeValue:

   DEFINE INPUT  PARAMETER lcMNPMessage AS CHAR      NO-UNDO.   
   DEFINE INPUT  PARAMETER pNodeName    AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER pNodeValue   AS CHARACTER NO-UNDO INIT "N/A".
   
   DEFINE VARIABLE lhDoc        AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhRoot       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lmDoc        AS MEMPTR    NO-UNDO.

   CREATE X-DOCUMENT lhDoc.
   CREATE X-NODEREF  lhRoot.
      
   SET-SIZE(lmDoc) = LENGTH(lcMNPMessage) + 1.

   PUT-STRING(lmDoc,1) = lcMNPMessage.

   lhDoc:LOAD("MEMPTR",lmDoc,FALSE).
      
   lhDoc:GET-DOCUMENT-ELEMENT(lhRoot) NO-ERROR.   

   EMPTY TEMP-TABLE ttNodeTable.

   RUN pMessage2NodeTable(lhRoot,1,0).
   
   SET-SIZE(lmDoc) = 0.

   DELETE OBJECT lhDoc.
   DELETE OBJECT lhRoot.

   FIND FIRST ttNodeTable WHERE
              ttNodeTable.NodeName = pNodeName
   NO-LOCK NO-ERROR.

   IF AVAIL ttNodeTable THEN pNodeValue = ttNodeTable.NodeValue.

   EMPTY TEMP-TABLE ttNodeTable.

END.

PROCEDURE pShowSchema:

   DEFINE INPUT PARAMETER pXMLMessage AS LONGCHAR NO-UNDO.

   DEFINE VARIABLE lmDoc   AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE lhRoot  AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhDoc   AS HANDLE    NO-UNDO.

   CREATE X-DOCUMENT lhDoc.
   CREATE X-NODEREF  lhRoot.

   SET-SIZE(lmDoc) = LENGTH(pXMLMessage) + 1.

   PUT-STRING(lmDoc,1) = pXMLMessage.

   lhDoc:LOAD("MEMPTR",lmDoc,FALSE).
      
   lhDoc:GET-DOCUMENT-ELEMENT(lhRoot) NO-ERROR.   

   EMPTY TEMP-TABLE ttXML.
   EMPTY TEMP-TABLE ttXMLSchema.

   RUN pMessage2XMLTable(lhRoot,1,0).
   
   SET-SIZE(lmDoc) = 0.

   IF VALID-HANDLE(lhDoc)  THEN DELETE OBJECT lhDoc.
   IF VALID-HANDLE(lhRoot) THEN DELETE OBJECT lhRoot.

   FOR EACH ttXMLSchema EXCLUSIVE-LOCK:
      ttXMLSchema.XML = FILL("  ", ttXMLSchema.LevelNum - 1) + ttXMLSchema.XML.
   END.
   
   RUN Mc/xmlbrowser.p(INPUT TABLE ttXMLSchema).

   EMPTY TEMP-TABLE ttXMLSchema.

END.

FUNCTION fRPCStruct RETURNS LOGICAL
  (INPUT pcCommand AS CHARACTER,
   INPUT pcParams  AS CHARACTER,
   INPUT phDoc     AS HANDLE):

   DEFINE VARIABLE liLoop AS INTEGER NO-UNDO.
   DEFINE VARIABLE liAmt  AS INTEGER NO-UNDO.
   DEFINE VARIABLE llOK   AS LOGICAL NO-UNDO.

   liAmt = NUM-ENTRIES(pcParams).

   CASE pcCommand:
      WHEN "Start" THEN DO liLoop = 1 TO liAmt:
         llOK = phDOc:START-ELEMENT(ENTRY(liLoop,pcParams)).
      END.
      WHEN "End" THEN DO liLoop = liAmt TO 1 BY -1:
         llOK = phDOc:END-ELEMENT(ENTRY(liLoop,pcParams)).
      END.
   END.
   
END FUNCTION.

&ENDIF
