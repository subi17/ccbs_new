
/* Simulate Credit Scoring  Response XML */


{commali.i}
{cparam2.i}
gcBrand = "1".

{xmlfunction.i}
DEF VAR lcHostname AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF LOOKUP(lcHostName,'angetenar,alpheratz') = 0 THEN DO:
   MESSAGE 'This script is not allowed to run in'
   lcHostName VIEW-AS ALERT-BOX.
   RETURN.
END.

/*
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
*/
FUNCTION fCreditScoring RETURN LOGICAL
         (INPUT-OUTPUT phSAXWriter AS HANDLE):

       DEFINE VARIABLE lcGenRespCode AS CHARACTER NO-UNDO. 
       lcGenRespCode = "A". /* by the moment always accept it */

       phSAXWriter:WRITE-DATA-ELEMENT("value",lcGenRespCode).

END FUNCTION.


/* main */

DEFINE INPUT PARAMETER pcRequest AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcMethodRequest AS CHARACTER NO-UNDO. 
lcMethodRequest = fGetNodeValue(pcRequest,"methodName") NO-ERROR.

/* generate an error XML response - probability 1/1000 */
IF RANDOM(1,1000) EQ 1 THEN RETURN "ERROR".

DEFINE VARIABLE lhSAXWriter  AS HANDLE    NO-UNDO.
DEFINE VARIABLE llOK         AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE lmXML        AS MEMPTR    NO-UNDO.
DEFINE VARIABLE lcRoot       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStruct     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcXML        AS CHARACTER NO-UNDO.
  
ASSIGN
    lcRoot   = "methodResponse"
    lcStruct = "params,param".


CREATE SAX-WRITER lhSAXWriter. 
   
lhSAXWriter:FORMATTED = FALSE. 
   
llOK = lhSAXWriter:SET-OUTPUT-DESTINATION("memptr",lmXML).
   
llOK = lhSAXWriter:START-DOCUMENT().
llOK = lhSAXWriter:START-ELEMENT(lcRoot).

fRPCStruct("Start",lcStruct,lhSAXWriter).


/* select request type */
CASE lcMethodRequest :
     WHEN "CreditScoring.do_scoring" THEN fCreditScoring(INPUT-OUTPUT lhSAXWriter).
END CASE.


fRPCStruct("End",lcStruct,lhSAXWriter).

llOK = lhSAXWriter:END-ELEMENT(lcRoot).

llOK = lhSAXWriter:END-DOCUMENT().

DELETE OBJECT lhSAXWriter.

lcXML = GET-STRING(lmXML,1).

SET-SIZE(lmXML) = 0.

RETURN lcXML.
