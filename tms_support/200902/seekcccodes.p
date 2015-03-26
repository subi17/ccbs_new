DEFINE INPUT PARAMETER pcInputFile AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcLogFile AS CHARACTER NO-UNDO. 

DEFINE STREAM sInput.
DEFINE STREAM sLogFile.
INPUT STREAM sInput FROM VALUE(pcInputFile).
OUTPUT STREAM sLogFile TO VALUE(pcLogFile).

DEFINE VARIABLE cCountry AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lFirstLine AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cSep AS CHARACTER INIT ";" NO-UNDO. 
lFirstLine = TRUE.
DEFINE VARIABLE cCountryCode AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cPLMNInfo AS CHARACTER NO-UNDO. 

REPEAT:
   IMPORT STREAM sInput UNFORMATTED cLine.
   IF lFirstLine THEN
   DO:
      lFirstLine = FALSE.
      NEXT.
   END.
   cCountry = ENTRY(1, cLine, cSep).
   cCountryCode = "".
   cPLMNInfo = "".
   FIND FIRST PLMN WHERE 
      PLMN.CoName = cCountry AND PLMN.Country NE "" NO-LOCK NO-ERROR.
   IF AVAIL PLMN THEN
   DO:
      cCountryCode = PLMN.Country.
      cPLMNInfo = cLine + ";" + cCountryCode.
   END.
   ELSE
   DO:
      CASE cCountry:
         WHEN "LiechtenStein" THEN cCountryCode = "423".
         OTHERWISE cCountryCode = "".
      END.  
      IF cCountryCode EQ "" THEN
         cPLMNInfo = "Country " + cCountry + " not exist in PLMN table".
      ELSE
         cPLMNInfo = cLine + ";" + cCountryCode.
   END.
   PUT STREAM sLogFile UNFORMATTED cPLMNInfo SKIP. 
END.

INPUT STREAM sInput CLOSE.
OUTPUT STREAM sLogFile CLOSE.
