DEFINE INPUT PARAMETER pcInputFile AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcSuccessLogFile AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER pcErrorLogFile AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER plSimulate AS LOGICAL NO-UNDO. 

DEFINE VARIABLE cLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCountry AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cPLMN AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCountryCode AS CHARACTER NO-UNDO. 

DEFINE STREAM sInput.
DEFINE STREAM sLogFile.
DEFINE STREAM sLogFile2.
INPUT STREAM sInput FROM VALUE(pcInputFile).
OUTPUT STREAM sLogFile TO VALUE(pcSuccessLogFile).
OUTPUT STREAM sLogFile2 TO VALUE(pcErrorLogFile).

DEFINE VARIABLE lFirstLine AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cSep AS CHARACTER INIT ";" NO-UNDO. 
lFirstLine = TRUE.

REPEAT:
   IMPORT STREAM sInput UNFORMATTED cLine.
   IF lFirstLine THEN
   DO:
      lFirstLine = FALSE.
      NEXT.
   END.
   IF NUM-ENTRIES(cLine, cSep) NE 4 THEN
   DO:
      PUT STREAM sLogFile2 UNFORMATTED 
         "Error! Line " cLine " did not contain 4 entries separated by ;" SKIP.
   END.
   cCountry = ENTRY(1, cLine, cSep).
   cName = ENTRY(2, cLine, cSep).
   cPLMN = ENTRY(3, cLine, cSep).
   cCountryCode = ENTRY(4, cLine, cSep).
   FIND FIRST PLMN WHERE 
      PLMN.Country = cCountryCode AND
      PLMN.PLMN = cPLMN NO-LOCK NO-ERROR.
   FIND FIRST RzItem WHERE 
      RzItem.PlmnCode = cPLMN NO-LOCK NO-ERROR. 

   IF NOT AVAIL PLMN THEN
   DO:
      IF NOT plSimulate THEN
      DO:
         CREATE PLMN.
         ASSIGN PLMN.CommName = cName
                PLMN.CoName =  cCountry
                PLMN.Country = cCountryCode
                PLMN.PLMN = cPLMN
                PLMN.PrefToNor = "".
      END.
      PUT STREAM sLogFile UNFORMATTED
         "Added PLMN with countryCode " cCountryCode
         ", Country name " cCountry 
         ", commercial name " cName 
         " and PLMN " cPLMN  SKIP.
   END.
   ELSE
   DO:
      IF AVAIL PLMN THEN
        PUT STREAM sLogFile2 UNFORMATTED
           "Error! PLMN for countrycode " cCountryCode
           " with PLMN " cPLMN " already existed." SKIP.
   END.
   IF NOT AVAIL RzItem THEN
   DO:
      IF NOT plSimulate THEN
      DO:
         CREATE RzItem.
         ASSIGN RzItem.CountryPrefix = cCountryCode
                RzItem.DialType = 4 
                RzItem.PlmnCode = cPLMN
                RzItem.RoamZone = "ROAM_EU".
      END.
      PUT STREAM sLogFile UNFORMATTED
         "Added RzItem with countryCode " cCountryCode
         ", DialType = 4, RoamZone = ROAM_EU, PlmnCode = " cPLMN SKIP.
   END.
   ELSE
   DO:
      IF AVAIL RzItem THEN
        PUT STREAM sLogFile2 UNFORMATTED
           "Error! RzItem with PLMN " cPLMN 
           " already existed." SKIP. 
   END.
END.

INPUT STREAM sInput CLOSE.
OUTPUT STREAM sLogFile CLOSE.
OUTPUT STREAM sLogFile2 CLOSE.
