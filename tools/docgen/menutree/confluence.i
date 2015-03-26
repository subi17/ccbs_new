&IF "{&CONFLUENCE_I}" NE "YES" &THEN
&GLOBAL-DEFINE CONFLUENCE_I YES

&GLOBAL-DEFINE LF CHR(10)
&GLOBAL-DEFINE BREAKLINE " " + CHR(92) + CHR(92) + " "


FUNCTION fWrapExternalLink RETURN CHAR
(INPUT icText AS CHAR,
 INPUT icTargetURL AS CHAR):

   RETURN "[" + icText + "|" + icTargetURL + "]".
END. 
   


FUNCTION fWrapAnchorLink RETURN CHAR
(INPUT icText AS CHAR,
 INPUT icTargetAnchor AS CHAR):
   
   IF TRIM(icText) EQ "" THEN RETURN " ".
   IF TRIM(icTargetAnchor) EQ "" THEN RETURN " ".

   RETURN " [" + icText + "|#" + icTargetAnchor + "] ".

END. 

FUNCTION fWrapMonospaced RETURN CHAR 
(INPUT icText AS CHAR):

   RETURN "~{~{monospaced}}" + icText + "~{~{monospaced}}".
END.

FUNCTION fWrapBlockQuote RETURN CHAR
(INPUT icText AS CHAR):
   RETURN "bq. " + icText.
   
END.

FUNCTION fLevelToString RETURN CHAR
(INPUT icLevel AS CHAR):
   
   DEFINE VARIABLE lcTemp AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE i AS INTEGER NO-UNDO. 

   DO i = 1 TO LENGTH(icLevel):
      lcTemp = lcTemp + " F" + SUBSTRING(icLevel,i,1).
   END.

   RETURN TRIM(lcTemp).
   
END.

FUNCTION fWrapColor RETURN CHAR
(INPUT icText AS CHAR,
 INPUT icColor AS CHAR):
   IF icText EQ "" OR icText EQ ? THEN RETURN "".
   RETURN "~{color:" + icColor + "}" + icText + "~{color}".
END.

FUNCTION fWrapRed RETURN CHAR
(INPUT icText AS CHAR):
   RETURN fWrapColor(icText,"red").
END.

FUNCTION fWrapMenuText RETURN CHAR
(INPUT icText AS CHAR,
 INPUT icTargetAnchor AS CHAR):

   IF icText EQ ? THEN
      RETURN fWrapBlockQuote(   
         " " + {&BREAKLINE} +
         fWrapColor(".","lightblue") + " ").
         
   DEFINE VARIABLE lcLine1 AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcLine2 AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcTemp AS CHARACTER NO-UNDO. 

   lcLine1 = SUBSTRING(icText,1,8).
   lcLine2 = SUBSTRING(icText,9,8).

   IF TRIM(lcLine2) EQ "" AND TRIM(lcLine1) NE "" THEN DO:
      lcTemp = lcLine1.
      lcLine1 = lcLine2.
      lcLine2 = lcTemp.
   END.
   
   IF icTargetAnchor NE ? AND icTargetAnchor NE "" THEN
      RETURN fWrapBlockQuote(
         fWrapAnchorLink(lcLine1,icTargetAnchor) +
         {&BREAKLINE} +
         fWrapAnchorLink(lcLine2,icTargetAnchor)).

   RETURN fWrapBlockQuote(   
      lcLine1 + {&BREAKLINE} +
      lcLine2 + " ").
END.

FUNCTION fTrimName RETURN CHAR
(INPUT icName AS CHAR):
   DO WHILE icName NE REPLACE(icName,"  "," "):
      icName = REPLACE(icName,"  "," ").
   END.
   RETURN icName.
END.

FUNCTION fWrapTip RETURN CHAR
(INPUT icTitle AS CHAR,
 INPUT icMsg AS CHAR):
   RETURN "~{tip:icon=false|title=" + icTitle + "}" + icMsg + "~{tip}".
END. 


FUNCTION fWrapNote RETURN CHAR
(INPUT icTitle AS CHAR,
 INPUT icMsg AS CHAR):
   RETURN "~{note:icon=false|title=" + icTitle + "}" + icMsg + "~{note}".
END. 

FUNCTION fWrapWarning RETURN CHAR
(INPUT icTitle AS CHAR,
 INPUT icMsg AS CHAR):
   RETURN "~{warning:icon=false|title=" + icTitle + "}" + icMsg + "~{warning}".
END. 

FUNCTION fWrapInfo RETURN CHAR
(INPUT icTitle AS CHAR,
 INPUT icMsg AS CHAR):
   RETURN "~{info:icon=false|title=" + icTitle + "}" + icMsg + "~{info}".
END. 

FUNCTION fWrapColumn RETURN CHAR
(INPUT icText AS CHAR): 

   RETURN 
      "~{column:width=12.5%}" +
      icText +
      "~{column}".

END FUNCTION.


&ENDIF
