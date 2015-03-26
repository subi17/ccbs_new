DEFINE TEMP-TABLE ttCharSet NO-UNDO
   FIELD tcIRA AS CHARACTER
   FIELD charCode AS CHARACTER
   FIELD tcAsc AS CHARACTER CASE-SENSITIVE
   
   INDEX tcAsc AS PRIMARY tcAsc.
   

DEFINE VARIABLE lcLine    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIRAFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcGSMCharCodes AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcGSMCharCode AS CHARACTER NO-UNDO. 

lcIRAFile = fCParamC("IRACharFile").

INPUT FROM VALUE(lcIRAFile).

REPEAT:

   IMPORT UNFORMATTED lcLine.

   lcGSMCharCodes = ENTRY(2,lcLine,CHR(9)).
   IF NUM-ENTRIES(lcGSMCharCodes, " ") EQ 1 THEN
      lcGSMCharCode = CHR(INT(lcGSMCharCodes)).
   ELSE
      lcGSMCharCode = CHR(INT(ENTRY(1,lcGSMCharCodes, " "))) +
                      CHR(INT(ENTRY(2,lcGSMCharCodes, " "))).

   CREATE ttCharSet.
   ASSIGN
      ttCharSet.tcIRA = ENTRY(1,lcLine,CHR(9))
      ttCharSet.tcAsc = ENTRY(4,lcLine,CHR(9))
      ttCharSet.charCode = lcGSMCharCode.

END.

INPUT CLOSE.

FUNCTION fIRAChar RETURNS CHARACTER
  (INPUT pcChar AS CHARACTER):

   DEFINE VARIABLE lcIRA  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liLoop AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcChar AS CHARACTER NO-UNDO. 

   DO liLoop = 1 TO LENGTH(pcChar):

      lcChar = SUBSTR(pcChar,liLoop,1).
      /* temporary hot fix */
      IF lcChar EQ "?" THEN DO:
         lcIRA = lcIRA + "3F".
         NEXT.
      END.

      FIND FIRST ttCharSet WHERE
                 COMPARE(ttCharSet.tcAsc, "=",lcChar,"RAW")
      NO-LOCK NO-ERROR.
      IF AVAIL ttCharSet THEN lcIRA = lcIRA + ttCharSet.tcIRA.
   
   END.
   
   RETURN lcIRA.

END FUNCTION.

FUNCTION fConvertToGSM0338 RETURNS CHARACTER
  (INPUT pcChar AS CHARACTER):

   DEFINE VARIABLE lcIRA  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liLoop AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcChar AS CHARACTER NO-UNDO. 

   DO liLoop = 1 TO LENGTH(pcChar):

      lcChar = SUBSTR(pcChar,liLoop,1).
      /* temporary hot fix */
      IF lcChar EQ "?" THEN DO:
         lcIRA = lcIRA + CHR(63).
         NEXT.
      END.

      FIND FIRST ttCharSet WHERE
                 COMPARE(ttCharSet.tcAsc, "=",lcChar,"RAW")
      NO-LOCK NO-ERROR.
      IF AVAIL ttCharSet THEN lcIRA = lcIRA + ttCharSet.charCode.
   
   END.
   
   RETURN lcIRA.

END FUNCTION.
