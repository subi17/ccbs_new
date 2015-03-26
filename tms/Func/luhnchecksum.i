/* ----------------------------------------------------------------------
  MODULE .......: luhnchecksum.i
  TASK .. ......: apply the Luhn formula to validate a number
  APPLICATION ..: TMS
  AUTHOR .......:
---------------------------------------------------------------------- */
FUNCTION fLuhnCheckSum RETURNS LOGICAL (INPUT lcNumber AS CHAR).
   DEFINE VARIABLE liStrLength AS INTEGER NO-UNDO.
   DEFINE VARIABLE liSum       AS INTEGER NO-UNDO.
   DEFINE VARIABLE liCount     AS INTEGER NO-UNDO.
   DEFINE VARIABLE liDigit     AS INTEGER NO-UNDO.
   DEFINE VARIABLE llSkip      AS LOGICAL NO-UNDO INIT FALSE.

   ASSIGN
         liStrLength = LENGTH(lcNumber)
         liSum       = 0.

   DO liCount = liStrLength to 1 by -1 :
        liDigit = INT(SUBSTRING(lcNumber,liCount,1)).
        IF llSkip THEN DO: 
            liDigit = liDigit * 2.
            IF liDigit > 9 THEN liDigit = liDigit - 9.
        END.
        liSum = liSum + liDigit.
        llSkip = NOT llSkip.
   END.

   IF (liSum MOD 10) = 0 THEN RETURN TRUE.
   ELSE RETURN FALSE.

END FUNCTION.
