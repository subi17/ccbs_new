/* ----------------------------------------------------------------------
  MODULE .......: fsepa.i
  TASK .........: functions for SEPA
  APPLICATION ..: TMS 
  AUTHOR .......: Vikas
  CREATED ......: 16.07.14
  Version ......: Yoigo
---------------------------------------------------------------------- */

FUNCTION fCheckSEPASpecialChar RETURNS CHAR (INPUT icText AS CHAR):

   DEF VAR liCount    AS INT  NO-UNDO.
   DEF VAR lcNewText  AS CHAR NO-UNDO.
   DEF VAR lcChar     AS CHAR NO-UNDO.
   DEF VAR liASCChar  AS INT  NO-UNDO.

   IF icText = "" OR icText = ? THEN RETURN "".

   ASSIGN icText = REPLACE(icText,"Ñ","N")
          icText = REPLACE(icText,"Ç","C").

   DO liCount = 1 TO LENGTH(icText):

      ASSIGN lcChar    = SUBSTRING(icText,liCount,1)
             liASCChar = ASC(lcChar).

      IF (liASCChar >= ASC("a") AND liASCChar <= ASC("z")) OR 
         (liASCChar >= ASC("A") AND liASCChar <= ASC("Z")) OR
         (liASCChar >= ASC("0") AND liASCChar <= ASC("9")) OR
         (liASCChar >= ASC("+") AND liASCChar <= ASC("/"))THEN
         lcNewText = lcNewText + lcChar.
      ELSE IF
         lcChar = "?" OR lcChar = ":" OR lcChar = "(" OR
         lcChar = ")" OR lcChar = "'" OR lcChar = " " THEN
         lcNewText = lcNewText + lcChar.
      ELSE
         lcNewText = lcNewText + "0".

   END.

   RETURN lcNewText.

END.