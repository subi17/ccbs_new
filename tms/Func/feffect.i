/* feffect.i        23.03.04/aam 

*/


DEF VAR lcSFontOn    AS CHAR                         NO-UNDO. 
DEF VAR lcSFontOff   AS CHAR                         NO-UNDO. 
DEF VAR lcMFontOn    AS CHAR                         NO-UNDO. 
DEF VAR lcMFontOff   AS CHAR                         NO-UNDO. 
DEF VAR lcBFontOn    AS CHAR                         NO-UNDO. 
DEF VAR lcBFontOff   AS CHAR                         NO-UNDO. 

FUNCTION fEffectCode RETURNS CHARACTER
   (icPrintCode AS CHAR).

   DEF VAR lcEffect AS CHAR NO-UNDO.
   DEF VAR liECnt   AS INT  NO-UNDO. 
   
   lcEffect = "".
   
   DO liECnt= 1 TO LENGTH(icPrintCode):
      IF SUBSTRING(icPrintCode,liECnt,1) = CHR(92)
      THEN ASSIGN lcEffect = lcEffect + 
                             CHR(INT(SUBSTRING(icPrintCode,liECnt + 1,3)))
                  liECnt   = liECnt + 3.

      ELSE lcEffect = lcEffect + SUBSTRING(icPrintCode,liECnt,1).
   END.

   RETURN lcEffect. 
   
END FUNCTION.


FUNCTION fInitEffects RETURNS LOGICAL:

   ASSIGN lcSFontOn = fCParamC("PrintSmallFont")
          lcMFontOn = fCParamC("PrintMediumFont")
          lcBFontOn = fCParamC("PrintBoldFont").

   IF NUM-ENTRIES(lcSFontOn) = 2 THEN DO:
       ASSIGN lcSFontOff = fEffectCode(ENTRY(2,lcSFontOn))
              lcSFontOn  = fEffectCode(ENTRY(1,lcSFontOn)).
   END.           
   ELSE lcSFontOn = "".

   IF NUM-ENTRIES(lcMFontOn) = 2 THEN DO:
       ASSIGN lcMFontOff = fEffectCode(ENTRY(2,lcMFontOn))
              lcMFontOn  = fEffectCode(ENTRY(1,lcMFontOn)).
   END.           
   ELSE lcMFontOn = "".

   IF NUM-ENTRIES(lcBFontOn) = 2 THEN DO:
       ASSIGN lcBFontOff = fEffectCode(ENTRY(2,lcBFontOn))
              lcBFontOn  = fEffectCode(ENTRY(1,lcBFontOn)).
   END.           
   ELSE lcBFontOn = "".              

END FUNCTION.

