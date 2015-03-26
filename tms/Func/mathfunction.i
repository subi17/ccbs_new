FUNCTION fInt2Bin RETURNS CHARACTER
  (INPUT piInte AS INTEGER):

   DEF VAR liDigits AS INT  NO-UNDO.
   DEF VAR liLoop   AS INT  NO-UNDO.
   DEF VAR lcBina   AS CHAR NO-UNDO INIT "0".

   /* count length for binary string */
   DO WHILE EXP(2,liDigits) <= piInte:
      liDigits = liDigits + 1.
   END.
   liDigits = liDigits - 1.

   /* set ones and zeroes */
   DO liLoop = liDigits TO 0 by -1:

      IF piInte >= exp(2,liLoop) THEN ASSIGN
         lcBina = lcBina + "1"
         piInte = piInte - EXP(2,liLoop).
      ELSE lcBina = lcBina + "0".

   END.

   RETURN lcBina.

END.

FUNCTION fBin2Int RETURNS INTEGER
  (INPUT pcBina AS CHARACTER):

   DEF VAR liLoop AS INTEGER NO-UNDO.
   DEF VAR liInte AS INTEGER NO-UNDO.
   
   /* loop length of binary and calculate digits backwards */
   DO liLoop = 0 TO LENGTH(pcBina) - 1:
      liInte = liInte +
              INT(SUBSTR(pcBina,LENGTH(pcBina) - liLoop,1)) * EXP(2,liLoop).
   END.

   RETURN liInte.

END.

FUNCTION fInt2Hex RETURNS CHARACTER
  (INPUT piInte AS INTEGER):

   DEF VAR liDigits  AS INT  NO-UNDO.
   DEF VAR liLoop    AS INT  NO-UNDO.
   DEF VAR liLoop2   AS INT  NO-UNDO.
   DEF VAR lcHexa    AS CHAR NO-UNDO.
   DEF VAR lcHexList AS CHAR NO-UNDO INIT "0,1,2,3,4,5,6,7,8,9,A,B,C,D,E,F".
   
   /* count length for binary string */
   DO WHILE EXP(16,liDigits) <= piInte:
      liDigits = liDigits + 1.
   END.
   liDigits = liDigits - 1.

   /* set hexa characters */
   IF piInte >= 0 AND piInte <= 9 THEN lcHexa = STRING(piInte).
   ELSE DO liLoop = liDigits TO 0 by -1:

      IF piInte > exp(16,liLoop) THEN DO:
         
         liLoop2 = 0.
         DO WHILE liLoop2 * exp(16,liLoop) <= piInte:
            liLoop2 = liLoop2 + 1.
         END.
         
         ASSIGN
            lcHexa = lcHexa + ENTRY(liLoop2,lcHexList)
            piInte = piInte - ((liLoop2 - 1) * EXP(16,liLoop)).
      
      END.
      ELSE lcHexa = lcHexa + "0".

   END.

   RETURN lcHexa.
   
END.

FUNCTION fHex2Int RETURNS INTEGER
  (INPUT piHexa AS CHARACTER):

   DEF VAR liLoop  AS INT  NO-UNDO.
   DEF VAR liInte  AS INT  NO-UNDO.
   DEF VAR lcTmp   AS CHAR NO-UNDO.
   DEF VAR liValue AS INT  NO-UNDO.
   
   /* loop length of hexa and calculate digits backwards */
   DO liLoop = LENGTH(piHexa) TO 1 BY -1:
   
      lcTmp = UPPER(SUBSTR(piHexa,liLoop,1)).
      
      INT(lcTmp) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN liValue = KEYCODE(lcTmp) - 55.
      ELSE                       liValue = KEYCODE(lcTmp) - 48.
      
      liValue = liValue * exp(16,LENGTH(piHexa) - liLoop).

      liInte = liInte + liValue.
   
   END.
   
   RETURN liInte.

END.
