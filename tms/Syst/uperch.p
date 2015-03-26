/* ---------------------------------------------------------------------
  MODULE .......: UPERCH.P
  FUNCTION .....: Check validity of a Period code YYYYMM
  APPLICATION ..: NN
  CREATED ......: 01.12.98 pt
  MODIFIED .....:
  Version ......: M15
  -------------------------------------------------------------------- */

DEF INPUT PARAMETER   pcode AS i  NO-UNDO.
DEF OUTPUT PARAMETER  rc    AS i  NO-UNDO.

DEF VAR yyyy AS i  NO-UNDO.
DEF VAR mm   AS i  NO-UNDO.
DEF VAR msg  AS c  NO-UNDO.


rc = 0.
IF pcode < 199601 THEN DO:
   msg = "Invalid Period number (MUST be 199601 or greater)".
   rc  = 1.
END.
ELSE DO:
   yyyy = truncate(pcode / 100,0).
   mm   = pcode - (yyyy * 100).

   IF mm < 1 OR mm > 12 THEN DO:
      rc = 2.
      msg = "Invalid month (MUST be 01 ... 12 !)".
   END.
   IF yyyy ne 9999 AND yyyy > 2100 THEN DO:
      rc = 2.
      msg = "Invalid year (Must be less than 2100 !)".
   END.
END.

IF rc > 0 THEN DO:
   BELL.
   MESSAGE msg.
END.

