/* ----------------------------------------------------------------
  MODULE .......: UEXSKIP.P
  FUNCTION .....: Puts cr/lf -codes into shared STREAM excel.
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 17.07.1997
  MODIFIED .....: 22.06.1998 kl win32
  Version ......: M15
  ------------------------------------------------------------------ */

DEF INPUT PARAMETER lines AS INT.
DEF shared STREAM excel.

DEF VAR i AS INT NO-UNDO.

if opsys = "msdos" or opsys = "win32" THEN DO:
   IF lines = 1 THEN PUT STREAM excel SKIP.
   ELSE              PUT STREAM excel skip(lines - 1).
END.
ELSE DO:
   DO i = 1 TO lines.
      PUT STREAM excel UNFORMATTED chr(13) + chr(10).
   END.
END.

