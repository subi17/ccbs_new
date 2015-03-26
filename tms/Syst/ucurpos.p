/* -----------------------------------------------
  MODULE .......: ucurpos.p
  FUNCTION .....: Set printer's cursor position
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 02-04-98
  changePVM ....:
  Version ......: M15
  ------------------------------------------------------ */

DEF INPUT PARAM y-pos AS i NO-UNDO.  /* ROW    */
DEF INPUT PARAM x-pos AS i NO-UNDO.  /* column */

DEF shared STREAM tul.

/* set y-position */
 put stream tul unformatted chr(027) + "*p" + string(y-pos) + "Y".

/* set x-position */
 put stream tul unformatted chr(027) + "*p" + string(x-pos) + "X".

