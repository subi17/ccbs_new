/* -------------------------------------------------------------------------
  MODULE .......: UTAB.P
  PURPOSE ......: Search AND parse FIRST word from a TAB-separated STRING
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 16.07.197 PT
  CHANGED ......:
  Version ......: M15
------------------------------------------------------------------------- */

DEF input-output  PARAMETER xrow AS c.
DEF INPUT         PARAMETER trm  AS lo.
DEF OUTPUT        PARAMETER word AS c.

DEF VAR tab     AS c NO-UNDO.
DEF VAR i       AS i NO-UNDO.


tab = chr(9).

/* check IF there is a tab CHAR in the STRING 'xrow' */

i = index(xrow,tab).
if i = 0 and xrow ne "" THEN i = length(xrow) + 1.

IF i > 0 THEN DO:
   /* extract the STRING preceeding FIRST tab */
   ASSIGN word = substr(xrow,1,i - 1)
    /* cut the extracted STRING away from xrow */
    xrow = substr(xrow,i + 1).
    IF trm THEN word = trim(word).
END.
ELSE word = ?.

