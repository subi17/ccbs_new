{commali.i}
{tmsconst.i}

DEFINE VARIABLE lcMenu AS CHARACTER NO-UNDO
   EXTENT 9 FORMAT "X(30)".

CHOISES:
DO WHILE TRUE:

   ASSIGN
      ufk    = 0
      ufk[8] = 8
      ehto   = 3.

   RUN ufkey.

   on f1 bell.
   on f2 bell.
   on f4 bell.

   DISPLAY
      " A) MNP IN (OLD) " @ lcMenu[1]  SKIP
      " B) MNP IN " @ lcMenu[2]  SKIP
      " C) MNP OUT " @ lcMenu[3]  SKIP
      " D) TERMINATION " @ lcMenu[4]  SKIP
/*      " E) QUERIES " @ lcMenu[5]  SKIP
      " F) MIGRATION " @ lcMenu[6]  SKIP
      " F) MIGRATION NUMBER" @ lcMenu[7]  SKIP */
      " X) Quit  " @ lcMenu[9]  SKIP
   WITH OVERLAY FRAME choices NO-LABELS.

   CHOOSE FIELD lcMenu AUTO-RETURN GO-ON (F8) WITH FRAME choices
      TITLE " Choose MNP Type " CENTERED WITH COL 2 ROW 6.

   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"F8,x") > 0 THEN LEAVE.

   CASE FRAME-INDEX:

      WHEN 1 THEN RUN mnpstatus({&MNP_TYPE_OLD}).
      WHEN 2 THEN RUN mnpstatus({&MNP_TYPE_IN}).
      WHEN 3 THEN RUN mnpstatus({&MNP_TYPE_OUT}).
      WHEN 4 THEN RUN mnpstatus({&MNP_TYPE_TERMINATION}).
/*    WHEN 5 THEN RUN mnpstatus(4).
      WHEN 6 THEN RUN mnpstatus(5).
      WHEN 7 THEN RUN mnpstatus(6). */

      WHEN 9 THEN LEAVE CHOISES.
      OTHERWISE . /* RUN mnpstatus(FRAME-INDEX). */

   END.

END.

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.

