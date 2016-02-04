/*------------------------------------------------------
  MODULE .......: TARGETS.P
  KUTSUVAMODULI : MM. APPLHELP.P
  FUNCTION .....: Target  (Table name) For invoice texts
  SOVELLUTUS ...:
  AUTHOR .......: JR
  CREATED ......: 26.09.02
  CHANGED ......: 
  Version ......: M15

  ------------------------------------------------------ */

{Syst/commali.i}
{Func/invotxt.i}

DEF shared VAR siirto AS CHAR.
DEF VAR rtab          AS RECID EXTENT 11 NO-UNDO.
DEF VAR i             AS INT NO-UNDO.
DEF VAR ylin          AS RECID NO-UNDO.
DEF VAR must-print    AS logic NO-UNDO.

form
t-target.target COLUMN-LABEL "Target"
t-target.expl   COLUMN-LABEL "Description"
WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
title color value(ctc) " Targets "
OVERLAY FRAME kase.

cfc = "kase". RUN Syst/ufcolor. ASSIGN ccc = cfc.
runko:
repeat:

   ASSIGN

     ufk = 0 ufk[5] = 11
     ufk[6] = 0  ufk[7] = 0  ufk[8] = 8  ufk[9] = 1 siirto = ?.

   ehto = 3. RUN Syst/ufkey.p.

   FIND FIRST t-target no-lock no-error.
   IF NOT AVAILABLE t-target THEN DO:

      message " No Targets at all !".
      BELL. PAUSE 2 no-message.
      HIDE FRAME kase no-pause.
      RETURN.
   END.

   ELSE DO:
      Ylin = recid(t-target).
      must-print = TRUE.
   END.


LOOP:
   Repeat WITH FRAME kase:
      IF must-print THEN DO:
         CLEAR FRAME kase ALL no-pause.
         FIND t-target where recid(t-target) = ylin no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE t-target:
            DISPLAY  t-target.target t-target.expl WITH FRAME kase.
            rtab[FRAME-LINE] = recid(t-target).
            DOWN WITH FRAME kase.
            FIND NEXT t-target no-lock no-error.
         END.
         must-print = FALSE.
         up frame-line(kase) - 1 WITH FRAME kase.
      END. /* must-print */

BROWSE:
      repeat WITH FRAME kase ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE.
         CHOOSE ROW  t-target.target ;(uchoose.i;) no-error WITH FRAME kase.
         COLOR DISPLAY value(ccc)  t-target.target WITH FRAME kase.

         if frame-value = " " AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).


         /* previous line */
         if nap = "1" or nap = "f1" or nap = "cursor-up" THEN DO
         WITH FRAME kase:
            IF FRAME-LINE = 1 THEN DO:
               FIND t-target where recid(t-target) = rtab[FRAME-LINE] no-lock.
               FIND prev t-target no-lock no-error.
               IF NOT AVAILABLE t-target THEN DO:
                  BELL.
                  message "This is the 1st Row !".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.

               ELSE DO:
                  /* a previous one was found */
                  scroll DOWN.
                  DO i = 11 TO 2 BY -1:
                     rtab[i] = rtab[i - 1].
                  END.

                  DISPLAY  t-target.target t-target.expl.
                  rtab[FRAME-LINE] = recid(t-target).
                  ylin = recid(t-target).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         else if nap = "2" or nap = "f2" or nap = "cursor-down" THEN DO
         WITH FRAME kase:

            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND t-target where recid(t-target) = rtab[FRAME-LINE] no-lock .
               FIND NEXT t-target no-lock no-error.
               IF NOT AVAILABLE t-target THEN DO:
                  BELL.
                  message "This is the last row !".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  scroll up.
                  DO i = 1 TO 10:
                     rtab[i] = rtab[i + 1].
                  END.

                  DISPLAY  t-target.target t-target.expl.
                  rtab[FRAME-LINE] = recid(t-target).
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if nap = "page-up" or nap = "prev-page" THEN DO:
            FIND t-target where recid(t-target) = ylin no-lock no-error.
            FIND prev t-target no-lock no-error.

            IF AVAILABLE t-target THEN DO:

               /* go back one page */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev t-target no-lock no-error.
                  IF AVAILABLE t-target THEN ylin = recid(t-target).
                  ELSE i = FRAME-DOWN.
               END.
               must-print = TRUE.
               NEXT LOOP.
            END.
            ELSE DO:
               /* this is the FIRST data page */
               BELL.
               message "This is the first page !".
               PAUSE 1 no-message.
            END.
        END. /* previous page */


        /* NEXT page */
        else if nap = "page-down" or nap = "next-page" THEN DO
        WITH FRAME kase:

           IF rtab[FRAME-DOWN] = ? THEN DO:
               BELL.
               message "This is the last page !".
               PAUSE 1 no-message.
           END.
           ELSE DO: /* the downmost line wasn't empty */
               ylin = rtab[FRAME-DOWN].
               must-print = TRUE.
               NEXT LOOP.
           END.
        END. /* NEXT page */


        else  if nap = "enter" or nap = "return" OR
        nap = "f5" or nap = "5" THEN DO:
           /* valinta */
           siirto = frame-value.
           LEAVE runko.
        END.


        else if nap = "end,e" THEN DO : /* LAST record */
           FIND LAST t-target no-lock.
           ylin = recid(t-target).
           must-print = TRUE.
           NEXT LOOP.
        END.

        else if nap = "home,h" THEN DO:
           FIND FIRST t-target no-lock.
           ylin = recid(t-target).
           must-print = TRUE.
           NEXT LOOP.
        END.


        else if nap = "8" or nap = "f8" THEN LEAVE runko.

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* runko */
HIDE FRAME kase no-pause.

