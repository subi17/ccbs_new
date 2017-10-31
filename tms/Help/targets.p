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
WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(Syst.CUICommon:cfc)
title color value(Syst.CUICommon:ctc) " Targets "
OVERLAY FRAME kase.

Syst.CUICommon:cfc = "kase". RUN Syst/ufcolor.p. ASSIGN Syst.CUICommon:ccc = Syst.CUICommon:cfc.
runko:
repeat:

   ASSIGN

     Syst.CUICommon:ufk = 0 Syst.CUICommon:ufk[5] = 11
     Syst.CUICommon:ufk[6] = 0  Syst.CUICommon:ufk[7] = 0  Syst.CUICommon:ufk[8] = 8  Syst.CUICommon:ufk[9] = 1 siirto = ?.

   Syst.CUICommon:ehto = 3. RUN Syst/ufkey.p.

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
         CHOOSE ROW  t-target.target {Syst/uchoose.i} no-error WITH FRAME kase.
         COLOR DISPLAY value(Syst.CUICommon:ccc)  t-target.target WITH FRAME kase.

         if frame-value = " " AND rtab[FRAME-LINE] = ? THEN NEXT.
         Syst.CUICommon:nap = keylabel(LASTKEY).


         /* previous line */
         if Syst.CUICommon:nap = "1" or Syst.CUICommon:nap = "f1" or Syst.CUICommon:nap = "cursor-up" THEN DO
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
         else if Syst.CUICommon:nap = "2" or Syst.CUICommon:nap = "f2" or Syst.CUICommon:nap = "cursor-down" THEN DO
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
         else if Syst.CUICommon:nap = "page-up" or Syst.CUICommon:nap = "prev-page" THEN DO:
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
        else if Syst.CUICommon:nap = "page-down" or Syst.CUICommon:nap = "next-page" THEN DO
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


        else  if Syst.CUICommon:nap = "enter" or Syst.CUICommon:nap = "return" OR
        Syst.CUICommon:nap = "f5" or Syst.CUICommon:nap = "5" THEN DO:
           /* valinta */
           siirto = frame-value.
           LEAVE runko.
        END.


        else if Syst.CUICommon:nap = "end,e" THEN DO : /* LAST record */
           FIND LAST t-target no-lock.
           ylin = recid(t-target).
           must-print = TRUE.
           NEXT LOOP.
        END.

        else if Syst.CUICommon:nap = "home,h" THEN DO:
           FIND FIRST t-target no-lock.
           ylin = recid(t-target).
           must-print = TRUE.
           NEXT LOOP.
        END.


        else if Syst.CUICommon:nap = "8" or Syst.CUICommon:nap = "f8" THEN LEAVE runko.

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* runko */
HIDE FRAME kase no-pause.

