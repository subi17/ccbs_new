/*------------------------------------------------------
  MODULE .......: UKTSE.P
  KUTSUVAMODULI : MM. APPLHELP.P
  FUNCTION .....: KAyttAjAtunnusten BROWSE ja valinta
  SOVELLUTUS ...:
  AUTHOR .......: PT
  CREATED ......: 26.06.1989
  CHANGED ......: 08.02.1999 pt in English
  Version ......: M15

  Shared:

  siirto:  STRING, joka ilmaisee valitun numeron          (out)
  ------------------------------------------------------ */

{commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR rtab        AS RECID EXTENT 11 NO-UNDO.

DEF VAR i           AS INT NO-UNDO.
DEF VAR ylin        AS RECID NO-UNDO.
DEF VAR must-print  AS logic NO-UNDO.

form
TMSUser.UserCode  /* column-label "UserId"    */
TMSUser.UserName /* column-label "User Name" */
WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
title color value(ctc) " USERS "
OVERLAY FRAME kase.

cfc = "kase". RUN ufcolor. ASSIGN ccc = cfc.
runko:
repeat:

   ASSIGN

     ufk = 0 ufk[5] = 11
     ufk[6] = 0  ufk[7] = 0  ufk[8] = 8  ufk[9] = 1 siirto = ?.

   ehto = 3. RUN ufkey.p.

   FIND FIRST TMSUser no-lock no-error.
   IF NOT AVAILABLE TMSUser THEN DO:

      message " No users at all !".
      BELL. PAUSE 2 no-message.
      HIDE FRAME kase no-pause.
      RETURN.
   END.

   ELSE DO:
      Ylin = recid(TMSUser).
      must-print = TRUE.
   END.


LOOP:
   Repeat WITH FRAME kase:
      IF must-print THEN DO:
         CLEAR FRAME kase ALL no-pause.
         FIND TMSUser where recid(TMSUser) = ylin no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE TMSUser:
            DISPLAY TMSUser.UserCode TMSUser.UserName WITH FRAME kase.
            rtab[FRAME-LINE] = recid(TMSUser).
            DOWN WITH FRAME kase.
            FIND NEXT TMSUser no-lock no-error.
         END.
         must-print = FALSE.
         up frame-line(kase) - 1 WITH FRAME kase.
      END. /* must-print */



BROWSE:
      repeat WITH FRAME kase ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE.
         CHOOSE ROW TMSUser.UserCode ;(uchoose.i;) no-error WITH FRAME kase.
         COLOR DISPLAY value(ccc) TMSUser.UserCode WITH FRAME kase.

         if frame-value = " " AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).


         /* previous line */
         if nap = "1" or nap = "f1" or nap = "cursor-up" THEN DO
         WITH FRAME kase:
            IF FRAME-LINE = 1 THEN DO:
               FIND TMSUser where recid(TMSUser) = rtab[FRAME-LINE] no-lock.
               FIND prev TMSUser no-lock no-error.
               IF NOT AVAILABLE TMSUser THEN DO:
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

                  DISPLAY TMSUser.UserCode TMSUser.UserName.
                  rtab[FRAME-LINE] = recid(TMSUser).
                  ylin = recid(TMSUser).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         else if nap = "2" or nap = "f2" or nap = "cursor-down" THEN DO
         WITH FRAME kase:

            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND TMSUser where recid(TMSUser) = rtab[FRAME-LINE] no-lock .
               FIND NEXT TMSUser no-lock no-error.
               IF NOT AVAILABLE TMSUser THEN DO:
                  BELL.
                  message "This is the last row !".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* was found vielA seuraava tietue */
                  scroll up.
                  DO i = 1 TO 10:
                     rtab[i] = rtab[i + 1].
                  END.

                  DISPLAY TMSUser.UserCode TMSUser.UserName.
                  rtab[FRAME-LINE] = recid(TMSUser).
                  /* ja lopuksi pannaan memoryin ylimmAn linen avain */
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if nap = "page-up" or nap = "prev-page" THEN DO:
            FIND TMSUser where recid(TMSUser) = ylin no-lock no-error.
            FIND prev TMSUser no-lock no-error.

            IF AVAILABLE TMSUser THEN DO:

               /* go back one page */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev TMSUser no-lock no-error.
                  IF AVAILABLE TMSUser THEN ylin = recid(TMSUser).
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
           FIND LAST TMSUser no-lock.
           ylin = recid(TMSUser).
           must-print = TRUE.
           NEXT LOOP.
        END.

        else if nap = "home,h" THEN DO:
           FIND FIRST TMSUser no-lock.
           ylin = recid(TMSUser).
           must-print = TRUE.
           NEXT LOOP.
        END.


        else if nap = "8" or nap = "f8" THEN LEAVE runko.

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* runko */
HIDE FRAME kase no-pause.

