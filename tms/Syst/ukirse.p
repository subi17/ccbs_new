/*------------------------------------------------------
  MODULE .......: UKIRSE.P
  KUTSUVAMODULI : APPLHELP.P
  FUNCTION .....: Kirjoittimien BROWSE ja valinta
  SOVELLUTUS ...: TS
  AUTHOR .......: TT
  CREATED ......: 27.06.1991
  changePVM ....: 03.04.92/tt
                  25.09.96 /tt --> Ruotsinnettu
                  25.05.99 /JP --> Englannettu  
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR rtab        AS RECID EXTENT 11 NO-UNDO.
DEF VAR ufkey       AS LOG init TRUE.
DEF VAR i           AS INT NO-UNDO.
DEF VAR ylin        AS RECID NO-UNDO.
DEF VAR must-print  AS logic NO-UNDO.

form
TMSPrinter.PrinterId       /* column-label "Printer"
                          help "Printer"         */
WITH scroll 1 11 DOWN  ROW 4 col 15 COLOR value(Syst.Var:cfc)
title color value(Syst.Var:ctc) " ALL PRINTERS "
OVERLAY FRAME sel.

runko:
repeat ON ENDKEY UNDO runko, NEXT runko:
   ASSIGN
   siirto = ?
   Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.

   FIND FIRST TMSPrinter  no-lock no-error.
   IF NOT AVAILABLE TMSPrinter THEN DO:

      message "You haven't any printers in database ! ".
      BELL. PAUSE 2 no-message.
      RETURN.
   END.

   ELSE DO:
      Ylin = recid(TMSPrinter).
      must-print = TRUE.
   END.

LOOP:
   Repeat WITH FRAME sel:
      IF must-print THEN DO:
         CLEAR FRAME sel ALL no-pause.

         FIND TMSPrinter where recid(TMSPrinter) = ylin no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE TMSPrinter:
            DISPLAY TMSPrinter.PrinterId WITH FRAME sel.
            rtab[FRAME-LINE] = recid(TMSPrinter).
            DOWN WITH FRAME sel.
            FIND NEXT TMSPrinter  no-lock no-error.
         END.
         must-print = FALSE.
         up frame-line(sel) - 1 WITH FRAME sel.
      END. /* must-print */

      IF ufkey THEN DO:
        ASSIGN
        Syst.Var:ufk[1] = 0 
        Syst.Var:ufk[6] = 11 Syst.Var:ufk[7] = 0  Syst.Var:ufk[8] = 8  Syst.Var:ufk[9] = 1 ufkey = FALSE Syst.Var:ehto = 3.
        RUN Syst/ufkey.p.
     END.

BROWSE:
      repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE.
         CHOOSE ROW TMSPrinter.PrinterId {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(Syst.Var:ccc) TMSPrinter.PrinterId WITH FRAME sel.

         if frame-value = " " AND rtab[FRAME-LINE] = ? THEN NEXT.
         Syst.Var:nap = keylabel(LASTKEY).

         /* previous line */
         if Syst.Var:nap = "1" or Syst.Var:nap = "f1" or Syst.Var:nap = "cursor-up" THEN DO
         WITH FRAME sel:
            IF FRAME-LINE = 1 THEN DO:
               FIND TMSPrinter where recid(TMSPrinter) = rtab[FRAME-LINE] no-lock.
               FIND prev TMSPrinter  no-lock no-error.
               IF NOT AVAILABLE TMSPrinter THEN DO:
                  BELL.
                  message "YOU ARE ON THE FIRST ROW !".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.

               ELSE DO:
                  /* a previous one was found */
                  scroll DOWN.
                  DO i = 11 TO 2 BY -1:
                     rtab[i] = rtab[i - 1].
                  END.

                  DISPLAY TMSPrinter.PrinterId .
                  rtab[FRAME-LINE] = recid(TMSPrinter).
                  ylin = recid(TMSPrinter).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         else if Syst.Var:nap = "2" or Syst.Var:nap = "f2" or Syst.Var:nap = "cursor-down" THEN DO
         WITH FRAME sel:

            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND TMSPrinter where recid(TMSPrinter) = rtab[FRAME-LINE] no-lock.
               FIND NEXT TMSPrinter  no-lock no-error.
               IF NOT AVAILABLE TMSPrinter THEN DO:
                  BELL.
                  message "YOU ARE ON THE LAST ROW !".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* was found vielA seuraava tietue */
                  scroll up.
                  DO i = 1 TO 10:
                     rtab[i] = rtab[i + 1].
                  END.

                  DISPLAY TMSPrinter.PrinterId .
                  rtab[FRAME-LINE] = recid(TMSPrinter).
                  /* ja lopuksi pannaan memoryin ylimmAn linen avain */
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if Syst.Var:nap = "page-up" or Syst.Var:nap = "prev-page" THEN DO:
            FIND TMSPrinter where recid(TMSPrinter) = ylin no-lock no-error.
            FIND prev TMSPrinter  no-lock no-error.

            IF AVAILABLE TMSPrinter THEN DO:

               /* go back one page */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev TMSPrinter  no-lock no-error.
                  IF AVAILABLE TMSPrinter THEN ylin = recid(TMSPrinter).
                  ELSE i = FRAME-DOWN.
               END.
               must-print = TRUE.
               NEXT LOOP.
            END.
            ELSE DO:
               /* this is the FIRST data page */
               BELL.
               message "YOU ARE ON THE FIRST PAGE !".
               PAUSE 1 no-message.
            END.
        END. /* previous page */


        /* NEXT page */
        else if Syst.Var:nap = "page-down" or Syst.Var:nap = "next-page" THEN DO WITH FRAME sel:

           IF rtab[FRAME-DOWN] = ? THEN DO:
               BELL.
               message "YOU ARE ON THE LAST PAGE !".
               PAUSE 1 no-message.
           END.
           ELSE DO: /* the downmost line wasn't empty */
               ylin = rtab[FRAME-DOWN].
               must-print = TRUE.
               NEXT LOOP.
           END.
        END. /* NEXT page */


        else  if Syst.Var:nap = "enter" or Syst.Var:nap = "return" OR
        Syst.Var:nap = "f6" or Syst.Var:nap = "6" THEN DO:
           /* valinta */
           FIND TMSPrinter where recid(TMSPrinter) = rtab[FRAME-LINE] no-lock.
           siirto = TMSPrinter.PrinterId.
           LEAVE runko.
        END.

        else if Syst.Var:nap = "end,e" THEN DO : /* LAST record */
           FIND LAST TMSPrinter  no-lock.
           ylin = recid(TMSPrinter).
           must-print = TRUE.
           NEXT LOOP.
        END.

        else if Syst.Var:nap = "home,h" THEN DO:
           FIND FIRST TMSPrinter  no-lock.
           ylin = recid(TMSPrinter).
           must-print = TRUE.
           NEXT LOOP.
        END.

        else if Syst.Var:nap = "8" or Syst.Var:nap = "f8" THEN LEAVE runko.

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* runko */
HIDE FRAME sel no-pause.

