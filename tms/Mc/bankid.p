/*------------------------------------------------------
  MODULE .......: Bankid.P
  KUTSUVAMODULI : MM. APPLHELP.P
  FUNCTION .....: BankId
  SOVELLUTUS ...:
  AUTHOR .......: JR
  CREATED ......: 09.10.02
  CHANGED ......: 16.09.03/aam brand
  Version ......: M15

  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.
DEF VAR rtab          AS RECID EXTENT 11 NO-UNDO.
DEF VAR i             AS INT NO-UNDO.
DEF VAR ylin          AS RECID NO-UNDO.
DEF VAR must-print    AS logic NO-UNDO.

form
bank.bankid COLUMN-LABEL "Bank"
bank.bankoffice   COLUMN-LABEL "Office"
WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(Syst.Var:cfc)
title color value(Syst.Var:ctc) " Banks (" + Syst.Var:gcBrand + ") "
OVERLAY FRAME kase.

Syst.Var:cfc = "kase". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
runko:
repeat:

   ASSIGN

     Syst.Var:ufk = 0 Syst.Var:ufk[5] = 11
     Syst.Var:ufk[6] = 0  Syst.Var:ufk[7] = 0  Syst.Var:ufk[8] = 8  Syst.Var:ufk[9] = 1 siirto = ?.

   Syst.Var:ehto = 3. RUN Syst/ufkey.p.

   FIND FIRST bank WHERE Bank.Brand = Syst.Var:gcBrand NO-LOCK no-error.
   IF NOT AVAILABLE bank THEN DO:

      message " No Banks at all !".
      BELL. PAUSE 2 no-message.
      HIDE FRAME kase no-pause.
      RETURN.
   END.

   ELSE DO:
      Ylin = recid(bank).
      must-print = TRUE.
   END.


LOOP:
   Repeat WITH FRAME kase:
      IF must-print THEN DO:
         CLEAR FRAME kase ALL no-pause.
         FIND bank where recid(bank) = ylin no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE bank:
            DISPLAY  bank.bankid bank.bankoffice WITH FRAME kase.
            rtab[FRAME-LINE] = recid(bank).
            DOWN WITH FRAME kase.
            FIND NEXT bank WHERE Bank.Brand = Syst.Var:gcBrand NO-LOCK no-error.
         END.
         must-print = FALSE.
         up frame-line(kase) - 1 WITH FRAME kase.
      END. /* must-print */

BROWSE:
      repeat WITH FRAME kase ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE.
         CHOOSE ROW  bank.bankid {Syst/uchoose.i} no-error WITH FRAME kase.
         COLOR DISPLAY value(Syst.Var:ccc)  bank.bankid WITH FRAME kase.

         if frame-value = " " AND rtab[FRAME-LINE] = ? THEN NEXT.
         Syst.Var:nap = keylabel(LASTKEY).


         /* previous line */
         if Syst.Var:nap = "1" or Syst.Var:nap = "f1" or Syst.Var:nap = "cursor-up" THEN DO
         WITH FRAME kase:
            IF FRAME-LINE = 1 THEN DO:
               FIND bank where recid(bank) = rtab[FRAME-LINE] no-lock.
               FIND prev bank WHERE Bank.Brand = Syst.Var:gcBrand NO-LOCK no-error.
               IF NOT AVAILABLE bank THEN DO:
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

                  DISPLAY  bank.bankid bank.bankoffice.
                  rtab[FRAME-LINE] = recid(bank).
                  ylin = recid(bank).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         else if Syst.Var:nap = "2" or Syst.Var:nap = "f2" or Syst.Var:nap = "cursor-down" THEN DO
         WITH FRAME kase:

            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND bank where recid(bank) = rtab[FRAME-LINE] no-lock .
               FIND NEXT bank WHERE Bank.Brand = Syst.Var:gcBrand NO-LOCK no-error.
               IF NOT AVAILABLE bank THEN DO:
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

                  DISPLAY  bank.bankid bank.bankoffice.
                  rtab[FRAME-LINE] = recid(bank).
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if Syst.Var:nap = "page-up" or Syst.Var:nap = "prev-page" THEN DO:
            FIND bank where recid(bank) = ylin no-lock no-error.
            FIND prev bank WHERE Bank.Brand = Syst.Var:gcBrand NO-LOCK no-error.

            IF AVAILABLE bank THEN DO:

               /* go back one page */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev bank WHERE Bank.Brand = Syst.Var:gcBrand NO-LOCK no-error.
                  IF AVAILABLE bank THEN ylin = recid(bank).
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
        else if Syst.Var:nap = "page-down" or Syst.Var:nap = "next-page" THEN DO
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


        else  if Syst.Var:nap = "enter" or Syst.Var:nap = "return" OR
        Syst.Var:nap = "f5" or Syst.Var:nap = "5" THEN DO:
           /* valinta */
           siirto = frame-value.
           LEAVE runko.
        END.


        else if Syst.Var:nap = "end,e" THEN DO : /* LAST record */
           FIND LAST bank WHERE Bank.Brand = Syst.Var:gcBrand NO-LOCK.
           ylin = recid(bank).
           must-print = TRUE.
           NEXT LOOP.
        END.

        else if Syst.Var:nap = "home,h" THEN DO:
           FIND FIRST bank WHERE Bank.Brand = Syst.Var:gcBrand NO-LOCK.
           ylin = recid(bank).
           must-print = TRUE.
           NEXT LOOP.
        END.


        else if Syst.Var:nap = "8" or Syst.Var:nap = "f8" THEN LEAVE runko.

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* runko */
HIDE FRAME kase no-pause.

