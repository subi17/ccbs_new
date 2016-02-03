/*------------------------------------------------------
  MODULE .......: NNSECSE.P
  Called BY ....: APPLHELP.P
  FUNCTION .....: Browse CDR sections
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 02.06.1999 kl
  MODIFIED  ....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR RepType       LIKE MedSect.Type   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 11     NO-UNDO.
DEF VAR ufkey      AS LOG init TRUE       NO-UNDO.
DEF VAR i          AS INT                 NO-UNDO.
DEF VAR ylin       AS RECID               NO-UNDO.
DEF VAR must-print AS logic               NO-UNDO.
DEF VAR must-add   AS logic               NO-UNDO.
def var nro        as char format "x(5)"  NO-UNDO.
def var tyhja      as char format "x(80)" NO-UNDO.

form
   MedSect.Type
   MedSect.Name
   MedSect.Num 
   MedSect.Resvd
   MedSect.Pref
   MedSect.Uniq
WITH width 55 OVERLAY centered ROW 3 scroll 1 12 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " FTAM sections "
   + string(pvm,"99-99-99") + " "
   FRAME tlse.

cfc = "tlse". RUN ufcolor. ASSIGN ccc = cfc.
Runko:
repeat:

   FIND FIRST MedSect no-lock no-error.
   IF NOT AVAILABLE MedSect THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      Ylin = recid(MedSect).
      must-print = TRUE.
   END.

LOOP:
   Repeat WITH FRAME tlse:

print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME tlse ALL no-pause.
         FIND MedSect where recid(MedSect) = ylin no-lock no-error.

         /* TMSReporttaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE MedSect:
            DISP 
               MedSect.Type
               MedSect.Name
               MedSect.Num 
               MedSect.Resvd
               MedSect.Pref
               MedSect.Uniq
            WITH FRAME tlse.
            rtab[FRAME-LINE] = recid(MedSect).
            DOWN WITH FRAME tlse.
            FIND NEXT MedSect
            no-lock no-error.
         END.
         must-print = FALSE.
         up frame-line(tlse) - 1 WITH FRAME tlse.
      END. /* must-print */

      IF ufkey THEN DO:
         ASSIGN
         ufk = 0 ufk[1] = 35 ufk[5] = 11
         ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.
  END. /* print-line */

BROWSE:
      repeat WITH FRAME tlse ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW MedSect.Type ;(uchoose.i;) no-error WITH FRAME tlse.
         COLOR DISPLAY value(ccc) 
            MedSect.Type
            MedSect.Name
            MedSect.Num 
            MedSect.Resvd
            MedSect.Pref
            MedSect.Uniq
         WITH FRAME tlse.
         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME tlse:
            IF FRAME-LINE = 1 THEN DO:
               FIND MedSect where recid(MedSect) = rtab[FRAME-LINE] no-lock.
               FIND prev MedSect no-lock no-error.
               IF NOT AVAILABLE MedSect THEN DO:
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
                  DISP 
                     MedSect.Type
                     MedSect.Name
                     MedSect.Num 
                     MedSect.Resvd
                     MedSect.Pref
                     MedSect.Uniq
                  WITH FRAME tlse.
                  rtab[FRAME-LINE] = recid(MedSect).
                  ylin = recid(MedSect).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME tlse:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND MedSect where recid(MedSect) = rtab[FRAME-LINE] no-lock .
               FIND NEXT MedSect no-lock no-error.
               IF NOT AVAILABLE MedSect THEN DO:
                  BELL.
                  message "YOU ARE ON THE LAST ROW".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* yet another record was found */
                  scroll up.
                  DO i = 1 TO 10:
                     rtab[i] = rtab[i + 1].
                  END.
                  DISP 
                     MedSect.Type
                     MedSect.Name
                     MedSect.Num 
                     MedSect.Resvd
                     MedSect.Pref
                     MedSect.Uniq
                  WITH FRAME tlse.
                  rtab[FRAME-LINE] = recid(MedSect).
                  /* finally LAST line's KeyValue is saved */
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME tlse:
            FIND MedSect where recid(MedSect) = ylin no-lock no-error.
            FIND prev MedSect no-lock no-error.
            IF AVAILABLE MedSect THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev MedSect no-lock no-error.
                  IF AVAILABLE MedSect THEN ylin = recid(MedSect).
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
        else if lookup(nap,"page-down,next-page") > 0 THEN DO WITH FRAME tlse:
           IF rtab[FRAME-DOWN] = ? THEN DO:
               BELL.
               message "YOU ARE ON THE LAST PAGE".
               PAUSE 1 no-message.
           END.
           ELSE DO: /* the downmost line wasn't empty */
               ylin = rtab[FRAME-DOWN].
               must-print = TRUE.
               NEXT LOOP.
           END.
        END. /* NEXT page */

        /* RepType */
        if lookup(nap,"1,f1") > 0 THEN DO:  /* RepType */
           cfc = "puyr". RUN ufcolor.
           RepType = 0. ehto = 9. RUN ufkey. ufkey = TRUE.
           UPDATE RepType WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           IF RepType <> 0 THEN DO:
              FIND FIRST MedSect where MedSect.Type >= type
              no-lock no-error.
              IF NOT AVAILABLE MedSect THEN DO:
                 BELL.
                 message "CAN'T FIND".
                 PAUSE 1 no-message.
                 NEXT BROWSE.
              END.
              /*  was found */
              ASSIGN
              ylin = recid(MedSect)
              must-print = TRUE.
           END.
           NEXT LOOP.
        END. /* RepType */

        /* Valinta */
        else if lookup(nap,"return,enter,5,f5") > 0 THEN DO:
           FIND MedSect where recid(MedSect) = rtab[FRAME-LINE] no-lock.
           siirto = string(MedSect.Type).
           LEAVE runko.
        END. /* Valinta */

        /* Lisays */
        else if lookup(nap,"6,f6") > 0 THEN DO:
           ASSIGN must-add = TRUE.
           NEXT LOOP.
        END. /* Lisays */

        /* EnSIMmainen tietue */
        else if lookup(nap,"home,h") > 0 THEN DO:
           FIND FIRST MedSect no-lock.
           ylin = recid(MedSect).
           must-print = TRUE.
           NEXT LOOP.
        END. /* EnSIMmainen tietue */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST MedSect no-lock.
           ylin = recid(MedSect).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE runko. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* Runko */
HIDE FRAME tlse no-pause.

