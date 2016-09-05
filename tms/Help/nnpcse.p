/*------------------------------------------------------
  MODULE .......: NNPCSE.P
  CALLING MODULE: APPLHELP.P
  FUNCTION .....: Search of Program Classes
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 27.04.99
  CHANGED ......:
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR MenuClass       LIKE MenuClass.MenuClass      NO-UNDO.
DEF VAR rtab        AS RECID EXTENT 11      NO-UNDO.
DEF VAR ufkey       AS LOG init TRUE        NO-UNDO.
DEF VAR i           AS INT                  NO-UNDO.
DEF VAR ylin        AS RECID                NO-UNDO.
DEF VAR must-print  AS logic                NO-UNDO.
DEF VAR must-add    AS logic                NO-UNDO.
def var nro         as char format "x(5)"   NO-UNDO.
def var tyhja       as char format "x(80)"  NO-UNDO.
DEF VAR tlli-ots    AS CHAR.

form
    MenuClass.MenuClass
    MenuClass.MCName  format "x(30)"
    MenuClass.Memo[1]  format "x(34)"
WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
    title color value(ctc) " Program classes " OVERLAY FRAME tlse.

form
    MenuClass.MenuClass SKIP
    MenuClass.MCName SKIP
    MenuClass.Memo[1]    SKIP

WITH OVERLAY ROW 8 centered
    TITLE COLOR value(ctc) tlli-ots
    COLOR value(cfc) side-labels 1 col
    FRAME tlli.

form /* Program Class :n hakua varten */
    MenuClass
    help "Enter No of Class"
    with row 4 col 2 title color value(ctc) " FIND Program Class "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

cfc = "tlse". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
Runko:
repeat:

   FIND FIRST MenuClass no-lock no-error.
   IF NOT AVAILABLE MenuClass THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      Ylin = recid(MenuClass).
      must-print = TRUE.
   END.

LOOP:
   Repeat WITH FRAME tlse:
   IF must-add THEN DO:  /* Program Class  lisays  */
      ASSIGN
      cfc = "tlli"
      tlli-ots = " ADD ".
      RUN Syst/ufcolor.p.
add-new:
      repeat WITH FRAME tlli:
         PAUSE 0 no-message.
         CLEAR FRAME tlli no-pause.
         repeat WITH FRAME tlli:
            PROMPT-FOR MenuClass.MenuClass.
            if input MenuClass.MenuClass = "" THEN DO:
               HIDE FRAME tlli no-pause.
               LEAVE add-new.
            END.
            IF CAN-FIND (MenuClass where MenuClass.MenuClass =
            INPUT MenuClass.MenuClass) THEN DO:
               BELL.
               message "Program Class  " + string(INPUT MenuClass.MenuClass)
               + " exists already !".
               NEXT.
            END.
            LEAVE.
         END.
         CREATE MenuClass.
         ASSIGN
           ylin = recid(MenuClass)
           MenuClass.MenuClass = INPUT MenuClass.MenuClass.
         UPDATE MenuClass.MCName MenuClass.Memo[1] .
         CLEAR FRAME tlli no-pause.
      END.  /* add-new */
      ASSIGN
      must-add = FALSE
      must-print = TRUE.

      /* onko yhtaan tietuetta ? */
      FIND FIRST MenuClass no-lock no-error.
      IF NOT AVAILABLE MenuClass THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME tlse ALL no-pause.
         FIND MenuClass where recid(MenuClass) = ylin no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE MenuClass:
            DISPLAY
            MenuClass.MenuClass
            MenuClass.MCName
            MenuClass.Memo[1] WITH FRAME tlse.
            rtab[FRAME-LINE] = recid(MenuClass).
            DOWN WITH FRAME tlse.
            FIND NEXT MenuClass no-lock no-error.
         END.
         must-print = FALSE.
         up frame-line(tlse) - 1 WITH FRAME tlse.
      END. /* must-print */

      IF ufkey THEN DO:
         ASSIGN
         ufk = 0 ufk[1] = 35 ufk[5] = 11
         ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.
  END. /* print-line */

BROWSE:
      repeat WITH FRAME tlse ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW MenuClass.MenuClass {Syst/uchoose.i} no-error WITH FRAME tlse.
         COLOR DISPLAY value(ccc) MenuClass.MenuClass WITH FRAME tlse.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME tlse:
            IF FRAME-LINE = 1 THEN DO:
               FIND MenuClass where recid(MenuClass) = rtab[FRAME-LINE] no-lock.
               FIND prev MenuClass no-lock no-error.
               IF NOT AVAILABLE MenuClass THEN DO:
                  BELL.
                  message "THIS IS THE FIRST ROW !".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* a previous one was found */
                  scroll DOWN.
                  DO i = 11 TO 2 BY -1:
                     rtab[i] = rtab[i - 1].
                  END.
                  DISPLAY MenuClass.MenuClass MenuClass.MCName MenuClass.Memo[1].
                  rtab[FRAME-LINE] = recid(MenuClass).
                  ylin = recid(MenuClass).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME tlse:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND MenuClass where recid(MenuClass) = rtab[FRAME-LINE] no-lock .
               FIND NEXT MenuClass no-lock no-error.
               IF NOT AVAILABLE MenuClass THEN DO:
                  BELL.
                  message "THIS IS THE LAST ROW !".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* yet another record was found */
                  scroll up.
                  DO i = 1 TO 10:
                     rtab[i] = rtab[i + 1].
                  END.
                  DISPLAY MenuClass.MenuClass MenuClass.MCName MenuClass.Memo[1].
                  rtab[FRAME-LINE] = recid(MenuClass).
                  /* finally LAST line's KeyValue is saved */
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME tlse:
            FIND MenuClass where recid(MenuClass) = ylin no-lock no-error.
            FIND prev MenuClass no-lock no-error.
            IF AVAILABLE MenuClass THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev MenuClass no-lock no-error.
                  IF AVAILABLE MenuClass THEN ylin = recid(MenuClass).
                  ELSE i = FRAME-DOWN.
               END.
               must-print = TRUE.
               NEXT LOOP.
            END.
            ELSE DO:
               /* this is the FIRST data page */
               BELL.
               message "THIS IS THE FIRST PAGE !".
               PAUSE 1 no-message.
            END.
        END. /* previous page */

        /* NEXT page */
        else if lookup(nap,"page-down,next-page") > 0 THEN DO WITH FRAME tlse:
           IF rtab[FRAME-DOWN] = ? THEN DO:
               BELL.
               message "THIS IS THE LAST PAGE !".
               PAUSE 1 no-message.
           END.
           ELSE DO: /* the downmost line wasn't empty */
               ylin = rtab[FRAME-DOWN].
               must-print = TRUE.
               NEXT LOOP.
           END.
        END. /* NEXT page */

        /* Haku */
        if lookup(nap,"1,f1") > 0 THEN DO:  /* haku */
           cfc = "puyr". RUN Syst/ufcolor.p.
           MenuClass = 0.
           ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           UPDATE MenuClass WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           IF MenuClass <> 0 THEN DO:
              FIND FIRST MenuClass where MenuClass.MenuClass >= INPUT MenuClass
              no-lock no-error.
              IF NOT AVAILABLE MenuClass THEN DO:
                 message "NOT FOUND !".
                 PAUSE 1 no-message.
                 NEXT BROWSE.
              END.
              /*  Program Class  was found */
              ASSIGN
                ylin = recid(MenuClass)
                must-print = TRUE.
           END.
           NEXT LOOP.
        END. /* Haku */

        /* Valinta */
        else if lookup(nap,"return,enter,5,f5") > 0 THEN DO:
           FIND MenuClass where recid(MenuClass) = rtab[FRAME-LINE] no-lock.
           siirto = string(MenuClass.MenuClass).
           LEAVE runko.
        END. /* Valinta */

        /* Ensimmainen tietue */
        else if lookup(nap,"home,h") > 0 THEN DO:
           FIND FIRST MenuClass no-lock.
           ylin = recid(MenuClass).
           must-print = TRUE.
           NEXT LOOP.
        END. /* Ensimmainen tietue */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST MenuClass no-lock.
           ylin = recid(MenuClass).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE runko. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* Runko */
HIDE FRAME tlse no-pause.

