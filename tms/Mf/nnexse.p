/*------------------------------------------------------
  MODULE .......: NNEXSE.P
  Called BY ....: APPLHELP.P
  FUNCTION .....: Vaihteiden (Exchange) BROWSE
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 03.09.1997 pt
  MODIFIED  ....: 09.03.1999 kl OnLine 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR opnro      LIKE Exchange.ExCode     NO-UNDO.
DEF VAR haku       LIKE Exchange.ExCode     NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 11     NO-UNDO.
DEF VAR ufkey      AS LOG init TRUE       NO-UNDO.
DEF VAR i          AS INT                 NO-UNDO.
DEF VAR ylin       AS RECID               NO-UNDO.
DEF VAR must-print AS logic               NO-UNDO.
DEF VAR must-add   AS logic               NO-UNDO.
def var nro        as char format "x(5)"  NO-UNDO.
def var tyhja      as char format "x(80)" NO-UNDO.

form
    Exchange.ExCode  column-label "Ex.num"
    ExName        column-label "Name"
WITH
    scroll 1 11 DOWN  ROW 4 centered COLOR value(Syst.Var:cfc)
    title color value(Syst.Var:ctc) " EXCHANGES "
    OVERLAY FRAME tlse.

form /* Maa :n hakua varten */
    haku
    help "Ange vAxelkod"
WITH
    row 4 col 2 title color value(Syst.Var:ctc) " FIND CODE  "
    COLOR value(Syst.Var:cfc) NO-LABELS OVERLAY FRAME hayr.

Syst.Var:cfc = "tlse". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
Runko:
repeat:

   FIND FIRST Exchange no-lock no-error.
   IF NOT AVAILABLE Exchange THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      Ylin = recid(Exchange).
      must-print = TRUE.
   END.

LOOP:
   Repeat WITH FRAME tlse:

print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME tlse ALL no-pause.
         FIND Exchange where recid(Exchange) = ylin no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE Exchange:
            DISPLAY Exchange.ExCode ExName WITH FRAME tlse.
            rtab[FRAME-LINE] = recid(Exchange).
            DOWN WITH FRAME tlse.
            FIND NEXT Exchange
            no-lock no-error.
         END.
         must-print = FALSE.
         up frame-line(tlse) - 1 WITH FRAME tlse.
      END. /* must-print */

      IF ufkey THEN DO:
         ASSIGN
         Syst.Var:ufk = 0 Syst.Var:ufk[1] = 35 Syst.Var:ufk[5] = 11
         Syst.Var:ufk[6] = 0 Syst.Var:ufk[8] = 8  Syst.Var:ufk[9] = 1
         siirto = ? Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.
  END. /* print-line */

BROWSE:
      repeat WITH FRAME tlse ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW Exchange.ExCode {Syst/uchoose.i} no-error WITH FRAME tlse.
         COLOR DISPLAY value(Syst.Var:ccc) Exchange.ExCode WITH FRAME tlse.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         Syst.Var:nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(Syst.Var:nap,"cursor-up") > 0 THEN DO
         WITH FRAME tlse:
            IF FRAME-LINE = 1 THEN DO:
               FIND Exchange where recid(Exchange) = rtab[FRAME-LINE] no-lock.
               FIND prev Exchange no-lock no-error.
               IF NOT AVAILABLE Exchange THEN DO:
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
                  DISPLAY Exchange.ExCode ExName.
                  rtab[FRAME-LINE] = recid(Exchange).
                  ylin = recid(Exchange).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(Syst.Var:nap,"cursor-down") > 0 THEN DO WITH FRAME tlse:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND Exchange where recid(Exchange) = rtab[FRAME-LINE] no-lock .
               FIND NEXT Exchange no-lock no-error.
               IF NOT AVAILABLE Exchange THEN DO:
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
                  DISPLAY Exchange.ExCode ExName.
                  rtab[FRAME-LINE] = recid(Exchange).
                  /* finally LAST line's KeyValue is saved */
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(Syst.Var:nap,"page-up,prev-page") > 0 THEN DO WITH FRAME tlse:
            FIND Exchange where recid(Exchange) = ylin no-lock no-error.
            FIND prev Exchange no-lock no-error.
            IF AVAILABLE Exchange THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev Exchange no-lock no-error.
                  IF AVAILABLE Exchange THEN ylin = recid(Exchange).
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
        else if lookup(Syst.Var:nap,"page-down,next-page") > 0 THEN DO WITH FRAME tlse:
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

        /* Haku */
        if lookup(Syst.Var:nap,"1,f1") > 0 THEN DO:  /* haku */
           Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
           haku = "". Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           UPDATE haku WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           IF haku <> "" THEN DO:
              FIND FIRST Exchange where Exchange.ExCode >= haku
              no-lock no-error.
              IF NOT AVAILABLE Exchange THEN DO:
                 BELL.
                 message "CAN'T FIND".
                 PAUSE 1 no-message.
                 NEXT BROWSE.
              END.
              /*  was found */
              ASSIGN
              ylin = recid(Exchange)
              must-print = TRUE.
           END.
           NEXT LOOP.
        END. /* Haku */

        /* Valinta */
        else if lookup(Syst.Var:nap,"return,enter,5,f5") > 0 THEN DO:
           FIND Exchange where recid(Exchange) = rtab[FRAME-LINE] no-lock.
           siirto = string(Exchange.ExCode).
           LEAVE runko.
        END. /* Valinta */

        /* Lisays */
        else if lookup(Syst.Var:nap,"6,f6") > 0 THEN DO:
           ASSIGN must-add = TRUE.
           NEXT LOOP.
        END. /* Lisays */

        /* Ensimmainen tietue */
        else if lookup(Syst.Var:nap,"home,h") > 0 THEN DO:
           FIND FIRST Exchange no-lock.
           ylin = recid(Exchange).
           must-print = TRUE.
           NEXT LOOP.
        END. /* Ensimmainen tietue */

        /* LAST record */
        else if lookup(Syst.Var:nap,"end,e") > 0 THEN DO :
           FIND LAST Exchange no-lock.
           ylin = recid(Exchange).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if Syst.Var:nap = "8" or Syst.Var:nap = "f8" THEN LEAVE runko. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* Runko */
HIDE FRAME tlse no-pause.

