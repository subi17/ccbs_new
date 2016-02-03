/*------------------------------------------------------
  MODULE .......: NNCUSE.P
  CALLD BY .....: APPLHELP.P
  FUNCTION .....: Currency BROWSE
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 30-06-99
  MODIFIED  ....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR Currency     AS c                    NO-UNDO.
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
   Currency.Currency  
   Currency.CurrName  column-label "Name"
WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
   title color value(ctc) " Currency CODE "
   OVERLAY FRAME tlse.

form
   Currency.Currency label "Code" SKIP
   Currency.CurrName label "Name" skip(1)
WITH OVERLAY ROW 8 centered TITLE COLOR value(ctc) tlli-ots
   COLOR value(cfc) side-labels 1 col  FRAME tlli.

form 
   Currency
   help "Give a country's Name or beginning of it"
with row 4 col 2 title color value(ctc) " FIND Code  "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

cfc = "tlse". RUN ufcolor. ASSIGN ccc = cfc.
Runko:
repeat:

   FIND FIRST Currency no-lock no-error.
   IF NOT AVAILABLE Currency THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      Ylin = recid(Currency).
      must-print = TRUE.
   END.

LOOP:
   Repeat WITH FRAME tlse:

   IF must-add THEN DO:  /* Maa  lisays  */
      ASSIGN
      cfc = "tlli"
      tlli-ots = " ADD ".
      RUN ufcolor.
add-new:
      repeat WITH FRAME tlli:
         PAUSE 0 no-message.
         CLEAR FRAME tlli no-pause.
         repeat WITH FRAME tlli:
            Currency = "". UPDATE Currency.
            if input Currency = "" THEN DO:
               HIDE FRAME tlli no-pause.
               LEAVE add-new.
            END.
            IF can-find(Currency where 
                        Currency.Currency = Currency) THEN DO:
               BELL.
               message "Land  " + string(input Currency) + " already exists !".
               NEXT.
            END.
            LEAVE.
         END.
         CREATE Currency.
         ASSIGN
           ylin = recid(Currency)
           Currency.Currency = Currency.
         UPDATE Currency.CurrName.
         CLEAR FRAME tlli no-pause.
      END.  /* add-new */
      ASSIGN
      must-add = FALSE
      must-print = TRUE.

      /* onko yhtaan tietuetta ? */
      FIND FIRST Currency where no-lock no-error.
      IF NOT AVAILABLE Currency THEN LEAVE LOOP.
      NEXT LOOP.
   END.


print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME tlse ALL no-pause.
         FIND Currency where recid(Currency) = ylin no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE Currency:
            DISPLAY Currency.Currency Currency.CurrName WITH FRAME tlse.
            rtab[FRAME-LINE] = recid(Currency).
            DOWN WITH FRAME tlse.
            FIND NEXT Currency where no-lock no-error.
         END.
         must-print = FALSE.
         up frame-line(tlse) - 1 WITH FRAME tlse.
      END. /* must-print */

      IF ufkey THEN DO:
         ASSIGN
         ufk = 0 ufk[1] = 718 ufk[5] = 11
         ufk[6] = 5 ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = FALSE.
         {Syst/uright1.i '"6"'}
         RUN ufkey.p.
      END.
  END. /* print-line */

BROWSE:
      repeat WITH FRAME tlse ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW Currency.CurrName ;(uchoose.i;) no-error WITH FRAME tlse.
         COLOR DISPLAY value(ccc) Currency.CurrName WITH FRAME tlse.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME tlse:
            IF FRAME-LINE = 1 THEN DO:
               FIND Currency where recid(Currency) = rtab[FRAME-LINE] no-lock.
               FIND prev Currency where no-lock no-error.
               IF NOT AVAILABLE Currency THEN DO:
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
                  DISPLAY Currency.Currency Currency.CurrName.
                  rtab[FRAME-LINE] = recid(Currency).
                  ylin = recid(Currency).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME tlse:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND Currency where recid(Currency) = rtab[FRAME-LINE] no-lock .
               FIND NEXT Currency where no-lock no-error.
               IF NOT AVAILABLE Currency THEN DO:
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
                  DISPLAY Currency.Currency Currency.CurrName.
                  rtab[FRAME-LINE] = recid(Currency).
                  /* finally LAST line's KeyValue is saved */
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME tlse:
            FIND Currency where recid(Currency) = ylin no-lock no-error.
            FIND prev Currency where no-lock no-error.
            IF AVAILABLE Currency THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev Currency where no-lock no-error.
                  IF AVAILABLE Currency THEN ylin = recid(Currency).
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

        /* Currency */
        if lookup(nap,"1,f1") > 0 THEN DO:  /* Currency */
           cfc = "puyr". RUN ufcolor.
           Currency = "". ehto = 9. RUN ufkey. ufkey = TRUE.
           UPDATE Currency WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           if Currency <> "" THEN DO:
              FIND FIRST Currency where 
                         Currency.CurrName >= Currency
              no-lock no-error.
              IF NOT AVAILABLE Currency THEN DO:
                 BELL.
                 message "CAN'T FIND".
                 PAUSE 1 no-message.
                 NEXT BROWSE.
              END.
              /*  Maa  was found */
              ASSIGN
              ylin = recid(Currency)
              must-print = TRUE.
           END.
           NEXT LOOP.
        END. /* Currency */

        /* Valinta */
        else if lookup(nap,"return,enter,5,f5") > 0 THEN DO:
           FIND Currency where recid(Currency) = rtab[FRAME-LINE] no-lock.
           siirto = string(Currency.Currency).
           LEAVE runko.
        END. /* Valinta */

        /* Lisays */
        else if lookup(nap,"6,f6") > 0 THEN DO:
           {Syst/uright2.i}
           ASSIGN must-add = TRUE.
           NEXT LOOP.
        END. /* Lisays */

        /* Ensimmainen tietue */
        else if lookup(nap,"home,h") > 0 THEN DO:
           FIND FIRST Currency where no-lock no-error.
           ylin = recid(Currency).
           must-print = TRUE.
           NEXT LOOP.
        END. /* Ensimmainen tietue */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST Currency where no-lock no-error.
           ylin = recid(Currency).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE runko. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* Runko */
HIDE FRAME tlse no-pause.

