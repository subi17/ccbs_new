/*------------------------------------------------------
  MODULE .......: NNAKSE.P
  KUTSUVAMODULI : APPLHELP.P
  FUNCTION .....: Kategorioiden BROWSE
  SOVELLUTUS ...: NN
  AUTHOR .......: PT
  CREATED ......: 29.09.96
  changePVM ....: 25.05.99 jp joitakuita englannuksia
                  16.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
DEF shared VAR siirto AS CHAR.

DEF VAR opnro LIKE CustCat.Category NO-UNDO.
DEF VAR haku LIKE CustCat.Category NO-UNDO.
DEF VAR rtab AS RECID EXTENT 11 NO-UNDO.
DEF VAR ufkey AS LOG init TRUE NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR ylin AS RECID NO-UNDO.
DEF VAR must-print AS logic NO-UNDO.
DEF VAR must-add AS logic NO-UNDO.
def var nro      as char format "x(5)" NO-UNDO.
def var tyhja as char format "x(80)" NO-UNDO.

form
    CustCat.Category
    CatName
    WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
    title color value(ctc) " CATEGORIES (" + gcBrand + ") " OVERLAY FRAME tlse.

form /* Maa :n hakua varten */
    haku
    help "Give a code or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND CODE  "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

cfc = "tlse". RUN ufcolor. ASSIGN ccc = cfc.
Runko:
repeat:

   FIND FIRST CustCat WHERE CustCat.Brand = gcBrand NO-LOCK no-error.
   IF NOT AVAILABLE CustCat THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      Ylin = recid(CustCat).
      must-print = TRUE.
   END.

LOOP:
   Repeat WITH FRAME tlse:

print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME tlse ALL no-pause.
         FIND CustCat where recid(CustCat) = ylin no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE CustCat:
            DISPLAY CustCat.Category CatName WITH FRAME tlse.
            rtab[FRAME-LINE] = recid(CustCat).
            DOWN WITH FRAME tlse.
            FIND NEXT CustCat
            WHERE CustCat.Brand = gcBrand NO-LOCK no-error.
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
         CHOOSE ROW CustCat.Category ;(uchoose.i;) no-error WITH FRAME tlse.
         COLOR DISPLAY value(ccc) CustCat.Category WITH FRAME tlse.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME tlse:
            IF FRAME-LINE = 1 THEN DO:
               FIND CustCat where recid(CustCat) = rtab[FRAME-LINE] no-lock.
               FIND prev CustCat WHERE CustCat.Brand = gcBrand NO-LOCK no-error.
               IF NOT AVAILABLE CustCat THEN DO:
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
                  DISPLAY CustCat.Category CatName.
                  rtab[FRAME-LINE] = recid(CustCat).
                  ylin = recid(CustCat).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME tlse:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND CustCat where recid(CustCat) = rtab[FRAME-LINE] no-lock .
               FIND NEXT CustCat WHERE CustCat.Brand = gcBrand NO-LOCK no-error.
               IF NOT AVAILABLE CustCat THEN DO:
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
                  DISPLAY CustCat.Category CatName.
                  rtab[FRAME-LINE] = recid(CustCat).
                  /* finally LAST line's KeyValue is saved */
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME tlse:
            FIND CustCat where recid(CustCat) = ylin no-lock no-error.
            FIND prev CustCat WHERE CustCat.Brand = gcBrand NO-LOCK no-error.
            IF AVAILABLE CustCat THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev CustCat WHERE CustCat.Brand = gcBrand 
                  NO-LOCK no-error.
                  IF AVAILABLE CustCat THEN ylin = recid(CustCat).
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

        /* Haku */
        if lookup(nap,"1,f1") > 0 THEN DO:  /* haku */
           cfc = "puyr". RUN ufcolor.
           haku = "". ehto = 9. RUN ufkey. ufkey = TRUE.
           UPDATE haku WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           if haku <> "" THEN DO:
              FIND FIRST CustCat where 
                 CustCat.Brand     = gcBrand AND
                 CustCat.Category >= haku
              no-lock no-error.
              IF NOT AVAILABLE CustCat THEN DO:
                 BELL.
                 message "CAN'T FIND".
                 PAUSE 1 no-message.
                 NEXT BROWSE.
              END.
              /*  was found */
              ASSIGN
              ylin = recid(CustCat)
              must-print = TRUE.
           END.
           NEXT LOOP.
        END. /* Haku */

        /* Valinta */
        else if lookup(nap,"return,enter,5,f5") > 0 THEN DO:
           FIND CustCat where recid(CustCat) = rtab[FRAME-LINE] no-lock.
           siirto = string(CustCat.Category).
           LEAVE runko.
        END. /* Valinta */

        /* Lisays */
        else if lookup(nap,"6,f6") > 0 THEN DO:
           ASSIGN must-add = TRUE.
           NEXT LOOP.
        END. /* Lisays */

        /* Ensimmainen tietue */
        else if lookup(nap,"home,h") > 0 THEN DO:
           FIND FIRST CustCat WHERE CustCat.Brand = gcBrand NO-LOCK.
           ylin = recid(CustCat).
           must-print = TRUE.
           NEXT LOOP.
        END. /* Ensimmainen tietue */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST CustCat WHERE CustCat.Brand = gcBrand NO-LOCK.
           ylin = recid(CustCat).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE runko. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* Runko */
HIDE FRAME tlse no-pause.

