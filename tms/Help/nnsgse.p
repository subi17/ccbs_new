/*------------------------------------------------------
  MODULE .......: NNSGSE.P
  KUTSUVAMODULI : APPLHELP.P
  FUNCTION .....: Search of Salesman Groups
  SOVELLUTUS ...: NN
  CREATED ......: 18.12.98 pt
  changePVM ....: 15.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR SMGroup        LIKE  SMGroup.SmGroup    NO-UNDO.
DEF VAR rtab           AS re EXTENT 11          NO-UNDO.
DEF VAR ufkey          AS lo init TRUE          NO-UNDO.
DEF VAR i              AS i                     NO-UNDO.
DEF VAR muisti         AS re                    NO-UNDO.
DEF VAR must-print     AS lo                    NO-UNDO.
DEF VAR must-add       AS lo                    NO-UNDO.
def var nro            as c  format "x(5)"      NO-UNDO.
def var tyhja          as c  format "x(80)"     NO-UNDO.
DEF VAR tlli-ots       AS c.

form
    SMGroup.SmGroup
    SMGroup.SGName  format "x(30)"
    SMGroup.Memo[1]  format "x(20)"
WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
    title color value(ctc) " Salesman Groups (" + gcBrand + ") " 
    OVERLAY FRAME tlse.

form
    SMGroup.SmGroup SKIP
    SMGroup.SGName SKIP
    SMGroup.Memo[1]    SKIP

WITH OVERLAY ROW 8 centered
    TITLE COLOR value(ctc) tlli-ots
    COLOR value(cfc) side-labels 1 col
    FRAME tlli.

form /* Invoicing Group :n hakua varten */
    SMGroup
    help "Enter Code of a Salesman Group"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

cfc = "tlse". RUN ufcolor. ASSIGN ccc = cfc.
Runko:
repeat:

   FIND FIRST SMGroup WHERE SMGroup.Brand = gcBrand NO-LOCK no-error.
   IF NOT AVAILABLE SMGroup THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      muisti = recid(SMGroup).
      must-print = TRUE.
   END.

LOOP:
   Repeat WITH FRAME tlse:
   IF must-add THEN DO:  /* Invoicing Group  lisays  */
      ASSIGN
      cfc = "tlli"
      tlli-ots = " ADD ".
      RUN ufcolor.
add-new:
      repeat WITH FRAME tlli:
         PAUSE 0 no-message.
         CLEAR FRAME tlli no-pause.
         repeat WITH FRAME tlli:
            PROMPT-FOR SMGroup.SmGroup.
            if input SMGroup.SmGroup = "" THEN DO:
               HIDE FRAME tlli no-pause.
               LEAVE add-new.
            END.
            IF CAN-FIND (SMGroup where 
                         SMGroup.Brand   = gcBrand AND
                         SMGroup.SmGroup = INPUT SMGroup.SmGroup) THEN DO:
               BELL.
               message "Salesman Group  " + string(INPUT SMGroup.SmGroup)
               + " already exists !".
               NEXT.
            END.
            LEAVE.
         END.
         CREATE SMGroup.
         ASSIGN
           muisti = recid(SMGroup)
           SMGroup.Brand   = gcBrand
           SMGroup.SmGroup = INPUT SMGroup.SmGroup.
         UPDATE SMGroup.SGName SMGroup.Memo[1] .
         CLEAR FRAME tlli no-pause.
      END.  /* add-new */
      ASSIGN
      must-add = FALSE
      must-print = TRUE.

      /* onko yhtaan tietuetta ? */
      FIND FIRST SMGroup WHERE SMGroup.Brand = gcBrand NO-LOCK no-error.
      IF NOT AVAILABLE SMGroup THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME tlse ALL no-pause.
         FIND SMGroup where recid(SMGroup) = muisti no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE SMGroup:
            DISPLAY
            SMGroup.SmGroup
            SMGroup.SGName
            SMGroup.Memo[1] WITH FRAME tlse.
            rtab[FRAME-LINE] = recid(SMGroup).
            DOWN WITH FRAME tlse.
            FIND NEXT SMGroup WHERE SMGroup.Brand = gcBrand NO-LOCK no-error.
         END.
         must-print = FALSE.
         up frame-line(tlse) - 1 WITH FRAME tlse.
      END. /* must-print */

      IF ufkey THEN DO:
         ASSIGN
         ufk = 0 ufk[1] = 35 ufk[5] = 11
         /* ufk[6] = 5  no NEW records here ... */
         ufk[8] = 8  ufk[9] = 1 siirto = ? ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.
  END. /* print-line */

BROWSE:
      repeat WITH FRAME tlse ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW SMGroup.SmGroup ;(uchoose.i;) no-error WITH FRAME tlse.
         COLOR DISPLAY value(ccc) SMGroup.SmGroup WITH FRAME tlse.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME tlse:
            IF FRAME-LINE = 1 THEN DO:
               FIND SMGroup where recid(SMGroup) = rtab[FRAME-LINE] no-lock.
               FIND prev SMGroup WHERE SMGroup.Brand = gcBrand NO-LOCK no-error.
               IF NOT AVAILABLE SMGroup THEN DO:
                  BELL.
                  message "You are on the 1st row !".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* a previous one was found */
                  scroll DOWN.
                  DO i = 11 TO 2 BY -1:
                     rtab[i] = rtab[i - 1].
                  END.
                  DISPLAY SMGroup.SmGroup SMGroup.SGName SMGroup.Memo[1].
                  rtab[FRAME-LINE] = recid(SMGroup).
                  muisti = recid(SMGroup).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME tlse:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND SMGroup where recid(SMGroup) = rtab[FRAME-LINE] no-lock .
               FIND NEXT SMGroup WHERE SMGroup.Brand = gcBrand NO-LOCK no-error.
               IF NOT AVAILABLE SMGroup THEN DO:
                  BELL.
                  message "You are on the last row !".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* yet another record was found */
                  scroll up.
                  DO i = 1 TO 10:
                     rtab[i] = rtab[i + 1].
                  END.
                  DISPLAY SMGroup.SmGroup SMGroup.SGName SMGroup.Memo[1].
                  rtab[FRAME-LINE] = recid(SMGroup).
                  /* finally LAST line's KeyValue is saved */
                  muisti = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME tlse:
            FIND SMGroup where recid(SMGroup) = muisti no-lock no-error.
            FIND prev SMGroup WHERE SMGroup.Brand = gcBrand NO-LOCK no-error.
            IF AVAILABLE SMGroup THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev SMGroup WHERE SMGroup.Brand = gcBrand 
                  NO-LOCK no-error.
                  IF AVAILABLE SMGroup THEN muisti = recid(SMGroup).
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
        else if lookup(nap,"page-down,next-page") > 0 THEN DO WITH FRAME tlse:
           IF rtab[FRAME-DOWN] = ? THEN DO:
               BELL.
               message "This is the last page !".
               PAUSE 1 no-message.
           END.
           ELSE DO: /* the downmost line wasn't empty */
               muisti = rtab[FRAME-DOWN].
               must-print = TRUE.
               NEXT LOOP.
           END.
        END. /* NEXT page */

        /* Haku */
        if lookup(nap,"1,f1") > 0 THEN DO:  /* haku */
           cfc = "puyr". RUN ufcolor.
           SMGroup = "".
           ehto = 9. RUN ufkey. ufkey = TRUE.
           UPDATE SMGroup WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           if SMGroup <> "" THEN DO:
              FIND FIRST SMGroup where 
                         SMGroup.Brand    = gcBrand AND
                         SMGroup.SmGroup >= INPUT SMGroup
              no-lock no-error.
              IF NOT AVAILABLE SMGroup THEN DO:
                 message "Not found !".
                 PAUSE 1 no-message.
                 NEXT BROWSE.
              END.
              /* Some Salesman Group  was found */
              ASSIGN
                muisti = recid(SMGroup)
                must-print = TRUE.
              NEXT LOOP.
           END.
        END. /* Haku */

        /* Valinta */
        else if lookup(nap,"return,enter,5,f5") > 0 THEN DO:
           FIND SMGroup where recid(SMGroup) = rtab[FRAME-LINE] no-lock.
           siirto = string(SMGroup.SmGroup).
           LEAVE runko.
        END. /* Valinta */
        /* Ensimmainen tietue */
        else if lookup(nap,"home,h") > 0 THEN DO:
           FIND FIRST SMGroup WHERE SMGroup.Brand = gcBrand NO-LOCK.
           muisti = recid(SMGroup).
           must-print = TRUE.
           NEXT LOOP.
        END. /* Ensimmainen tietue */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST SMGroup WHERE SMGroup.Brand = gcBrand NO-LOCK.
           muisti = recid(SMGroup).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE runko. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* Runko */
HIDE FRAME tlse no-pause.

