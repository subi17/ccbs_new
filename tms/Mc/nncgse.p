/*------------------------------------------------------
  MODULE .......: NNCGSE.P
  CALLING MOD. .: APPLHELP.P
  FUNCTION .....: Search of Customer Groups
  SOVELLUTUS ...: NN
  CREATED ......: 13.12.98 pt
  CHANGED ......: 09.01.99 pt f4: members
                  25.05.99 jp uright1 & uright2 added  
                  16.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR CustGroup        LIKE  CustGroup.CustGroup     NO-UNDO.
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
    CustGroup.CustGroup
    CustGroup.CGName  format "x(30)"
    CustGroup.Memo[1]  format "x(20)"
WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
    title color value(ctc) " Customer Groups (" + gcBrand + ") " 
    OVERLAY FRAME tlse.

form
    CustGroup.CustGroup SKIP
    CustGroup.CGName SKIP
    CustGroup.Memo[1]    SKIP

WITH OVERLAY ROW 8 centered
    TITLE COLOR value(ctc) tlli-ots
    COLOR value(cfc) side-labels 1 col
    FRAME tlli.

form /* Invoicing Group :n hakua varten */
    CustGroup
    help "Enter Code of a Customer Group"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

cfc = "tlse". RUN ufcolor. ASSIGN ccc = cfc.
Runko:
repeat:

   FIND FIRST CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
   IF NOT AVAILABLE CustGroup THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      muisti = recid(CustGroup).
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
            PROMPT-FOR CustGroup.CustGroup.
            if input CustGroup.CustGroup = "" THEN DO:
               HIDE FRAME tlli no-pause.
               LEAVE add-new.
            END.
            IF CAN-FIND (CustGroup where 
                         CustGroup.Brand     = gcBrand AND
                         CustGroup.CustGroup = INPUT CustGroup.CustGroup) 
            THEN DO:
               BELL.
               message "Customer Group  " + string(INPUT CustGroup.CustGroup)
               + " already exists !".
               NEXT.
            END.
            LEAVE.
         END.
         CREATE CustGroup.
         ASSIGN
           muisti = recid(CustGroup)
           CustGroup.Brand     = gcBrand
           CustGroup.CustGroup = INPUT CustGroup.CustGroup.
         UPDATE CustGroup.CGName CustGroup.Memo[1] .
         CLEAR FRAME tlli no-pause.
      END.  /* add-new */
      ASSIGN
      must-add = FALSE
      must-print = TRUE.

      /* onko yhtaan tietuetta ? */
      FIND FIRST CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
      IF NOT AVAILABLE CustGroup THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME tlse ALL no-pause.
         FIND CustGroup where recid(CustGroup) = muisti no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE CustGroup:
            DISPLAY
            CustGroup.CustGroup
            CustGroup.CGName
            CustGroup.Memo[1] WITH FRAME tlse.
            rtab[FRAME-LINE] = recid(CustGroup).
            DOWN WITH FRAME tlse.
            FIND NEXT CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
         END.
         must-print = FALSE.
         up frame-line(tlse) - 1 WITH FRAME tlse.
      END. /* must-print */

      IF ufkey THEN DO:
         ASSIGN
         ufk = 0 ufk[1] = 35 ufk[4] = 510 ufk[5] = 11
         ufk[8] = 8  ufk[9] = 1 siirto = ? ehto = 3 ufkey = FALSE.
         {Syst/uright1.i '"4"'}
         RUN ufkey.p.
      END.
  END. /* print-line */

BROWSE:
      repeat WITH FRAME tlse ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW CustGroup.CustGroup ;(uchoose.i;) no-error WITH FRAME tlse.
         COLOR DISPLAY value(ccc) CustGroup.CustGroup WITH FRAME tlse.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME tlse:
            IF FRAME-LINE = 1 THEN DO:
               FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] no-lock.
               FIND prev CustGroup WHERE CustGroup.Brand = gcBrand 
                  NO-LOCK no-error.
               IF NOT AVAILABLE CustGroup THEN DO:
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
                  DISPLAY CustGroup.CustGroup CustGroup.CGName 
                          CustGroup.Memo[1].
                  rtab[FRAME-LINE] = recid(CustGroup).
                  muisti = recid(CustGroup).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME tlse:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] 
                  no-lock .
               FIND NEXT CustGroup WHERE CustGroup.Brand = gcBrand 
                  NO-LOCK no-error.
               IF NOT AVAILABLE CustGroup THEN DO:
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
                  DISPLAY CustGroup.CustGroup CustGroup.CGName 
                          CustGroup.Memo[1].
                  rtab[FRAME-LINE] = recid(CustGroup).
                  /* finally LAST line's KeyValue is saved */
                  muisti = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME tlse:
            FIND CustGroup where recid(CustGroup) = muisti no-lock no-error.
            FIND prev CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK no-error.
            IF AVAILABLE CustGroup THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev CustGroup WHERE CustGroup.Brand = gcBrand 
                     NO-LOCK no-error.
                  IF AVAILABLE CustGroup THEN muisti = recid(CustGroup).
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
           CustGroup = "".
           ehto = 9. RUN ufkey. ufkey = TRUE.
           UPDATE CustGroup WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           if CustGroup <> "" THEN DO:
              FIND FIRST CustGroup where 
                         CustGroup.Brand      = gcBrand AND
                         CustGroup.CustGroup >= INPUT CustGroup
              no-lock no-error.
              IF NOT AVAILABLE CustGroup THEN DO:
                 message "Not found !".
                 PAUSE 1 no-message.
                 NEXT BROWSE.
              END.
              /*  Customer Group  was found */
              ASSIGN
                muisti = recid(CustGroup)
                must-print = TRUE.
              NEXT LOOP.
           END.
        END. /* Haku */

        /* look members */
        else if lookup(nap,"4,f4") > 0 THEN DO:
           {Syst/uright2.i}
           FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] no-lock.

           ufkey = TRUE.
           RUN nncgme1(CustGroup.CustGroup).
           NEXT LOOP.
        END.


        /* Valinta */
        else if lookup(nap,"return,enter,5,f5") > 0 THEN DO:
           FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] no-lock.
           siirto = string(CustGroup.CustGroup).
           LEAVE runko.
        END. /* Valinta */

        /* Ensimmainen tietue */
        else if lookup(nap,"home,h") > 0 THEN DO:
           FIND FIRST CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK.
           muisti = recid(CustGroup).
           must-print = TRUE.
           NEXT LOOP.
        END. /* Ensimmainen tietue */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST CustGroup WHERE CustGroup.Brand = gcBrand NO-LOCK.
           muisti = recid(CustGroup).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE runko. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* Runko */
HIDE FRAME tlse no-pause.

