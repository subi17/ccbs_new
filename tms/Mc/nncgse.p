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
WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(Syst.CUICommon:cfc)
    title color value(Syst.CUICommon:ctc) " Customer Groups (" + Syst.CUICommon:gcBrand + ") " 
    OVERLAY FRAME tlse.

form
    CustGroup.CustGroup SKIP
    CustGroup.CGName SKIP
    CustGroup.Memo[1]    SKIP

WITH OVERLAY ROW 8 centered
    TITLE COLOR value(Syst.CUICommon:ctc) tlli-ots
    COLOR value(Syst.CUICommon:cfc) side-labels 1 col
    FRAME tlli.

form /* Invoicing Group :n hakua varten */
    CustGroup
    help "Enter Code of a Customer Group"
    with row 4 col 2 title color value(Syst.CUICommon:ctc) " FIND CODE "
    COLOR value(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME hayr.

Syst.CUICommon:cfc = "tlse". RUN Syst/ufcolor.p. ASSIGN Syst.CUICommon:ccc = Syst.CUICommon:cfc.
Runko:
repeat:

   FIND FIRST CustGroup WHERE CustGroup.Brand = Syst.CUICommon:gcBrand NO-LOCK no-error.
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
      Syst.CUICommon:cfc = "tlli"
      tlli-ots = " ADD ".
      RUN Syst/ufcolor.p.

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
                         CustGroup.Brand     = Syst.CUICommon:gcBrand AND
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
           CustGroup.Brand     = Syst.CUICommon:gcBrand
           CustGroup.CustGroup = INPUT CustGroup.CustGroup.
         UPDATE CustGroup.CGName CustGroup.Memo[1] .
         CLEAR FRAME tlli no-pause.
      END.  /* add-new */
      ASSIGN
      must-add = FALSE
      must-print = TRUE.

      /* onko yhtaan tietuetta ? */
      FIND FIRST CustGroup WHERE CustGroup.Brand = Syst.CUICommon:gcBrand NO-LOCK no-error.
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
            FIND NEXT CustGroup WHERE CustGroup.Brand = Syst.CUICommon:gcBrand NO-LOCK no-error.
         END.
         must-print = FALSE.
         up frame-line(tlse) - 1 WITH FRAME tlse.
      END. /* must-print */

      IF ufkey THEN DO:
         ASSIGN
         Syst.CUICommon:ufk = 0 Syst.CUICommon:ufk[1] = 35 Syst.CUICommon:ufk[4] = 510 Syst.CUICommon:ufk[5] = 11
         Syst.CUICommon:ufk[8] = 8  Syst.CUICommon:ufk[9] = 1 siirto = ? Syst.CUICommon:ehto = 3 ufkey = FALSE.
         {Syst/uright1.i '"4"'}
         RUN Syst/ufkey.p.
      END.
  END. /* print-line */

BROWSE:
      repeat WITH FRAME tlse ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW CustGroup.CustGroup {Syst/uchoose.i} no-error WITH FRAME tlse.
         COLOR DISPLAY value(Syst.CUICommon:ccc) CustGroup.CustGroup WITH FRAME tlse.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         Syst.CUICommon:nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(Syst.CUICommon:nap,"cursor-up") > 0 THEN DO
         WITH FRAME tlse:
            IF FRAME-LINE = 1 THEN DO:
               FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] no-lock.
               FIND prev CustGroup WHERE CustGroup.Brand = Syst.CUICommon:gcBrand 
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
         if lookup(Syst.CUICommon:nap,"cursor-down") > 0 THEN DO WITH FRAME tlse:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] 
                  no-lock .
               FIND NEXT CustGroup WHERE CustGroup.Brand = Syst.CUICommon:gcBrand 
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
         else if lookup(Syst.CUICommon:nap,"page-up,prev-page") > 0 THEN DO WITH FRAME tlse:
            FIND CustGroup where recid(CustGroup) = muisti no-lock no-error.
            FIND prev CustGroup WHERE CustGroup.Brand = Syst.CUICommon:gcBrand NO-LOCK no-error.
            IF AVAILABLE CustGroup THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev CustGroup WHERE CustGroup.Brand = Syst.CUICommon:gcBrand 
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
        else if lookup(Syst.CUICommon:nap,"page-down,next-page") > 0 THEN DO WITH FRAME tlse:
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
        if lookup(Syst.CUICommon:nap,"1,f1") > 0 THEN DO:  /* haku */
           Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
           CustGroup = "".
           Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           UPDATE CustGroup WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           if CustGroup <> "" THEN DO:
              FIND FIRST CustGroup where 
                         CustGroup.Brand      = Syst.CUICommon:gcBrand AND
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
        else if lookup(Syst.CUICommon:nap,"4,f4") > 0 THEN DO:
           {Syst/uright2.i}
           FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] no-lock.

           ufkey = TRUE.
           RUN Mc/nncgme1.p(CustGroup.CustGroup).
           NEXT LOOP.
        END.


        /* Valinta */
        else if lookup(Syst.CUICommon:nap,"return,enter,5,f5") > 0 THEN DO:
           FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] no-lock.
           siirto = string(CustGroup.CustGroup).
           LEAVE runko.
        END. /* Valinta */

        /* Ensimmainen tietue */
        else if lookup(Syst.CUICommon:nap,"home,h") > 0 THEN DO:
           FIND FIRST CustGroup WHERE CustGroup.Brand = Syst.CUICommon:gcBrand NO-LOCK.
           muisti = recid(CustGroup).
           must-print = TRUE.
           NEXT LOOP.
        END. /* Ensimmainen tietue */

        /* LAST record */
        else if lookup(Syst.CUICommon:nap,"end,e") > 0 THEN DO :
           FIND LAST CustGroup WHERE CustGroup.Brand = Syst.CUICommon:gcBrand NO-LOCK.
           muisti = recid(CustGroup).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if Syst.CUICommon:nap = "8" or Syst.CUICommon:nap = "f8" THEN LEAVE runko. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* Runko */
HIDE FRAME tlse no-pause.

