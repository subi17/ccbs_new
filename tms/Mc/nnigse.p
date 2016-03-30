/*------------------------------------------------------
  MODULE .......: NNIGSE.P
  PARENT .......: APPLHELP.P
  FUNCTION .....: Search of Invoicing Groups
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 05-10-97
  MODIFIED .....: 18.01.09 kl adding commented
                  21.01.99 pt in English
                  13.11.02 jr Removed memos
                  13.11.02 jr Removed create and update
                  16.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF /*new*/ shared VAR siirto AS CHAR.

DEF VAR haku        LIKE InvGroup.InvGroup   NO-UNDO.
DEF VAR rtab        AS RECID EXTENT 11      NO-UNDO.
DEF VAR ufkey       AS LOG init TRUE        NO-UNDO.
DEF VAR i           AS INT                  NO-UNDO.
DEF VAR ylin        AS RECID                NO-UNDO.
DEF VAR must-print  AS logic                NO-UNDO.
def var nro         as char format "x(5)"   NO-UNDO.
def var tyhja       as char format "x(80)"  NO-UNDO.
DEF VAR tlli-ots    AS CHAR.

form
    InvGroup.InvGroup
    InvGroup.IGName  format "x(30)"
WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
    title color value(ctc) " Invoicing Groups (" + gcBrand + ") " 
    OVERLAY FRAME tlse.

form
    InvGroup.InvGroup SKIP
    InvGroup.IGName SKIP

WITH OVERLAY ROW 8 centered
    TITLE COLOR value(ctc) tlli-ots
    COLOR value(cfc) side-labels 1 col
    FRAME tlli.

form /* Invoicing Group :n hakua varten */
    haku
    help "Enter Code of an Invoicing Group"
    with row 4 col 2 title color value(ctc) "FIND Inv.Group"
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

cfc = "tlse". RUN Syst/ufcolor. ASSIGN ccc = cfc.
Runko:
repeat:

   FIND FIRST InvGroup WHERE InvGroup.Brand = gcBrand NO-LOCK no-error.
   IF NOT AVAILABLE InvGroup THEN DO:
      must-print = FALSE.
   END.
   ELSE DO:
      Ylin = recid(InvGroup).
      must-print = TRUE.
   END.

LOOP:
Repeat WITH FRAME tlse:
   print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME tlse ALL no-pause.
         FIND InvGroup where recid(InvGroup) = ylin no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE InvGroup:
            DISPLAY
            InvGroup.InvGroup
            InvGroup.IGName
            WITH FRAME tlse.
            rtab[FRAME-LINE] = recid(InvGroup).
            DOWN WITH FRAME tlse.
            FIND NEXT InvGroup WHERE InvGroup.Brand = gcBrand NO-LOCK no-error.
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
         CHOOSE ROW InvGroup.InvGroup {Syst/uchoose.i} no-error WITH FRAME tlse.
         COLOR DISPLAY value(ccc) InvGroup.InvGroup WITH FRAME tlse.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME tlse:
            IF FRAME-LINE = 1 THEN DO:
               FIND InvGroup where recid(InvGroup) = rtab[FRAME-LINE] no-lock.
               FIND prev InvGroup WHERE InvGroup.Brand = gcBrand 
               NO-LOCK no-error.
               IF NOT AVAILABLE InvGroup THEN DO:
                  BELL.
                  message "You are on 1st row !".              
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* a previous one was found */
                  scroll DOWN.
                  DO i = 11 TO 2 BY -1:
                     rtab[i] = rtab[i - 1].
                  END.
                  DISPLAY InvGroup.InvGroup InvGroup.IGName.
                  rtab[FRAME-LINE] = recid(InvGroup).
                  ylin = recid(InvGroup).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME tlse:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND InvGroup where recid(InvGroup) = rtab[FRAME-LINE] no-lock .
               FIND NEXT InvGroup WHERE InvGroup.Brand = gcBrand 
               NO-LOCK no-error.
               IF NOT AVAILABLE InvGroup THEN DO:
                  BELL.
                  message "You are on last row !".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* yet another record was found */
                  scroll up.
                  DO i = 1 TO 10:
                     rtab[i] = rtab[i + 1].
                  END.
                  DISPLAY InvGroup.InvGroup InvGroup.IGName.
                  rtab[FRAME-LINE] = recid(InvGroup).
                  /* finally LAST line's KeyValue is saved */
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME tlse:
            FIND InvGroup where recid(InvGroup) = ylin no-lock no-error.
            FIND prev InvGroup WHERE InvGroup.Brand = gcBrand NO-LOCK no-error.
            IF AVAILABLE InvGroup THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev InvGroup WHERE InvGroup.Brand = gcBrand 
                  NO-LOCK no-error.
                  IF AVAILABLE InvGroup THEN ylin = recid(InvGroup).
                  ELSE i = FRAME-DOWN.
               END.
               must-print = TRUE.
               NEXT LOOP.
            END.
            ELSE DO:
               /* this is the FIRST data page */
               BELL.
               message "This is the 1st page !".          
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
               ylin = rtab[FRAME-DOWN].
               must-print = TRUE.
               NEXT LOOP.
           END.
        END. /* NEXT page */

        /* Haku */
        if lookup(nap,"1,f1") > 0 THEN DO:  /* haku */
           cfc = "puyr". RUN Syst/ufcolor.
           haku = "".
           ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
           UPDATE haku WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           if haku <> "" THEN DO:

              FIND FIRST InvGroup where 
                 InvGroup.Brand    = gcBrand AND
                 InvGroup.InvGroup = INPUT haku
              no-lock no-error.

              IF NOT AVAILABLE InvGroup THEN 
                 FIND FIRST InvGroup where 
                    InvGroup.Brand    =  gcBrand AND
                    InvGroup.InvGroup ge INPUT haku
                 no-lock no-error.
              IF NOT AVAILABLE InvGroup THEN 
                 FIND FIRST InvGroup where 
                    InvGroup.Brand    =  gcBrand AND
                    InvGroup.InvGroup le INPUT haku
                 no-lock no-error.

              IF NOT AVAILABLE InvGroup THEN DO:
                 BELL.
                 message "None found !".    
                 PAUSE 1 no-message.
                 NEXT BROWSE.
              END.

              /*  Invoicing Group  was found */
              ASSIGN
                ylin = recid(InvGroup)
                must-print = TRUE.
           END.
           NEXT LOOP.
        END. /* Haku */

        /* Valinta */
        else if lookup(nap,"return,enter,5,f5") > 0 THEN DO:
           FIND InvGroup where recid(InvGroup) = rtab[FRAME-LINE] no-lock.
           siirto = string(InvGroup.InvGroup).
           LEAVE runko.
        END. /* Valinta */
        /* Ensimmainen tietue */
        else if lookup(nap,"home,h") > 0 THEN DO:
           FIND FIRST InvGroup WHERE InvGroup.Brand = gcBrand NO-LOCK.
           ylin = recid(InvGroup).
           must-print = TRUE.
           NEXT LOOP.
        END. /* Ensimmainen tietue */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST InvGroup WHERE InvGroup.Brand = gcBrand NO-LOCK.
           ylin = recid(InvGroup).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE runko. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* Runko */
HIDE FRAME tlse no-pause.

