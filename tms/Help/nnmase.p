/*------------------------------------------------------
  MODULE .......: NNMASE.P
  KUTSUVAMODULI : APPLHELP.P
  FUNCTION .....: Maiden BROWSE
  SOVELLUTUS ...: NN
  AUTHOR .......: PT
  CREATED ......: 24-01-96
  changePVM ....: 14.07.97 tt Kaytetaan AINA orderestyksena NIMEA
                  25.05.99 jp uright1 & uright2 added  
                  15.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
DEF shared VAR siirto AS CHAR.

def var liCCN       as int format "zz9"     NO-UNDO.
DEF VAR haku        LIKE CCN.CCNName        NO-UNDO.
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
   CCN.CCN  /* column-label "Land" */
   CCNName          column-label "Name"
WITH
   scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
   title color value(ctc) " LANDCODE " OVERLAY
FRAME tlse.

form
   liCCN       label "Land" SKIP
   CCNName      label "Name" SKIP(1)
WITH
   OVERLAY ROW 8 centered TITLE COLOR value(ctc) tlli-ots
   COLOR value(cfc) side-labels 1 col
FRAME tlli.

form /* Maa :n hakua varten */
   haku
      help "Give a country's Name or beginning of it"
with row 4 col 2 title color value(ctc) " FIND Land  "
    COLOR value(cfc) NO-LABELS OVERLAY
FRAME hayr.

cfc = "tlse". RUN Syst/ufcolor. ASSIGN ccc = cfc.
Runko:
repeat:

   FIND FIRST CCN
   USE-INDEX CCNName WHERE CCN.Brand = gcBrand NO-LOCK NO-ERROR.
   IF NOT AVAILABLE CCN THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      Ylin = recid(CCN).
      must-print = TRUE.
   END.

LOOP:
   Repeat WITH FRAME tlse:

   IF must-add THEN DO:  /* Maa  lisays  */
      ASSIGN
      cfc = "tlli"
      tlli-ots = " ADD ".
      RUN Syst/ufcolor.
       add-new:
      repeat WITH FRAME tlli:
         PAUSE 0 no-message.
         CLEAR FRAME tlli no-pause.
         repeat WITH FRAME tlli:
            liCCN = 0. UPDATE liCCN.
            IF INPUT liCCN = 0 THEN DO:
               HIDE FRAME tlli no-pause.
               LEAVE add-new.
            END.
            IF can-find(CCN WHERE CCN.Brand = gcBrand AND CCN.CCN = liCCN )
            THEN DO:
               BELL.
               message "Land  " + string(input liCCN) + " already exists !".
               NEXT.
            END.
            LEAVE.
         END.
         CREATE CCN.
         ASSIGN
           ylin = recid(CCN)
           CCN.Brand = gcBrand
           CCN.CCN = liCCN.
         UPDATE CCNName.
         CLEAR FRAME tlli no-pause.
      END.  /* add-new */
      ASSIGN
      must-add = FALSE
      must-print = TRUE.

      /* onko yhtaan tietuetta ? */
      FIND FIRST CCN
      USE-INDEX CCNName WHERE CCN.Brand = gcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CCN THEN LEAVE LOOP.
      NEXT LOOP.
   END.


print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME tlse ALL no-pause.
         FIND CCN WHERE recid(CCN) = ylin NO-LOCK NO-ERROR.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE CCN:
            DISPLAY CCN.CCN CCNName WITH FRAME tlse.
            rtab[FRAME-LINE] = recid(CCN).
            DOWN WITH FRAME tlse.
            FIND NEXT CCN USE-INDEX CCNName WHERE CCN.Brand = gcBrand 
            NO-LOCK NO-ERROR.
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
         RUN Syst/ufkey.p.
      END.
  END. /* print-line */

BROWSE:
      repeat WITH FRAME tlse ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW CCN.CCNName {Syst/uchoose.i} no-error WITH FRAME tlse.
         COLOR DISPLAY value(ccc) CCN.CCNName WITH FRAME tlse.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME tlse:
            IF FRAME-LINE = 1 THEN DO:
               FIND CCN WHERE recid(CCN) = rtab[FRAME-LINE] NO-LOCK.
               FIND prev CCN 
               USE-INDEX CCNName WHERE CCN.Brand = gcBrand NO-LOCK NO-ERROR.
               IF NOT AVAILABLE CCN THEN DO:
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
                  DISPLAY CCN.CCN CCNName.
                  rtab[FRAME-LINE] = recid(CCN).
                  ylin = recid(CCN).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME tlse:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND CCN WHERE recid(CCN) = rtab[FRAME-LINE] NO-LOCK .
               FIND NEXT CCN 
               USE-INDEX CCNName WHERE CCN.Brand = gcBrand NO-LOCK NO-ERROR.
               IF NOT AVAILABLE CCN THEN DO:
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
                  DISPLAY CCN.CCN CCNName.
                  rtab[FRAME-LINE] = recid(CCN).
                  /* finally LAST line's KeyValue is saved */
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME tlse:
            FIND CCN WHERE recid(CCN) = ylin NO-LOCK NO-ERROR.
            FIND prev CCN 
            USE-INDEX CCNName WHERE CCN.Brand = gcBrand NO-LOCK NO-ERROR.
            IF AVAILABLE CCN THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev CCN 
                  USE-INDEX CCNName
                  WHERE CCN.Brand = gcBrand NO-LOCK NO-ERROR.
                  IF AVAILABLE CCN THEN ylin = recid(CCN).
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
           cfc = "puyr". RUN Syst/ufcolor.
           haku = "". ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
           UPDATE haku WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           if haku <> "" THEN DO:
              FIND FIRST CCN WHERE 
                 CCN.Brand = gcBrand AND
                 CCN.CCNName >= haku
              USE-INDEX CCNName NO-LOCK NO-ERROR.
              IF NOT AVAILABLE CCN THEN DO:
                 BELL.
                 message "CAN'T FIND".
                 PAUSE 1 no-message.
                 NEXT BROWSE.
              END.
              /*  Maa  was found */
              ASSIGN
              ylin = recid(CCN)
              must-print = TRUE.
           END.
           NEXT LOOP.
        END. /* Haku */

        /* Valinta */
        else if lookup(nap,"return,enter,5,f5") > 0 THEN DO:
           FIND CCN WHERE recid(CCN) = rtab[FRAME-LINE] NO-LOCK.
           siirto = string(CCN.CCN).
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
           FIND FIRST CCN 
           USE-INDEX CCNName WHERE CCN.Brand = gcBrand NO-LOCK.
           ylin = recid(CCN).
           must-print = TRUE.
           NEXT LOOP.
        END. /* Ensimmainen tietue */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST CCN 
           USE-INDEX CCNName WHERE CCN.Brand = gcBrand NO-LOCK.
           ylin = recid(CCN).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE runko. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* Runko */
HIDE FRAME tlse no-pause.

