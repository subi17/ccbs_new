/*------------------------------------------------------
  MODULE .......: NNPLSE.P
  KUTSUVAMODULI : APPLHELP.P
  FUNCTION .....: Search of Price Lists
  SOVELLUTUS ...: NN
  AUTHOR .......: PT
  CREATED ......: 05-10-97
  MODIFIED .....: 21.01.99 pt  in English ...
                  25.05.99 jp  uright1 & uright2 added  
                  26.02.03 aam Prefix and DedicList added
                  16.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR haku        LIKE PriceList.PriceList      NO-UNDO.
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
    PriceList.PriceList
    PriceList.PLName  format "x(25)"
    PriceList.Prefix  
    PriceList.DedicList 
    PriceList.Memo  format "x(20)"
WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
    title color value(ctc) " Price lists (" + gcBrand + ") " 
    OVERLAY FRAME tlse.

form
    PriceList.PriceList SKIP
    PriceList.PLName SKIP
    PriceList.Prefix SKIP
    PriceList.DedicList SKIP
    PriceList.Memo    SKIP

WITH OVERLAY ROW 8 centered
    TITLE COLOR value(ctc) tlli-ots
    COLOR value(cfc) side-labels 1 col
    FRAME tlli.

form /* Invoicing Group :n hakua varten */
    haku
    help "Enter Code of a Price List"
    with row 4 col 2 title color value(ctc) " FIND Price List "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

cfc = "tlse". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
Runko:
repeat:

   FIND FIRST PriceList WHERE PriceList.Brand = gcBrand NO-LOCK no-error.
   IF NOT AVAILABLE PriceList THEN DO:
      MESSAGE "No pricelists available"
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE DO:
      Ylin = recid(PriceList).
      must-print = TRUE.
   END.

LOOP:
   Repeat WITH FRAME tlse:

   print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME tlse ALL no-pause.
         FIND PriceList where recid(PriceList) = ylin no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE PriceList:
            DISPLAY
            PriceList.PriceList
            PriceList.PLName
            PriceList.Prefix 
            Pricelist.DedicList
            PriceList.Memo WITH FRAME tlse.
            rtab[FRAME-LINE] = recid(PriceList).
            DOWN WITH FRAME tlse.
            FIND NEXT PriceList WHERE PriceList.Brand = gcBrand 
            NO-LOCK no-error.
         END.
         must-print = FALSE.
         up frame-line(tlse) - 1 WITH FRAME tlse.
      END. /* must-print */

      IF ufkey THEN DO:
         ASSIGN
         ufk = 0 ufk[1] = 35 ufk[5] = 11
         ufk[6] = 5 ufk[8] = 8  ufk[9] = 1
         siirto = ? ehto = 3 ufkey = FALSE.
         {Syst/uright1.i '"6"'}
         RUN Syst/ufkey.p.
      END.
  END. /* print-line */

BROWSE:
      repeat WITH FRAME tlse ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW PriceList.PriceList {Syst/uchoose.i} no-error WITH FRAME tlse.
         COLOR DISPLAY value(ccc) PriceList.PriceList WITH FRAME tlse.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME tlse:
            IF FRAME-LINE = 1 THEN DO:
               FIND PriceList where recid(PriceList) = rtab[FRAME-LINE] no-lock.
               FIND prev PriceList WHERE PriceList.Brand = gcBrand 
               NO-LOCK no-error.
               IF NOT AVAILABLE PriceList THEN DO:
                  BELL.
                  message "This is the 1st row !".            
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* a previous one was found */
                  scroll DOWN.
                  DO i = 11 TO 2 BY -1:
                     rtab[i] = rtab[i - 1].
                  END.
                  DISPLAY PriceList.PriceList PriceList.PLName 
                          PriceList.Prefix 
                          Pricelist.DedicList
                          PriceList.Memo.
                  rtab[FRAME-LINE] = recid(PriceList).
                  ylin = recid(PriceList).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME tlse:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND PriceList where recid(PriceList) = rtab[FRAME-LINE] 
               no-lock .
               FIND NEXT PriceList WHERE PriceList.Brand = gcBrand 
               NO-LOCK no-error.
               IF NOT AVAILABLE PriceList THEN DO:
                  BELL.
                  message "This is the last row !".          
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* yet another record was found */
                  scroll up.
                  DO i = 1 TO 10:
                     rtab[i] = rtab[i + 1].
                  END.
                  DISPLAY PriceList.PriceList 
                          PriceList.PLName 
                          PriceList.Prefix 
                          Pricelist.DedicList
                          PriceList.Memo.
                  rtab[FRAME-LINE] = recid(PriceList).
                  /* finally LAST line's KeyValue is saved */
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME tlse:
            FIND PriceList where recid(PriceList) = ylin no-lock no-error.
            FIND prev PriceList WHERE PriceList.Brand = gcBrand 
            NO-LOCK no-error.
            IF AVAILABLE PriceList THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev PriceList WHERE PriceList.Brand = gcBrand 
                  NO-LOCK no-error.
                  IF AVAILABLE PriceList THEN ylin = recid(PriceList).
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
           cfc = "puyr". RUN Syst/ufcolor.p.
           haku = "".
           ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           UPDATE haku WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           if haku <> "" THEN DO:
              FIND FIRST PriceList where 
                         PriceList.Brand     = gcBrand AND                                               PriceList.PriceList = INPUT haku
              no-lock no-error.
              IF NOT AVAILABLE PriceList THEN 
              FIND FIRST PriceList where 
                         PriceList.Brand     = gcBrand AND                                               PriceList.PriceList ge INPUT haku
              no-lock no-error.
              IF NOT AVAILABLE PriceList THEN 
              FIND FIRST PriceList where 
                         PriceList.Brand     = gcBrand AND                                               PriceList.PriceList le INPUT haku
              no-lock no-error.

              IF NOT AVAILABLE PriceList THEN DO:
                 BELL.
                 message "None found !".        
                 PAUSE 1 no-message.
                 NEXT BROWSE.
              END.

              /*  Invoicing Group  was found */
              ASSIGN
                ylin = recid(PriceList)
                must-print = TRUE.
           END.     
           NEXT LOOP.
        END. /* Haku */

        /* Valinta */
        else if lookup(nap,"return,enter,5,f5") > 0 THEN DO:
           FIND PriceList where recid(PriceList) = rtab[FRAME-LINE] no-lock.
           siirto = string(PriceList.PriceList).
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
           FIND FIRST PriceList WHERE PriceList.Brand = gcBrand NO-LOCK.
           ylin = recid(PriceList).
           must-print = TRUE.
           NEXT LOOP.
        END. /* Ensimmainen tietue */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST PriceList WHERE PriceList.Brand = gcBrand NO-LOCK.
           ylin = recid(PriceList).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE runko. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* Runko */
HIDE FRAME tlse no-pause.

