/*------------------------------------------------------
  MODULE .......: NNSOSE.P
  KUTSUVAMODULI : APPLHELP.P
  FUNCTION .....: Salesoffice browse
  SOVELLUTUS ...: NN
  AUTHOR .......: PT
  CREATED ......: 28-11-96
  changePVM ....: 25-05-99 jp uright1 & uright2 added
                  16.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.


DEF VAR Salesoffice     LIKE Salesoffice.SalesOffice    NO-UNDO.
DEF VAR rtab        AS RECID EXTENT 11          NO-UNDO.
DEF VAR ufkey       AS LOG   init TRUE          NO-UNDO.
DEF VAR i           AS INT                      NO-UNDO.
DEF VAR ylin        AS RECID                    NO-UNDO.
DEF VAR must-print  AS logic                    NO-UNDO.
DEF VAR must-add    AS logic                    NO-UNDO.
def var nro         as char format "x(5)"       NO-UNDO.
def var tyhja       as char format "x(80)"      NO-UNDO.
DEF VAR tlli-ots    AS CHAR.

form
    Salesoffice.SalesOffice  
    Salesoffice.SOName
WITH
    scroll 1 11 DOWN  ROW 4 centered COLOR value(cfc)
    title color value(ctc) " SALES OFFICE (" + gcBrand + ") " 
    OVERLAY FRAME sel.

form
    Salesoffice    label "Sales off."
    SOName         label "Name"

    WITH OVERLAY ROW 8 centered TITLE COLOR value(ctc) tlli-ots
    COLOR value(cfc) side-labels 1 col  FRAME tlli.

form /* hakua varten */
    Salesoffice
    help "Give sales office's code"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
Runko:
repeat:

   FIND FIRST Salesoffice WHERE SalesOffice.Brand = gcBrand NO-LOCK no-error.
   IF NOT AVAILABLE Salesoffice THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      Ylin = recid(Salesoffice).
      must-print = TRUE.
   END.

LOOP:
   Repeat WITH FRAME sel:

   IF must-add THEN DO:  /* lisays  */
      ASSIGN
      cfc = "tlli"
      tlli-ots = " ADD ".
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME tlli:
         PAUSE 0 no-message.
         CLEAR FRAME tlli no-pause.
         repeat WITH FRAME tlli:
            Salesoffice = "". UPDATE Salesoffice.
            if Salesoffice = "" THEN DO:
               HIDE FRAME tlli no-pause.
               LEAVE add-new.
            END.
            IF can-find(Salesoffice where 
                        SalesOffice.Brand       = gcBrand AND
                        Salesoffice.SalesOffice = Salesoffice)
            THEN DO:
               bell. message "Salesman" Salesoffice " already exists !". NEXT.
            END.
            LEAVE.
         END.
         CREATE Salesoffice.
         ASSIGN
           ylin = recid(Salesoffice)
           SalesOffice.Brand       = gcBrand
           Salesoffice.SalesOffice = Salesoffice.
         UPDATE Salesoffice.SOName.
         CLEAR FRAME tlli no-pause.
      END.  /* add-new */
      ASSIGN
      must-add = FALSE
      must-print = TRUE.

      /* onko yhtaan tietuetta ? */
      FIND FIRST Salesoffice  WHERE 
         SalesOffice.Brand = gcBrand NO-LOCK no-error.
      IF NOT AVAILABLE Salesoffice THEN LEAVE LOOP.
      NEXT LOOP.
   END.


print-line:
   DO :
      IF must-print THEN DO:
         CLEAR FRAME sel ALL no-pause.
         FIND Salesoffice where recid(Salesoffice) = ylin no-lock no-error.

         /* Tulostetaan ruudullinen */
         rtab = ?.
         DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE Salesoffice:
            DISPLAY Salesoffice.SalesOffice Salesoffice.SOName WITH FRAME sel.
            rtab[FRAME-LINE] = recid(Salesoffice).
            DOWN WITH FRAME sel.
            FIND NEXT Salesoffice WHERE 
               SalesOffice.Brand = gcBrand NO-LOCK no-error.
         END.
         must-print = FALSE.
         up frame-line(sel) - 1 WITH FRAME sel.
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
      repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW Salesoffice.SalesOffice ;(uchoose.i;) no-error 
         WITH FRAME sel.
         COLOR DISPLAY value(ccc) Salesoffice.SalesOffice WITH FRAME sel.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(nap,"cursor-up") > 0 THEN DO
         WITH FRAME sel:
            IF FRAME-LINE = 1 THEN DO:
               FIND Salesman where recid(Salesman) = rtab[FRAME-LINE] no-lock.
               FIND prev Salesoffice WHERE 
                  SalesOffice.Brand = gcBrand NO-LOCK no-error.
               IF NOT AVAILABLE Salesoffice THEN DO:
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
                  DISPLAY Salesoffice.SalesOffice Salesoffice.SOName.
                  rtab[FRAME-LINE] = recid(Salesoffice).
                  ylin = recid(Salesoffice).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(nap,"cursor-down") > 0 THEN DO WITH FRAME sel:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND Salesman where recid(Salesman) = rtab[FRAME-LINE] no-lock .
               FIND NEXT Salesoffice WHERE 
                  SalesOffice.Brand = gcBrand NO-LOCK no-error.
               IF NOT AVAILABLE Salesoffice THEN DO:
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
                  DISPLAY Salesoffice.SalesOffice Salesoffice.SOName.
                  rtab[FRAME-LINE] = recid(Salesoffice).
                  /* finally LAST line's KeyValue is saved */
                  ylin = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 THEN DO WITH FRAME sel:
            FIND Salesoffice where recid(Salesoffice) = ylin no-lock no-error.
            FIND prev Salesoffice where 
               SalesOffice.Brand = gcBrand NO-LOCK no-error.
            IF AVAILABLE Salesoffice THEN DO:
               /* mennaan tiedostoa taaksepAin 1 sivun verran */
               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev Salesoffice where 
                     SalesOffice.Brand = gcBrand NO-LOCK no-error.
                  IF AVAILABLE Salesoffice THEN ylin = recid(Salesoffice).
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
        else if lookup(nap,"page-down,next-page") > 0 THEN DO WITH FRAME sel:
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
           Salesoffice = "". ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
           UPDATE Salesoffice WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           if Salesoffice <> "" THEN DO:
              FIND FIRST Salesoffice where 
                         SalesOffice.Brand        = gcBrand AND
                         Salesoffice.SalesOffice >= Salesoffice
                 no-lock no-error.
              IF NOT AVAILABLE Salesoffice THEN DO:
                 bell. message "CAN'T FIND". PAUSE 1 no-message.
                 NEXT BROWSE.
              END.
              /*  myyjA  was found */
              ASSIGN
              ylin = recid(Salesoffice)
              must-print = TRUE.
           END.
           NEXT LOOP.
        END. /* Haku */

        /* Valinta */
        else if lookup(nap,"return,enter,5,f5") > 0 THEN DO:
           FIND Salesoffice where recid(Salesoffice) = rtab[FRAME-LINE] no-lock.
           siirto = string(Salesoffice.SalesOffice).
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
           FIND FIRST Salesoffice WHERE 
              SalesOffice.Brand = gcBrand NO-LOCK no-error.
           ylin = recid(Salesoffice).
           must-print = TRUE.
           NEXT LOOP.
        END. /* Ensimmainen tietue */

        /* LAST record */
        else if lookup(nap,"end,e") > 0 THEN DO :
           FIND LAST Salesoffice WHERE 
              SalesOffice.Brand = gcBrand NO-LOCK no-error.
           ylin = recid(Salesoffice).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if nap = "8" or nap = "f8" THEN LEAVE runko. /* Paluu */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* Runko */
HIDE FRAME sel no-pause.

