/* -----------------------------------------------
  MODULE .......: NNRNSE.P
  TASK .........: BROWSER FOR NORWEGIAN RSO'S
  APPLICATION ..: NN
  CREATED ......: 17.06.98 pt
  CHANGED ......: 25.05.99 jp Englannettu
  VERSIO .......: <16.12.93>
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR ekarivi         AS INT                  NO-UNDO.
DEF VAR jarj            AS INT                  NO-UNDO.
DEF VAR ed-jarj         AS INT                  NO-UNDO.
DEF VAR muisti          AS RECID                NO-UNDO.
def var rivi            as int format "99"      NO-UNDO.
DEF VAR privi           AS INT                  NO-UNDO.
DEF VAR TMSReportttava    AS LOG                  NO-UNDO.
DEF VAR ufkey           AS LOG                  NO-UNDO.
DEF VAR lm-ots          AS CHAR.

def var rnhaku          as char format "x(30)"  NO-UNDO.
DEF VAR nrohaku         AS LOG                  NO-UNDO.
DEF VAR haettava        AS LOG init TRUE        NO-UNDO.
DEF VAR numero          AS LOG                  NO-UNDO.
DEF VAR rtab            AS RECID EXTENT 24      NO-UNDO.
DEF VAR i               AS INT                  NO-UNDO.

DEF VAR xrecid          AS RECID.

form
    AreaPlan.AreaName  format "x(21)" /* column-label "Name of RSO " */
    AreaPlan.TrafficArea                   /* column-label "RSO"          */
WITH
    centered OVERLAY scroll 1 13 DOWN ROW 3
    COLOR value(cfc)
    title color value(ctc) " AREACODE FINDING FROM " + rnhaku + "' " FRAME sel.


form
    rnhaku NO-LABEL
    help "Give a City/AreaNAME + ENTER, areanumber +HOME"
    SKIP
    "What to find" SKIP
    "  - Name       + (ENTER)" SKIP
    "  - number     + (HOME)"  SKIP

with row 1 centered overlay title " SEEK RSO" FRAME alku.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
   FIND FIRST AreaPlan USE-INDEX TrafficArea no-lock no-error.
   IF NOT AVAIL AreaPlan THEN DO:
      BELL.
      message "There isn't any Producters - hit ENTER !".
      PAUSE no-message.
      RETURN.
   END.


   ASSIGN
   haettava = TRUE xrecid = ? privi = 0 ufkey = TRUE ekarivi = 0 siirto = ?.

kierros:
repeat WITH FRAME sel:

    IF haettava THEN DO:

       ASSIGN haettava = FALSE nrohaku = FALSE.
       PAUSE 0 no-message.
alku:  repeat WITH FRAME alku:
          ehto = 9. RUN ufkey. ufkey = TRUE.
          UPDATE rnhaku WITH FRAME alku EDITING:
             READKEY. nap = keylabel(LASTKEY).
             /* onko painettu home */
             if nap = "home" then assign nrohaku = true nap = "enter".
             APPLY keycode(nap).
          END.

          if rnhaku = "" THEN LEAVE kierros.

          IF NOT nrohaku THEN DO:
             FIND FIRST AreaPlan where AreaPlan.AreaName >= rnhaku
             no-lock no-error.
             jarj = 1.
          END.
          ELSE DO:
             FIND FIRST AreaPlan where AreaPlan.TrafficArea >= integer(rnhaku)
             no-lock no-error.
             jarj = 2.
          END.

          IF NOT AVAIL AreaPlan THEN DO:
             BELL.
             message "NONE FOUND !".
             NEXT alku.
          END.
          ASSIGN muisti = recid(AreaPlan) TMSReportttava = TRUE.
          view FRAME sel.
          LEAVE.
       END. /* repeat */
    END.

    IF jarj <> ed-jarj THEN DO:
       ed-jarj = jarj.
       if jarj = 2 then put screen row 19 col 35 " By number ".
       if jarj = 1 then put screen row 19 col 35 "  By Name  ".
    END.


tulostus:
   DO :
      IF TMSReportttava THEN DO:
         up FRAME-LINE - 1.
         FIND AreaPlan where recid(AreaPlan) = muisti no-lock no-error.

         /* TMSReporttaan 1 sivullinen tietoa ruudulle
         alkaen tietueesta, jonka avainarvo = muisti.
         alkaen rivilta privi */

         /* jos on juuri poistettu rivi, niin ... */
         IF privi > 0 THEN DOWN privi - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE AreaPlan THEN DO:
               DISPLAY AreaPlan.TrafficArea AreaPlan.AreaName.
               rtab[FRAME-LINE] = recid(AreaPlan).
               IF jarj = 2 THEN FIND NEXT AreaPlan
               USE-INDEX TrafficArea no-lock no-error.
               ELSE IF jarj = 1 THEN FIND NEXT AreaPlan
               USE-INDEX AreaName no-lock no-error.
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
         up FRAME-LINE - 1.
         DOWN ekarivi.
         ASSIGN ekarivi = 0
                TMSReportttava = FALSE.
         PAUSE 0 no-message.

         /* nyt on TMSReportttu 1 ruudullinen tavaraa ja kursori on ylim-
         mällä rivillä choosea varten. */
      END. /* TMSReportttava = TRUE */
   END. /* tulostus */

   /* jos on viimeksi poistettu rivi: */
   IF privi > 0 THEN DOWN privi - 1.
   ASSIGN privi = 0.

selaus:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 0   ufk[2]= 0   ufk[3]= 0 ufk[4]= 0
         ufk[5]= 11 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 2 THEN DO:
         CHOOSE ROW AreaPlan.TrafficArea ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) AreaPlan.TrafficArea WITH FRAME sel.
      END.
      ELSE IF jarj = 1 THEN DO:
         CHOOSE ROW AreaPlan.AreaName ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) AreaPlan.AreaName WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
         jarj = jarj + 1. IF jarj = 3 THEN jarj = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         jarj = jarj - 1. IF jarj = 0 THEN jarj = 2.
      END.

      IF jarj <> ed-jarj THEN DO:
         ASSIGN ekarivi = 0 muisti = rtab[FRAME-LINE].
         FIND AreaPlan where recid(AreaPlan) = muisti.
         DO i = 1 TO FRAME-LINE - 1:
            IF jarj = 2 THEN FIND prev AreaPlan
            USE-INDEX TrafficArea no-lock no-error.
            ELSE IF jarj = 1 THEN FIND prev AreaPlan
            USE-INDEX AreaName no-lock no-error.
            IF AVAILABLE AreaPlan THEN
               ASSIGN ekarivi = i muisti = recid(AreaPlan).
            ELSE LEAVE.
         END.
         TMSReportttava = TRUE.
         NEXT kierros.
      END.

      IF rtab[FRAME-LINE] = ? THEN DO:
         BELL.
         message "You are on an empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* edellinen rivi */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND AreaPlan where recid(AreaPlan) = rtab[1] no-lock.
            IF jarj = 2 THEN FIND prev AreaPlan
            USE-INDEX TrafficArea no-lock no-error.
            ELSE IF jarj = 1 THEN FIND prev AreaPlan
            USE-INDEX AreaName no-lock no-error.
            IF NOT AVAILABLE AreaPlan THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT selaus.
            END.
            ELSE DO:
               /* edellinen loytyi */
               scroll DOWN.
               DISPLAY AreaPlan.TrafficArea AreaPlan.AreaName.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(AreaPlan)
               muisti = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* edellinen rivi */

      /* seuraava rivi */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND AreaPlan where recid(AreaPlan) = rtab[FRAME-DOWN] no-lock .
            IF jarj = 2 THEN FIND NEXT AreaPlan
            USE-INDEX TrafficArea no-lock no-error.
            ELSE IF jarj = 1 THEN FIND NEXT AreaPlan
            USE-INDEX AreaName no-lock no-error.
            IF NOT AVAILABLE AreaPlan THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT selaus.
            END.
            ELSE DO:
               /* loytyi viela seuraava tietue */
               scroll up.
               DISPLAY AreaPlan.TrafficArea AreaPlan.AreaName.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(AreaPlan).
               /* ja lopuksi pannaan muistiin ylimman rivin avain */
               muisti = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
         muisti = rtab[1].
         FIND AreaPlan where recid(AreaPlan) = muisti no-lock no-error.
         IF jarj = 2 THEN FIND prev AreaPlan
         USE-INDEX TrafficArea no-lock no-error.
         ELSE IF jarj = 1 THEN FIND prev AreaPlan
         USE-INDEX AreaName no-lock no-error.
         IF AVAILABLE AreaPlan THEN DO:
            muisti = recid(AreaPlan).

            /* mennään tiedostoa taaksepäin 1 sivun verran */
            DO rivi = 1 TO (FRAME-DOWN - 1):
               IF jarj = 2 THEN FIND prev AreaPlan
               USE-INDEX TrafficArea no-lock no-error.
               ELSE IF jarj = 1 THEN FIND prev AreaPlan
               USE-INDEX AreaName no-lock no-error.
               IF AVAILABLE AreaPlan THEN muisti = recid(AreaPlan).
               ELSE rivi = FRAME-DOWN.
            END.
            TMSReportttava = TRUE.
            NEXT kierros.
         END.
         ELSE DO:
            /* sivun eka oli myos tiedoston eka */
            message "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* edellinen sivu */

     /* seuraava sivu */
     else if lookup(nap,"next-page,page-down") > 0 THEN DO WITH FRAME sel:
        /* kohdistin alimmalle riville */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* alin rivi ei ollut tyhja */
            muisti = rtab[FRAME-DOWN].
            FIND AreaPlan where recid(AreaPlan) = muisti no-lock.
            TMSReportttava = TRUE.
            NEXT kierros.
        END.
     END. /* seuraava sivu */

     else if lookup(nap,"5,f5,enter,return") > 0 THEN DO: /* valinta */
        FIND AreaPlan where recid(AreaPlan) = rtab[FRAME-LINE] no-lock.
        siirto = string(AreaPlan.TrafficArea).
        LEAVE kierros.
     END.


     else if lookup(nap,"home") > 0 THEN DO:
        IF jarj = 2 THEN FIND FIRST AreaPlan
        USE-INDEX TrafficArea no-lock no-error.
        ELSE IF jarj = 1 THEN FIND FIRST AreaPlan
        USE-INDEX AreaName no-lock no-error.
        ASSIGN muisti = recid(AreaPlan) TMSReportttava = TRUE.
        NEXT kierros.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
        IF jarj = 2 THEN FIND LAST AreaPlan
        USE-INDEX TrafficArea no-lock no-error.
        ELSE IF jarj = 1 THEN FIND LAST AreaPlan
        USE-INDEX AreaName no-lock no-error.
        ASSIGN muisti = recid(AreaPlan) TMSReportttava = TRUE.
        NEXT kierros.
     END.

     else if lookup(nap,"8,f8") > 0 THEN DO:
        haettava = TRUE.
        HIDE FRAME sel no-pause.
        NEXT kierros.
     END.

  END.  /* selaus */
END.  /* kierros */

HIDE FRAME sel no-pause.
HIDE FRAME alku no-pause.
si-recid = xrecid.

