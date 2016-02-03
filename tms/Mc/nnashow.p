/* ----------------------------------------------------------------------------
  MODULE .......: nnashow.p
  TASK .. ......: UPDATE A-sub.Number
  APPLICATION ..: nn
  AUTHOR .......: lp
  CREATED ......: 11.09.02
  MODIFIED .....: 09.10.02 lp DefClStamp instead fMakeTS()
  VERSIO .......:   
  -------------------------------------------------------------------------- */
{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}

DEF /*NEW*/ shared VAR siirto AS CHAR.

DEF INPUT PARAMETER cust AS I NO-UNDO.
DEF INPUT PARAMETER bt   AS I NO-UNDO.
DEF VAR haku-CLI     LIKE CLI.CLI  NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR ekarivi      AS INT                    NO-UNDO  init 0.
DEF VAR jarj         AS INT                    NO-UNDO  init 1.
DEF VAR jarjlkm      AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR privi        AS INT                    NO-UNDO  init 0.
DEF VAR ed-jarj      AS INT                    NO-UNDO.
DEF VAR muisti       AS RECID                  NO-UNDO.
def VAR rivi         AS INT FORMAT "99"        NO-UNDO.

DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR lm-ots       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS LOG FORMAT "Yes/No"    NO-UNDO.
DEF VAR todate       AS DATE                   NO-UNDO.
DEF VAR tempdate     AS DATE                   NO-UNDO.
DEF VAR lcfrom       AS CHAR                   NO-UNDO.

DEF VAR delline      AS INT                    NO-UNDO.
DEF VAR lhist        AS LO  FORMAT "*/ "       NO-UNDO.
DEF VAR lclstamp     AS DEC                    NO-UNDO.
DEF TEMP-TABLE ttcli LIKE CLI.
DEF BUFFER bufcli FOR CLI.

lclstamp = fMakeTS().

form
    CLI.CLI    
    CLI.OwnerName                             FORMAT "x(25)"
    lcfrom          COLUMN-LABEL "Valid from" FORMAT "x(16)"
    lhist           COLUMN-LABEL "H"
WITH centered OVERLAY ROW 2 13 DOWN
    color value(cfc) title color value(ctc) 
   " A-sub.Nums Of Invoicing Target " + STRING(bt) + " Of Cust " + STRING(cust)
FRAME sel.

FORM
    SKIP(1)
    "Note: Now You CAN define a new OWNER for this" SKIP
    "A-Number (CLI)"                                SKIP(1)

    ttCLI.CLI          LABEL "A-sub number .. :"
    todate             LABEL "Valid to ...... :" FORMAT "99-99-99"
    HELP "Last date when this number belonged to old owner"
    ttcli.Ownername    LABEL "New Owner ..... :"
    HELP "Name of new owner (for call specifications etc)"
WITH CENTERED OVERLAY ROW 8 1 COL
    TITLE " NEW OWNER "
FRAME lis.


form /* Numeron haku kent‰ll‰ CustNum */
    haku-CLI 
    help "Enter A-sub no. or its FIRST digits"         
WITH row 4 col 2 title color value(ctc) " FIND A-SUB. NO "
     COLOR value(cfc) NO-LABELS OVERLAY 
FRAME haku-f1.


cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST CLI WHERE
           CLI.Custnum    = cust AND
           CLI.BillTarget = bt   AND
           CLI.CrStamp    <= lclstamp AND
           CLI.ClStamp    >= lclstamp
USE-INDEX CLI NO-LOCK NO-ERROR.

IF AVAILABLE CLI THEN ASSIGN
   muisti      = recid(CLI)
   must-print  = TRUE
   must-add    = FALSE.
ELSE  DO:
   MESSAGE "There are no A-numbers in this billing target !" VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
repeat WITH FRAME sel:

    IF jarj <> ed-jarj THEN DO:
       ed-jarj = jarj.
    END.
tulostus:
   DO :
      IF must-print THEN DO:
         UP FRAME-LINE - 1.
         FIND CLI WHERE recid(CLI) = muisti NO-LOCK NO-ERROR.

         /* TMSReporttaan 1 sivullinen tietoa ruudulle
         alkaen tietueesta, jonka avainarvo = muisti.
         alkaen rivilta privi */

         /* jos on juuri poistettu rivi, niin ... */
         IF privi > 0 THEN DOWN privi - 1.

         REPEAT WITH FRAME sel:
            IF AVAILABLE CLI THEN DO:

               IF CAN-FIND(FIRST bufcli WHERE
                                 LENGTH(bufCLI.CLI) = LENGTH(CLI.CLI) AND
                                 bufcli.cli = CLI.CLI)  
               THEN lhist = true.
               ELSE lhist = false.

               DISPLAY CLI.CLI 
                       CLI.OwnerName
                       fTS2HMS(CLI.CrStamp) @ lcFrom
                       lhist.
               rtab[FRAME-LINE] = recid(CLI).
               IF jarj = 1 THEN FIND NEXT CLI WHERE
                                          CLI.CustNum    = cust AND
                                          CLI.BillTarget = bt AND
                                          CLI.CrStamp    <= lclstamp AND
                                          CLI.ClStamp    >= lclstamp
               USE-INDEX CLI NO-LOCK NO-ERROR.
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
         UP FRAME-LINE - 1.
         DOWN ekarivi.
         ASSIGN ekarivi = 0
                must-print = FALSE.
         PAUSE 0 no-message.

         /* nyt on TMSReportttu 1 ruudullinen tavaraa ja kursori on ylim-
         m‰ll‰ rivill‰ choosea varten. */
      END. /* must-print = TRUE */
   END. /* tulostus */

   /* jos on viimeksi poistettu rivi: */
   IF privi > 0 THEN DOWN privi - 1.
   ASSIGN privi = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 36  ufk[2]= 0 ufk[3]= 0  ufk[4]= 0
         ufk[5]= 0 /*5*/   ufk[6]= 0 /*4*/ ufk[7]= 37 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
         CHOOSE ROW CLI.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) CLI.CLI WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
         jarj = jarj + 1. IF jarj > jarjlkm THEN jarj = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         jarj = jarj - 1. IF jarj = 0 THEN jarj = jarjlkm.
      END.

      IF jarj <> ed-jarj THEN DO:
         ASSIGN ekarivi = 0 muisti = rtab[FRAME-LINE].
         FIND CLI WHERE recid(CLI) = muisti.
         DO i = 1 TO FRAME-LINE - 1:
            IF jarj = 1 THEN FIND prev CLI WHERE 
                                       CLI.Custnum    = cust AND
                                       CLI.BillTarget = bt   AND
                                       CLI.CrStamp    <= lclstamp AND
                                       CLI.ClStamp    >= lclstamp
                    USE-INDEX CLI NO-LOCK NO-ERROR.
            IF AVAILABLE CLI THEN
               ASSIGN ekarivi = i muisti = recid(CLI).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         message "You are on empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* edellinen rivi */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND CLI WHERE recid(CLI) = rtab[1] NO-LOCK.
            IF jarj = 1 THEN FIND prev CLI WHERE 
                                       CLI.CustNum    = cust AND
                                       CLI.BillTarget = bt   AND
                                       CLI.CrStamp    <= lclstamp AND
                                       CLI.ClStamp    >= lclstamp
            USE-INDEX CLI NO-LOCK NO-ERROR.
            IF NOT AVAILABLE CLI THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL. PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* edellinen loytyi */
               scroll DOWN.
               IF CAN-FIND(FIRST bufcli WHERE
                                 LENGTH(bufCLI.CLI) = LENGTH(CLI.CLI) AND
                                 bufCLI.CLI         = CLI.CLI) 
                  THEN lhist = true.
                  ELSE lhist = false.

               DISPLAY 
                  CLI.CLI
                  CLI.OwnerName
                  fTS2HMS(CLI.CrStamp) @ lcFrom
                  lhist.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(CLI)
               muisti = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* edellinen rivi */

      /* seuraava rivi */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND CLI WHERE recid(CLI) = rtab[FRAME-DOWN] NO-LOCK .
            IF jarj = 1 THEN FIND NEXT CLI WHERE 
                                       CLI.CustNum    = cust AND
                                       CLI.BillTarget = bt   AND
                                       CLI.CrStamp    <= lclstamp AND
                                       CLI.ClStamp    >= lclstamp
            USE-INDEX CLI NO-LOCK NO-ERROR.
            IF NOT AVAILABLE CLI THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL. PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* loytyi viela seuraava tietue */
               scroll up.
               IF CAN-FIND(FIRST bufcli WHERE
                                 LENGTH(bufCLI.CLI) = LENGTH(CLI.CLI) AND
                                 bufCLI.CLI = CLI.CLI)
                  THEN lhist = true.
                  ELSE lhist = false.

               DISPLAY 
                  CLI.CLI
                  CLI.OwnerName
                  fTS2HMS(CLI.CrStamp) @ lcFrom
                  lhist.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(CLI).
               /* ja lopuksi pannaan muistiin ylimman rivin avain */
               muisti = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
         muisti = rtab[1].
         FIND CLI WHERE recid(CLI) = muisti NO-LOCK NO-ERROR.
         IF jarj = 1 THEN FIND prev CLI WHERE 
                                    CLI.CustNum    = cust AND
                                    CLI.BillTarget = bt   AND
                                    CLI.CrStamp    <= lclstamp AND
                                    CLI.ClStamp    >= lclstamp
         USE-INDEX CLI NO-LOCK NO-ERROR.
         IF AVAILABLE CLI THEN DO:
            muisti = recid(CLI).

            /* menn‰‰n tiedostoa taaksep‰in 1 sivun verran */
            DO rivi = 1 TO (FRAME-DOWN - 1):
               IF jarj = 1 THEN FIND prev CLI WHERE 
                                          CLI.CustNum    = cust AND
                                          CLI.BillTarget = bt   AND
                                          CLI.CrStamp    <= lclstamp AND
                                          CLI.ClStamp    >= lclstamp
               USE-INDEX CLI NO-LOCK NO-ERROR.
               IF AVAILABLE CLI THEN muisti = recid(CLI).
               ELSE rivi = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* sivun eka oli myos tiedoston eka */
            message "THIS IS THE FIRST LINE !".
            BELL. PAUSE 1 no-message.
         END.
     END. /* edellinen sivu */

     /* seuraava sivu */
     else if lookup(nap,"next-page,page-down") > 0 THEN DO WITH FRAME sel:
        /* kohdistin alimmalle riville */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "THIS IS THE LAST PAGE !".
            BELL. PAUSE 1 no-message.
        END.
        ELSE DO: /* alin rivi ei ollut tyhja */
            muisti = rtab[FRAME-DOWN].
            FIND CLI WHERE recid(CLI) = muisti NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* seuraava sivu */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". RUN ufcolor.
        haku-CLI = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        UPDATE haku-CLI WITH FRAME haku-f1.
        HIDE FRAME haku-f1 no-pause.
        if haku-CLI <> "" THEN DO:
           FIND FIRST CLI WHERE 
                      LENGTH(CLI.CLI) = LENGTH(haku-CLI) AND
                      CLI.CLI >= haku-CLI AND 
                      CLI.CustNum = cust AND
                      CLI.BillTarget = bt AND
                      CLI.CrStamp    <= lclstamp AND
                      CLI.ClStamp    >= lclstamp
           NO-LOCK NO-ERROR.
           IF NOT AVAILABLE CLI THEN DO:
              BELL.
              message "NONE FOUND !".      
              PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           /* joku CLI/CustNum loytyi */
           ASSIGN jarj = 1 muisti = recid(CLI) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     ELSE IF lookup(nap,"5,f5") > 0 THEN DO:

        must-add = true.
        NEXT LOOP.

     END.

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME sel TRANSACTION:
        /* muutos */
        FIND CLI WHERE recid(CLI) = rtab[frame-line(sel)]
        exclusive-lock.
        UPDATE CLI.OwnerName.
        xrecid = recid(CLI).
     END.

     else if lookup(nap,"home") > 0 THEN DO:
        IF jarj = 1 THEN FIND FIRST CLI WHERE 
                                    CLI.CustNum = cust AND
                                    CLI.BillTarget = bt AND
                                    CLI.CrStamp    <= lclstamp AND
                                    CLI.ClStamp    >= lclstamp
                         USE-INDEX CLI NO-LOCK NO-ERROR.
        ASSIGN muisti = recid(CLI) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
        IF jarj = 1 THEN FIND LAST CLI WHERE 
                                   CLI.CustNum = cust AND
                                   CLI.BillTarget = bt AND
                                   CLI.CrStamp    <= lclstamp AND
                                   CLI.ClStamp    >= lclstamp
                         USE-INDEX CLI NO-LOCK NO-ERROR.
        ASSIGN muisti = recid(CLI) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO:
        FIND CLI WHERE recid(CLI) = rtab[frame-line(sel)].
        RUN asubhist.p(CLI.CLI).
        must-print = true.
        ufkey = true.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

