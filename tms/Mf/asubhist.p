/*-----------------------------------------------------------------------------
  MODULE .......: ASUBHIST.p
  FUNCTION .....: Show all previous owners of (A-sub.) records
  APPLICATION ..: tms
  AUTHOR .......: tk
  CREATED ......: 01.07.2002
  MODIFIED .....: 05.09.02 lp added DefClStamp
                  21.10.02 kl new version
                  21.02.03 kl delete pHandle
                  06.03.03 KL MASTER 1.0 
                  12.03.03 tk fixed tulostus
  VERSION ......: M15
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}

DEF /* NEW */ SHARED VAR siirto AS CHAR.

DEF INPUT PARAMETER an1 AS CHAR NO-UNDO.

DEF VAR haku-CLI  LIKE CLI.CLI  NO-UNDO.
DEF VAR xrecid       AS recid                           INIT ?.
DEF VAR ekarivi      AS INT                    NO-UNDO  INIT 0.
DEF VAR jarj         AS INT                    NO-UNDO  INIT 1.
DEF VAR jarjlkm      AS INT                    NO-UNDO  INIT 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  INIT true.
DEF VAR privi        AS INT                    NO-UNDO  INIT 0.
DEF VAR ed-jarj      AS INT                    NO-UNDO.
DEF VAR muisti       AS recid                  NO-UNDO.
DEF VAR rivi         AS INT FORMAT "99"        NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR lm-ots       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS LOG FORMAT "Yes/No"    NO-UNDO.
DEF VAR todate       AS DATE                   NO-UNDO.
DEF VAR tempdate     AS DATE                   NO-UNDO.
DEF VAR lcfrom       AS CHAR                   NO-UNDO.
DEF VAR lcto         AS CHAR                   NO-UNDO.
DEF VAR delline      AS INT                    NO-UNDO.
DEF VAR lhist        AS LO  FORMAT "*/ "       NO-UNDO.
DEF VAR lclstamp     AS DEC                    NO-UNDO.

DEF VAR pHandle      AS HANDLE                 NO-UNDO.
RUN Mf/clipers.p PERSISTENT SET pHandle.

DEF TEMP-TABLE ttCLI LIKE CLI.
DEF BUFFER bufatno FOR CLI.
lclstamp = DEC(fCparamC("DefClStamp")).

FORM
   CLI.CustNum
   Customer.CustName                           FORMAT "x(16)" 
   CLI.Owner                                   FORMAT "x(16)"
   lcfrom            COLUMN-LABEL "Valid from" FORMAT "x(16)"
   lcto              COLUMN-LABEL "Valid to"   FORMAT "x(16)"
WITH CENTERED OVERLAY ROW 2 13 DOWN
   COLOR value(cfc) title COLOR value(ctc) 
   " HISTORY OF A-SUB NUMBER " + an1 + " " 
FRAME sel.

FORM /* Numeron haku kentällä CustNum */
   haku-CLI  
      HELP "Enter A-sub no. or its first digits"          
WITH
   ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND A-SUB. NO " 
   COLOR VALUE(cfc) NO-LABELS OVERLAY 
FRAME haku-f1.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST CLI WHERE 
           CLI.CLI     = an1 /* AND 
           CLI.CustNum = asn  AND
           CLI.clStamp NE lclstamp */
USE-INDEX valid NO-LOCK NO-ERROR.

IF AVAILABLE CLI THEN ASSIGN
   muisti     = recid(CLI)
   must-print = true.
ELSE DO:
   MESSAGE "No history available !" VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF jarj <> ed-jarj THEN DO:
       ed-jarj = jarj.
/*
       IF jarj = 1 THEN put screen row 19 col 30 " By Number  ".
               IF jarj = 2 THEN put screen row 19 col 30 " By Name    ".
*/
    END.

   tulostus:
   do :
      IF must-print THEN DO:
         up frame-line - 1.
         FIND CLI WHERE recid(CLI) = muisti NO-LOCK NO-ERROR.

         /* tulostetaan 1 sivullinen tietoa ruudulle
         alkaen tietueesta, jonka avainarvo = muisti.
         alkaen rivilta privi */

         /* jos on juuri poistettu rivi, niin ... */
         IF privi > 0 THEN DOwn privi - 1.

         repeat WITH FRAME sel:
            IF available CLI THEN DO:

               RUN LOCAL-DISP-ALL.

               rtab[frame-line] = recid(CLI).

               RUN pFindNext.

            END.
            ELSE DO:
               clear NO-PAUSE.
               rtab[frame-line] = ?.
            END.
            IF frame-line = frame-down THEN LEAVE.
            down.
         END.
         up frame-line - 1.
         down ekarivi.
         ASSIGN ekarivi = 0
                must-print = false.
         PAUSE 0 NO-MESSAGE.

         /* nyt on tulostettu 1 ruudullinen tavaraa ja kursORi on ylim-
         mällällä rivillä choosea varten. */
      END. /* must-print = true */
   END. /* tulostus */

   /* jos on viimeksi poistettu rivi: */
   IF privi > 0 THEN DOwn privi - 1.
   ASSIGN privi = 0.

BROWSE:
   repeat WITH FRAME sel on endkey undo, return:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 0  ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 0   ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      END.

      hide MESSAGE NO-PAUSE.
      IF jarj = 1 THEN DO:
         CHOOSE ROW lcfrom {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) lcfrom WITH FRAME sel.
      END.
/*      ELSE IF jarj = 2 THEN DO:
         CHOOSE ROW CLI.CLI {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) CLI.CLI WITH FRAME sel.
      END.
    IF jarj = 3 THEN DO:
         CHOOSE ROW CLI.?? {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) CLI.?? WITH FRAME sel.
      END.
      ELSE IF jarj = 4 THEN DO:
         CHOOSE ROW CLI.??  {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) CLI.? WITH FRAME sel.
      END.
*/
      IF rtab[frame-line] = ? THEN NEXT.

      nap = keylabel(lastkey).

      IF lookup(nap,"cursor-right") > 0 THEN DO:
         jarj = jarj + 1. IF jarj > jarjlkm THEN jarj = 1.
      END.
      IF lookup(nap,"cursOR-left") > 0 THEN DO:
         jarj = jarj - 1. IF jarj = 0 THEN jarj = jarjlkm.
      END.

      IF jarj <> ed-jarj THEN DO:
         ASSIGN ekarivi = 0 muisti = rtab[frame-line].
         FIND CLI WHERE recid(CLI) = muisti.
         do i = 1 to frame-line - 1:
            RUN pFindPrev.
            IF available CLI THEN
               ASSIGN ekarivi = i muisti = recid(CLI).
            ELSE LEAVE.
         END.
         must-print = true.
         NEXT LOOP.
      END.

      IF rtab[frame-line] = ? THEN DO:
         BELL.
         MESSAGE "You are on empty row, move upwards !".
         PAUSE 1 NO-MESSAGE.
         NEXT.
      END.

      ASSIGN nap = keylabel(lastkey).

      /* edellinen rivi */
      IF lookup(nap,"cursOR-up") > 0 THEN DO WITH FRAME sel:
         IF frame-line = 1 THEN DO:
            FIND CLI WHERE recid(CLI) = rtab[1] NO-LOCK.
            RUN pFindPrev.
            IF NOT AVAILABLE CLI THEN DO:
               MESSAGE "YOU ARE ON THE FIRST ROW !".
               BELL. PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* edellinen loytyi */
               scroll down.
               RUN LOCAL-DISP-ALL.
               do i = frame-down to 2 by -1:
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
      ELSE IF lookup(nap,"cursOR-down") > 0 THEN DO
      WITH FRAME sel:
         IF frame-line = frame-down THEN DO:
            FIND CLI WHERE recid(CLI) = rtab[frame-down] NO-LOCK .
            RUN pFindNext.
            IF NOT AVAILABLE CLI THEN DO:
               MESSAGE "YOU ARE ON THE LAST ROW !".
               BELL. PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* loytyi viela seuraava tietue */
               scroll up.
               RUN LOCAL-DISP-ALL.
               do i = 1 to frame-down - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[frame-down] = recid(CLI).
               /* ja lopuksi pannaan muistiin ylimman rivin avain */
               muisti = rtab[1].
            END.
         END.
         ELSE DOwn 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      ELSE IF lookup(nap,"prev-page,page-up") > 0 THEN DO:
         muisti = rtab[1].
         FIND CLI WHERE recid(CLI) = muisti NO-LOCK NO-ERROR.
         RUN pFindPrev.
         IF available CLI THEN DO:
            muisti = recid(CLI).

            /* mennään tiedostoa taaksepäin 1 sivun verran */
            do rivi = 1 to (frame-down - 1):
               RUN pFindPrev.
               IF available CLI THEN muisti = recid(CLI).
               ELSE rivi = frame-down.
            END.
            must-print = true.
            NEXT LOOP.
         END.
         ELSE DO:
            /* sivun eka oli myos tiedoston eka */
            MESSAGE "THIS IS THE FIRST LINE !".
            BELL. PAUSE 1 NO-MESSAGE.
         END.
     END. /* edellinen sivu */

     /* seuraava sivu */
     ELSE IF lookup(nap,"NEXT-page,page-down") > 0 THEN DO WITH FRAME sel:
        /* kohdistin alimmalle riville */
        IF rtab[frame-down] = ? THEN DO:
            MESSAGE "THIS IS THE LAST PAGE !".
            BELL. PAUSE 1 NO-MESSAGE.
        END.
        ELSE DO: /* alin rivi ei ollut tyhja */
            muisti = rtab[frame-down].
            FIND CLI WHERE recid(CLI) = muisti NO-LOCK.
            must-print = true.
            NEXT LOOP.
        END.
     END. /* seuraava sivu */
/*
     /* Haku 1 */
     ELSE IF lookup(nap,"1,f1") > 0 THEN DO on endkey undo, NEXT LOOP:
        cfc = "puyr". RUN Syst/ufcolor.p.
        haku-CLI = "".
        ehto = 9. RUN Syst/ufkey.p. ufkey = true.
        UPDATE haku-CLI WITH FRAME haku-f1.
        HIDE FRAME haku-f1 NO-PAUSE.
        IF haku-CLI <> "" THEN DO:
           FIND FIRST CLI WHERE 
                      CLI.CLI >= haku-CLI
           NO-LOCK NO-ERROR.
           IF NOT AVAILABLE CLI THEN DO:
              BELL.
              MESSAGE "NONE FOUND !".      
              PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           /* joku CLI/CustNum loytyi */
           ASSIGN jarj = 1 muisti = recid(CLI) must-print = true.
           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */
*/     

     ELSE IF lookup(nap,"enter,return") > 0 THEN DO:
        muisti = rtab[frame-line(sel)].
        RUN pUpdateCLI IN pHandle(INPUT-OUTPUT muisti).
        must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF lookup(nap,"home") > 0 THEN DO:
        RUN pFindFirst.
        ASSIGN muisti = recid(CLI) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
        RUN pFindLast.
        ASSIGN muisti = recid(CLI) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

DELETE PROCEDURE pHandle.

PROCEDURE LOCAL-DISP-ALL: 

   lcTo = fTS2HMS(CLI.clStamp). 
   IF CLI.clStamp GE lclstamp THEN lcTo = "UNTIL FURTHER". 

   FIND FIRST Customer WHERE 
              Customer.CustNum = CLI.CustNum 
   NO-LOCK NO-ERROR.  

   DISPLAY  
      CLI.CustNum 
      Customer.CustName 
      CLI.Owner  
      fTS2HMS(CLI.crStamp) @ lcfrom  
      lcto 
   WITH FRAME SEL. 

END PROCEDURE.

PROCEDURE pFindNext:
   IF jarj = 1 THEN
      FIND NEXT CLI WHERE 
                CLI.CLI = an1
      USE-INDEX valid NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE pFindPrev:
   IF jarj = 1 THEN
      FIND PREV CLI WHERE 
                CLI.CLI = an1
      USE-INDEX valid NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE pFindFirst:
   IF jarj = 1 THEN
      FIND FIRST CLI WHERE 
                 CLI.CLI = an1
      USE-INDEX valid NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE pFindLast:
   IF jarj = 1 THEN
      FIND LAST CLI WHERE 
                CLI.CLI = an1
      USE-INDEX valid NO-LOCK NO-ERROR.
END PROCEDURE.

