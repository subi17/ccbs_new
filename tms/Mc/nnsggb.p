/* -----------------------------------------------
  MODULI .......: NNSGGP.P
  TEHTÄVÄ ......: Browse AND pick up Salesman Groups
  SOVELLUS .....: NN
  TEKIJÄ .......: PT
  LUONTIPVM ....: 02-12-98
  MUUTOSPVM ....: 11-11-02 jr  Eventlog
                  25.03.03/aam nnsgme1 instead of nncgme1
                  17.09.03/aam brand
  VERSIO .......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 

DEF INPUT PARAMETER Salesman AS c NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR SMGroup  LIKE SMGroup.SmGroup  NO-UNDO.
DEF VAR SGName LIKE SMGroup.SGName NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR ekarivi      AS INT                    NO-UNDO  init 0.
DEF VAR jarj         AS INT                    NO-UNDO  init 1.
DEF VAR jarjlkm      AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR privi        AS INT                    NO-UNDO  init 0.
DEF VAR ed-jarj      AS INT                    NO-UNDO.
DEF VAR muisti       AS RECID                  NO-UNDO.
def var rivi         as int format "99"        NO-UNDO.
DEF VAR tulostettava AS LOG                    NO-UNDO.
DEF VAR lisattava    AS LOG                    NO-UNDO.
DEF VAR lm-ots       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
def var ok           as log format "Yes/No"    NO-UNDO.
def var memb         as lo  format "*/"        NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSMGMember AS HANDLE NO-UNDO.
   lhSMGMember = BUFFER SMGMember:HANDLE.
   RUN StarEventInitialize(lhSMGMember).

   DEFINE VARIABLE lhSMGroup AS HANDLE NO-UNDO.
   lhSMGroup = BUFFER SMGroup:HANDLE.
   RUN StarEventInitialize(lhSMGroup).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhSMGroup).
   END.
END.

form
    memb                column-label "Member"
    SMGroup.SmGroup      /* COLUMN-LABEL FORMAT */
    SMGroup.SGName      /* COLUMN-LABEL FORMAT */
WITH centered OVERLAY scroll 1 13 DOWN ROW 2 COLOR value(cfc)
    TITLE COLOR value(ctc)
    " Join SMan '" + string(Salesman) + "' " + substr(Salesman.SmName,1,16) +
    " into groups (" + gcBrand + ") "
    FRAME sel.

form
   SMGroup.Memo
WITH
   centered 1 col no-label overlay row 2 title " Group '" + SMGroup.SmGroup +
   "' " + SMGroup.SGName + "  memo " FRAME memo.

form /* Salesman Group :n haku kentällä SMGroup */
    SMGroup
    help "Type Group Code"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* Salesman Group :n haku kentällä SGName */
    SGName
    help "Type first characters of a name"
    with row 4 col 2 title color value(ctc) " FIND name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.


FIND Salesman where 
     Salesman.Brand    = gcBrand AND
     Salesman.Salesman = Salesman no-lock.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST SMGroup
WHERE SMGroup.Brand = gcBrand no-lock no-error.
IF AVAILABLE SMGroup THEN ASSIGN
   muisti       = recid(SMGroup)
   tulostettava = TRUE
   lisattava    = FALSE.
ELSE DO:
   bell. message "No Salesman groups available - press ENTER !".
   PAUSE no-message.
   RETURN.
END.

LOOP:
repeat WITH FRAME sel:

    IF jarj <> ed-jarj THEN DO:
       ed-jarj = jarj.
       if jarj = 1 then put screen row 18 col 35 " By Code ".
       if jarj = 2 then put screen row 18 col 35 " By Name ".
    END.

tulostus:
   DO :
      IF tulostettava THEN DO:
   up FRAME-LINE - 1.
   FIND SMGroup where recid(SMGroup) = muisti no-lock no-error.

   /* tulostetaan 1 sivullinen tietoa ruudulle
   alkaen tietueesta, jonka avainarvo = muisti.
   alkaen rivilta privi */

   /* jos on juuri poistettu rivi, niin ... */
   IF privi > 0 THEN DOWN privi - 1.

   repeat WITH FRAME sel:
      IF AVAILABLE SMGroup THEN DO:

         RUN local-disp-row.

         rtab[FRAME-LINE] = recid(SMGroup).
         IF jarj = 1 THEN FIND NEXT SMGroup
         WHERE SMGroup.Brand = gcBrand no-lock no-error.
         ELSE IF jarj = 2 THEN FIND NEXT SMGroup USE-INDEX SGName
         WHERE SMGroup.Brand = gcBrand no-lock no-error.
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
          tulostettava = FALSE.
   PAUSE 0 no-message.

   /* nyt on tulostettu 1 ruudullinen tavaraa ja kursori on ylim-
   mällä rivillä choosea varten. */
      END. /* tulostettava = TRUE */
   END. /* tulostus */

   /* jos on viimeksi poistettu rivi: */
   IF privi > 0 THEN DOWN privi - 1.
   ASSIGN privi = 0.

SELAUS:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
   ASSIGN
   ufk[1]= 35   ufk[2]= 30 ufk[3]= 927 ufk[4]= 510
   ufk[5]= 515  ufk[6]= 0  ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
   ehto = 3 ufkey = FALSE.
   RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
   CHOOSE ROW SMGroup.SmGroup {Syst/uchoose.i} no-error WITH FRAME sel.
   COLOR DISPLAY value(ccc) SMGroup.SmGroup WITH FRAME sel.
      END.
      ELSE IF jarj = 2 THEN DO:
   CHOOSE ROW SMGroup.SGName {Syst/uchoose.i} no-error WITH FRAME sel.
   COLOR DISPLAY value(ccc) SMGroup.SGName WITH FRAME sel.
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
   FIND SMGroup where recid(SMGroup) = muisti NO-LOCK.
   DO i = 1 TO FRAME-LINE - 1:
      IF jarj = 1 THEN FIND prev SMGroup
      WHERE SMGroup.Brand = gcBrand no-lock no-error.
      ELSE IF jarj = 2 THEN FIND prev SMGroup USE-INDEX SGName
      WHERE SMGroup.Brand = gcBrand no-lock no-error.
      IF AVAILABLE SMGroup THEN
         ASSIGN ekarivi = i muisti = recid(SMGroup).
      ELSE LEAVE.
   END.
   tulostettava = TRUE.
   NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT lisattava THEN DO:
   BELL.
   message "You are on an empty row, move upwards !".
   PAUSE 1 no-message.
   NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* edellinen rivi */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
   IF FRAME-LINE = 1 THEN DO:
      FIND SMGroup where recid(SMGroup) = rtab[1] no-lock.
      IF jarj = 1 THEN FIND prev SMGroup
      WHERE SMGroup.Brand = gcBrand no-lock no-error.
      ELSE IF jarj = 2 THEN FIND prev SMGroup USE-INDEX SGName
      WHERE SMGroup.Brand = gcBrand no-lock no-error.
      IF NOT AVAILABLE SMGroup THEN DO:
         message "YOU ARE ON THE FIRST ROW !".
         BELL. PAUSE 1 no-message.
         NEXT SELAUS.
      END.
      ELSE DO:
         /* edellinen loytyi */
         scroll DOWN.
         RUN local-disp-row.
         DO i = FRAME-DOWN TO 2 BY -1:
       rtab[i] = rtab[i - 1].
         END.
         ASSIGN
         rtab[1] = recid(SMGroup)
         muisti = rtab[1].
      END.
   END.
   ELSE up 1.
      END. /* edellinen rivi */

      /* seuraava rivi */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
   IF FRAME-LINE = FRAME-DOWN THEN DO:
      FIND SMGroup where recid(SMGroup) = rtab[FRAME-DOWN] no-lock .
      IF jarj = 1 THEN FIND NEXT SMGroup
      WHERE SMGroup.Brand = gcBrand no-lock no-error.
      ELSE IF jarj = 2 THEN FIND NEXT SMGroup USE-INDEX SGName
      WHERE SMGroup.Brand = gcBrand no-lock no-error.
      IF NOT AVAILABLE SMGroup THEN DO:
         message "YOU ARE ON THE LAST ROW !".
         BELL. PAUSE 1 no-message.
         NEXT SELAUS.
      END.
      ELSE DO:
         /* loytyi viela seuraava tietue */
         scroll up.
         RUN local-disp-row.
         DO i = 1 TO FRAME-DOWN - 1:
       rtab[i] = rtab[i + 1].
         END.
         rtab[FRAME-DOWN] = recid(SMGroup).
         /* ja lopuksi pannaan muistiin ylimman rivin avain */
         muisti = rtab[1].
      END.
   END.
   ELSE DOWN 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
   muisti = rtab[1].
   FIND SMGroup where recid(SMGroup) = muisti no-lock no-error.
   IF jarj = 1 THEN FIND prev SMGroup
   WHERE SMGroup.Brand = gcBrand no-lock no-error.
   ELSE IF jarj = 2 THEN FIND prev SMGroup USE-INDEX SGName
   WHERE SMGroup.Brand = gcBrand no-lock no-error.
   IF AVAILABLE SMGroup THEN DO:
      muisti = recid(SMGroup).

      /* mennään tiedostoa taaksepäin 1 sivun verran */
      DO rivi = 1 TO (FRAME-DOWN - 1):
         IF jarj = 1 THEN FIND prev SMGroup
         WHERE SMGroup.Brand = gcBrand no-lock no-error.
         ELSE IF jarj = 2 THEN FIND prev SMGroup USE-INDEX SGName
         WHERE SMGroup.Brand = gcBrand no-lock no-error.
         IF AVAILABLE SMGroup THEN muisti = recid(SMGroup).
         ELSE rivi = FRAME-DOWN.
      END.
      tulostettava = TRUE.
      NEXT LOOP.
   END.
   ELSE DO:
      /* sivun eka oli myos tiedoston eka */
      message "YOU ARE ON THE FIRST PAGE !".
      BELL. PAUSE 1 no-message.
   END.
     END. /* edellinen sivu */

     /* seuraava sivu */
     else if lookup(nap,"next-page,page-down") > 0 THEN DO WITH FRAME sel:
       /* kohdistin alimmalle riville */
       IF rtab[FRAME-DOWN] = ? THEN DO:
      message "YOU ARE ON THE LAST PAGE !".
      BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* alin rivi ei ollut tyhja */
      muisti = rtab[FRAME-DOWN].
      FIND SMGroup where recid(SMGroup) = muisti no-lock.
      tulostettava = TRUE.
      NEXT LOOP.
       END.
     END. /* seuraava sivu */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       SMGroup = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE SMGroup WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if SMGroup <> "" THEN DO:
          FIND FIRST SMGroup where 
                     SMGroup.Brand = gcBrand AND
                     SMGroup.SmGroup >= SMGroup
          no-lock no-error.
          IF NOT AVAILABLE SMGroup THEN DO:
             BELL.
             message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT SELAUS.
          END.

          /* joku smgroup/sg-code loytyi */
          ASSIGN jarj = 1 muisti = recid(SMGroup) tulostettava = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       SGName = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE SGName WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if SGName <> "" THEN DO:
          FIND FIRST SMGroup where 
                     SMGroup.Brand = gcBrand AND
                     SMGroup.SGName >= SGName
          USE-INDEX SGName no-lock no-error.

          IF NOT AVAILABLE SMGroup THEN DO:
             bell. message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT SELAUS.
          END.
          /* joku smgroup/sg-name loytyi */
          ASSIGN jarj = 2 muisti = recid(SMGroup) tulostettava = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     else if lookup(nap,"3,f3") > 0 THEN     /* memo */
     DO TRANS WITH FRAME memo ON ENDKEY UNDO, NEXT LOOP:
       assign ehto = 9 cfc = "lis" ufkey = TRUE.
       RUN Syst/ufkey. RUN Syst/ufcolor.
       FIND SMGroup where recid(SMGroup) = rtab[frame-line(sel)]
       exclusive-lock.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSMGroup).
       UPDATE text(SMGroup.Memo [1 FOR 15]) WITH FRAME memo 1 col.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSMGroup).
       /* UNDO WITH F4 -key */
       if keylabel(lastkey) = "F4" THEN DO:
     HIDE FRAME memo no-pause.
     UNDO LOOP.
       END.
       HIDE FRAME memo no-pause.
     END.

     else if lookup(nap,"4,f4") > 0 THEN DO TRANSAction:  /* poisto */
   FIND SMGroup where recid(SMGroup) = rtab[FRAME-LINE] no-lock.
   RUN Mc/nnsgme1(SMGroup.SmGroup).
   ufkey = TRUE.
   NEXT LOOP.
     END.

     if lookup(nap,"5,f5,enter,return") > 0 THEN DO TRANS: /* ADD OR REMOVE */

   FIND SMGroup where recid(SMGroup) = rtab[FRAME-LINE] no-lock.

   FIND SMGMember where 
        SMGMember.Brand    = SMGroup.Brand AND
        SMGMember.SmGroup  = SMGroup.SmGroup AND
        SMGMember.Salesman = Salesman.Salesman
   exclusive-lock no-error.

   IF AVAIL SMGMember THEN DO:
      IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSMGMember).
      DELETE SMGMember.
      DISP FALSE @ memb WITH FRAME sel.
   END.
   ELSE DO:
      CREATE SMGMember.
      ASSIGN
      SMGMember.Brand    = SMGroup.Brand
      SMGMember.SmGroup  = SMGroup.SmGroup
      SMGMember.Salesman = Salesman.Salesman
      SMGMember.SmName   = Salesman.SmName.
      IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSMGMember).
      DISP TRUE @ memb WITH FRAME sel.
   END.
     END.

     else if lookup(nap,"home") > 0 THEN DO:
       IF jarj = 1 THEN FIND FIRST SMGroup
       WHERE SMGroup.Brand = gcBrand no-lock no-error.
       ELSE IF jarj = 2 THEN FIND FIRST SMGroup USE-INDEX SGName
       WHERE SMGroup.Brand = gcBrand no-lock no-error.
       ASSIGN muisti = recid(SMGroup) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
       IF jarj = 1 THEN FIND LAST SMGroup
       WHERE SMGroup.Brand = gcBrand no-lock no-error.
       ELSE IF jarj = 2 THEN FIND LAST SMGroup USE-INDEX SGName
       WHERE SMGroup.Brand = gcBrand no-lock no-error.
       ASSIGN muisti = recid(SMGroup) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* SELAUS */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-disp-row:

   memb = can-find(SMGMember where 
                   SMGMember.Brand    = gcBrand AND
                   SMGMember.SmGroup  = SMGroup.SmGroup AND
                   SMGMember.Salesman = Salesman).

   DISPLAY memb SMGroup.SmGroup SMGroup.SGName WITH FRAME sel.

END PROCEDURE.

