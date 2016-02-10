/* -----------------------------------------------
  MODULI .......: NNCGGP.P
  TEHTÄVÄ ......: Browse AND pick Customer Groups
  SOVELLUS .....: NN
  TEKIJÄ .......: PT
  LUONTIPVM ....: 02-12-98
  MUUTOSPVM ....: 08-11-02 jr Eventlog
                  18.03.03 tk RUN Mc/memo
                  19.03.03 aam run tasks when changes occur (fecgtask)
                  16.09.03 aam brand
                  06.02.04 jp custnum for memo
  VERSIO .......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 
{Func/fecgtask.i}

DEF INPUT PARAMETER CustNum AS i NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR CustGroup  LIKE CustGroup.CustGroup  NO-UNDO.
DEF VAR CGName LIKE CustGroup.CGName NO-UNDO.
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
DEF VAR lcTask       AS CHAR                   NO-UNDO. 

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCGmember AS HANDLE NO-UNDO.
   lhCGMember = BUFFER CGMember:HANDLE.
   RUN StarEventInitialize(lhCGMember).

   DEFINE VARIABLE lhCustGroup AS HANDLE NO-UNDO.
   lhCustGroup = BUFFER CustGroup:HANDLE.
   RUN StarEventInitialize(lhCustGroup).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhCGMember).
   END.
END.

form
    memb                column-label "Member"
    CustGroup.CustGroup      /* COLUMN-LABEL FORMAT */
    CustGroup.CGName      /* COLUMN-LABEL FORMAT */
WITH centered OVERLAY scroll 1 13 DOWN ROW 2 COLOR value(cfc)
    TITLE COLOR value(ctc)
    " Join CustNo " + string(CustNum) + " into group(s) (" + gcBrand + ") "
    FRAME sel.

form
   CustGroup.Memo
WITH
   centered 1 col no-label overlay row 2 title " Group '" + CustGroup.CustGroup +
   "' " + CustGroup.CGName + "  memo " FRAME memo.

form /* Customer Group :n haku kentällä CustGroup */
    CustGroup
    help "Type Group Code"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* Customer Group :n haku kentällä CGName */
    CGName
    help "Type first characters of a name"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.


FIND Customer where Customer.CustNum = CustNum no-lock.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST CustGroup
WHERE CustGroup.Brand = gcBrand no-lock no-error.
IF AVAILABLE CustGroup THEN ASSIGN
   muisti       = recid(CustGroup)
   tulostettava = TRUE
   lisattava    = FALSE.
ELSE DO:
   bell. message "No customer groups in the File - press ENTER !".
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
   FIND CustGroup where recid(CustGroup) = muisti no-lock no-error.

   /* tulostetaan 1 sivullinen tietoa ruudulle
   alkaen tietueesta, jonka avainarvo = muisti.
   alkaen rivilta privi */

   /* jos on juuri poistettu rivi, niin ... */
   IF privi > 0 THEN DOWN privi - 1.

   repeat WITH FRAME sel:
      IF AVAILABLE CustGroup THEN DO:

         memb = can-find(CGMember where CGMember.CustGroup = CustGroup.CustGroup
                         AND CGMember.CustNum = CustNum).

         DISPLAY memb CustGroup.CustGroup CustGroup.CGName.
         rtab[FRAME-LINE] = recid(CustGroup).
         IF jarj = 1 THEN FIND NEXT CustGroup
         WHERE CustGroup.Brand = gcBrand no-lock no-error.
         ELSE IF jarj = 2 THEN FIND NEXT CustGroup USE-INDEX CGName
         WHERE CustGroup.Brand = gcBrand no-lock no-error.
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
   ufk[1]= 35 ufk[2]= 30 ufk[3]= 927 ufk[4]= 510
   ufk[5]= 5  ufk[6]= 0  ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
   ehto = 3 ufkey = FALSE.
   RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
   CHOOSE ROW CustGroup.CustGroup ;(uchoose.i;) no-error WITH FRAME sel.
   COLOR DISPLAY value(ccc) CustGroup.CustGroup WITH FRAME sel.
      END.
      ELSE IF jarj = 2 THEN DO:
   CHOOSE ROW CustGroup.CGName ;(uchoose.i;) no-error WITH FRAME sel.
   COLOR DISPLAY value(ccc) CustGroup.CGName WITH FRAME sel.
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
   FIND CustGroup where recid(CustGroup) = muisti.
   DO i = 1 TO FRAME-LINE - 1:
      IF jarj = 1 THEN FIND prev CustGroup
      WHERE CustGroup.Brand = gcBrand no-lock no-error.
      ELSE IF jarj = 2 THEN FIND prev CustGroup USE-INDEX CGName
      WHERE CustGroup.Brand = gcBrand no-lock no-error.
      IF AVAILABLE CustGroup THEN
         ASSIGN ekarivi = i muisti = recid(CustGroup).
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
      FIND CustGroup where recid(CustGroup) = rtab[1] no-lock.
      IF jarj = 1 THEN FIND prev CustGroup
      WHERE CustGroup.Brand = gcBrand no-lock no-error.
      ELSE IF jarj = 2 THEN FIND prev CustGroup USE-INDEX CGName
      WHERE CustGroup.Brand = gcBrand no-lock no-error.
      IF NOT AVAILABLE CustGroup THEN DO:
         message "YOU ARE ON THE FIRST ROW !".
         BELL. PAUSE 1 no-message.
         NEXT SELAUS.
      END.
      ELSE DO:
         /* edellinen loytyi */
         scroll DOWN.
         DISPLAY memb CustGroup.CustGroup CustGroup.CGName.
         DO i = FRAME-DOWN TO 2 BY -1:
       rtab[i] = rtab[i - 1].
         END.
         ASSIGN
         rtab[1] = recid(CustGroup)
         muisti = rtab[1].
      END.
   END.
   ELSE up 1.
      END. /* edellinen rivi */

      /* seuraava rivi */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
   IF FRAME-LINE = FRAME-DOWN THEN DO:
      FIND CustGroup where recid(CustGroup) = rtab[FRAME-DOWN] no-lock .
      IF jarj = 1 THEN FIND NEXT CustGroup
      WHERE CustGroup.Brand = gcBrand no-lock no-error.
      ELSE IF jarj = 2 THEN FIND NEXT CustGroup USE-INDEX CGName
      WHERE CustGroup.Brand = gcBrand no-lock no-error.
      IF NOT AVAILABLE CustGroup THEN DO:
         message "YOU ARE ON THE LAST ROW !".
         BELL. PAUSE 1 no-message.
         NEXT SELAUS.
      END.
      ELSE DO:
         /* loytyi viela seuraava tietue */
         scroll up.
         DISPLAY memb CustGroup.CustGroup CustGroup.CGName.
         DO i = 1 TO FRAME-DOWN - 1:
       rtab[i] = rtab[i + 1].
         END.
         rtab[FRAME-DOWN] = recid(CustGroup).
         /* ja lopuksi pannaan muistiin ylimman rivin avain */
         muisti = rtab[1].
      END.
   END.
   ELSE DOWN 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
   muisti = rtab[1].
   FIND CustGroup where recid(CustGroup) = muisti no-lock no-error.
   IF jarj = 1 THEN FIND prev CustGroup
   WHERE CustGroup.Brand = gcBrand no-lock no-error.
   ELSE IF jarj = 2 THEN FIND prev CustGroup USE-INDEX CGName
   WHERE CustGroup.Brand = gcBrand no-lock no-error.
   IF AVAILABLE CustGroup THEN DO:
      muisti = recid(CustGroup).

      /* mennään tiedostoa taaksepäin 1 sivun verran */
      DO rivi = 1 TO (FRAME-DOWN - 1):
         IF jarj = 1 THEN FIND prev CustGroup
         WHERE CustGroup.Brand = gcBrand no-lock no-error.
         ELSE IF jarj = 2 THEN FIND prev CustGroup USE-INDEX CGName
         WHERE CustGroup.Brand = gcBrand no-lock no-error.
         IF AVAILABLE CustGroup THEN muisti = recid(CustGroup).
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
      FIND CustGroup where recid(CustGroup) = muisti no-lock.
      tulostettava = TRUE.
      NEXT LOOP.
       END.
     END. /* seuraava sivu */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       CustGroup = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE CustGroup WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if CustGroup <> "" THEN DO:
          FIND FIRST CustGroup where 
                     CustGroup.Brand = gcBrand AND
                     CustGroup.CustGroup >= CustGroup
          no-lock no-error.
          IF NOT AVAILABLE CustGroup THEN DO:
             BELL.
             message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT SELAUS.
          END.

          /* joku cgroup/cg-code loytyi */
          ASSIGN jarj = 1 muisti = recid(CustGroup) tulostettava = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       CGName = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE CGName WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if CGName <> "" THEN DO:
          FIND FIRST CustGroup where 
                     CustGroup.Brand   = gcBrand AND
                     CustGroup.CGName >= CGName
          USE-INDEX CGName no-lock no-error.
          IF NOT AVAILABLE CustGroup THEN DO:
             bell. message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT SELAUS.
          END.

          /* joku cgroup/cg-name loytyi */
          ASSIGN jarj = 2 muisti = recid(CustGroup) tulostettava = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     else if lookup(nap,"3,f3") > 0 THEN DO:

       FIND CustGroup where recid(CustGroup) = rtab[frame-line(sel)]
       no-lock.
       RUN Mc/memo(INPUT 0,
                INPUT "CustGroup",
                INPUT STRING(CustGroup.CustGroup),
                INPUT "Customer group").
       ufkey = TRUE.
       NEXT LOOP.

     END.

     else if lookup(nap,"4,f4") > 0 THEN DO TRANSAction:  /* poisto */
   FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] no-lock.
   RUN Mc/nncgme1(CustGroup.CustGroup).
   ufkey = TRUE.
   NEXT LOOP.
     END.

     if lookup(nap,"5,f5,enter,return") > 0 THEN DO TRANS: /* ADD OR REMOVE */

        FIND CustGroup where recid(CustGroup) = rtab[FRAME-LINE] no-lock.

        FIND CGMember where CGMember.CustGroup = CustGroup.CustGroup AND
             CGMember.CustNum  = Customer.CustNum
        exclusive-lock no-error.

        IF AVAIL CGMember THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCGMember).

           lcTask = fECGLeaveTask(TRUE).

           DELETE CGMember.
           DISP FALSE @ memb WITH FRAME sel.
        END.
        ELSE DO:
           CREATE CGMember.
           ASSIGN
           CGMember.Brand     = CustGroup.Brand
           CGMember.CustGroup = CustGroup.CustGroup
           CGMember.CustNum  = Customer.CustNum
           CGMember.CustName = Customer.CustName.
           lcTask            = fECGEnterTask(TRUE).

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCGMember).
           DISP TRUE @ memb WITH FRAME sel.
        END.
     END.

     else if lookup(nap,"home") > 0 THEN DO:
       IF jarj = 1 THEN FIND FIRST CustGroup
       WHERE CustGroup.Brand = gcBrand no-lock no-error.
       ELSE IF jarj = 2 THEN FIND FIRST CustGroup USE-INDEX CGName
       WHERE CustGroup.Brand = gcBrand no-lock no-error.
       ASSIGN muisti = recid(CustGroup) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
       IF jarj = 1 THEN FIND LAST CustGroup
       WHERE CustGroup.Brand = gcBrand no-lock no-error.
       ELSE IF jarj = 2 THEN FIND LAST CustGroup USE-INDEX CGName
       WHERE CustGroup.Brand = gcBrand no-lock no-error.
       ASSIGN muisti = recid(CustGroup) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* SELAUS */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

