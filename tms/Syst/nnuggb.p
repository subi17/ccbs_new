/* -----------------------------------------------
  MODULI .......: NNUGGP.P
  TEHTÄVÄ ......: Browse AND pick Customer Groups
  SOVELLUS .....: NN
  TEKIJÄ .......: PT
  LUONTIPVM ....: 07-02-99
  MUUTOSPVM ....: 31.10.02 jr Eventlog
  VERSIO .......: SCRUNKO3, (23.10.96)
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 

DEF INPUT PARAMETER UserCode LIKE TMSUser.UserCode NO-UNDO.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR UserGroup  LIKE UserGrp.UserGroup  NO-UNDO.
DEF VAR UGName LIKE UserGrp.UGName NO-UNDO.
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

   DEFINE VARIABLE lhUserGrp AS HANDLE NO-UNDO.
   lhUserGrp = BUFFER UserGrp:HANDLE.
   RUN StarEventInitialize(lhUserGrp).

   DEFINE VARIABLE lhUgMember AS HANDLE NO-UNDO.
   lhUgMember = BUFFER UgMember:HANDLE.
   RUN StarEventInitialize(lhUgMember).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhUserGrp).
   END.
END.


form
    memb                column-label "Member"
    UserGrp.UserGroup      /* COLUMN-LABEL FORMAT */
    UserGrp.UGName      /* COLUMN-LABEL FORMAT */
WITH centered OVERLAY scroll 1 13 DOWN ROW 2 COLOR value(cfc)
    TITLE COLOR value(ctc)
    " Join CustNo " + string(UserCode) + " into group(s) "
    FRAME sel.

form
   UserGrp.Memo
WITH
   centered 1 col no-label overlay row 2 title " Group '" + UserGrp.UserGroup +
   "' " + UserGrp.UGName + "  memo " FRAME memo.

form /* Customer Group :n haku kentällä UserGroup */
    UserGroup
    help "Type Group Code"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* Customer Group :n haku kentällä UGName */
    UGName
    help "Type first characters of a name"
    with row 4 col 2 title color value(ctc) " FIND name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.


FIND TMSUser where TMSUser.UserCode = UserCode no-lock.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST UserGrp
/* hakuehto */ no-lock no-error.
IF AVAILABLE UserGrp THEN ASSIGN
   muisti       = recid(UserGrp)
   tulostettava = TRUE
   lisattava    = FALSE.
ELSE DO:
   bell. message "No customer groups in the PaymFile - press ENTER !".
   PAUSE no-message.
   RETURN.
END.

LOOP:
repeat WITH FRAME sel:

    IF jarj <> ed-jarj THEN DO:
       ed-jarj = jarj.
       if jarj = 1 then put screen row 18 col 35 " By Code ".
       if jarj = 2 then put screen row 18 col 35 " By name ".
    END.

tulostus:
   DO :
      IF tulostettava THEN DO:
   up FRAME-LINE - 1.
   FIND UserGrp where recid(UserGrp) = muisti no-lock no-error.

   /* tulostetaan 1 sivullinen tietoa ruudulle
   alkaen tietueesta, jonka avainarvo = muisti.
   alkaen rivilta privi */

   /* jos on juuri poistettu rivi, niin ... */
   IF privi > 0 THEN DOWN privi - 1.

   repeat WITH FRAME sel:
      IF AVAILABLE UserGrp THEN DO:

         memb = can-find(UGMember where UGMember.UserGroup = UserGrp.UserGroup
           AND UGMember.UserCode = UserCode).
         DISPLAY memb UserGrp.UserGroup UserGrp.UGName.
         rtab[FRAME-LINE] = recid(UserGrp).
         IF jarj = 1 THEN FIND NEXT UserGrp
         /* hakuehto */ no-lock no-error.
         ELSE IF jarj = 2 THEN FIND NEXT UserGrp USE-INDEX UGName
         /* hakuehto */ no-lock no-error.
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
   CHOOSE ROW UserGrp.UserGroup {Syst/uchoose.i} no-error WITH FRAME sel.
   COLOR DISPLAY value(ccc) UserGrp.UserGroup WITH FRAME sel.
      END.
      ELSE IF jarj = 2 THEN DO:
   CHOOSE ROW UserGrp.UGName {Syst/uchoose.i} no-error WITH FRAME sel.
   COLOR DISPLAY value(ccc) UserGrp.UGName WITH FRAME sel.
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
   FIND UserGrp where recid(UserGrp) = muisti.
   DO i = 1 TO FRAME-LINE - 1:
      IF jarj = 1 THEN FIND prev UserGrp
      /* hakuehto */ no-lock no-error.
      ELSE IF jarj = 2 THEN FIND prev UserGrp USE-INDEX UGName
      /* hakuehto */ no-lock no-error.
      IF AVAILABLE UserGrp THEN
         ASSIGN ekarivi = i muisti = recid(UserGrp).
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
      FIND UserGrp where recid(UserGrp) = rtab[1] no-lock.
      IF jarj = 1 THEN FIND prev UserGrp
      /* hakuehto */ no-lock no-error.
      ELSE IF jarj = 2 THEN FIND prev UserGrp USE-INDEX UGName
      /* hakuehto */ no-lock no-error.
      IF NOT AVAILABLE UserGrp THEN DO:
         message "YOU ARE ON THE FIRST ROW !".
         BELL. PAUSE 1 no-message.
         NEXT SELAUS.
      END.
      ELSE DO:
         /* edellinen loytyi */
         scroll DOWN.
         DISPLAY memb UserGrp.UserGroup UserGrp.UGName.
         DO i = FRAME-DOWN TO 2 BY -1:
       rtab[i] = rtab[i - 1].
         END.
         ASSIGN
         rtab[1] = recid(UserGrp)
         muisti = rtab[1].
      END.
   END.
   ELSE up 1.
      END. /* edellinen rivi */

      /* seuraava rivi */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
   IF FRAME-LINE = FRAME-DOWN THEN DO:
      FIND UserGrp where recid(UserGrp) = rtab[FRAME-DOWN] no-lock .
      IF jarj = 1 THEN FIND NEXT UserGrp
      /* hakuehto */ no-lock no-error.
      ELSE IF jarj = 2 THEN FIND NEXT UserGrp USE-INDEX UGName
      /* hakuehto */ no-lock no-error.
      IF NOT AVAILABLE UserGrp THEN DO:
         message "YOU ARE ON THE LAST ROW !".
         BELL. PAUSE 1 no-message.
         NEXT SELAUS.
      END.
      ELSE DO:
         /* loytyi viela seuraava tietue */
         scroll up.
         DISPLAY memb UserGrp.UserGroup UserGrp.UGName.
         DO i = 1 TO FRAME-DOWN - 1:
       rtab[i] = rtab[i + 1].
         END.
         rtab[FRAME-DOWN] = recid(UserGrp).
         /* ja lopuksi pannaan muistiin ylimman rivin avain */
         muisti = rtab[1].
      END.
   END.
   ELSE DOWN 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
   muisti = rtab[1].
   FIND UserGrp where recid(UserGrp) = muisti no-lock no-error.
   IF jarj = 1 THEN FIND prev UserGrp
   /* hakuehto */ no-lock no-error.
   ELSE IF jarj = 2 THEN FIND prev UserGrp USE-INDEX UGName
   /* hakuehto */ no-lock no-error.
   IF AVAILABLE UserGrp THEN DO:
      muisti = recid(UserGrp).

      /* mennään tiedostoa taaksepäin 1 sivun verran */
      DO rivi = 1 TO (FRAME-DOWN - 1):
         IF jarj = 1 THEN FIND prev UserGrp
         /* hakuehto */ no-lock no-error.
         ELSE IF jarj = 2 THEN FIND prev UserGrp USE-INDEX UGName
         /* hakuehto */ no-lock no-error.
         IF AVAILABLE UserGrp THEN muisti = recid(UserGrp).
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
      FIND UserGrp where recid(UserGrp) = muisti no-lock.
      tulostettava = TRUE.
      NEXT LOOP.
       END.
     END. /* seuraava sivu */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       UserGroup = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE UserGroup WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if UserGroup <> "" THEN DO:
     FIND FIRST UserGrp where UserGrp.UserGroup >= UserGroup
     /* hakuehto */ no-lock no-error.
     IF NOT AVAILABLE UserGrp THEN DO:
        BELL.
        message "NOT FOUND !".
        PAUSE 1 no-message.
        NEXT SELAUS.
     END.
     /* joku ugroup/ug-code loytyi */
     ASSIGN jarj = 1 muisti = recid(UserGrp) tulostettava = TRUE.
     NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       UGName = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE UGName WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if UGName <> "" THEN DO:
     FIND FIRST UserGrp where UserGrp.UGName >= UGName
     USE-INDEX UGName /* hakuehto */ no-lock no-error.
     IF NOT AVAILABLE UserGrp THEN DO:
        bell. message "NOT FOUND !".
        PAUSE 1 no-message.
        NEXT SELAUS.
     END.
     /* joku ugroup/ug-name loytyi */
     ASSIGN jarj = 2 muisti = recid(UserGrp) tulostettava = TRUE.
     NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     else if lookup(nap,"3,f3") > 0 THEN     /* memo */
     DO TRANS WITH FRAME memo ON ENDKEY UNDO, NEXT LOOP:
       assign ehto = 9 cfc = "lis" ufkey = TRUE.
       RUN Syst/ufkey.p. RUN Syst/ufcolor.p.
       FIND UserGrp where recid(UserGrp) = rtab[frame-line(sel)]
       exclusive-lock.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhUserGrp).

       UPDATE text(UserGrp.Memo [1 FOR 15]) WITH FRAME memo 1 col.

       /* UNDO WITH F4 -key */
       if keylabel(lastkey) = "F4" THEN DO:
         HIDE FRAME memo no-pause.
         UNDO LOOP.
       END.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhUserGrp).

       HIDE FRAME memo no-pause.
     END.

     else if lookup(nap,"4,f4") > 0 THEN DO TRANSAction:  /* poisto */
   FIND UserGrp where recid(UserGrp) = rtab[FRAME-LINE] no-lock.
   RUN Syst/nnugme1.p(UserGrp.UserGroup).
   ufkey = TRUE.
   NEXT LOOP.
     END.

     if lookup(nap,"5,f5,enter,return") > 0 THEN DO TRANS: /* ADD OR REMOVE */

   FIND UserGrp where recid(UserGrp) = rtab[FRAME-LINE] no-lock.

   FIND UGMember where UGMember.UserGroup = UserGrp.UserGroup AND
             UGMember.UserCode  = TMSUser.UserCode
   exclusive-lock no-error.

   IF AVAIL UGMember THEN DO:
      IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhUgMember).
      DELETE UGMember.
      DISP FALSE @ memb WITH FRAME sel.
   END.
   ELSE DO:
      CREATE UGMember.
      ASSIGN
      UGMember.UserGroup = UserGrp.UserGroup
      UGMember.UserCode  = TMSUser.UserCode
      UGMember.UserName = TMSUser.UserName.
      IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhUgMember).
      DISP TRUE @ memb WITH FRAME sel.
   END.
     END.

     else if lookup(nap,"home") > 0 THEN DO:
       IF jarj = 1 THEN FIND FIRST UserGrp
       /* hakuehto */ no-lock no-error.
       ELSE IF jarj = 2 THEN FIND FIRST UserGrp USE-INDEX UGName
       /* hakuehto */ no-lock no-error.
       ASSIGN muisti = recid(UserGrp) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
       IF jarj = 1 THEN FIND LAST UserGrp
       /* hakuehto */ no-lock no-error.
       ELSE IF jarj = 2 THEN FIND LAST UserGrp USE-INDEX UGName
       /* hakuehto */ no-lock no-error.
       ASSIGN muisti = recid(UserGrp) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* SELAUS */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

