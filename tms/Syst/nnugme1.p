/* -----------------------------------------------------------------
   MODULI .......: NNUGME1.P
   TEHTÄVÄ ......: Members in a User group
   SOVELLUS .....: nn
   TEKIJÄ .......: pt
   LUONTIPVM ....: 08.02.99
   MUUTOSPVM ....: 29.12.98
                   31.10.02 jr Eventlog
   VERSIO .......: SCRUNKO3, (23.10.96)
   -------------------------------------------------------------- */
{Syst/eventval.i} 
{Syst/commali.i}

DEF INPUT PARAMETER UserGroup LIKE UserGrp.UserGroup NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.
DEF BUFFER xUserGrp FOR UserGrp.
DEF BUFFER cmember FOR UGMember.

DEF VAR UserCode   LIKE TMSUser.UserCode  NO-UNDO.
DEF VAR UserName   LIKE UGMember.UserName NO-UNDO.
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
DEF VAR Qty          AS INT                    NO-UNDO.
def var ok           as log format "Yes/No"    NO-UNDO.
DEF VAR xug-code      AS c                      NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhUgMember AS HANDLE NO-UNDO.
   lhUgMember = BUFFER UgMember:HANDLE.
   RUN StarEventInitialize(lhUgMember).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhUgMember).
   END.
END.

form
    UGMember.UserCode      /* COLUMN-LABEL FORMAT */
    UGMember.UserName     /* COLUMN-LABEL FORMAT */
    UGMember.Memo     /* COLUMN-LABEL FORMAT */
WITH centered OVERLAY scroll 1 13 DOWN ROW 2
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    " Members in an User Group " + UserGroup + ": " +
    substring(UserGrp.UGName,1,16)
    + " " FRAME sel.

form
    UGMember.UserCode     /* LABEL FORMAT */
    TMSUser.UserName     /* LABEL FORMAT */
WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    lm-ots WITH side-labels 1 columns
    FRAME lis.

form /* member :n haku kentällä UserCode */
    UserCode
    help "Type User ID "
    with row 4 col 2 title color value(ctc) " FIND USER ID "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* member :n haku kentällä UserName */
    UserName
    help "Type User's Name"
    with row 4 col 2 title color value(ctc) " FIND UserName "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

form
   skip(1)
"  Note:  All members (that are NOT already members in this group) "
"         from group" xug-code
help "Enter Code of the Group You want to copy" xUserGrp.UGName
"         are copied also into this gïUser group"
skip(1)
WITH
   centered row 5 overlay no-labels title " Copy members into group " +
   UserGrp.UserGroup + " " FRAME copy .


FIND UserGrp where UserGrp.UserGroup = UserGroup no-lock.


cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST UGMember
where UGMember.UserGroup = UserGroup no-lock no-error.

IF AVAILABLE UGMember THEN ASSIGN
   muisti       = recid(UGMember)
   tulostettava = TRUE
   lisattava    = FALSE.
ELSE ASSIGN
   muisti       = ?
   tulostettava = FALSE
   lisattava    = TRUE.

LOOP:
repeat WITH FRAME sel:

    IF jarj <> ed-jarj THEN DO:
       ed-jarj = jarj.
       if jarj = 1 then put screen row 18 col 30 " By UserNo.  ".
       if jarj = 2 then put screen row 18 col 30 " By UserName ".
    END.


    IF lisattava THEN DO:
       lisattava = FALSE. ufkey = TRUE.

ADD-USER:
       repeat TRANS ON ENDKEY UNDO ADD-USER, LEAVE ADD-USER.
     ASSIGN ufkey = TRUE ufk = 0 ehto = 0
     ufk[1] = 540 ufk[2] = 541 ufk[3] = 516 ufk[8] = 8.
     RUN ufkey.

     IF toimi = 8 THEN LEAVE ADD-USER.
     IF toimi = 1 THEN DO:
        lm-ots = " ADD ONE User ".
add-single:
        repeat WITH FRAME lis ON ENDKEY UNDO ADD-USER,
                NEXT ADD-USER:
      PAUSE 0.
      ehto = 9. RUN ufkey.
      CLEAR FRAME lis no-pause.
      PROMPT-FOR UGMember.UserCode
      validate(input UGMember.UserCode = "" OR can-find(TMSUser where
      TMSUser.UserCode = input UGMember.UserCode),"Unknown User !").
      if input UGMember.UserCode = "" THEN DO:
         HIDE FRAME lis.
         UNDO, NEXT LOOP.
      END.

      FIND TMSUser where TMSUser.UserCode = INPUT UGMember.UserCode no-lock.
      DISP TMSUser.UserName.

      /* is this User already a member in this group ? */
      IF can-find(UGMember where UGMember.UserCode  = TMSUser.UserCode AND
                  UGMember.UserGroup = UserGroup)
      THEN DO:
          BELL.
          message "This User is already a member in this group !".
          message "Press ENTER !".
          PAUSE no-message.
          NEXT add-single.
      END.
      ELSE DO:
         CREATE UGMember.
         ASSIGN
         muisti              = recid(UGMember)
         tulostettava        = TRUE
         UGMember.UserGroup  = UserGroup
         UGMember.UserCode   = TMSUser.UserCode
         UGMember.UserName   = TMSUser.UserName.
         UPDATE UGMember.Memo.
         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhUgMember).
      END.
        END.
     END. /* toimi = 1: add a single group */

     ELSE IF toimi = 2 THEN DO:
        RUN nnugcb(UserGrp.UserGroup).
        LEAVE add-USER.
     END.
     ELSE IF toimi = 3 THEN DO WITH FRAME copy:
        /* copy members */

        PAUSE 0.
        ufkey = true. xug-code = "".
        update xug-code validate(input xug-code = "" OR can-find(UserGrp where
          UserGrp.UserGroup = input xug-code),"Unknown group !").

        if xug-code ne "" THEN DO:
      FIND xUserGrp where xUserGrp.UserGroup = xug-code no-lock.
      DISP xUserGrp.UGName.

      ok = FALSE.
      message "Are You SURE that You want to copy members (Y/N) ? "
      UPDATE ok.
      IF ok THEN DO:
         message "Copying members into group " UserGroup "...".
         i = 0.
         FOR EACH cmember no-lock where
             cmember.UserGroup = xug-code:

             FIND UGMember where
             UGMember.UserGroup = UserGroup  AND
             UGMember.UserCode  = cmember.UserCode
             no-lock no-error.

             IF NOT AVAIL UGMember THEN DO:
                CREATE UGMember.
                ASSIGN
                UGMember.UserCode  = cmember.UserCode
                UGMember.UserGroup = UserGroup
                UGMember.UserName = cmember.UserName
                i                = i + 1 .
                IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhUgMember).
             END.
         END.
         message "Totally" i "members were copied - press ENTER !".
         PAUSE no-message.
      END.
        END.
        PAUSE 0.
        HIDE FRAME copy.
        LEAVE add-USER.
     END. /* toimi = 3 */
      END. /* add-USER */

      /* onko yhtään tietuetta ? */
      FIND FIRST UGMember where UGMember.UserGroup = UserGroup no-lock no-error.
      IF NOT AVAILABLE UGMember THEN LEAVE LOOP.

      muisti = recid(UGMember).
      tulostettava = TRUE.
      NEXT LOOP.
   END.

tulostus:
   DO :
      IF tulostettava THEN DO:
   up FRAME-LINE - 1.
   FIND UGMember where recid(UGMember) = muisti no-lock no-error.

   /* tulostetaan 1 sivullinen tietoa ruudulle
   alkaen tietueesta, jonka avainarvo = muisti.
   alkaen rivilta privi */

   /* jos on juuri poistettu rivi, niin ... */
   IF privi > 0 THEN DOWN privi - 1.

   repeat WITH FRAME sel:
      IF AVAILABLE UGMember THEN DO:
         FIND TMSUser where TMSUser.UserCode = UGMember.UserCode
         no-lock no-error. 
         DISPLAY 
         UGMember.UserCode UGMember.UserName UGMember.Memo.
         rtab[FRAME-LINE] = recid(UGMember).
         IF jarj = 1 THEN FIND NEXT UGMember
         where UGMember.UserGroup = UserGroup no-lock no-error.
         ELSE IF jarj = 2 THEN FIND NEXT UGMember USE-INDEX UserName
         where UGMember.UserGroup = UserGroup no-lock no-error.
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
   ufk[1]= 542 ufk[2]= 30 ufk[3]= 530 ufk[4]= 518
   ufk[5]= 5   ufk[6]= 4  ufk[7]=   0 ufk[8]=   8 ufk[9]= 1
   ehto = 3 ufkey = FALSE.
   RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
   CHOOSE ROW UGMember.UserCode ;(uchoose.i;) no-error WITH FRAME sel.
   COLOR DISPLAY value(ccc) UGMember.UserCode WITH FRAME sel.
      END.
      ELSE IF jarj = 2 THEN DO:
   CHOOSE ROW UGMember.UserName ;(uchoose.i;) no-error WITH FRAME sel.
   COLOR DISPLAY value(ccc) UGMember.UserName WITH FRAME sel.
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
   FIND UGMember where recid(UGMember) = muisti.
   DO i = 1 TO FRAME-LINE - 1:
      IF jarj = 1 THEN FIND prev UGMember
      where UGMember.UserGroup = UserGroup no-lock no-error.
      ELSE IF jarj = 2 THEN FIND prev UGMember USE-INDEX UserName
      where UGMember.UserGroup = UserGroup no-lock no-error.
      IF AVAILABLE UGMember THEN
         ASSIGN ekarivi = i muisti = recid(UGMember).
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
      FIND UGMember where recid(UGMember) = rtab[1] no-lock.
      IF jarj = 1 THEN FIND prev UGMember
      where UGMember.UserGroup = UserGroup no-lock no-error.
      ELSE IF jarj = 2 THEN FIND prev UGMember USE-INDEX UserName
      where UGMember.UserGroup = UserGroup no-lock no-error.
      IF NOT AVAILABLE UGMember THEN DO:
         message "YOU ARE ON THE FIRST ROW !".
         BELL. PAUSE 1 no-message.
         NEXT SELAUS.
      END.
      ELSE DO:
         /* edellinen loytyi */
         scroll DOWN.
         FIND TMSUser where TMSUser.UserCode = UGMember.UserCode 
         no-lock no-error.
         DISPLAY
         UGMember.UserCode UGMember.UserName UGMember.Memo.
         DO i = FRAME-DOWN TO 2 BY -1:
       rtab[i] = rtab[i - 1].
         END.
         ASSIGN
         rtab[1] = recid(UGMember)
         muisti = rtab[1].
      END.
   END.
   ELSE up 1.
      END. /* edellinen rivi */

      /* seuraava rivi */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
   IF FRAME-LINE = FRAME-DOWN THEN DO:
      FIND UGMember where recid(UGMember) = rtab[FRAME-DOWN] no-lock .
      IF jarj = 1 THEN FIND NEXT UGMember
      where UGMember.UserGroup = UserGroup no-lock no-error.
      ELSE IF jarj = 2 THEN FIND NEXT UGMember USE-INDEX UserName
      where UGMember.UserGroup = UserGroup no-lock no-error.
      IF NOT AVAILABLE UGMember THEN DO:
         message "YOU ARE ON THE LAST ROW !".
         BELL. PAUSE 1 no-message.
         NEXT SELAUS.
      END.
      ELSE DO:
         /* loytyi viela seuraava tietue */
         scroll up.
         FIND TMSUser where TMSUser.UserCode = UGMember.UserCode 
         no-lock no-error.
         DISPLAY UGMember.UserCode UGMember.UserName UGMember.Memo.
         DO i = 1 TO FRAME-DOWN - 1:
       rtab[i] = rtab[i + 1].
         END.
         rtab[FRAME-DOWN] = recid(UGMember).
         /* ja lopuksi pannaan muistiin ylimman rivin avain */
         muisti = rtab[1].
      END.
   END.
   ELSE DOWN 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
   muisti = rtab[1].
   FIND UGMember where recid(UGMember) = muisti no-lock no-error.
   IF jarj = 1 THEN FIND prev UGMember
   where UGMember.UserGroup = UserGroup no-lock no-error.
   ELSE IF jarj = 2 THEN FIND prev UGMember USE-INDEX UserName
   where UGMember.UserGroup = UserGroup no-lock no-error.
   IF AVAILABLE UGMember THEN DO:
      muisti = recid(UGMember).

      /* mennään tiedostoa taaksepäin 1 sivun verran */
      DO rivi = 1 TO (FRAME-DOWN - 1):
         IF jarj = 1 THEN FIND prev UGMember
         where UGMember.UserGroup = UserGroup no-lock no-error.
         ELSE IF jarj = 2 THEN FIND prev UGMember USE-INDEX UserName
         where UGMember.UserGroup = UserGroup no-lock no-error.
         IF AVAILABLE UGMember THEN muisti = recid(UGMember).
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
      FIND UGMember where recid(UGMember) = muisti no-lock.
      tulostettava = TRUE.
      NEXT LOOP.
       END.
     END. /* seuraava sivu */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       UserCode = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE UserCode WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if UserCode <> "" THEN DO:
     FIND FIRST UGMember where
           UGMember.UserCode >= UserCode AND
           UGMember.UserGroup = UserGroup no-lock no-error.
     IF NOT AVAILABLE UGMember THEN DO:
        BELL.
        message "NOT FOUND !".
        PAUSE 1 no-message.
        NEXT SELAUS.
     END.
     /* joku ugmember/us-id loytyi */
     ASSIGN jarj = 1 muisti = recid(UGMember) tulostettava = TRUE.
     NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       UserName = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE UserName WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if UserName <> "" THEN DO:
     FIND FIRST UGMember USE-INDEX UserName where
           UGMember.UserName >= UserName  AND
           UGMember.UserGroup = UserGroup
     no-lock no-error.
     IF NOT AVAILABLE UGMember THEN DO:
        bell. message "NOT FOUND !".
        PAUSE 1 no-message.
        NEXT SELAUS.
     END.
     /* joku ugmember/us-name loytyi */
     ASSIGN jarj = 2 muisti = recid(UGMember) tulostettava = TRUE.
     NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     else if lookup(nap,"4,f4") > 0 THEN DO:  /* other memberships */
   FIND UGMember where recid(UGMember) = rtab[FRAME-LINE] no-lock.
   RUN nnugme2(UGMember.UserCode).
   ufkey = TRUE.
   NEXT LOOP.
     END.

     else if lookup(nap,"5,f5") > 0 THEN DO:  /* ADD */
   lisattava = TRUE.
   NEXT LOOP.
     END.

     else if lookup(nap,"3,f3") > 0 THEN DO:  /* count # of members */
   PAUSE 0.
   message "Calculating no. of members, wait a moment please ...".
   Qty = 0.
   FOR EACH UGMember no-lock where
       UGMember.UserGroup = UserGroup.
      Qty = Qty + 1.
   END.
   PAUSE 0.
   message "There are totally" Qty
   "members in this User group - press ENTER !".
   PAUSE no-message.
   NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 THEN DO TRANSAction:  /* poisto */
       privi = FRAME-LINE.
       FIND UGMember where recid(UGMember) = rtab[FRAME-LINE] no-lock.

       /* valaistaan poistettava rivi */
       COLOR DISPLAY value(ctc)
       UGMember.UserCode UGMember.UserName UGMember.Memo.

       IF jarj = 1 THEN FIND NEXT UGMember
       where UGMember.UserGroup = UserGroup no-lock no-error.
       ELSE IF jarj = 2 THEN FIND NEXT UGMember USE-INDEX UserName
       where UGMember.UserGroup = UserGroup no-lock no-error.
       IF AVAILABLE UGMember THEN muisti = recid(UGMember).
       ELSE DO:
     /* luetaan takaisin poistettava */
     FIND UGMember where recid(UGMember) = rtab[FRAME-LINE] no-lock.
     /* sitten edellinen */
     IF jarj = 1 THEN FIND prev UGMember
     where UGMember.UserGroup = UserGroup no-lock no-error.
     ELSE IF jarj = 2 THEN FIND prev UGMember USE-INDEX UserName
     where UGMember.UserGroup = UserGroup no-lock no-error.
     IF AVAILABLE UGMember THEN DO:
        ASSIGN
        privi = privi - 1  /* koska poistetaan viimeinen */
        muisti = recid(UGMember).
     END.
       END.

       /* FIND takaisin poistettavaan riviin */
       FIND UGMember where recid(UGMember) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
       UGMember.UserCode UGMember.UserName UGMember.Memo.
       IF ok THEN DO:

      IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhUgMember).
      DELETE UGMember.

      /* poistettiinko viimeinen tietue ? */
      IF NOT can-find(FIRST UGMember
      where UGMember.UserGroup = UserGroup) THEN DO:
         CLEAR FRAME sel no-pause.
         PAUSE 0 no-message.
         LEAVE LOOP.
      END.
      tulostettava = TRUE.
      NEXT LOOP.
       END.
       ELSE privi = 0. /* ei poistettukaan */
     END. /* poisto */

     else if lookup(nap,"enter,return") > 0 THEN DO WITH FRAME sel TRANSAction:
       /* muutos */
       FIND UGMember where recid(UGMember) = rtab[frame-line(sel)]
       exclusive-lock.
       assign lm-ots = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhUgMember).
       UPDATE UGMember.Memo.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhUgMember).  
       xrecid = recid(UGMember).
     END.

     else if lookup(nap,"home") > 0 THEN DO:
       IF jarj = 1 THEN FIND FIRST UGMember
       where UGMember.UserGroup = UserGroup no-lock no-error.
       ELSE IF jarj = 2 THEN FIND FIRST UGMember USE-INDEX UserName
       where UGMember.UserGroup = UserGroup no-lock no-error.
       ASSIGN muisti = recid(UGMember) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
       IF jarj = 1 THEN FIND LAST UGMember
       where UGMember.UserGroup = UserGroup no-lock no-error.
       ELSE IF jarj = 2 THEN FIND LAST UGMember USE-INDEX UserName
       where UGMember.UserGroup = UserGroup no-lock no-error.
       ASSIGN muisti = recid(UGMember) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* SELAUS */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

