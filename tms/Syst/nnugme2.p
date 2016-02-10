/* --------------------------------------------------------
  MODULI .......: NNUGME2.P
  TEHTÄVÄ ......: Memberships of a single user
  SOVELLUS .....: nn
  TEKIJÄ .......: pt
  LUONTIPVM ....: 12-12-98
  MUUTOSPVM ....: 31.10.02 jr Eventlog
  VERSIO .......: SCRUNKO3, (23.10.96)
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 

DEF INPUT PARAMETER UserCode LIKE TMSUser.UserCode NO-UNDO.

DEF BUFFER xTMSUser FOR TMSUser.
DEF BUFFER xmember FOR UGMember.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR UserGroup LIKE UGMember.UserGroup NO-UNDO.
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
def var ok           as log format "Yes/No"  NO-UNDO.
DEF VAR xUserCode      LIKE TMSUser.UserCode       NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhUgMember AS HANDLE NO-UNDO.
   lhUgMember = BUFFER Ugmember:HANDLE.
   RUN StarEventInitialize(lhUgMember).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhUgMember).
   END.
END.


form
    UGMember.UserGroup     /* COLUMN-LABEL FORMAT */
    UserGrp.UGName     /* COLUMN-LABEL FORMAT */
    UGMember.Memo     /* COLUMN-LABEL FORMAT */
WITH centered OVERLAY scroll 1 13 DOWN ROW 2
    COLOR value(cfc) TITLE COLOR value(ctc)
    " Memberships of UserID " + string(UserCode) + ": " +
    substring(TMSUser.UserName,1,16)
    + " " FRAME sel.

form
    UGMember.UserGroup    label "GroupCode"
       help "Enter now a user Group Code"
    UserGrp.UGName NO-LABEL                        SKIP
    UGMember.Memo    label "Info ...."
WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    title color value(ctc) " ADD ONE GROUP "
    WITH side-labels
    FRAME lis.

form
    UserGrp.Memo
WITH
    no-label 1 col overlay row 2 centered title " memo OF AN USER GROUP " +
    UserGrp.UserGroup + ": " + UserGrp.UGName + " " FRAME memo.

form /* member :n haku kentällä UserGroup */
    UserGroup
    help "Type Group Code "
with row 4 col 2 title color value(ctc) " FIND GROUP CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.


form
   skip(1)
"  Note:  All memberships (in groups where this user is NOT a member yet) "
"         from user" xUserCode
help "Enter user number whose memberships You want to copy"
xTMSUser.UserName
"         are copied also onto this user"
skip(1)
WITH
   centered ROW 5 OVERLAY NO-LABELS TITLE
   " Copy memberships from another user "
    FRAME copy .

FIND TMSUser where TMSUser.UserCode = UserCode no-lock.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST UGMember
where UGMember.UserCode = UserCode no-lock no-error.
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
    END.

    IF lisattava THEN DO:
       lisattava = FALSE. ufkey = TRUE.

add-group:
       repeat TRANS ON ENDKEY UNDO add-group, LEAVE add-group.
          ASSIGN ufkey = TRUE ufk = 0 ehto = 0
          ufk[1] = 512 ufk[2] = 514 ufk[3] = 517
          ufk[8] = 8.
          RUN Syst/ufkey.

          IF toimi = 8 THEN LEAVE add-group.
          IF toimi = 1 THEN
add-single:
          repeat WITH FRAME lis ON ENDKEY UNDO add-group,
                            NEXT add-group:
             PAUSE 0.
             ehto = 9. RUN Syst/ufkey.
             CLEAR FRAME lis no-pause.
             PROMPT-FOR UGMember.UserGroup
             validate(input frame lis UGMember.UserGroup = "" OR
             can-find(UserGrp where
                      UserGrp.UserGroup = INPUT FRAME lis UGMember.UserGroup),
                      "Unknown Group !").
             .
             if input UGMember.UserGroup = "" THEN DO:
                HIDE FRAME lis.
                UNDO, LEAVE add-group.
             END.

             FIND UserGrp where UserGrp.UserGroup = INPUT UGMember.UserGroup
             no-lock.
             DISP UserGrp.UGName.

             /* is this user already a member in this group ? */
             IF can-find(UGMember where UGMember.UserCode  = UserCode AND
                                        UGMember.UserGroup = UserGrp.UserGroup)
             THEN DO:
                 BELL.
                 message "This user is already a member in this group !".
                 message "Press ENTER !".
                 PAUSE no-message.
                 NEXT add-single.
             END.
             ELSE DO:
                CREATE UGMember.
                ASSIGN
                muisti = recid(UGMember)
                tulostettava = TRUE
                INPUT FRAME lis UGMember.UserGroup
                UGMember.UserCode = UserCode
                UGMember.UserName = TMSUser.UserName.
                UPDATE UGMember.Memo.

                IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhUgMember).
             END.
          END. /* toimi = 1: add a single group */

          ELSE IF toimi = 2 THEN DO:
             RUN Syst/nnuggb(TMSUser.UserCode).
             LEAVE add-group.
          END.

          ELSE IF toimi = 3 THEN DO WITH FRAME copy:
             /* copy memberships from one user TO another user */

             PAUSE 0.
             ufkey = true. xUserCode = "".
             UPDATE xUserCode
                    validate(input xUserCode = "" OR can-find(TMSUser where
                    TMSUser.UserCode = input xUserCode),"Unknown user !").

             if xUserCode ne "" THEN DO:
                FIND xTMSUser where xTMSUser.UserCode = xUserCode no-lock.
                DISP xTMSUser.UserName.

                ok = FALSE.
                MESSAGE
                "Are You SURE that You want to copy memberships (Y/N) ? "
                UPDATE ok.
                IF ok THEN DO:
                   MESSAGE
                   "Copying memberships from UserID. " xUserCode
                   "to UserID." UserCode.

                   i = 0.
                   FOR EACH xmember no-lock where
                            xmember.UserCode  = xUserCode:

                       FIND UGMember where
                            UGMember.UserGroup = xmember.UserGroup AND
                            UGMember.UserCode  = TMSUser.UserCode
                       no-lock no-error.

                       IF NOT AVAIL UGMember THEN DO:
                          CREATE UGMember.
                          ASSIGN
                          UGMember.UserCode  = TMSUser.UserCode
                          UGMember.UserGroup = xmember.UserGroup
                          UGMember.UserName = TMSUser.UserName
                          i                = i + 1.
                          IF llDoEvent 
                          THEN RUN StarEventMakeCreateEvent(lhUgMember).
                       END.
                    END.

                    MESSAGE
                    "Totally" i "memberships were copied - press ENTER !".
                    PAUSE no-message.
                 END.
                 HIDE FRAME copy.
              END.
              LEAVE add-group.
          END.
       END. /* add-group */

       FIND FIRST UGMember where UGMember.UserCode = UserCode no-lock no-error.
       IF NOT AVAIL UGMember THEN LEAVE LOOP.

       ASSIGN
       tulostettava = TRUE
       muisti = recid(UGMember).
       NEXT LOOP.

    END.

tulostus:
   DO :
      IF tulostettava THEN DO:
        up FRAME-LINE - 1.
        FIND UGMember where recid(UGMember) = muisti no-lock no-error.

        /* tulostetaan 1 sivullinen tietoa ruudulle
        alkaen tietueesta, jonka avainarvo = muisti, alkaen rivilta privi */

        /* jos on juuri poistettu rivi, niin ... */
        IF privi > 0 THEN DOWN privi - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE UGMember THEN DO:
              FIND UserGrp of UGMember no-lock.
              DISPLAY UGMember.UserGroup UserGrp.UGName UGMember.Memo.
              rtab[FRAME-LINE] = recid(UGMember).
              IF jarj = 1 THEN FIND NEXT UGMember
              where UGMember.UserCode = UserCode no-lock no-error.
              ELSE IF jarj = 2 THEN FIND NEXT UGMember USE-INDEX UserName
              where UGMember.UserCode = UserCode no-lock no-error.
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
        ufk[1]= 35 ufk[2]= 0 ufk[3]= 0 ufk[4]= 519
        ufk[5]= 5  ufk[6]= 4 ufk[7] = 528 ufk[8] = 8  ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
        CHOOSE ROW UGMember.UserGroup ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) UGMember.UserGroup WITH FRAME sel.
      END.
      ELSE IF jarj = 2 THEN DO:
        CHOOSE ROW UserGrp.UGName ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) UserGrp.UGName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).
      IF jarj <> ed-jarj THEN DO:
        ASSIGN ekarivi = 0 muisti = rtab[FRAME-LINE].
        FIND UGMember where recid(UGMember) = muisti.
        DO i = 1 TO FRAME-LINE - 1:
           IF jarj = 1 THEN FIND prev UGMember
           where UGMember.UserCode = UserCode no-lock no-error.
           ELSE IF jarj = 2 THEN FIND prev UGMember USE-INDEX UserName
           where UGMember.UserCode = UserCode no-lock no-error.
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
           where UGMember.UserCode = UserCode no-lock no-error.
           ELSE IF jarj = 2 THEN FIND prev UGMember USE-INDEX UserName
           where UGMember.UserCode = UserCode no-lock no-error.
           IF NOT AVAILABLE UGMember THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* edellinen loytyi */
              scroll DOWN.
              FIND UserGrp of UGMember no-lock.
              DISPLAY UGMember.UserGroup UserGrp.UGName UGMember.Memo
                      /* sd */.
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
           where UGMember.UserCode = UserCode no-lock no-error.
           ELSE IF jarj = 2 THEN FIND NEXT UGMember USE-INDEX UserName
           where UGMember.UserCode = UserCode no-lock no-error.
           IF NOT AVAILABLE UGMember THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* loytyi viela seuraava tietue */
              scroll up.
              FIND UserGrp of UGMember no-lock.
              DISPLAY UGMember.UserGroup UserGrp.UGName UGMember.Memo
                      /* sd */.
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
        where UGMember.UserCode = UserCode no-lock no-error.
        ELSE IF jarj = 2 THEN FIND prev UGMember USE-INDEX UserName
        where UGMember.UserCode = UserCode no-lock no-error.
        IF AVAILABLE UGMember THEN DO:
           muisti = recid(UGMember).

           /* mennään tiedostoa taaksepäin 1 sivun verran */
           DO rivi = 1 TO (FRAME-DOWN - 1):
              IF jarj = 1 THEN FIND prev UGMember
              where UGMember.UserCode = UserCode no-lock no-error.
              ELSE IF jarj = 2 THEN FIND prev UGMember USE-INDEX UserName
              where UGMember.UserCode = UserCode no-lock no-error.
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
       cfc = "puyr". RUN Syst/ufcolor.
       UserGroup = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE UserGroup WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if UserGroup <> "" THEN DO:
          FIND FIRST UGMember where
                     UGMember.UserGroup >= UserGroup AND
                     UGMember.UserCode = UserCode no-lock no-error.
          IF NOT AVAILABLE UGMember THEN DO:
             BELL.
             message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT SELAUS.
          END.
          /* joku ugmember/ug-code loytyi */
          ASSIGN jarj = 1 muisti = recid(UGMember) tulostettava = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */


     else if lookup(nap,"4,f4") > 0 THEN DO:  /* other members */
        FIND UGMember where recid(UGMember) = rtab[FRAME-LINE] no-lock.
        RUN Syst/nnugme1(UGMember.UserGroup).
        ufkey = TRUE.
        NEXT LOOP.
     END.

     if lookup(nap,"5,f5") > 0 THEN DO:  /* lisays */
        lisattava = TRUE.
        NEXT LOOP.
     END.


     else if lookup(nap,"6,f6") > 0 THEN DO TRANSAction:  /* poisto */
       privi = FRAME-LINE.
       FIND UGMember where recid(UGMember) = rtab[FRAME-LINE] no-lock.

       /* valaistaan poistettava rivi */
       COLOR DISPLAY value(ctc)
       UGMember.UserGroup UserGrp.UGName UGMember.Memo.

       IF jarj = 1 THEN FIND NEXT UGMember
       where UGMember.UserCode = UserCode no-lock no-error.
       ELSE IF jarj = 2 THEN FIND NEXT UGMember USE-INDEX UserName
       where UGMember.UserCode = UserCode no-lock no-error.
       IF AVAILABLE UGMember THEN muisti = recid(UGMember).
       ELSE DO:
          /* luetaan takaisin poistettava */
          FIND UGMember where recid(UGMember) = rtab[FRAME-LINE] no-lock.
          /* sitten edellinen */
          IF jarj = 1 THEN FIND prev UGMember
          where UGMember.UserCode = UserCode no-lock no-error.
          ELSE IF jarj = 2 THEN FIND prev UGMember USE-INDEX UserName
          where UGMember.UserCode = UserCode no-lock no-error.
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
       UGMember.UserGroup UserGrp.UGName UGMember.Memo /* sd */.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhUgMember).
           DELETE UGMember.

           /* poistettiinko viimeinen tietue ? */
           IF NOT can-find(FIRST UGMember
           where UGMember.UserCode = UserCode) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           tulostettava = TRUE.
           NEXT LOOP.
       END.
       ELSE privi = 0. /* ei poistettukaan */
     END. /* poisto */

     else if lookup(nap,"7,f7") > 0 THEN DO:  /* other members */
        FIND UGMember where recid(UGMember) = rtab[FRAME-LINE] no-lock.
        FIND UserGrp of UGMember no-lock.
        PAUSE 0.
        DISP UserGrp.Memo WITH FRAME memo.
        ASSIGN ufk = 0 ufk[8] = 8 ehto = 0 ufkey = TRUE.  RUN Syst/ufkey.
        HIDE FRAME memo.
        NEXT LOOP.
     END.

     else if lookup(nap,"enter,return") > 0 THEN DO WITH FRAME sel TRANSAction:
       /* muutos */
       FIND UGMember where recid(UGMember) = rtab[frame-line(sel)]
       exclusive-lock.
       assign lm-ots = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhUgMember).
       UPDATE UGMember.Memo.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhUgMember).
       xrecid = recid(UGMember).
     END.

     else if lookup(nap,"home") > 0 THEN DO:
       IF jarj = 1 THEN FIND FIRST UGMember
       where UGMember.UserCode = UserCode no-lock no-error.
       ELSE IF jarj = 2 THEN FIND FIRST UGMember USE-INDEX UserName
       where UGMember.UserCode = UserCode no-lock no-error.
       ASSIGN muisti = recid(UGMember) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
       IF jarj = 1 THEN FIND LAST UGMember
       where UGMember.UserCode = UserCode no-lock no-error.
       ELSE IF jarj = 2 THEN FIND LAST UGMember USE-INDEX UserName
       where UGMember.UserCode = UserCode no-lock no-error.
       ASSIGN muisti = recid(UGMember) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* SELAUS */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

