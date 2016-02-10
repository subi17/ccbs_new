/*----- -----------------------------------------------
  MODULI .......: NNUGRO.P
  TEHTÄVÄ ......: User Groups
  SOVELLUS .....: NN
  TEKIJÄ .......: PT
  LUONTIPVM ....: 02-12-98
  MUUTOSPVM ....: 25-05-99 jp uright1 & uright2 added
                  31.10.02 jr Eventlog 
                  20.02.03 tk F7 tokens, ugmembers removed
                  05.03.03 tk RUN Mc/memo, check tokens
                  06.02.04 jp custnum for memo
  VERSIO .......: SCRUNKO3, (23.10.96)
  ------------------------------------------------------ */

{Syst/commali.i}                           
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'usergrp'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR UserGroup  LIKE UserGrp.UserGroup  NO-UNDO.
DEF VAR xg-code  LIKE UserGrp.UserGroup  NO-UNDO.
DEF VAR UGName  LIKE UserGrp.UGName NO-UNDO.
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
DEF VAR lShoTokens   AS CHAR                   NO-UNDO.
DEF VAR lModTokens   AS CHAR                   NO-UNDO.


IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhUserGrp AS HANDLE NO-UNDO.
   lhUserGrp = BUFFER UserGrp:HANDLE.
   RUN StarEventInitialize(lhUserGrp).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhUserGrp).
   END.
END.


form
    UserGrp.UserGroup      /* COLUMN-LABEL FORMAT */
    UserGrp.UGName      /* COLUMN-LABEL FORMAT */
    UserGrp.CreDate
WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " User Groups "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    UserGrp.UserGroup     /* LABEL FORMAT */
    UserGrp.UGName    /* LABEL FORMAT */
    UserGrp.CreDate
    UserGrp.CreUser
    UserGrp.ChgDate
    UserGrp.ChgUser
    UserGrp.CreDate
    UserGrp.CreUser
WITH  OVERLAY ROW 4 centered
    COLOR value(cfc) TITLE COLOR value(ctc) lm-ots WITH side-labels 1 columns
    FRAME lis.

form
   UserGrp.Memo
WITH
   centered 1 col no-label overlay row 2 title " User Group '" +
   UserGrp.UserGroup + "' " + UserGrp.UGName + "  memo " FRAME memo.

form /* User Group :n haku kentällä UserGroup */
    UserGroup
    help "Type Group Code"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* User Group :n haku kentällä UGName */
    UGName
    help "Type first characters of a name"
    with row 4 col 2 title color value(ctc) " FIND name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST UserGrp
/* hakuehto */ no-lock no-error.
IF AVAILABLE UserGrp THEN ASSIGN
   muisti       = RECID(UserGrp)
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

   IF lisattava THEN DO:  /* ugroupn lisäys  */
      assign cfc = "lis" ufkey = true lm-ots = " ADD " lisattava = FALSE.
      RUN Syst/ufcolor.
lisaa:
      repeat WITH FRAME lis ON ENDKEY UNDO lisaa, LEAVE lisaa.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSAction:
           PROMPT-FOR UserGrp.UserGroup
           VALIDATE
              (UserGrp.UserGroup = "" OR
              NOT can-find(UserGrp using  UserGrp.UserGroup),
              "User Group " + string(INPUT UserGrp.UserGroup) +
              " already exists !").
           if input UserGrp.UserGroup = "" THEN LEAVE lisaa.
           CREATE UserGrp.
           ASSIGN
           UserGrp.UserGroup   = INPUT FRAME lis UserGrp.UserGroup
           UserGrp.CreUser = katun.
           UPDATE UserGrp.UGName
                  UserGrp.CreDate UserGrp.CreUser .
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhUserGrp).       
           ASSIGN
           muisti = RECID(UserGrp)
           xrecid = muisti.
        END.
      END.  /* lisaa */
      HIDE FRAME lis no-pause.
      ASSIGN tulostettava = TRUE.

      /* onko yhtään tietuetta ? */
      FIND FIRST UserGrp
      /* hakuehto */ no-lock no-error.
      IF NOT AVAILABLE UserGrp THEN LEAVE LOOP.
      NEXT LOOP.
   END.

tulostus:
   DO :
      IF tulostettava THEN DO:
        up FRAME-LINE - 1.
        FIND UserGrp where RECID(UserGrp) = muisti no-lock no-error.

        /* tulostetaan 1 sivullinen tietoa ruudulle
        alkaen tietueesta, jonka avainarvo = muisti.
        alkaen rivilta privi */

        /* jos on juuri poistettu rivi, niin ... */
        IF privi > 0 THEN DOWN privi - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE UserGrp THEN DO:
              DISPLAY UserGrp.UserGroup UserGrp.UGName
                 UserGrp.CreDate UserGrp.CreUser /* sd */.
              rtab[FRAME-LINE] = RECID(UserGrp).
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
        ufk[1]= 35 ufk[2]= 30 ufk[3]= 927 ufk[4]= 0 /*510*/
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7]= 1900   ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
        CHOOSE ROW UserGrp.UserGroup ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) UserGrp.UserGroup WITH FRAME sel.
      END.
      ELSE IF jarj = 2 THEN DO:
        CHOOSE ROW UserGrp.UGName ;(uchoose.i;) no-error WITH FRAME sel.
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
        FIND UserGrp where RECID(UserGrp) = muisti.
        DO i = 1 TO FRAME-LINE - 1:
           IF jarj = 1 THEN FIND PREV UserGrp
           /* hakuehto */ no-lock no-error.
           ELSE IF jarj = 2 THEN FIND PREV UserGrp USE-INDEX UGName
           /* hakuehto */ no-lock no-error.
           IF AVAILABLE UserGrp THEN
              ASSIGN ekarivi = i muisti = RECID(UserGrp).
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
           FIND UserGrp where RECID(UserGrp) = rtab[1] no-lock.
           IF jarj = 1 THEN FIND PREV UserGrp
           /* hakuehto */ no-lock no-error.
           ELSE IF jarj = 2 THEN FIND PREV UserGrp USE-INDEX UGName
           /* hakuehto */ no-lock no-error.
           IF NOT AVAILABLE UserGrp THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT SELAUS.
           END.
           ELSE DO:
              /* edellinen loytyi */
              scroll DOWN.
              DISPLAY UserGrp.UserGroup UserGrp.UGName
                      UserGrp.CreDate UserGrp.CreUser.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = RECID(UserGrp)
              muisti = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* edellinen rivi */

      /* seuraava rivi */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND UserGrp where RECID(UserGrp) = rtab[FRAME-DOWN] no-lock .
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
              DISPLAY UserGrp.UserGroup UserGrp.UGName
                      UserGrp.CreDate UserGrp.CreUser.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = RECID(UserGrp).
              /* ja lopuksi pannaan muistiin ylimman rivin avain */
              muisti = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
        muisti = rtab[1].
        FIND UserGrp where RECID(UserGrp) = muisti no-lock no-error.
        IF jarj = 1 THEN FIND PREV UserGrp
        /* hakuehto */ no-lock no-error.
        ELSE IF jarj = 2 THEN FIND PREV UserGrp USE-INDEX UGName
        /* hakuehto */ no-lock no-error.
        IF AVAILABLE UserGrp THEN DO:
           muisti = RECID(UserGrp).

           /* mennään tiedostoa taaksepäin 1 sivun verran */
           DO rivi = 1 TO (FRAME-DOWN - 1):
              IF jarj = 1 THEN FIND PREV UserGrp
              /* hakuehto */ no-lock no-error.
              ELSE IF jarj = 2 THEN FIND PREV UserGrp USE-INDEX UGName
              /* hakuehto */ no-lock no-error.
              IF AVAILABLE UserGrp THEN muisti = RECID(UserGrp).
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
           FIND UserGrp where RECID(UserGrp) = muisti no-lock.
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
          FIND FIRST UserGrp where UserGrp.UserGroup >= UserGroup
          /* hakuehto */ no-lock no-error.
          IF NOT AVAILABLE UserGrp THEN DO:
             BELL.
             message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT SELAUS.
          END.
          /* joku ugroup/ug-code loytyi */
          ASSIGN jarj = 1 muisti = RECID(UserGrp) tulostettava = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       UGName = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
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
          ASSIGN jarj = 2 muisti = RECID(UserGrp) tulostettava = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     else if lookup(nap,"3,f3") > 0 THEN  DO: /* memo */
        FIND UserGrp where RECID(UserGrp) = rtab[FRAME-LINE] no-lock.
        RUN Mc/memo(INPUT 0,
                 INPUT "UserGrp",
                 INPUT STRING(UserGrp.UserGroup),
                 INPUT "UserGroup").
        ufkey = TRUE.
        ehto = 9.
        NEXT LOOP.
     END.
     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
        lisattava = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* poisto */
       privi = FRAME-LINE.
       FIND UserGrp where RECID(UserGrp) = rtab[FRAME-LINE] no-lock.

       /* valaistaan poistettava rivi */
       COLOR DISPLAY value(ctc)
       UserGrp.UserGroup UserGrp.UGName UserGrp.CreDate UserGrp.CreUser
       /* sd */.

       IF jarj = 1 THEN FIND NEXT UserGrp
       /* hakuehto */ no-lock no-error.
       ELSE IF jarj = 2 THEN FIND NEXT UserGrp USE-INDEX UGName
       /* hakuehto */ no-lock no-error.
       IF AVAILABLE UserGrp THEN muisti = RECID(UserGrp).
       ELSE DO:
          /* luetaan takaisin poistettava */
          FIND UserGrp where RECID(UserGrp) = rtab[FRAME-LINE] no-lock.
          /* sitten edellinen */
          IF jarj = 1 THEN FIND PREV UserGrp
          /* hakuehto */ no-lock no-error.
          ELSE IF jarj = 2 THEN FIND PREV UserGrp USE-INDEX UGName
          /* hakuehto */ no-lock no-error.
          IF AVAILABLE UserGrp THEN DO:
             ASSIGN
             privi = privi - 1  /* koska poistetaan viimeinen */
             muisti = RECID(UserGrp).
          END.
       END.

       /* FIND takaisin poistettavaan riviin */
       FIND UserGrp where RECID(UserGrp) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
       UserGrp.UserGroup UserGrp.UGName UserGrp.CreDate UserGrp.CreUser
       /* sd */.
       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhUserGrp).
           DELETE UserGrp.

           /* poistettiinko viimeinen tietue ? */
           IF NOT can-find(FIRST UserGrp
           /* hakuehto */) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           tulostettava = TRUE.
           NEXT LOOP.
       END.
       ELSE privi = 0. /* ei poistettukaan */
     END. /* poisto */

     else if lookup(nap,"7,f7") > 0 THEN DO TRANSACTION:

       FIND UserGrp where RECID(UserGrp) = rtab[frame-line(sel)] 
       exclusive-lock.
       ASSIGN
         lShoTokens = UserGrp.ShowTokens
         lModTokens = UserGrp.ModifyTokens.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhUserGrp).

       RUN Syst/ugtokens(UserGrp.UserGroup,
                    INPUT-OUTPUT lShoTokens,
                    INPUT-OUTPUT lModTokens).

       IF   UserGrp.ShowTokens NE lShoTokens
       THEN UserGrp.ShowTokens =  lShoTokens.

       IF   UserGrp.ModifyTokens NE lModTokens
       THEN UserGrp.ModifyTokens =  lModTokens.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhUserGrp).
       
       ufkey=true.
       NEXT LOOP.
     END.


     else if lookup(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     DO:
       /* muutos */
       FIND UserGrp where RECID(UserGrp) = rtab[frame-line(sel)] no-lock.
       PAUSE 0 no-message.
       SHOW-GROUP:
         REPEAT WITH FRAME lis ON ENDKEY UNDO SHOW-GROUP, LEAVE SHOW-GROUP.

          DISPLAY UserGrp.UserGroup
                  UserGrp.CreUser 
                  UserGrp.CreDate      
                  UserGrp.ChgUser 
                  UserGrp.ChgDate
          WITH FRAME lis.


          ASSIGN ehto   = 0
                 ufk    = 0            
                 ufk[1] = 7    
                 ufk[3] = 31  
                 ufk[8] = 8
                 lm-ots = " DETAILS "
                 ufkey = TRUE.
          RUN Syst/ufkey.
          cfc = "lis". RUN Syst/ufcolor.

          IF toimi = 1 THEN DO:

              assign lm-ots = " CHANGE " ufkey = TRUE ehto = 9.
              RUN Syst/ufkey.
              cfc = "lis". RUN Syst/ufcolor.
              FIND CURRENT UserGrp EXCLUSIVE-LOCK.
              IF llDoEvent THEN RUN StarEventSetOldBuffer(lhUserGrp).
              UPDATE UserGrp.UGName.
               ASSIGN UserGrp.ChgUser = katun
                      UserGrp.ChgDate = TODAY.

              IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhUserGrp).

          END.

          IF toimi = 3 THEN DO:
             RUN Syst/adduserlimitcui ("UserGroup", UserGrp.UserGroup).
          END.

          IF toimi = 8 THEN LEAVE.

       END. /* end show-group */
     
       
        DISPLAY UserGrp.UGName
                UserGrp.CreDate
                UserGrp.CreUser /* sd */
        WITH FRAME sel.
        xrecid = RECID(UserGrp).
     END.

     else if lookup(nap,"home") > 0 THEN DO:
       IF jarj = 1 THEN FIND FIRST UserGrp
       /* hakuehto */ no-lock no-error.
       ELSE IF jarj = 2 THEN FIND FIRST UserGrp USE-INDEX UGName
       /* hakuehto */ no-lock no-error.
       ASSIGN muisti = RECID(UserGrp) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
       IF jarj = 1 THEN FIND LAST UserGrp
       /* hakuehto */ no-lock no-error.
       ELSE IF jarj = 2 THEN FIND LAST UserGrp USE-INDEX UGName
       /* hakuehto */ no-lock no-error.
       ASSIGN muisti = RECID(UserGrp) tulostettava = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* SELAUS */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

