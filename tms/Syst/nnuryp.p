/* -------------------------------------------------------------------
  MODULE .......: NNURYP.P
  TASK .........: UPDATE User Rights
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 27-04-99
  CHANGED ......: 18-05-99 jp uright1 & uright2 added
                  29.04.02 tk eventlogging added
                  06.03.03 tk tokens
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'UserRight'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhUserRight AS HANDLE NO-UNDO.
   lhUserRight = BUFFER UserRight:HANDLE.
   RUN StarEventInitialize(lhUserRight).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhUserRight).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR UserCode     LIKE UserRight.UserCode  NO-UNDO.
DEF VAR MenuClass       LIKE UserRight.MenuClass    NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR ekarivi      AS INT                    NO-UNDO  init 0.
DEF VAR jarj         AS INT                    NO-UNDO  init 1.
DEF VAR jarjlkm      AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR privi        AS INT                    NO-UNDO  init 0.
DEF VAR ed-jarj      AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
def var rivi         as int format "99"        NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR lisattava    AS LOG                    NO-UNDO.
DEF VAR lm-ots       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
def var ok           as log format "Yes/No"  NO-UNDO.
def var urname       as c format "x(12)"       NO-UNDO.
DEF VAR usname       AS c                      NO-UNDO.
DEF VAR pcname       AS c                      NO-UNDO.
DEF VAR urnames      AS c                      NO-UNDO.


urnames = "PROHIBITED,READ-ONLY,READ-WRITE".


form
    UserRight.UserCode  /* COLUMN-LABEL FORMAT  */
    usname              column-label "User's Name"      format "x(20)"
    UserRight.MenuClass    /* column-label "Class" */
    pcname              column-label "Name of Class"    format "x(20)"
    urname              column-label "Type of Right"
WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " User Rights "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    UserRight.UserCode    /* label "User ID" */
    TMSUser.UserName NO-LABEL AT 20
    SKIP

    UserRight.MenuClass      /* label "PrClass" */
    MenuClass.MCName NO-LABEL AT 20
    SKIP

    UserRight.UsrRight urname  NO-LABEL

WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    lm-ots WITH side-labels
    FRAME lis.

form /* seek User Right  BY  UserCode */
    UserCode
    help "Enter User ID"
    with row 4 col 2 title color value(ctc) " FIND USER ID "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek User Right  BY MenuClass */
    MenuClass
    help "Enter Program Class No."
    with row 4 col 2 title color value(ctc) " FIND CLASS NO. "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST UserRight
/* srule */ no-lock no-error.
IF AVAILABLE UserRight THEN ASSIGN
   memory       = recid(UserRight)
   must-print = TRUE
   lisattava    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No user rights available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      memory       = ?
      must-print = FALSE
      lisattava    = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF jarj <> ed-jarj THEN DO:
       ed-jarj = jarj.

       if jarj = 1 then put screen row 19 col 32 "    By User ID    ".
       if jarj = 2 then put screen row 19 col 32 " By Program Class ".
    END.

   IF lisattava THEN DO:  /* usrightn lisäys  */
      assign cfc = "lis" ufkey = true lm-ots = " ADD " lisattava = FALSE.
      RUN Syst/ufcolor.
ADD-ROW:
      repeat WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSAction:
           PROMPT-FOR
              UserRight.UserCode
              UserRight.MenuClass
           WITH FRAME lis  EDITING:
              READKEY.
              IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.

                 if frame-field = "user-id" THEN DO:
                    if input UserRight.UserCode = "" THEN LEAVE ADD-ROW.
                    FIND TMSUser where TMSUser.UserCode = INPUT UserRight.UserCode
                    no-lock no-error.
                    IF NOT AVAIL TMSUser THEN DO:
                       BELL.
                       message "Unknown user !".
                       NEXT.
                    END.
                    DISP TMSUser.UserName.
                 END.


                 else if frame-field = "pc-no" THEN DO:
                    IF INPUT UserRight.MenuClass = 0 THEN DO:
                       NEXT-PROMPT UserRight.UserCode.
                       NEXT.
                    END.
                    FIND MenuClass where MenuClass.MenuClass = INPUT UserRight.MenuClass
                    no-lock no-error.
                    IF NOT AVAIL MenuClass THEN DO:
                       BELL.
                       message "Unknown Program Class !".
                       NEXT.
                    END.
                    DISP MenuClass.MCName.

                    IF can-find(UserRight where
                                UserRight.UserCode = INPUT UserRight.UserCode AND
                                UserRight.MenuClass   = INPUT UserRight.MenuClass)
                    THEN DO:
                       BELL.
                       message "There is already a right record for user"
                       input UserRight.UserCode "/ class" input UserRight.MenuClass "!".
                       NEXT.
                    END.



                 END.
              END.
              APPLY LASTKEY.
           END.  /* EDITING */

           CREATE UserRight.
           ASSIGN
           UserRight.UserCode = INPUT FRAME lis UserRight.UserCode
           UserRight.MenuClass   = INPUT FRAME lis UserRight.MenuClass.


           UPDATE UserRight.UsrRight.
           urname = entry(UserRight.UsrRight + 1,urnames).

           DISP urname.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhUserRight).

           ASSIGN
           memory = recid(UserRight)
           xrecid = memory.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST UserRight
      /* srule */ no-lock no-error.
      IF NOT AVAILABLE UserRight THEN LEAVE LOOP.
      NEXT LOOP.
   END.

tulostus:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND UserRight where recid(UserRight) = memory no-lock no-error.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'memory'.
        alkaen rivilta privi */

        /* IF a ROW was recently deleted ... */
        IF privi > 0 THEN DOWN privi - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE UserRight THEN DO:

              urname = entry(UserRight.UsrRight + 1,urnames).
              FIND TMSUser where TMSUser.UserCode = UserRight.UserCode no-lock no-error.
              IF AVAIL TMSUser   THEN usname = TMSUser.UserName.
                               else usname = "!! REMOVED !!".
              FIND MenuClass of UserRight no-lock no-error.
              IF AVAIL MenuClass THEN pcname = MenuClass.MCName.
                               else pcname = "!! REMOVED !!".

              DISPLAY UserRight.UserCode UserRight.MenuClass
                 urname usname pcname.
              rtab[FRAME-LINE] = recid(UserRight).
              IF jarj = 1 THEN FIND NEXT UserRight
              /* srule */ no-lock no-error.
              ELSE IF jarj = 2 THEN FIND NEXT UserRight USE-INDEX MenuClass
              /* srule */ no-lock no-error.
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
               must-print = FALSE.
        PAUSE 0 no-message.

        /* Now there is one page displayed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* tulostus */

   /* IF a ROW was recently deleted: */
   IF privi > 0 THEN DOWN privi - 1.
   ASSIGN privi = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 542  ufk[2]= 133 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 13 ELSE 0)  
        ufk[6]= 0   ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
        CHOOSE ROW UserRight.UserCode ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) UserRight.UserCode WITH FRAME sel.
      END.
      ELSE IF jarj = 2 THEN DO:
        CHOOSE ROW UserRight.MenuClass ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) UserRight.MenuClass WITH FRAME sel.
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
        ASSIGN ekarivi = 0 memory = rtab[FRAME-LINE].
        FIND UserRight where recid(UserRight) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF jarj = 1 THEN FIND prev UserRight
           /* srule */ no-lock no-error.
           ELSE IF jarj = 2 THEN FIND prev UserRight USE-INDEX MenuClass
           /* srule */ no-lock no-error.
           IF AVAILABLE UserRight THEN
              ASSIGN ekarivi = i memory = recid(UserRight).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT lisattava THEN DO:
        BELL.
        message "You are on an empty row, move upwards !".
        PAUSE 1 no-message.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous ROW */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND UserRight where recid(UserRight) = rtab[1] no-lock.
           IF jarj = 1 THEN FIND prev UserRight
           /* srule */ no-lock no-error.
           ELSE IF jarj = 2 THEN FIND prev UserRight USE-INDEX MenuClass
           /* srule */ no-lock no-error.
           IF NOT AVAILABLE UserRight THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              scroll DOWN.
              urname = entry(UserRight.UsrRight + 1,urnames).
              FIND TMSUser where TMSUser.UserCode = UserRight.UserCode no-lock no-error.
              IF AVAIL TMSUser   THEN usname = TMSUser.UserName.
                               else usname = "!! REMOVED !!".
              FIND MenuClass of UserRight no-lock no-error.
              IF AVAIL MenuClass THEN pcname = MenuClass.MCName.
                               else pcname = "!! REMOVED !!".
              DISPLAY UserRight.UserCode UserRight.MenuClass
                      urname usname pcname.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(UserRight)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND UserRight where recid(UserRight) = rtab[FRAME-DOWN] no-lock .
           IF jarj = 1 THEN FIND NEXT UserRight
           /* srule */ no-lock no-error.
           ELSE IF jarj = 2 THEN FIND NEXT UserRight USE-INDEX MenuClass
           /* srule */ no-lock no-error.
           IF NOT AVAILABLE UserRight THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              scroll up.
              urname = entry(UserRight.UsrRight + 1,urnames).
              FIND TMSUser where TMSUser.UserCode = UserRight.UserCode no-lock no-error.
              IF AVAIL TMSUser   THEN usname = TMSUser.UserName.
                               else usname = "!! REMOVED !!".
              FIND MenuClass of UserRight no-lock no-error.
              IF AVAIL MenuClass THEN pcname = MenuClass.MCName.
                               else pcname = "!! REMOVED !!".
              DISPLAY UserRight.UserCode UserRight.MenuClass
                      urname usname pcname.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(UserRight).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND UserRight where recid(UserRight) = memory no-lock no-error.
        IF jarj = 1 THEN FIND prev UserRight
        /* srule */ no-lock no-error.
        ELSE IF jarj = 2 THEN FIND prev UserRight USE-INDEX MenuClass
        /* srule */ no-lock no-error.
        IF AVAILABLE UserRight THEN DO:
           memory = recid(UserRight).

           /* reverse 1 page */
           DO rivi = 1 TO (FRAME-DOWN - 1):
              IF jarj = 1 THEN FIND prev UserRight
              /* srule */ no-lock no-error.
              ELSE IF jarj = 2 THEN FIND prev UserRight USE-INDEX MenuClass
              /* srule */ no-lock no-error.
              IF AVAILABLE UserRight THEN memory = recid(UserRight).
              ELSE rivi = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           message "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-message.
        END.
     END. /* previoius page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* kohdistin alimmalle riville */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           message "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND UserRight where recid(UserRight) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* seuraava sivu */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       UserCode = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE UserCode WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if UserCode <> "" THEN DO:
          FIND FIRST UserRight where UserRight.UserCode >= UserCode
          /* srule */ no-lock no-error.
          IF NOT AVAILABLE UserRight THEN DO:
             BELL.
             message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /* some usright/user-id was found */
          ASSIGN jarj = 1 memory = recid(UserRight) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       MenuClass = 0.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE MenuClass WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       IF MenuClass <> 0 THEN DO:
          FIND FIRST UserRight where UserRight.MenuClass >= MenuClass
          USE-INDEX MenuClass /* srule */ no-lock no-error.
          IF NOT AVAILABLE UserRight THEN DO:
             bell. message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /* some usright/pc-no was found */
          ASSIGN jarj = 2 memory = recid(UserRight) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     else if lookup(nap,"enter,return,5,f5") > 0 AND lcRight = "RW" THEN
     DO WITH FRAME sel TRANSAction:
       /* change */

       FIND UserRight where recid(UserRight) = rtab[frame-line(sel)]
       exclusive-lock.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhUserRight).

       IF UserRight.UsrRight < 2 THEN UserRight.UsrRight = UserRight.UsrRight + 1.
                              ELSE UserRight.UsrRight = 0.
       urname = entry(UserRight.UsrRight + 1,urnames).
       DISP urname WITH FRAME sel.
       xrecid = recid(UserRight).

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhUserRight).

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF jarj = 1 THEN FIND FIRST UserRight
       /* srule */ no-lock no-error.
       ELSE IF jarj = 2 THEN FIND FIRST UserRight USE-INDEX MenuClass
       /* srule */ no-lock no-error.
       ASSIGN memory = recid(UserRight) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF jarj = 1 THEN FIND LAST UserRight
       /* srule */ no-lock no-error.
       ELSE IF jarj = 2 THEN FIND LAST UserRight USE-INDEX MenuClass
       /* srule */ no-lock no-error.
       ASSIGN memory = recid(UserRight) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

