/* ----------------------------------------------------------------------
  MODULE .......: NNPCYP.P
  TASK .........: UPDATE Program Classes
  APPLICATION ..: nn
  AUTHOR .......: PT
  CREATED ......: 27-04-99
  CHANGED ......: 18-05-99 - jp uright1 & uright2 added
                  29.04.02/tk eventlogging added
                  06.03.03/tk tokens
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'menuclass'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMenuClass AS HANDLE NO-UNDO.
   lhMenuClass = BUFFER MenuClass:HANDLE.
   RUN StarEventInitialize(lhMenuClass).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhMenuClass).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR MenuClass   LIKE MenuClass.MenuClass   NO-UNDO.
DEF VAR MCName LIKE MenuClass.MCName NO-UNDO.
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

form
    MenuClass.MenuClass      /* COLUMN-LABEL FORMAT */
    MenuClass.MCName    /* COLUMN-LABEL FORMAT */
    MenuClass.Memo[1]    /* column-label */ format "x(30)"
    WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " Program Classes "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    MenuClass.MenuClass     label "No. "    SKIP
    MenuClass.MCName   label "Name"    skip(1)
                            "memo:"
    MenuClass.Memo[1 FOR 10] NO-LABEL   AT 7
 WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    lm-ots WITH side-labels
    FRAME lis.

form /* seek Program Class  BY  MenuClass */
    MenuClass
    help "Enter no. of class"
    with row 4 col 2 title color value(ctc) " FIND NO. "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Program Class  BY MCName */
    MCName
    help "Enter Name of class"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST MenuClass
/* srule */ no-lock no-error.
IF AVAILABLE MenuClass THEN ASSIGN
   memory       = recid(MenuClass)
   must-print = TRUE
   lisattava    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No menuclasses available !" VIEW-AS ALERT-BOX.
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
       if jarj = 1 then put screen row 19 col 36 " By No   ".
       if jarj = 2 then put screen row 19 col 36 " By Name ".
    END.

   IF lisattava THEN DO:  /* prclassn lisäys  */
      assign cfc = "lis" ufkey = true lm-ots = " ADD " lisattava = FALSE.
      RUN Syst/ufcolor.
ADD-ROW:
      repeat WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSAction:
           PROMPT-FOR MenuClass.MenuClass
           VALIDATE
              (MenuClass.MenuClass = 0 OR
              NOT can-find(MenuClass using  MenuClass.MenuClass),
              "Program Class " + string(INPUT MenuClass.MenuClass) +
              " already exists !").
           IF INPUT MenuClass.MenuClass = 0 THEN LEAVE ADD-ROW.
           CREATE MenuClass.
           ASSIGN
           MenuClass.MenuClass = INPUT FRAME lis MenuClass.MenuClass.
           UPDATE MenuClass.MCName
                  MenuClass.Memo[1 FOR 10] .

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMenuClass).
           PAUSE 0.


           ASSIGN
           memory = recid(MenuClass)
           xrecid = memory.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST MenuClass
      /* srule */ no-lock no-error.
      IF NOT AVAILABLE MenuClass THEN LEAVE LOOP.
      NEXT LOOP.
   END.

tulostus:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND MenuClass where recid(MenuClass) = memory no-lock no-error.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'memory'.
        alkaen rivilta privi */

        /* IF a ROW was recently deleted ... */
        IF privi > 0 THEN DOWN privi - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE MenuClass THEN DO:
              DISPLAY MenuClass.MenuClass MenuClass.MCName
                 MenuClass.Memo[1].
              rtab[FRAME-LINE] = recid(MenuClass).
              IF jarj = 1 THEN FIND NEXT MenuClass
              /* srule */ no-lock no-error.
              ELSE IF jarj = 2 THEN FIND NEXT MenuClass USE-INDEX MCName
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
        CHOOSE ROW MenuClass.MenuClass ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MenuClass.MenuClass WITH FRAME sel.
      END.
      ELSE IF jarj = 2 THEN DO:
        CHOOSE ROW MenuClass.MCName ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MenuClass.MCName WITH FRAME sel.
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
        FIND MenuClass where recid(MenuClass) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF jarj = 1 THEN FIND prev MenuClass
           /* srule */ no-lock no-error.
           ELSE IF jarj = 2 THEN FIND prev MenuClass USE-INDEX MCName
           /* srule */ no-lock no-error.
           IF AVAILABLE MenuClass THEN
              ASSIGN ekarivi = i memory = recid(MenuClass).
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
           FIND MenuClass where recid(MenuClass) = rtab[1] no-lock.
           IF jarj = 1 THEN FIND prev MenuClass
           /* srule */ no-lock no-error.
           ELSE IF jarj = 2 THEN FIND prev MenuClass USE-INDEX MCName
           /* srule */ no-lock no-error.
           IF NOT AVAILABLE MenuClass THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              scroll DOWN.
              DISPLAY MenuClass.MenuClass MenuClass.MCName
                      MenuClass.Memo[1].
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(MenuClass)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND MenuClass where recid(MenuClass) = rtab[FRAME-DOWN] no-lock .
           IF jarj = 1 THEN FIND NEXT MenuClass
           /* srule */ no-lock no-error.
           ELSE IF jarj = 2 THEN FIND NEXT MenuClass USE-INDEX MCName
           /* srule */ no-lock no-error.
           IF NOT AVAILABLE MenuClass THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              scroll up.
              DISPLAY MenuClass.MenuClass MenuClass.MCName
                      MenuClass.Memo[1].
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(MenuClass).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND MenuClass where recid(MenuClass) = memory no-lock no-error.
        IF jarj = 1 THEN FIND prev MenuClass
        /* srule */ no-lock no-error.
        ELSE IF jarj = 2 THEN FIND prev MenuClass USE-INDEX MCName
        /* srule */ no-lock no-error.
        IF AVAILABLE MenuClass THEN DO:
           memory = recid(MenuClass).

           /* reverse 1 page */
           DO rivi = 1 TO (FRAME-DOWN - 1):
              IF jarj = 1 THEN FIND prev MenuClass
              /* srule */ no-lock no-error.
              ELSE IF jarj = 2 THEN FIND prev MenuClass USE-INDEX MCName
              /* srule */ no-lock no-error.
              IF AVAILABLE MenuClass THEN memory = recid(MenuClass).
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
           FIND MenuClass where recid(MenuClass) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* seuraava sivu */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       MenuClass = 0.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE MenuClass WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       IF MenuClass <> 0 THEN DO:
          FIND FIRST MenuClass where MenuClass.MenuClass >= MenuClass
          /* srule */ no-lock no-error.
          IF NOT AVAILABLE MenuClass THEN DO:
             BELL.
             message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /* some prclass/pc-no was found */
          ASSIGN jarj = 1 memory = recid(MenuClass) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       MCName = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE MCName WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if MCName <> "" THEN DO:
          FIND FIRST MenuClass where MenuClass.MCName >= MCName
          USE-INDEX MCName /* srule */ no-lock no-error.
          IF NOT AVAILABLE MenuClass THEN DO:
             bell. message "NOT FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /* some prclass/pc-name was found */
          ASSIGN jarj = 2 memory = recid(MenuClass) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */

        lisattava = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* DELETE */

       privi = FRAME-LINE.
       FIND MenuClass where recid(MenuClass) = rtab[FRAME-LINE] no-lock.

       /* are there MenuText items connected into this class ? */
       IF can-find(FIRST MenuTree of MenuClass) THEN DO:
          BELL.
          message "This class occurs on menu items - it may not be deleted !".
          PAUSE 3 no-message.
          NEXT.
       END.



       /* valaistaan poistettava rivi */
       COLOR DISPLAY value(ctc)
       MenuClass.MenuClass MenuClass.MCName MenuClass.Memo[1].

       IF jarj = 1 THEN FIND NEXT MenuClass
       /* srule */ no-lock no-error.
       ELSE IF jarj = 2 THEN FIND NEXT MenuClass USE-INDEX MCName
       /* srule */ no-lock no-error.
       IF AVAILABLE MenuClass THEN memory = recid(MenuClass).
       ELSE DO:
          /* luetaan takaisin poistettava */
          FIND MenuClass where recid(MenuClass) = rtab[FRAME-LINE] no-lock.
          /* sitten edellinen */
          IF jarj = 1 THEN FIND prev MenuClass
          /* srule */ no-lock no-error.
          ELSE IF jarj = 2 THEN FIND prev MenuClass USE-INDEX MCName
          /* srule */ no-lock no-error.
          IF AVAILABLE MenuClass THEN DO:
             ASSIGN
             privi = privi - 1  /* 'cause the LAST record is deleted */
             memory = recid(MenuClass).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       FIND MenuClass where recid(MenuClass) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
       MenuClass.MenuClass MenuClass.MCName MenuClass.Memo[1].
       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMenuClass).

           DELETE MenuClass.

           /* was LAST record deleted ? */
           IF NOT can-find(FIRST MenuClass
           /* srule */) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE privi = 0. /* UNDO DELETE */
     END. /* DELETE */

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSAction:
       /* change */
       FIND MenuClass where recid(MenuClass) = rtab[frame-line(sel)]
       exclusive-lock.
       assign lm-ots = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.
       DISPLAY 
          MenuClass.MenuClass 
          MenuClass.MCName 
          MenuClass.Memo[1 FOR 10].

       IF lcRight = "RW" THEN DO:

          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMenuClass).

          UPDATE MenuClass.MCName
                 MenuClass.Memo[1 FOR 10].

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMenuClass).
       END.
       ELSE PAUSE.

       HIDE FRAME lis no-pause.
       DISPLAY MenuClass.MCName
                  MenuClass.Memo[1]
       WITH FRAME sel.
       xrecid = recid(MenuClass).


     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF jarj = 1 THEN FIND FIRST MenuClass
       /* srule */ no-lock no-error.
       ELSE IF jarj = 2 THEN FIND FIRST MenuClass USE-INDEX MCName
       /* srule */ no-lock no-error.
       ASSIGN memory = recid(MenuClass) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF jarj = 1 THEN FIND LAST MenuClass
       /* srule */ no-lock no-error.
       ELSE IF jarj = 2 THEN FIND LAST MenuClass USE-INDEX MCName
       /* srule */ no-lock no-error.
       ASSIGN memory = recid(MenuClass) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

