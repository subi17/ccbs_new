/* -----------------------------------------------
  MODULE .......: NNSTYP.P
  FUNCTION .....: Update table AreaPlan
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 03-09-97
  changePVM ....: 18-05-99 jp - uright1 & uright2 added
                  30.10.02 jr Eventlog
                  03.04.03 tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'areaplan'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR haku-st-nr      LIKE AreaPlan.TrafficArea         NO-UNDO.
DEF VAR haku-AreaName    LIKE AreaPlan.AreaName       NO-UNDO.
DEF VAR xrecid          AS RECID                           init ?.
DEF VAR firstline       AS INT                    NO-UNDO  init 0.
DEF VAR order           AS INT                    NO-UNDO  init 1.
DEF VAR ordercount      AS INT                    NO-UNDO  init 2.
DEF VAR ufkey           AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline         AS INT                    NO-UNDO  init 0.
DEF VAR ex-order        AS INT                    NO-UNDO.
DEF VAR Memory          AS RECID                  NO-UNDO.
def var line            as int format "99"        NO-UNDO.
DEF VAR must-print      AS LOG                    NO-UNDO.
DEF VAR must-add        AS LOG                    NO-UNDO.
DEF VAR fr-header       AS CHAR                   NO-UNDO.
DEF VAR rtab            AS RECID EXTENT 24        NO-UNDO.
DEF VAR i               AS INT                    NO-UNDO.
def var ok              as log format "Yes/No"    NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhAreaPlan AS HANDLE NO-UNDO.
   lhAreaPlan = BUFFER AreaPlan:HANDLE.
   RUN StarEventInitialize(lhAreaPlan).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhAreaPlan).
   END.
END.

form
    AreaPlan.TrafficArea       /* COLUMN-LABEL FORMAT */
    AreaPlan.AreaName     /* COLUMN-LABEL FORMAT */
             /* COLUMN-LABEL FORMAT */
    WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " Traffic Areas "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    AreaPlan.TrafficArea     /* LABEL FORMAT */
    AreaPlan.AreaName    /* LABEL FORMAT */
            /* LABEL FORMAT */

    WITH  OVERLAY ROW 4 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH side-labels 1 columns
    FRAME lis.

form /* samtrafiksomrAde search WITH FIELD TrafficArea */
    haku-st-nr
    help "Give ...."
    with row 4 col 2 title color value(ctc) " FIND Code "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

form /* samtrafiksomrAde search WITH FIELD AreaName */
    haku-AreaName
    help "Give ..."
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST AreaPlan
/* search condition */ no-lock no-error.
IF AVAILABLE AreaPlan THEN ASSIGN
   Memory       = recid(AreaPlan)
   must-print = TRUE
   must-add    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No areaplans available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      Memory       = ?
      must-print = FALSE
      must-add    = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 34 "  By number  ".
       if order = 2 then put screen row 19 col 34 "   By Name   ".
    END.

   IF must-add THEN DO:  /* AreaPlan -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSACTION:
           PROMPT-FOR AreaPlan.TrafficArea
           VALIDATE
              (AreaPlan.TrafficArea = 0 OR
              NOT can-find(AreaPlan WHERE  AreaPlan.TrafficArea =
                                       INPUT AreaPlan.TrafficArea ),
              "TrafficArea " + string(INPUT AreaPlan.TrafficArea) +
              " already exists !").
           IF INPUT AreaPlan.TrafficArea = 0 THEN LEAVE add-new.
           CREATE AreaPlan.
           ASSIGN
           AreaPlan.TrafficArea = INPUT FRAME lis AreaPlan.TrafficArea.
           UPDATE AreaPlan.AreaName.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhAreaPlan).

           ASSIGN
           Memory = recid(AreaPlan)
           xrecid = Memory.
        END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST AreaPlan
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE AreaPlan THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND AreaPlan where recid(AreaPlan) = Memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = Memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE AreaPlan THEN DO:
              DISPLAY AreaPlan.TrafficArea AreaPlan.AreaName
                 .
              rtab[FRAME-LINE] = recid(AreaPlan).
              IF order = 1 THEN FIND NEXT AreaPlan
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT AreaPlan USE-INDEX AreaName
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND NEXT AreaPlan USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND NEXT AreaPlan USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
           END.
           ELSE DO:
              CLEAR no-pause.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN firstline.
        ASSIGN firstline = 0
               must-print = FALSE.
        PAUSE 0 no-message.

        /* one page of data has been Printed AND
        the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

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
      IF order = 1 THEN DO:
        CHOOSE ROW AreaPlan.TrafficArea ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) AreaPlan.TrafficArea WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW AreaPlan.AreaName ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) AreaPlan.AreaName WITH FRAME sel.
      END.
/*    IF order = 3 THEN DO:
        CHOOSE ROW AreaPlan.?? ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) AreaPlan.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW AreaPlan.??  ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) AreaPlan.? WITH FRAME sel.
      END.
*/
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > ordercount THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = ordercount.
      END.

      IF order <> ex-order THEN DO:
        ASSIGN firstline = 0 Memory = rtab[FRAME-LINE].
        FIND AreaPlan where recid(AreaPlan) = Memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev AreaPlan
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev AreaPlan USE-INDEX AreaName
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev AreaPlan USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev AreaPlan USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE AreaPlan THEN
              ASSIGN firstline = i Memory = recid(AreaPlan).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        message "You are on a empty row, move upwards !".
        PAUSE 1 no-message.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND AreaPlan where recid(AreaPlan) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev AreaPlan
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev AreaPlan USE-INDEX AreaName
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev AreaPlan USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev AreaPlan USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE AreaPlan THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              DISPLAY AreaPlan.TrafficArea AreaPlan.AreaName
                      .
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(AreaPlan)
              Memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND AreaPlan where recid(AreaPlan) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT AreaPlan
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT AreaPlan USE-INDEX AreaName
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND NEXT AreaPlan USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND NEXT AreaPlan USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE AreaPlan THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              DISPLAY AreaPlan.TrafficArea AreaPlan.AreaName
                      .
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(AreaPlan).
              /* finally LAST line's KeyValue is saved */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND AreaPlan where recid(AreaPlan) = Memory no-lock no-error.
        IF order = 1 THEN FIND prev AreaPlan
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev AreaPlan USE-INDEX AreaName
        /* search condition */ no-lock no-error.
   /*    ELSE IF order = 3 THEN FIND prev AreaPlan USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND prev AreaPlan USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE AreaPlan THEN DO:
           Memory = recid(AreaPlan).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev AreaPlan
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev AreaPlan USE-INDEX AreaName
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND prev AreaPlan USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND prev AreaPlan USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
              IF AVAILABLE AreaPlan THEN Memory = recid(AreaPlan).
              ELSE line = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* this is the FIRST data page */
           message "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-message.
        END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* cursor TO the downmost line */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           message "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* the downmost line wasn't empty */
           Memory = rtab[FRAME-DOWN].
           FIND AreaPlan where recid(AreaPlan) = Memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       haku-st-nr = 0.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE haku-st-nr WITH FRAME haku-f1.
       HIDE FRAME haku-f1 no-pause.
       IF haku-st-nr <> 0 THEN DO:
          FIND FIRST AreaPlan where AreaPlan.TrafficArea >= haku-st-nr
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE AreaPlan THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  AreaPlan/st-nr was found */
          ASSIGN order = 1 Memory = recid(AreaPlan) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       haku-AreaName = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE haku-AreaName WITH FRAME haku-f2.
       HIDE FRAME haku-f2 no-pause.
       if haku-AreaName <> "" THEN DO:
          FIND FIRST AreaPlan where AreaPlan.AreaName >= haku-AreaName
          USE-INDEX AreaName /* search condition */ no-lock no-error.
          IF NOT AVAILABLE AreaPlan THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  AreaPlan/AreaName was found */
          ASSIGN order = 2 Memory = recid(AreaPlan) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */

        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* removal */

       delline = FRAME-LINE.
       FIND AreaPlan where recid(AreaPlan) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
       AreaPlan.TrafficArea AreaPlan.AreaName .

       IF order = 1 THEN FIND NEXT AreaPlan
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT AreaPlan USE-INDEX AreaName
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND NEXT AreaPlan USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND NEXT AreaPlan USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       IF AVAILABLE AreaPlan THEN Memory = recid(AreaPlan).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND AreaPlan where recid(AreaPlan) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev AreaPlan
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev AreaPlan USE-INDEX AreaName
          /* search condition */ no-lock no-error.
     /*    ELSE IF order = 3 THEN FIND prev AreaPlan USE-INDEX index-3
          /* search condition */ no-lock no-error.
          ELSE IF order = 4 THEN FIND prev AreaPlan USE-INDEX index-4
          /* search condition */ no-lock no-error.   */
          IF AVAILABLE AreaPlan THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             Memory = recid(AreaPlan).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND AreaPlan where recid(AreaPlan) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
       AreaPlan.TrafficArea AreaPlan.AreaName .
       IF ok THEN DO:

           FIND FIRST AreaCode of AreaPlan no-lock no-error.
           IF AVAIL AreaCode THEN DO:
              BELL.
              MESSAGE
              "Det finns riktnummer inom detta omrAde - kan ej raderas !".
              PAUSE 2 no-message.
              NEXT.
           END.

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhAreaPlan).
           DELETE AreaPlan.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST AreaPlan
           /* search condition */) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND AreaPlan where recid(AreaPlan) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.
       DISPLAY 
         AreaPlan.TrafficArea
         AreaPlan.AreaName.

       IF lcRight = "RW" THEN DO:

          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhAreaPlan).
          UPDATE AreaPlan.AreaName.
          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhAreaPlan).

       END.
       ELSE PAUSE.

       HIDE FRAME lis no-pause.
       DISPLAY AreaPlan.AreaName

       WITH FRAME sel.
       xrecid = recid(AreaPlan).
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST AreaPlan
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST AreaPlan USE-INDEX AreaName
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND FIRST AreaPlan USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND FIRST AreaPlan USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN Memory = recid(AreaPlan) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST AreaPlan
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST AreaPlan USE-INDEX AreaName
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND LAST AreaPlan USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND LAST AreaPlan USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN Memory = recid(AreaPlan) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

