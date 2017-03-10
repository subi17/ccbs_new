/* -----------------------------------------------
  MODULE .......: NNMPYP
  FUNCTION .....: Mobile prefixes
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 04.08.98
  MODIFIED .....: 18.05.99 jp - uright1 & uright2 added
                  26.04.02 tk - eventlogging added
                  04.03.03 tk - tokens
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/function.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobpref'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMobPref AS HANDLE NO-UNDO.
   lhMobPref = BUFFER MobPref:HANDLE.
   RUN StarEventInitialize(lhMobPref).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhMobPref).
   END.

END.



DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Prefix    LIKE MobPref.Prefix NO-UNDO.

DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 1.
DEF VAR ufkey      AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline    AS INT                    NO-UNDO  init 0.
DEF VAR ex-order   AS INT                    NO-UNDO.
DEF VAR memory     AS RECID                  NO-UNDO.
def var line       as int format "99"        NO-UNDO.
DEF VAR must-print AS LOG                    NO-UNDO.
DEF VAR must-add   AS LOG                    NO-UNDO.
DEF VAR fr-header  AS CHAR                   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24        NO-UNDO.
DEF VAR i          AS INT                    NO-UNDO.
def var ok         as log format "Yes/No"    NO-UNDO.
def var OperName    as c   format "x(22)"     NO-UNDO.

form
    MobPref.Prefix      /* COLUMN-LABEL FORMAT */
    MobPref.Operator  OperName
    MobPref.Memo  
    WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " Prefixes for Mobile numbers "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    MobPref.Prefix 
    MobPref.Operator
    MobPref.Memo
WITH  OVERLAY ROW 4 centered
    COLOR value(cfc) TITLE COLOR value(ctc) fr-header side-labels 1 columns
    FRAME lis.

form /* Nat prefix search WITH FIELD Prefix */
    Prefix
    help "Give prefix"
    with row 4 col 2 title color value(ctc) " FIND PREFIX "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST MobPref
/* search condition */ no-lock no-error.
IF AVAILABLE MobPref THEN ASSIGN
   memory     = recid(MobPref)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No mobile prefixes available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory     = ?
      must-print = FALSE
      must-add   = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 35 " By Prefix ".
    END.

   IF must-add THEN DO:  /* MobPref -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.p.
        DO TRANSAction:
           CREATE MobPref.
           UPDATE 
              MobPref.Prefix
              MobPref.Operator
              MobPref.Memo
           EDITING.
              READKEY.
              IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
                 HIDE MESSAGE no-pause.
                 if frame-field = "Prefix" THEN DO:
                    if input frame lis MobPref.Prefix = "" THEN 
                       UNDO, LEAVE add-new.
                    ELSE IF can-find(MobPref where  
                    MobPref.Prefix = INPUT FRAME lis MobPref.Prefix) THEN DO:
                       message "Mobile prefix already exists" +
                               " - press ENTER to continue !" .
                       PAUSE no-message.
                       NEXT-PROMPT MobPref.Prefix.
                       NEXT.
                    END.
                 END.
                 IF FRAME-FIELD = "Operator" THEN DO:
                    IF NOT CAN-FIND(Operator WHERE Operator.Operator = 
                                    INPUT MobPref.Operator)
                    THEN DO:
                       BELL.
                       MESSAGE "Unknown Operator !".
                       NEXT.

                    END.
                 END.
              END.
              APPLY LASTKEY.
           END.
           /* CREATE OLRefresh -file FOR nncdr -module */
           IF NEW MobPref THEN fOLRefresh(TRUE).
           ASSIGN
           memory = recid(MobPref)
           xrecid = memory.
        END.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMobPref).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST MobPref
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE MobPref THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND MobPref where recid(MobPref) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE MobPref THEN DO:
              FIND FIRST Operator where 
                         Operator.Operator = MobPref.Operator
              no-lock no-error.
              IF AVAIL Operator THEN OperName = Operator.OperName.
              else OperName = "!! UNKNOWN !!".
              DISPLAY 
                 MobPref.Prefix 
                 MobPref.Operator OperName
                 MobPref.Memo.
              rtab[FRAME-LINE] = recid(MobPref).
              IF order = 1 THEN FIND NEXT MobPref
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 2 THEN FIND NEXT MobPref USE-INDEX Prefix
              /* search condition */ no-lock no-error.
              ELSE IF order = 3 THEN FIND NEXT MobPref USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND NEXT MobPref USE-INDEX index-4
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
        ufk[1] = 35 ufk[2] = 0 ufk[3] = 0 ufk[4] = 0
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6] = (If lcRight = "RW" THEN 4 ELSE 0)
        ufk[7] = 0 ufk[8] = 8 ufk[9] = 1
        ehto = 3 ufkey = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW MobPref.Prefix {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MobPref.Prefix MobPref.Memo WITH FRAME sel.
      END.
 /*     ELSE IF order = 2 THEN DO:
        CHOOSE ROW MobPref.Prefix {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MobPref.Prefix WITH FRAME sel.
      END.
     IF order = 3 THEN DO:
        CHOOSE ROW MobPref.?? {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MobPref.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW MobPref.??  {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MobPref.? WITH FRAME sel.
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
        ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
        FIND MobPref where recid(MobPref) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev MobPref
           /* search condition */ no-lock no-error.
      /*   ELSE IF order = 2 THEN FIND prev MobPref USE-INDEX Prefix
           /* search condition */ no-lock no-error.
           ELSE IF order = 3 THEN FIND prev MobPref USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev MobPref USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE MobPref THEN
              ASSIGN firstline = i memory = recid(MobPref).
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
           FIND MobPref where recid(MobPref) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev MobPref
           /* search condition */ no-lock no-error.
      /*   ELSE IF order = 2 THEN FIND prev MobPref USE-INDEX Prefix
           /* search condition */ no-lock no-error.
           ELSE IF order = 3 THEN FIND prev MobPref USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev MobPref USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE MobPref THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              FIND FIRST Operator where 
                         Operator.Operator = MobPref.Operator
              no-lock no-error.
              IF AVAIL Operator THEN OperName = Operator.OperName.
              else OperName = "!! UNKNOWN !!".
              DISPLAY 
                 MobPref.Prefix 
                 MobPref.Operator OperName
                 MobPref.Memo.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(MobPref)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND MobPref where recid(MobPref) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT MobPref
           /* search condition */ no-lock no-error.
      /*   ELSE IF order = 2 THEN FIND NEXT MobPref USE-INDEX Prefix
           /* search condition */ no-lock no-error.
           ELSE IF order = 3 THEN FIND NEXT MobPref USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND NEXT MobPref USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE MobPref THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              FIND FIRST Operator where 
                         Operator.Operator = MobPref.Operator
              no-lock no-error.
              IF AVAIL Operator THEN OperName = Operator.OperName.
              else OperName = "!! UNKNOWN !!".
              DISPLAY 
                 MobPref.Prefix 
                 MobPref.Operator OperName
                 MobPref.Memo.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(MobPref).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND MobPref where recid(MobPref) = memory no-lock no-error.
        IF order = 1 THEN FIND prev MobPref
        /* search condition */ no-lock no-error.
   /*   ELSE IF order = 2 THEN FIND prev MobPref USE-INDEX Prefix
        /* search condition */ no-lock no-error.
        ELSE IF order = 3 THEN FIND prev MobPref USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND prev MobPref USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE MobPref THEN DO:
           memory = recid(MobPref).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev MobPref
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 2 THEN FIND prev MobPref USE-INDEX Prefix
              /* search condition */ no-lock no-error.
              ELSE IF order = 3 THEN FIND prev MobPref USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND prev MobPref USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
              IF AVAILABLE MobPref THEN memory = recid(MobPref).
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
           memory = rtab[FRAME-DOWN].
           FIND MobPref where recid(MobPref) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       Prefix = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE Prefix WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if Prefix <> "" THEN DO:
          FIND FIRST MobPref where MobPref.Prefix >= Prefix
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE MobPref THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  mobpref/np-bsub was found */
          ASSIGN order = 1 memory = recid(MobPref) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       Prefix = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE Prefix WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if Prefix <> "" THEN DO:
          FIND FIRST MobPref where MobPref.Prefix >= Prefix
          USE-INDEX Prefix /* search condition */ no-lock no-error.
          IF NOT AVAILABLE MobPref THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  mobpref/mp-pref was found */
          ASSIGN order = 2 memory = recid(MobPref) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */

        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* removal */

       delline = FRAME-LINE.
       FIND MobPref where recid(MobPref) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc) MobPref.Prefix MobPref.Memo.

       IF order = 1 THEN FIND NEXT MobPref
       /* search condition */ no-lock no-error.
  /*   ELSE IF order = 2 THEN FIND NEXT MobPref USE-INDEX Prefix
       /* search condition */ no-lock no-error.
      ELSE IF order = 3 THEN FIND NEXT MobPref USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND NEXT MobPref USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       IF AVAILABLE MobPref THEN memory = recid(MobPref).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND MobPref where recid(MobPref) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev MobPref
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev MobPref USE-INDEX Prefix
          /* search condition */ no-lock no-error.
     /*    ELSE IF order = 3 THEN FIND prev MobPref USE-INDEX index-3
          /* search condition */ no-lock no-error.
          ELSE IF order = 4 THEN FIND prev MobPref USE-INDEX index-4
          /* search condition */ no-lock no-error.   */
          IF AVAILABLE MobPref THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(MobPref).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND MobPref where recid(MobPref) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc) MobPref.Prefix MobPref.Memo.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMobPref).

           DELETE MobPref.
           /* CREATE OLRefresh -file FOR nncdr -module */
           fOLRefresh(TRUE).
           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST MobPref
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
     DO WITH FRAME lis TRANSAction:
       /* change */

       FIND MobPref where recid(MobPref) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p.
       DISPLAY 
          MobPref.Prefix
          MobPref.Operator
          MobPref.Memo.

       IF lcRight = "RW" THEN DO:

          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobPref).

          UPDATE MobPref.Operator MobPref.Memo.

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobPref).
       END.
       ELSE PAUSE.

       HIDE FRAME lis no-pause.
       IF MobPref.Operator entered THEN DO:
          FIND FIRST Operator where 
                     Operator.Operator = MobPref.Operator
          no-lock no-error.
          IF AVAIL Operator THEN OperName = Operator.OperName.
          else OperName = "!! UNKNOWN !!".
       END.

       DISP 
          MobPref.Prefix 
          MobPref.Operator OperName
          MobPref.Memo 
       WITH FRAME sel.


       xrecid = recid(MobPref).
       HIDE FRAME lis.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST MobPref
       /* search condition */ no-lock no-error.
  /*   ELSE IF order = 2 THEN FIND FIRST MobPref USE-INDEX Prefix
       /* search condition */ no-lock no-error.
      ELSE IF order = 3 THEN FIND FIRST MobPref USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND FIRST MobPref USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(MobPref) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST MobPref
       /* search condition */ no-lock no-error.
  /*   ELSE IF order = 2 THEN FIND LAST MobPref USE-INDEX Prefix
       /* search condition */ no-lock no-error.
       ELSE IF order = 3 THEN FIND LAST MobPref USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND LAST MobPref USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(MobPref) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

