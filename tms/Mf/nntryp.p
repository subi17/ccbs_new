/* -----------------------------------------------
  MODULE .......: NNTRYP.P
  FUNCTION .....: Trunk groups
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 03.09.97
  MODIFIED .....: 23.02.98 kl => format "x(18)" > "x(16)"
                  21.07.98 kl => direct,in,out
                  11.08.98 pt "Trunk" translated into "CGR"
                  23.02.99 vk in english
                  17.05.99 jp uright1 & uright2 added
                  29.01.02 kl DISPLAY formats
                  16.05.02/tk Event logging added
                  04.03.03 tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'trunk'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhTrunk AS HANDLE NO-UNDO.
   lhTrunk = BUFFER Trunk:HANDLE.
   RUN StarEventInitialize(lhTrunk).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhTrunk). 
   END.

END.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR h-ex-code LIKE Trunk.ExCode NO-UNDO.
DEF VAR h-op-code LIKE Trunk.OpCode NO-UNDO.
DEF VAR h-tr-code LIKE Trunk.TrunkCode NO-UNDO.

DEF BUFFER xxtrunk FOR Trunk.

DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 2.
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

form
    Trunk.ExCode  /* column-label "Exc.code"       */  format "x(8)"
    Exchange.ExName      column-label "Sw. name"           format "x(11)"
    Trunk.TrunkCode  /* column-label "CGR"            */
    Trunk.TrunkName  /* column-label "Name of CGR"    */  format "x(16)"
    Trunk.OpCode     column-label "OpCode"             format "x(6)"
    Operator.OperName   /* column-label "Operators name" */  format "x(18)"
    Trunk.TrInt      column-label "D"                  format "D/I" space(1)
    Trunk.TrIn       column-label "B"                  format "Y/N"
WITH
    width 80 OVERLAY scroll 1 15 DOWN COLOR value(cfc)
    title color value(ctc) " " + ynimi + " CIRCUIT GROUPS "
    + string(pvm,"99-99-99") + " " FRAME sel.

form
    h-ex-code        label "Exchange code .." ExName NO-LABEL SKIP
    h-tr-code        label "Number of CGR .."
    HELP "Number of Circuit Group Route"
    Trunk.TrunkName  NO-LABEL  AT 28
    Trunk.OpCode  label "Operators code ." Operator.OperName NO-LABEL SKIP
    Trunk.TrInt   label "Direct ........." format "Direct/Indirect"
       help "Is this CGR direct connection ?"                  SKIP
    Trunk.TrIn    label "Billed trunk ..." format "Y/N"
       help "Is traffic analysed or allways Unbilled (Y/N) ?"        skip(1)
    Trunk.Memo[1]       label "Memo ..........." SKIP
    Trunk.Memo[2]    NO-LABEL AT 19              SKIP
    Trunk.Memo[3]    NO-LABEL AT 19              SKIP
    Trunk.Memo[4]    NO-LABEL AT 19              SKIP
    Trunk.Memo[5]    NO-LABEL AT 19              SKIP 
WITH
    OVERLAY ROW 4 centered COLOR value(cfc) TITLE COLOR value(ctc)
    fr-header WITH side-labels FRAME lis.

form /* Trunkgrupp search WITH FIELD ExCode */
    h-ex-code
    help "Give exchange code"
    with row 4 col 2 title color value(ctc) " FIND EXCHANGE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME h-f1.

form /* Trunkgrupp search WITH FIELD /* x */ Operator */
    h-op-code
    help "Give operator code"
    with row 4 col 2 title color value(ctc) " FIND OPERATOR "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME h-f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST Trunk
/* search condition */ no-lock no-error.
IF AVAILABLE Trunk THEN ASSIGN
   memory     = recid(Trunk)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No trunk groups available !" VIEW-AS ALERT-BOX.
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
       if order = 1 then put screen row 19 col 30 " Order by exchange ".
       if order = 2 then put screen row 19 col 30 " Order by operator ".
    END.

   IF must-add THEN DO:  /* Trunk -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSAction:
           CREATE Trunk.

           assign h-ex-code = "" h-op-code = "" h-tr-code = "".
           UPDATE 
              h-ex-code 
              h-tr-code 
              Trunk.TrunkName 
              Trunk.OpCode 
              Trunk.TrInt
              Trunk.TrIn
              Trunk.memo 
           WITH FRAME lis EDITING:
              READKEY.
              IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
                 HIDE MESSAGE.
                 if frame-field = "h-ex-code" THEN DO:
                    ASSIGN FRAME lis h-ex-code.
                    if h-ex-code = "" THEN UNDO add-new, LEAVE add-new.
                    FIND Exchange where Exchange.ExCode = h-ex-code
                    no-lock no-error.
                    IF NOT AVAIL Exchange THEN DO:
                       BELL.
                       message "Unknown exchange '" + h-ex-code + "' !".
                       NEXT.
                    END.
                    DISP Exchange.ExName WITH FRAME lis.
                 END.

                 else if frame-field = "h-tr-code" THEN DO:
                    ASSIGN FRAME lis h-tr-code.
                    if h-tr-code = "" THEN DO:
                       NEXT-PROMPT h-ex-code.
                       NEXT.
                    END.
                 END.

                 if lookup(frame-field,"h-ex-code,h-tr-code") > 0 THEN DO:
                    FIND FIRST xxtrunk where
                               xxtrunk.ExCode = h-ex-code AND
                               xxtrunk.TrunkCode = h-tr-code
                    no-lock no-error.
                    IF AVAIL xxtrunk THEN DO:
                       BELL. MESSAGE
                       "This trunkgroup already exists in this exchange !".
                       NEXT.
                    END.
                 END.

                 if frame-field = "OpCode" THEN DO:
                    ASSIGN FRAME lis Trunk.OpCode.
                    if Trunk.OpCode = "" THEN DO:
                       NEXT-PROMPT Trunk.TrunkName.
                       NEXT.
                    END.
                    FIND Operator where Operator.Operator = Trunk.OpCode
                    no-lock no-error.
                    IF NOT AVAIL Operator THEN DO:
                       BELL.
                       message "Unknown operator" + Trunk.OpCode + "' !".
                       NEXT.
                    END.
                    DISP Operator.OperName WITH FRAME lis.
                 END.

              END.
              APPLY LASTKEY.
           END.


           ASSIGN
           Trunk.ExCode = h-ex-code
           Trunk.TrunkCode = h-tr-code
           memory          = recid(Trunk)
           xrecid          = memory.
        END.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTrunk).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST Trunk
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE Trunk THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND Trunk where recid(Trunk) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE Trunk THEN DO:

              FIND Exchange of Trunk no-lock no-error.
              FIND Operator where Operator.Operator = Trunk.OpCode
              no-lock no-error.
              DISPLAY
                 Trunk.ExCode
                 Exchange.ExName when AVAIL Exchange
                 "?" when NOT AVAIL Exchange @ Exchange.ExName
                 Trunk.ExCode
                 Trunk.TrunkCode
                 Trunk.TrunkName
                 Trunk.OpCode
                 Operator.OperName when AVAIL Operator
                 "" when NOT AVAIL Operator @ Operator.OperName
                 Trunk.TrInt 
                 Trunk.TrIn
              /* sd */.
              rtab[FRAME-LINE] = recid(Trunk).
              IF order = 1 THEN FIND NEXT Trunk
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT Trunk USE-INDEX Operator
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND NEXT Trunk USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND NEXT Trunk USE-INDEX index-4
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
        ufk[1]= 865 ufk[2]= 770 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW Trunk.ExCode ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) Trunk.ExCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Trunk.OpCode ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) Trunk.OpCode WITH FRAME sel.
      END.
/*    IF order = 3 THEN DO:
        CHOOSE ROW Trunk.?? ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) Trunk.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW Trunk.??  ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) Trunk.? WITH FRAME sel.
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
        FIND Trunk where recid(Trunk) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev Trunk
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev Trunk USE-INDEX Operator
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev Trunk USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev Trunk USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE Trunk THEN
              ASSIGN firstline = i memory = recid(Trunk).
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
           FIND Trunk where recid(Trunk) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev Trunk
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev Trunk USE-INDEX Operator
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev Trunk USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev Trunk USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE Trunk THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.

              FIND Exchange of Trunk no-lock no-error.
              FIND Operator where Operator.Operator = Trunk.OpCode
              no-lock no-error.
              DISPLAY
                 Trunk.ExCode
                 Exchange.ExName when AVAIL Exchange
                 "" when NOT AVAIL Exchange @ Exchange.ExName
                 Trunk.ExCode
                 Trunk.TrunkCode
                 Trunk.TrunkName
                 Trunk.OpCode
                 Operator.OperName when AVAIL Operator
                 "" when NOT AVAIL Operator @ Operator.OperName
                 Trunk.TrInt
                 Trunk.TrIn
                 /* sd */.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(Trunk)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND Trunk where recid(Trunk) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT Trunk
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT Trunk USE-INDEX Operator
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND NEXT Trunk USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND NEXT Trunk USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE Trunk THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.

              FIND Exchange of Trunk no-lock no-error.
              FIND Operator where Operator.Operator = Trunk.OpCode
              no-lock no-error.
              DISPLAY
                 Trunk.ExCode
                 Exchange.ExName when AVAIL Exchange
                 "" when NOT AVAIL Exchange @ Exchange.ExName
                 Trunk.ExCode
                 Trunk.TrunkCode
                 Trunk.TrunkName
                 Trunk.OpCode
                 Operator.OperName when AVAIL Operator
                 "" when NOT AVAIL Operator @ Operator.OperName
                 Trunk.TrInt
                 Trunk.TrIn
                 /* sd */.

              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(Trunk).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND Trunk where recid(Trunk) = memory no-lock no-error.
        IF order = 1 THEN FIND prev Trunk
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev Trunk USE-INDEX Operator
        /* search condition */ no-lock no-error.
   /*    ELSE IF order = 3 THEN FIND prev Trunk USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND prev Trunk USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE Trunk THEN DO:
           memory = recid(Trunk).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev Trunk
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev Trunk USE-INDEX Operator
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND prev Trunk USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND prev Trunk USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
              IF AVAILABLE Trunk THEN memory = recid(Trunk).
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
           FIND Trunk where recid(Trunk) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       h-ex-code = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE h-ex-code WITH FRAME h-f1.
       HIDE FRAME h-f1 no-pause.
       if h-ex-code <> "" THEN DO:
          FIND FIRST Trunk where Trunk.ExCode >= h-ex-code
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE Trunk THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  nntrunk/ex-code was found */
          ASSIGN order = 1 memory = recid(Trunk) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       h-op-code = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE h-op-code WITH FRAME h-f2.
       HIDE FRAME h-f2 no-pause.
       if h-op-code <> "" THEN DO:
          FIND FIRST Trunk where Trunk.OpCode >= h-op-code
          USE-INDEX Operator /* search condition */ no-lock no-error.
          IF NOT AVAILABLE Trunk THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  Trunk.OpCode was found */
          ASSIGN order = 2 memory = recid(Trunk) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     else if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* removal */

       delline = FRAME-LINE.
       FIND Trunk where recid(Trunk) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
         Trunk.ExCode Exchange.ExName Trunk.ExCode Trunk.TrunkCode
         Trunk.TrunkName Trunk.OpCode Operator.OperName
         Trunk.TrInt Trunk.TrIn.


       IF order = 1 THEN FIND NEXT Trunk
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT Trunk USE-INDEX Operator
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND NEXT Trunk USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND NEXT Trunk USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       IF AVAILABLE Trunk THEN memory = recid(Trunk).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND Trunk where recid(Trunk) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev Trunk
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev Trunk USE-INDEX Operator
          /* search condition */ no-lock no-error.
     /*    ELSE IF order = 3 THEN FIND prev Trunk USE-INDEX index-3
          /* search condition */ no-lock no-error.
          ELSE IF order = 4 THEN FIND prev Trunk USE-INDEX index-4
          /* search condition */ no-lock no-error.   */
          IF AVAILABLE Trunk THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(Trunk).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND Trunk where recid(Trunk) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "DO YOU REALLY WANT TO DELETE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
         Trunk.ExCode Exchange.ExName Trunk.ExCode Trunk.TrunkCode
         Trunk.TrunkName Trunk.OpCode Operator.OperName
         Trunk.TrInt Trunk.TrIn.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTrunk).

           DELETE Trunk.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST Trunk
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
     DO WITH FRAME lis TRANSACTION ON ENDKEY UNDO, LEAVE:
       /* change */
       {Syst/uright2.i}
       FIND Trunk where recid(Trunk) = rtab[frame-line(sel)]
       exclusive-lock.
       FIND Exchange of Trunk no-lock no-error.
       FIND Operator where Operator.Operator = Trunk.OpCode no-lock no-error.

       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor.

       DISPLAY
         Trunk.ExCode @ h-ex-code
         Trunk.TrunkCode @ h-tr-code
         Trunk.OpCode
         Exchange.ExName when AVAIL Exchange
         "" when NOT AVAIL Exchange @ Exchange.ExName
         Operator.OperName when AVAIL Operator
         "" when NOT AVAIL Operator @ Operator.OperName
         Trunk.TrInt Trunk.TrIn
         Trunk.Trunkname Trunk.Memo
       WITH FRAME lis.

       IF lcRight = "RW" THEN DO:

          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTrunk).

          UPDATE
             Trunk.TrunkName Trunk.OpCode
             Trunk.TrInt Trunk.TrIn Trunk.Memo 
          WITH FRAME lis EDITING.
             READKEY.
             IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
                HIDE MESSAGE.

                if frame-field = "OpCode" THEN DO:
                   ASSIGN FRAME lis Trunk.OpCode.
                   if Trunk.OpCode = "" THEN DO:
                      NEXT-PROMPT Trunk.TrunkName.
                      NEXT.
                   END.
                   FIND Operator where Operator.Operator = Trunk.OpCode
                   no-lock no-error.
                   IF NOT AVAIL Operator THEN DO:
                      BELL.
                      message "Unknown operator '" + Trunk.OpCode + "' !".
                      NEXT.
                   END.
                   DISP Operator.OperName WITH FRAME lis.
                END.

             END.
             APPLY LASTKEY.
          END.

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTrunk).

       END.
       ELSE PAUSE.
       HIDE FRAME lis no-pause.
       DISPLAY 
          Trunk.TrunkName 
          Trunk.OpCode
          Trunk.TrInt 
          Trunk.TrIn
       WITH FRAME sel.
       xrecid = recid(Trunk).
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST Trunk
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST Trunk USE-INDEX Operator
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND FIRST Trunk USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND FIRST Trunk USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(Trunk) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST Trunk
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST Trunk USE-INDEX Operator
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND LAST Trunk USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND LAST Trunk USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(Trunk) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

