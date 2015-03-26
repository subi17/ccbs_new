/* -----------------------------------------------
  MODULE .......: NNSNYP.P
  FUNCTION .....: Uppdatering av svenska nummerp
  APPLICATION ..: NN
  AUTHOR .......: NN
  CREATED ......: 03-09-97
  MODIFIED .....: 23-02-99 vk in english
                  23-12-99 kl header into National
                  30.10.02 jr EventLog
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{eventval.i} 

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR h-rn-rnr   LIKE NumPlan.AreaCode       NO-UNDO.
DEF VAR h-op-code  LIKE NumPlan.Operator      NO-UNDO.
DEF VAR xrecid     AS   RECID                        init ?.
DEF VAR firstline  AS   INT                 NO-UNDO  init 0.
DEF VAR order      AS   INT                 NO-UNDO  init 1.
DEF VAR ordercount AS   INT                 NO-UNDO  init 2.
DEF VAR ufkey      AS   LOG                 NO-UNDO  init TRUE.
DEF VAR delline    AS   INT                 NO-UNDO  init 0.
DEF VAR ex-order   AS   INT                 NO-UNDO.
DEF VAR memory     AS   RECID               NO-UNDO.
def var line       as   int format "99"     NO-UNDO.
DEF VAR must-print AS   LOG                 NO-UNDO.
DEF VAR must-add   AS   LOG                 NO-UNDO.
DEF VAR fr-header  AS   CHAR                NO-UNDO.
DEF VAR rtab       AS   RECID EXTENT 24     NO-UNDO.
DEF VAR i          AS   INT                 NO-UNDO.
def var ok         as   log format "Yes/No" NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhNumPlan AS HANDLE NO-UNDO.
   lhNumPlan = BUFFER NumPlan:HANDLE.
   RUN StarEventInitialize(lhNumPlan).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhNumPlan).
   END.
END.


form
    NumPlan.AreaCode        column-label "Area"  
    NumPlan.Prefix     /* column-label "Serie"      */
    AreaCode.AreaName    /* column-label "Ort"        */  format "x(16)"
    NumPlan.State  /* column-label "Status"     */
    NumPlan.NumLength
    NumPlan.Operator    /* column-label "Op.code"    */
    Operator.OperName    /* column-label "Oper. name" */  format "x(16)"
    /* sd */          /* COLUMN-LABEL FORMAT */
    WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " National numberingplan "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    NumPlan.AreaCode   label "Areacode ............"
    NumPlan.Prefix     label "Numberserie ........."
    NumPlan.State      label "Status .............."
    NumPlan.NumLength  label "Length .............."
    NumPlan.Operator   label "Operator code ......."
    Operator.OperName  label "Operators name ......"
WITH
    OVERLAY ROW 4 centered COLOR value(cfc) TITLE COLOR value(ctc)
    fr-header WITH side-labels 1 columns FRAME lis.

form /* Nummerserie search WITH FIELD AreaCode */
    h-rn-rnr
    help "Give areacode "
    with row 4 col 2 title color value(ctc) " FIND AREACODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME h-f1.

form /* Nummerserie search WITH FIELD Operator */
    h-op-code
    help "Give operator code"
    with row 4 col 2 title color value(ctc) " FIND OPERATOR "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME h-f2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST NumPlan
/* search condition */ no-lock no-error.
IF AVAILABLE NumPlan THEN ASSIGN
   memory       = recid(NumPlan)
   must-print = TRUE
   must-add    = FALSE.
ELSE ASSIGN
   memory       = ?
   must-print = FALSE
   must-add    = TRUE.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order of Tel.numbers ".
       if order = 2 then put screen row 19 col 30 " Order of operators   ".
    END.

   IF must-add THEN DO:  /* NumPlan -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN ufkey.
        DO TRANSAction:
           PROMPT-FOR NumPlan.AreaCode
           VALIDATE
              (NumPlan.AreaCode = "" OR
              NOT can-find(NumPlan using  NumPlan.AreaCode),
              "Number series" + string(INPUT NumPlan.AreaCode) +
              " already exists !").
           if input NumPlan.AreaCode = "" THEN LEAVE add-new.
           CREATE NumPlan.
           ASSIGN
           NumPlan.AreaCode = INPUT FRAME lis NumPlan.AreaCode.
           UPDATE NumPlan.Operator
                  /* sd */
                  /* ld */.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhNumPlan).       
           ASSIGN
           memory = recid(NumPlan)
           xrecid = memory.
        END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST NumPlan
      /* search condition */ no-lock no-error.
      IF NOT AVAILABLE NumPlan THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND NumPlan where recid(NumPlan) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE NumPlan THEN DO:
              FIND Operator where Operator.Operator = NumPlan.Operator
              no-lock no-error.
              FIND AreaCode where AreaCode.AreaCode = NumPlan.AreaCode no-lock no-error.
              DISPLAY
              NumPlan.AreaCode NumPlan.Operator
              NumPlan.Prefix NumPlan.State NumPlan.NumLength
              AreaCode.AreaName      when     AVAIL AreaCode
              "" when NOT AVAIL AreaCode @ AreaCode.AreaName
              Operator.OperName      when     AVAIL Operator
              "" when NOT AVAIL Operator @ Operator.OperName
                 /* sd */.
              rtab[FRAME-LINE] = recid(NumPlan).
              IF order = 1 THEN FIND NEXT NumPlan
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT NumPlan USE-INDEX Operator
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND NEXT NumPlan USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND NEXT NumPlan USE-INDEX index-4
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
        ufk[1]= 701 ufk[2]= 770 ufk[3]= 0 ufk[4]= 0
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

       /* toist.  */ ufk[5] = 0. ufk[6] = 0.



        RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW NumPlan.AreaCode ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) NumPlan.AreaCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW NumPlan.Operator ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) NumPlan.Operator WITH FRAME sel.
      END.
/*    IF order = 3 THEN DO:
        CHOOSE ROW NumPlan.?? ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) NumPlan.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW NumPlan.??  ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) NumPlan.? WITH FRAME sel.
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
        FIND NumPlan where recid(NumPlan) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev NumPlan
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev NumPlan USE-INDEX Operator
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev NumPlan USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev NumPlan USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE NumPlan THEN
              ASSIGN firstline = i memory = recid(NumPlan).
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
           FIND NumPlan where recid(NumPlan) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev NumPlan
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev NumPlan USE-INDEX Operator
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev NumPlan USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev NumPlan USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE NumPlan THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.

              FIND Operator where Operator.Operator = NumPlan.Operator
              no-lock no-error.
              FIND AreaCode where AreaCode.AreaCode = NumPlan.AreaCode no-lock no-error.
              DISPLAY
              NumPlan.AreaCode NumPlan.Operator
              NumPlan.Prefix NumPlan.State NumPlan.NumLength
              AreaCode.AreaName      when     AVAIL AreaCode
              "" when NOT AVAIL AreaCode @ AreaCode.AreaName
              Operator.OperName      when     AVAIL Operator
              "" when NOT AVAIL Operator @ Operator.OperName
                 /* sd */.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(NumPlan)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND NumPlan where recid(NumPlan) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT NumPlan
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT NumPlan USE-INDEX Operator
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND NEXT NumPlan USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND NEXT NumPlan USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE NumPlan THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.

              FIND Operator where Operator.Operator = NumPlan.Operator
              no-lock no-error.
              FIND AreaCode where AreaCode.AreaCode = NumPlan.AreaCode no-lock no-error.
              DISPLAY
              NumPlan.AreaCode NumPlan.Operator
              NumPlan.Prefix NumPlan.State NumPlan.NumLength
              AreaCode.AreaName      when     AVAIL AreaCode
              "" when NOT AVAIL AreaCode @ AreaCode.AreaName
              Operator.OperName      when     AVAIL Operator
              "" when NOT AVAIL Operator @ Operator.OperName
                 /* sd */.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(NumPlan).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND NumPlan where recid(NumPlan) = memory no-lock no-error.
        IF order = 1 THEN FIND prev NumPlan
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev NumPlan USE-INDEX Operator
        /* search condition */ no-lock no-error.
   /*    ELSE IF order = 3 THEN FIND prev NumPlan USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND prev NumPlan USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE NumPlan THEN DO:
           memory = recid(NumPlan).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev NumPlan
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev NumPlan USE-INDEX Operator
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND prev NumPlan USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND prev NumPlan USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
              IF AVAILABLE NumPlan THEN memory = recid(NumPlan).
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
           FIND NumPlan where recid(NumPlan) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       h-rn-rnr = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE h-rn-rnr WITH FRAME h-f1.
       HIDE FRAME h-f1 no-pause.
       if h-rn-rnr <> "" THEN DO:
          FIND FIRST NumPlan where NumPlan.AreaCode >= h-rn-rnr
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE NumPlan THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  nnsnum/rn-rnr was found */
          ASSIGN order = 1 memory = recid(NumPlan) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       h-op-code = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE h-op-code WITH FRAME h-f2.
       HIDE FRAME h-f2 no-pause.
       if h-op-code <> "" THEN DO:
          FIND FIRST NumPlan where NumPlan.Operator >= h-op-code
          USE-INDEX Operator /* search condition */ no-lock no-error.
          IF NOT AVAILABLE NumPlan THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  nnsnum/op-code was found */
          ASSIGN order = 2 memory = recid(NumPlan) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */
 /*
     if lookup(nap,"5,f5") > 0 THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.
 */

 /*
     else if lookup(nap,"6,f6") > 0 THEN DO TRANSAction:  /* removal */
       delline = FRAME-LINE.
       FIND NumPlan where recid(NumPlan) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
       NumPlan.AreaCode NumPlan.Operator AreaCode.AreaName .

       IF order = 1 THEN FIND NEXT NumPlan
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND NEXT NumPlan USE-INDEX Operator
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND NEXT NumPlan USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND NEXT NumPlan USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       IF AVAILABLE NumPlan THEN memory = recid(NumPlan).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND NumPlan where recid(NumPlan) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          IF order = 1 THEN FIND prev NumPlan
          /* search condition */ no-lock no-error.
          ELSE IF order = 2 THEN FIND prev NumPlan USE-INDEX Operator
          /* search condition */ no-lock no-error.
     /*    ELSE IF order = 3 THEN FIND prev NumPlan USE-INDEX index-3
          /* search condition */ no-lock no-error.
          ELSE IF order = 4 THEN FIND prev NumPlan USE-INDEX index-4
          /* search condition */ no-lock no-error.   */
          IF AVAILABLE NumPlan THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(NumPlan).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND NumPlan where recid(NumPlan) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "DO YOU REALLY WANT TO DELETE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
       NumPlan.AreaCode NumPlan.Operator /* sd */.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhNumPlan).
           DELETE NumPlan.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST NumPlan
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
*/

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSAction:
       /* change */
       {uright2.i}
       FIND NumPlan where recid(NumPlan) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.
       cfc = "lis". RUN ufcolor.
       FIND Operator where Operator.Operator = 
          NumPlan.Operator no-lock no-error.
       FIND AreaCode where AreaCode.AreaCode = 
          NumPlan.AreaCode no-lock no-error.
       DISPLAY
          NumPlan.AreaCode NumPlan.Operator
          NumPlan.Prefix NumPlan.State NumPlan.NumLength
          Operator.OperName      when     AVAIL Operator
          "" when NOT AVAIL Operator @ Operator.OperName
          AreaCode.AreaName      when     AVAIL AreaCode
          "" when NOT AVAIL AreaCode @ AreaCode.AreaName
       WITH FRAME lis.


       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhNumPlan).

       update NumPlan.Operator  validate (Operator = "" OR
       can-find(Operator where Operator.Operator = INPUT FRAME lis  
                                NumPlan.Operator) ,"Unknown operator !").

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhNumPlan).

       HIDE FRAME lis no-pause.
       DISPLAY NumPlan.Operator
               /* sd */
       WITH FRAME sel.
       xrecid = recid(NumPlan).
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST NumPlan
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST NumPlan USE-INDEX Operator
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND FIRST NumPlan USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND FIRST NumPlan USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(NumPlan) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST NumPlan
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST NumPlan USE-INDEX Operator
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND LAST NumPlan USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND LAST NumPlan USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN memory = recid(NumPlan) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

