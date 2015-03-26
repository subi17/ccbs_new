/* -----------------------------------------------
  MODULE .......: NNOPSE.P
  FUNCTION .....: BROWSE OPERATORS / HELP
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 08.12.1999
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{commali.i} 

DEF shared VAR siirto AS CHAR.

DEF VAR Operator  LIKE Operator.Operator  NO-UNDO.
DEF VAR OperName LIKE Operator.OperName NO-UNDO.
DEF VAR firstline AS INT NO-UNDO.
DEF VAR order AS INT NO-UNDO.
DEF VAR ex-order AS INT NO-UNDO.
DEF VAR memory   AS RECID              NO-UNDO.
def var line     as int format "99"    NO-UNDO.
DEF VAR delline    AS INT                NO-UNDO.
DEF VAR must-print AS LOG            NO-UNDO.
DEF VAR ufkey        AS LOG            NO-UNDO.
DEF VAR fr-header AS CHAR.

DEF VAR nro AS LOG NO-UNDO.
DEF VAR seekit AS LOG init TRUE      NO-UNDO.
DEF VAR rtab AS RECID EXTENT 24      NO-UNDO.
DEF VAR i AS INT NO-UNDO.

DEF VAR xrecid AS RECID.

form
    Operator.OperName  format "x(30)" column-label "Operators Name"
    Operator.Operator                 column-label "OpCode" 
WITH centered OVERLAY scroll 1 10 DOWN ROW 6
    COLOR value(cfc)
    title color value(ctc) " OPERATOR BROWSER: SEEK FROM '"  + Operator + "' " FRAME sel.


form
    Operator no-label format "x(30)"
    help "Enter (Name + ENTER) or (CODE + HOME)"
    SKIP
    "How are You searching ?"  SKIP
    "  - By nnoper's Name (ENTER)" SKIP
    "  - By nnoper's CODE (HOME)"  SKIP

with row 1 centered overlay title " SEEK OPERATORS " FRAME alku.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
   FIND FIRST Operator USE-INDEX Operator no-lock no-error.
   IF NOT AVAIL Operator THEN DO:
      BELL.
      message "No products at all - press ENTER !".
      PAUSE no-message.
      RETURN.
   END.

   ASSIGN
      seekit  = TRUE 
      xrecid    = ? 
      delline   = 0 
      ufkey     = TRUE 
      firstline = 0 
      siirto    = ?.

LOOP:
repeat WITH FRAME sel:

    IF seekit THEN DO:

       ASSIGN seekit = FALSE nro = FALSE.
       PAUSE 0 no-message.
alku:  repeat WITH FRAME alku:
          ehto = 9. RUN ufkey. ufkey = TRUE.
          UPDATE Operator WITH FRAME alku EDITING:
             READKEY. nap = keylabel(LASTKEY).
             /* onko painettu home */
             if lookup(nap,"home") > 0 THEN 
             assign nro = true nap = "enter".
             APPLY keycode(nap).
          END.

          if Operator = "" THEN LEAVE LOOP.

          IF NOT nro THEN DO:
             FIND FIRST Operator where Operator.OperName >= Operator
             no-lock no-error.
             order = 1.
          END.
          ELSE DO:
             FIND FIRST Operator where Operator.Operator >= Operator
             no-lock no-error.
             order = 2.
          END.

          IF NOT AVAIL Operator THEN DO:
             BELL.
             message "CAN'T FIND !".
             NEXT alku.
          END.
          ASSIGN memory = recid(Operator) must-print = TRUE.
          view FRAME sel.
          LEAVE.
       END. /* repeat */
    END.

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 2 then put screen row 19 col 30 " By Code ".
       if order = 1 then put screen row 19 col 30 " By Name ".
    END.


print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND Operator where recid(Operator) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE Operator THEN DO:
               DISPLAY Operator.Operator Operator.OperName.
               rtab[FRAME-LINE] = recid(Operator).
               IF order = 2 THEN FIND NEXT Operator
               USE-INDEX Operator no-lock no-error.
               ELSE IF order = 1 THEN FIND NEXT Operator
               USE-INDEX OperName no-lock no-error.
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
         ufk[1]= 0   ufk[2]= 0   ufk[3]= 0 ufk[4]= 0
         ufk[5]= 11 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 2 THEN DO:
         CHOOSE ROW Operator.Operator ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Operator.Operator WITH FRAME sel.
      END.
      ELSE IF order = 1 THEN DO:
         CHOOSE ROW Operator.OperName ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Operator.OperName WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 3 THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 2.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND Operator where recid(Operator) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 2 THEN FIND prev Operator
            USE-INDEX Operator no-lock no-error.
            ELSE IF order = 1 THEN FIND prev Operator
            USE-INDEX OperName no-lock no-error.
            IF AVAILABLE Operator THEN
               ASSIGN firstline = i memory = recid(Operator).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? THEN DO:
         BELL.
         message "You are on a empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND Operator where recid(Operator) = rtab[1] no-lock.
            IF order = 2 THEN FIND prev Operator
            USE-INDEX Operator no-lock no-error.
            ELSE IF order = 1 THEN FIND prev Operator
            USE-INDEX OperName no-lock no-error.
            IF NOT AVAILABLE Operator THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               DISPLAY Operator.Operator Operator.OperName.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(Operator)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND Operator where recid(Operator) = rtab[FRAME-DOWN] no-lock .
            IF order = 2 THEN FIND NEXT Operator
            USE-INDEX Operator no-lock no-error.
            ELSE IF order = 1 THEN FIND NEXT Operator
            USE-INDEX OperName no-lock no-error.
            IF NOT AVAILABLE Operator THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               DISPLAY Operator.Operator Operator.OperName.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(Operator).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND Operator where recid(Operator) = memory no-lock no-error.
         IF order = 2 THEN FIND prev Operator
         USE-INDEX Operator no-lock no-error.
         ELSE IF order = 1 THEN FIND prev Operator
         USE-INDEX OperName no-lock no-error.
         IF AVAILABLE Operator THEN DO:
            memory = recid(Operator).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 2 THEN FIND prev Operator
               USE-INDEX Operator no-lock no-error.
               ELSE IF order = 1 THEN FIND prev Operator
               USE-INDEX OperName no-lock no-error.
               IF AVAILABLE Operator THEN memory = recid(Operator).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND Operator where recid(Operator) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     else if lookup(nap,"5,f5,enter,return") > 0 THEN DO: /* valinta */
        FIND Operator where recid(Operator) = rtab[FRAME-LINE] no-lock.
        siirto = string(Operator.Operator).
        LEAVE LOOP.
     END.


     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 2 THEN FIND FIRST Operator
        USE-INDEX Operator no-lock no-error.
        ELSE IF order = 1 THEN FIND FIRST Operator
        USE-INDEX OperName no-lock no-error.
        ASSIGN memory = recid(Operator) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 2 THEN FIND LAST Operator
        USE-INDEX Operator no-lock no-error.
        ELSE IF order = 1 THEN FIND LAST Operator
        USE-INDEX OperName no-lock no-error.
        ASSIGN memory = recid(Operator) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN DO:
        seekit = TRUE.
        HIDE FRAME sel no-pause.
        NEXT LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
HIDE FRAME alku no-pause.
si-recid = xrecid.

