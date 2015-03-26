/* -----------------------------------------------
  MODULE .......: NNMYSE.P
  FUNCTION .....: BROWSE SALESMEN / HELP
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 10.02.1999
  changePVM ....: 24.02.99 /pt minor changes
                  15.03.02 lp changed "repeat with frame alku" -->
                              "repeat with frame alku on endkey undo, return"
                  16.09.03/aam brand            
  Version ......: M15
  ------------------------------------------------------ */

{commali.i} 

DEF shared VAR siirto AS CHAR.

DEF VAR Salesman  LIKE Salesman.Salesman  NO-UNDO.
DEF VAR SmName LIKE Salesman.SmName NO-UNDO.
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
DEF VAR haettava AS LOG init TRUE      NO-UNDO.
DEF VAR numero AS LOG NO-UNDO.
DEF VAR rtab AS RECID EXTENT 24      NO-UNDO.
DEF VAR i AS INT NO-UNDO.

DEF VAR xrecid AS RECID.

form
    Salesman.SmName  format "x(30)" column-label "Salesman's Name"
    Salesman.Salesman                 column-label "SmCode" 
WITH centered OVERLAY scroll 1 10 DOWN ROW 6
    COLOR value(cfc)
    title color value(ctc) 
    " Salesman BROWSER: FROM (" + gcBrand + ") '"  + Salesman + "' " 
    FRAME sel.


form
    Salesman no-label format "x(30)"
    help "Enter (Name + ENTER) or (CODE + HOME)"
    SKIP
    "How are You searching ?"  SKIP
    "  - By Salesman's Name (ENTER)" SKIP
    "  - By Salesman's CODE (HOME)"  SKIP

with row 1 centered overlay title " SEEK SALESMEN " FRAME alku.


cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
   FIND FIRST Salesman USE-INDEX Salesman WHERE 
      Salesman.Brand = gcBrand NO-LOCK no-error.
   IF NOT AVAIL Salesman THEN DO:
      BELL.
      message "No products at all - press ENTER !".
      PAUSE no-message.
      RETURN.
   END.


   ASSIGN
   haettava = TRUE xrecid = ? delline = 0 ufkey = TRUE firstline = 0 siirto = ?.

LOOP:
repeat WITH FRAME sel:

    IF haettava THEN DO:

       ASSIGN haettava = FALSE nro = FALSE.
       PAUSE 0 no-message.
alku:  repeat WITH FRAME alku ON ENDKEY UNDO, RETURN:
          ehto = 9. RUN ufkey. ufkey = TRUE.
          UPDATE Salesman WITH FRAME alku EDITING:
             READKEY. nap = keylabel(LASTKEY).
             /* onko painettu home */
             if lookup(nap,"home") > 0 THEN 
             assign nro = true nap = "enter".
             APPLY keycode(nap).
          END.

          if Salesman = "" THEN LEAVE LOOP.

          IF NOT nro THEN DO:
             FIND FIRST Salesman where 
                        Salesman.Brand   = gcBrand AND
                        Salesman.SmName >= Salesman
             no-lock no-error.
             order = 1.
          END.
          ELSE DO:
             FIND FIRST Salesman where 
                        Salesman.Brand     = gcBrand AND
                        Salesman.Salesman >= Salesman
             no-lock no-error.
             order = 2.
          END.

          IF NOT AVAIL Salesman THEN DO:
             BELL.
             message "CAN'T FIND !".
             NEXT alku.
          END.
          ASSIGN memory = recid(Salesman) must-print = TRUE.
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
         FIND Salesman where recid(Salesman) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE Salesman THEN DO:
               DISPLAY Salesman.Salesman Salesman.SmName.
               rtab[FRAME-LINE] = recid(Salesman).
               IF order = 2 THEN FIND NEXT Salesman
               USE-INDEX Salesman WHERE 
                  Salesman.Brand = gcBrand NO-LOCK no-error.
               ELSE IF order = 1 THEN FIND NEXT Salesman
               USE-INDEX SmName WHERE 
                  Salesman.Brand = gcBrand NO-LOCK no-error.
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
         CHOOSE ROW Salesman.Salesman ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Salesman.Salesman WITH FRAME sel.
      END.
      ELSE IF order = 1 THEN DO:
         CHOOSE ROW Salesman.SmName ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Salesman.SmName WITH FRAME sel.
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
         FIND Salesman where recid(Salesman) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 2 THEN FIND prev Salesman
            USE-INDEX Salesman WHERE Salesman.Brand = gcBrand NO-LOCK no-error.
            ELSE IF order = 1 THEN FIND prev Salesman
            USE-INDEX SmName WHERE Salesman.Brand = gcBrand NO-LOCK no-error.
            IF AVAILABLE Salesman THEN
               ASSIGN firstline = i memory = recid(Salesman).
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
            FIND Salesman where recid(Salesman) = rtab[1] no-lock.
            IF order = 2 THEN FIND prev Salesman
            USE-INDEX Salesman WHERE Salesman.Brand = gcBrand NO-LOCK no-error.
            ELSE IF order = 1 THEN FIND prev Salesman
            USE-INDEX SmName WHERE Salesman.Brand = gcBrand NO-LOCK no-error.
            IF NOT AVAILABLE Salesman THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               DISPLAY Salesman.Salesman Salesman.SmName.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(Salesman)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND Salesman where recid(Salesman) = rtab[FRAME-DOWN] no-lock .
            IF order = 2 THEN FIND NEXT Salesman
            USE-INDEX Salesman WHERE Salesman.Brand = gcBrand NO-LOCK no-error.
            ELSE IF order = 1 THEN FIND NEXT Salesman
            USE-INDEX SmName WHERE Salesman.Brand = gcBrand NO-LOCK no-error.
            IF NOT AVAILABLE Salesman THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               DISPLAY Salesman.Salesman Salesman.SmName.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(Salesman).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND Salesman where recid(Salesman) = memory no-lock no-error.
         IF order = 2 THEN FIND prev Salesman
         USE-INDEX Salesman WHERE Salesman.Brand = gcBrand NO-LOCK no-error.
         ELSE IF order = 1 THEN FIND prev Salesman
         USE-INDEX SmName WHERE Salesman.Brand = gcBrand NO-LOCK no-error.
         IF AVAILABLE Salesman THEN DO:
            memory = recid(Salesman).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 2 THEN FIND prev Salesman
               USE-INDEX Salesman WHERE 
                  Salesman.Brand = gcBrand NO-LOCK no-error.
               ELSE IF order = 1 THEN FIND prev Salesman
               USE-INDEX SmName WHERE 
                  Salesman.Brand = gcBrand NO-LOCK no-error.
               IF AVAILABLE Salesman THEN memory = recid(Salesman).
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
            FIND Salesman where recid(Salesman) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     else if lookup(nap,"5,f5,enter,return") > 0 THEN DO: /* valinta */
        FIND Salesman where recid(Salesman) = rtab[FRAME-LINE] no-lock.
        siirto = string(Salesman.Salesman).
        LEAVE LOOP.
     END.


     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 2 THEN FIND FIRST Salesman
        USE-INDEX Salesman WHERE Salesman.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 1 THEN FIND FIRST Salesman
        USE-INDEX SmName WHERE Salesman.Brand = gcBrand NO-LOCK no-error.
        ASSIGN memory = recid(Salesman) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 2 THEN FIND LAST Salesman
        USE-INDEX Salesman WHERE Salesman.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 1 THEN FIND LAST Salesman
        USE-INDEX SmName WHERE Salesman.Brand = gcBrand NO-LOCK no-error.
        ASSIGN memory = recid(Salesman) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN DO:
        haettava = TRUE.
        HIDE FRAME sel no-pause.
        NEXT LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
HIDE FRAME alku no-pause.
si-recid = xrecid.

