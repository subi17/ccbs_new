/* -----------------------------------------------
  MODULE .......: NNPGSE.P
  FUNCTION .....: BillCode GROUP BROWSE / HELP
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 21-01-96
  MODIFIED .....: 16.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR haku       LIKE BItemGroup.BIGroup   NO-UNDO.
DEF VAR haku2      LIKE BItemGroup.BIGName   NO-UNDO.
DEF VAR firstline  AS INT               NO-UNDO.
DEF VAR order      AS INT               NO-UNDO.
DEF VAR ex-order   AS INT               NO-UNDO.
DEF VAR memory     AS RECID             NO-UNDO.
def var line       as int format "99"   NO-UNDO.
DEF VAR delline    AS INT               NO-UNDO.
DEF VAR must-print AS LOG               NO-UNDO.
DEF VAR ufkey      AS LOG               NO-UNDO.
DEF VAR fr-header  AS CHAR.

def var pgseek     as char format "x(30)"   NO-UNDO.
DEF VAR nrohaku    AS LOG                   NO-UNDO.
DEF VAR toseek     AS LOG init TRUE         NO-UNDO.
DEF VAR number     AS LOG                   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24       NO-UNDO.
DEF VAR i AS       INT                      NO-UNDO.

DEF VAR xrecid     AS RECID.

form
    BItemGroup.BIGName  format "x(21)" 
    BItemGroup.BIGroup                 


    WITH centered OVERLAY scroll 1 13 DOWN ROW 3
    COLOR value(cfc)
    TITLE COLOR value(ctc) 
       " BillCode GROUP FINDING FROM (" + gcBrand + ") '" + pgseek + "' " 
       FRAME sel.


form
    pgseek NO-LABEL
    help "Give BillCode group's Name +ENTER, code +HOME"
    SKIP
    "What to FIND" SKIP
    "  - BillCode group Name  (ENTER)" SKIP
    "  - BillCode group code  (HOME)"  SKIP

with row 1 centered overlay title " BillCode GROUP FINDING " FRAME alku.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
   FIND FIRST BItemGroup USE-INDEX BIGroup WHERE 
      BItemGroup.Brand = gcBrand NO-LOCK no-error.
   IF NOT AVAIL BItemGroup THEN DO:
      BELL.
      message "No BillCode groups - press ENTER !".
      PAUSE no-message.
      RETURN.
   END.


   ASSIGN
   toseek = TRUE xrecid = ? delline = 0 ufkey = TRUE firstline = 0 siirto = ?.

LOOP:
repeat WITH FRAME sel:

    IF toseek THEN DO:

       ASSIGN toseek = FALSE nrohaku = FALSE.
       PAUSE 0 no-message.
alku:  repeat WITH FRAME alku ON ENDKEY UNDO LOOP, LEAVE LOOP:
          ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
          UPDATE pgseek WITH FRAME alku EDITING:
             READKEY. nap = keylabel(LASTKEY).
             /* onko painettu home */
             if lookup(nap,"home,h") > 0  
             then assign nrohaku = true nap = "enter".
             APPLY keycode(nap).
          END.

          if pgseek = "" THEN LEAVE LOOP.

          IF NOT nrohaku THEN DO:
             FIND FIRST BItemGroup where 
                        BItemGroup.Brand    = gcBrand AND
                        BItemGroup.BIGName >= pgseek
             no-lock no-error.
             order = 1.
          END.
          ELSE DO:
             FIND FIRST BItemGroup where 
                        BItemGroup.Brand    = gcBrand AND
                        BItemGroup.BIGroup >= pgseek
             no-lock no-error.
             order = 2.
          END.

          IF NOT AVAIL BItemGroup THEN DO:
             BELL.
             message "CAN'T FIND !".
             NEXT alku.
          END.
          ASSIGN memory = recid(BItemGroup) must-print = TRUE.
          view FRAME sel.
          LEAVE.
       END. /* repeat */
    END.

    IF order <> ex-order THEN DO:
       ex-order = order.
    END.


print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND BItemGroup where recid(BItemGroup) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE BItemGroup THEN DO:
               DISPLAY BItemGroup.BIGroup BItemGroup.BIGName.
               rtab[FRAME-LINE] = recid(BItemGroup).
               IF order = 2 THEN FIND NEXT BItemGroup
               USE-INDEX BIGroup WHERE BItemGroup.Brand = gcBrand 
               NO-LOCK no-error.
               ELSE IF order = 1 THEN FIND NEXT BItemGroup
               USE-INDEX BIGName WHERE BItemGroup.Brand = gcBrand 
               NO-LOCK no-error.
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
         ufk[1]= 0  ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 11 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 2 THEN DO:
         CHOOSE ROW BItemGroup.BIGroup {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) BItemGroup.BIGroup WITH FRAME sel.
      END.
      ELSE IF order = 1 THEN DO:
         CHOOSE ROW BItemGroup.BIGName {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) BItemGroup.BIGName WITH FRAME sel.
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
         FIND BItemGroup where recid(BItemGroup) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 2 THEN FIND prev BItemGroup
            USE-INDEX BIGroup WHERE BItemGroup.Brand = gcBrand 
            NO-LOCK no-error.
            ELSE IF order = 1 THEN FIND prev BItemGroup
            USE-INDEX BIGName WHERE BItemGroup.Brand = gcBrand 
            NO-LOCK no-error.
            IF AVAILABLE BItemGroup THEN
               ASSIGN firstline = i memory = recid(BItemGroup).
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
            FIND BItemGroup where recid(BItemGroup) = rtab[1] no-lock.
            IF order = 2 THEN FIND prev BItemGroup
            USE-INDEX BIGroup WHERE BItemGroup.Brand = gcBrand 
            NO-LOCK no-error.
            ELSE IF order = 1 THEN FIND prev BItemGroup
            USE-INDEX BIGName WHERE BItemGroup.Brand = gcBrand 
            NO-LOCK no-error.
            IF NOT AVAILABLE BItemGroup THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               DISPLAY BItemGroup.BIGroup BItemGroup.BIGName.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(BItemGroup)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND BItemGroup where recid(BItemGroup) = rtab[FRAME-DOWN] no-lock .
            IF order = 2 THEN FIND NEXT BItemGroup
            USE-INDEX BIGroup WHERE BItemGroup.Brand = gcBrand 
            NO-LOCK no-error.
            ELSE IF order = 1 THEN FIND NEXT BItemGroup
            USE-INDEX BIGName WHERE BItemGroup.Brand = gcBrand 
            NO-LOCK no-error.
            IF NOT AVAILABLE BItemGroup THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               DISPLAY BItemGroup.BIGroup BItemGroup.BIGName.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(BItemGroup).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND BItemGroup where recid(BItemGroup) = memory no-lock no-error.
         IF order = 2 THEN FIND prev BItemGroup
         USE-INDEX BIGroup WHERE BItemGroup.Brand = gcBrand NO-LOCK no-error.
         ELSE IF order = 1 THEN FIND prev BItemGroup
         USE-INDEX BIGName WHERE BItemGroup.Brand = gcBrand NO-LOCK no-error.
         IF AVAILABLE BItemGroup THEN DO:
            memory = recid(BItemGroup).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 2 THEN FIND prev BItemGroup
               USE-INDEX BIGroup WHERE BItemGroup.Brand = gcBrand 
               NO-LOCK no-error.
               ELSE IF order = 1 THEN FIND prev BItemGroup
               USE-INDEX BIGName WHERE BItemGroup.Brand = gcBrand 
               NO-LOCK no-error.
               IF AVAILABLE BItemGroup THEN memory = recid(BItemGroup).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "YOU ARE ON THE FÖRSTA SIDAN !".
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
            FIND BItemGroup where recid(BItemGroup) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     else if lookup(nap,"5,f5,enter,return") > 0 THEN DO: /* valinta */
        FIND BItemGroup where recid(BItemGroup) = rtab[FRAME-LINE] no-lock.
        siirto = string(BIGroup).
        LEAVE LOOP.
     END.


     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 2 THEN FIND FIRST BItemGroup
        USE-INDEX BIGroup WHERE BItemGroup.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 1 THEN FIND FIRST BItemGroup
        USE-INDEX BIGName WHERE BItemGroup.Brand = gcBrand NO-LOCK no-error.
        ASSIGN memory = recid(BItemGroup) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 2 THEN FIND LAST BItemGroup
        USE-INDEX BIGroup WHERE BItemGroup.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 1 THEN FIND LAST BItemGroup
        USE-INDEX BIGName WHERE BItemGroup.Brand = gcBrand NO-LOCK no-error.
        ASSIGN memory = recid(BItemGroup) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN DO:
        toseek = TRUE.
        HIDE FRAME sel no-pause.
        NEXT LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
HIDE FRAME alku no-pause.
si-recid = xrecid.

