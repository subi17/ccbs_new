/* -----------------------------------------------
  MODULE .......: NNRSSE.P
  FUNCTION .....: Reseller browse
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 06-05-98
  MODIFIED .....: 21.01.99 pt title bar. "Agents"
                  24.05.99 pt layout
                  11.11.02 jr Eventlog
                  16.09.03/aam brand,
                               salesman removed,
                               no updates / add
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR Reseller    LIKE Reseller.Reseller    NO-UNDO.
DEF VAR RsName    LIKE Reseller.RsName    NO-UNDO.
DEF VAR xrecid     AS RECID                        init ?.
DEF VAR firstline  AS INT                 NO-UNDO  init 0.
DEF VAR order      AS INT                 NO-UNDO  init 1.
DEF VAR ordercount AS INT                 NO-UNDO  init 2.
DEF VAR ufkey      AS LOG                 NO-UNDO  init TRUE.
DEF VAR delline    AS INT                 NO-UNDO  init 0.
DEF VAR ex-order   AS INT                 NO-UNDO.
DEF VAR memory     AS RECID               NO-UNDO.
def var line       as int format "99"     NO-UNDO.
DEF VAR must-print AS LOG                 NO-UNDO.
DEF VAR must-add   AS LOG                 NO-UNDO.
DEF VAR fr-header  AS CHAR                NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24     NO-UNDO.
DEF VAR i          AS INT                 NO-UNDO.
def var ok         as log format "Yes/No" NO-UNDO.

form
    Reseller.Reseller
    Reseller.RsName       format "x(30)"
    WITH CENTERED OVERLAY scroll 1 ROW 2 12 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " Browse resellers (" + gcBrand + ") "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form /*  search WITH FIELD Reseller */
    Reseller
    help "Give ...."
    with row 4 col 2 title color value(ctc) " FIND CODE"
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /*  search WITH FIELD RsName */
    RsName
    help "Give ..."
    with row 4 col 2 title color value(ctc) " FIND NAME "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.


FIND FIRST Reseller
WHERE Reseller.Brand = gcBrand no-lock no-error.
IF AVAILABLE Reseller THEN ASSIGN
   memory       = recid(Reseller)
   must-print = TRUE
   must-add    = FALSE.
ELSE DO:
   MESSAGE "No resellers available for brand" gcBrand
   VIEW-AS ALERT-BOX 
   ERROR.
   RETURN.
END.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 17 col 30 " By reseller's code".
       if order = 2 then put screen row 17 col 30 " By reseller's name".
    END.

   print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND Reseller where recid(Reseller) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose KeyValue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE Reseller THEN DO:

              RUN local-disp-row.

              rtab[FRAME-LINE] = recid(Reseller).
              IF order = 1 THEN FIND NEXT Reseller
              WHERE Reseller.Brand = gcBrand no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT Reseller USE-INDEX RsName
              WHERE Reseller.Brand = gcBrand no-lock no-error.
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 885 ufk[4]= 0
        ufk[5]= 11  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW Reseller.Reseller ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) Reseller.Reseller WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Reseller.RsName ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) Reseller.RsName WITH FRAME sel.
      END.
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
        FIND Reseller where recid(Reseller) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev Reseller
           WHERE Reseller.Brand = gcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev Reseller USE-INDEX RsName
           WHERE Reseller.Brand = gcBrand no-lock no-error.
           IF AVAILABLE Reseller THEN
              ASSIGN firstline = i memory = recid(Reseller).
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
           FIND Reseller where recid(Reseller) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev Reseller
           WHERE Reseller.Brand = gcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev Reseller USE-INDEX RsName
           WHERE Reseller.Brand = gcBrand no-lock no-error.
           IF NOT AVAILABLE Reseller THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.

              RUN local-disp-row.

              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(Reseller)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND Reseller where recid(Reseller) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT Reseller
           WHERE Reseller.Brand = gcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT Reseller USE-INDEX RsName
           WHERE Reseller.Brand = gcBrand no-lock no-error.
           IF NOT AVAILABLE Reseller THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.

              RUN local-disp-row.

              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(Reseller).
              /* finally LAST line's KeyValue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND Reseller where recid(Reseller) = memory no-lock no-error.
        IF order = 1 THEN FIND prev Reseller
        WHERE Reseller.Brand = gcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND prev Reseller USE-INDEX RsName
        WHERE Reseller.Brand = gcBrand no-lock no-error.
        IF AVAILABLE Reseller THEN DO:
           memory = recid(Reseller).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev Reseller
              WHERE Reseller.Brand = gcBrand no-lock no-error.
              ELSE IF order = 2 THEN FIND prev Reseller USE-INDEX RsName
              WHERE Reseller.Brand = gcBrand no-lock no-error.
              IF AVAILABLE Reseller THEN memory = recid(Reseller).
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
           FIND Reseller where recid(Reseller) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       Reseller = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE Reseller WITH FRAME f1.
       HIDE FRAME f1 no-pause.
       if Reseller <> "" THEN DO:
          FIND FIRST Reseller where 
                     Reseller.Brand     = gcBrand AND
                     Reseller.Reseller >= Reseller
          no-lock no-error.
          IF NOT AVAILABLE Reseller THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  resell/rs-code was found */
          ASSIGN order = 1 memory = recid(Reseller) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       RsName = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE RsName WITH FRAME f2.
       HIDE FRAME f2 no-pause.
       if RsName <> "" THEN DO:
          FIND FIRST Reseller where 
                     Reseller.Brand   = gcBrand AND
                     Reseller.RsName >= RsName
          USE-INDEX RsName no-lock no-error.
          IF NOT AVAILABLE Reseller THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  resell/rs-name was found */
          ASSIGN order = 2 memory = recid(Reseller) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     else if lookup(nap,"enter,return,5,F5") > 0 THEN
     DO WITH FRAME lis TRANSAction:
       FIND Reseller where recid(Reseller) = rtab[frame-line(sel)]
       no-lock.
       siirto = Reseller.Reseller.
       LEAVE LOOP.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST Reseller
       WHERE Reseller.Brand = gcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST Reseller USE-INDEX RsName
       WHERE Reseller.Brand = gcBrand no-lock no-error.
       ASSIGN memory = recid(Reseller) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST Reseller
       WHERE Reseller.Brand = gcBrand no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST Reseller USE-INDEX RsName
       WHERE Reseller.Brand = gcBrand no-lock no-error.
       ASSIGN memory = recid(Reseller) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-disp-row:

   DISPLAY Reseller.Reseller Reseller.RsName WITH FRAME sel.

END PROCEDURE.
