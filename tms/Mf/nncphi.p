/* -----------------------------------------------
  MODULE .......: NNCPHI.P
  FUNCTION .....: CDR preprosessor file history
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 10-03-99
  MODIFIED .....: 20.09.02/jr Modified F1 Search
                  20.09.02/jr Fixed find's
                  20.09.02/jr Deleted F5 & F6
                  06.03.03/tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MedHist'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Ident LIKE MedHist.Ident NO-UNDO.
DEF VAR Date  LIKE MedHist.Date  NO-UNDO.
DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 2.
DEF VAR ufkey      AS LOG                    NO-UNDO  init TRUE.
DEF VAR ex-order   AS INT                    NO-UNDO.
DEF VAR Memory     AS RECID                  NO-UNDO.
def var line       as int format "99"        NO-UNDO.
DEF VAR must-print AS LOG                    NO-UNDO.
DEF VAR fr-header  AS CHAR                   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24        NO-UNDO.
DEF VAR i          AS INT                    NO-UNDO.
def var ok         as log format "Yes/No"    NO-UNDO.

form
   MedHist.FileName
   MedHist.Ident
   MedHist.Date
   MedHist.FileTime
WITH width 80 OVERLAY scroll 1 15 DOWN COLOR value(cfc)
   title color value(ctc) " " + ynimi + " Preprosessor configuration "
   + string(pvm,"99-99-99") + " " FRAME sel.

form /*  search WITH FIELD Ident */
    Ident
    help "Give Ident"
    with row 4 col 2 title color value(ctc) " FIND Ident "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

form /*  search WITH FIELD Date */
    Date
        help "Give Date"
            with row 4 col 2 title color value(ctc) " FIND Date "
                COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST MedHist
/* search condition */ no-lock no-error.
IF AVAILABLE MedHist THEN ASSIGN
   Memory     = recid(MedHist)
   must-print = TRUE.
ELSE DO:
   message "No configuration files created - HIT ENTER ! ".
   PAUSE 4 no-message.
   RETURN.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by Ident ".
       if order = 2 then put screen row 19 col 30 " Order by Date  ".
    END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND MedHist where recid(MedHist) = Memory no-lock no-error.

        repeat WITH FRAME sel:
           IF AVAILABLE MedHist THEN DO:
              DISP
                 MedHist.FileName
                 MedHist.Ident
                 MedHist.Date
                 MedHist.FileTime.
              rtab[FRAME-LINE] = recid(MedHist).
              IF order = 1 THEN FIND NEXT MedHist
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND NEXT MedHist USE-INDEX Date
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND NEXT MedHist USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND NEXT MedHist USE-INDEX index-4
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

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 2120  ufk[2]= 28 ufk[3]= 0 ufk[4]= 0
        ufk[5]= 0  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW MedHist.Ident {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedHist.Ident WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MedHist.Date {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedHist.Date WITH FRAME sel.
      END.
/*    IF order = 3 THEN DO:
        CHOOSE ROW MedHist.?? {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedHist.?? WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW MedHist.??  {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) MedHist.? WITH FRAME sel.
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
        FIND MedHist where recid(MedHist) = Memory.
        DO i = 1 TO FRAME-LINE - 1:
           IF order = 1 THEN FIND prev MedHist
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MedHist USE-INDEX Date
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev MedHist USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev MedHist USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF AVAILABLE MedHist THEN
              ASSIGN firstline = i Memory = recid(MedHist).
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
           FIND MedHist where recid(MedHist) = rtab[1] no-lock.
           IF order = 1 THEN FIND prev MedHist
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND prev MedHist USE-INDEX Date
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND prev MedHist USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND prev MedHist USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE MedHist THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              DISP
                 MedHist.FileName
                 MedHist.Ident
                 MedHist.Date
                 MedHist.FileTime.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(MedHist)
              Memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND MedHist where recid(MedHist) = rtab[FRAME-DOWN] no-lock .
           IF order = 1 THEN FIND NEXT MedHist
           /* search condition */ no-lock no-error.
           ELSE IF order = 2 THEN FIND NEXT MedHist USE-INDEX Date
           /* search condition */ no-lock no-error.
      /*    ELSE IF order = 3 THEN FIND NEXT MedHist USE-INDEX index-3
           /* search condition */ no-lock no-error.
           ELSE IF order = 4 THEN FIND NEXT MedHist USE-INDEX index-4
           /* search condition */ no-lock no-error.   */
           IF NOT AVAILABLE MedHist THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              DISP
                 MedHist.FileName
                 MedHist.Ident
                 MedHist.Date
                 MedHist.FileTime.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(MedHist).
              /* finally LAST line's KeyValue is saved */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MedHist where recid(MedHist) = Memory no-lock no-error.
        IF order = 1 THEN FIND prev MedHist
        /* search condition */ no-lock no-error.
        ELSE IF order = 2 THEN FIND prev MedHist USE-INDEX Date
        /* search condition */ no-lock no-error.
   /*    ELSE IF order = 3 THEN FIND prev MedHist USE-INDEX index-3
        /* search condition */ no-lock no-error.
        ELSE IF order = 4 THEN FIND prev MedHist USE-INDEX index-4
        /* search condition */ no-lock no-error.   */
        IF AVAILABLE MedHist THEN DO:
           Memory = recid(MedHist).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              IF order = 1 THEN FIND prev MedHist
              /* search condition */ no-lock no-error.
              ELSE IF order = 2 THEN FIND prev MedHist USE-INDEX Date
              /* search condition */ no-lock no-error.
        /*    ELSE IF order = 3 THEN FIND prev MedHist USE-INDEX index-3
              /* search condition */ no-lock no-error.
              ELSE IF order = 4 THEN FIND prev MedHist USE-INDEX index-4
              /* search condition */ no-lock no-error.   */
              IF AVAILABLE MedHist THEN Memory = recid(MedHist).
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
           FIND MedHist where recid(MedHist) = Memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       Ident = "".
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE Ident WITH FRAME haku-f1.
       HIDE FRAME haku-f1 no-pause.
       if Ident <>  "" THEN DO:
          FIND FIRST MedHist where MedHist.Ident >= Ident
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE MedHist THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  MedHist/Ident was found */
          ASSIGN order = 1 Memory = recid(MedHist) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       Date = ?.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE Date WITH FRAME haku-f2.
       HIDE FRAME haku-f2 no-pause.
       IF Date <> ? THEN DO:
          FIND FIRST MedHist where MedHist.Date >= Date
          USE-INDEX  Date /* search condition */ no-lock no-error.
          IF NOT AVAILABLE MedHist THEN DO:
             bell. message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  MedHist/ was found */
          ASSIGN order = 2 Memory = recid(MedHist) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     else if lookup(nap,"home,h") > 0 THEN DO:
       IF order = 1 THEN FIND FIRST MedHist
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND FIRST MedHist USE-INDEX Date
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND FIRST MedHist USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND FIRST MedHist USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN Memory = recid(MedHist) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       IF order = 1 THEN FIND LAST MedHist
       /* search condition */ no-lock no-error.
       ELSE IF order = 2 THEN FIND LAST MedHist USE-INDEX Date
       /* search condition */ no-lock no-error.
  /*    ELSE IF order = 3 THEN FIND LAST MedHist USE-INDEX index-3
       /* search condition */ no-lock no-error.
       ELSE IF order = 4 THEN FIND LAST MedHist USE-INDEX index-4
       /* search condition */ no-lock no-error.   */
       ASSIGN Memory = recid(MedHist) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

