/* ----------------------------------------------------------------------
  MODULE .......: StoBal3.p
  TASK .........: UPDATE /Browse Stock Balances  (one article / many Stocks)
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 01-06-99
  CHANGED ......: 01.07.99 pt
                  10.03.03 tk tokens
                  09.09.03 jp brand
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'StoBal'}

DEF INPUT PARAMETER icBrand  AS C NO-UNDO.
DEF INPUT PARAMETER p-SimArt LIKE Stock.Stock.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Stock  LIKE StoBal.StoBal  NO-UNDO.
DEF VAR SimArt LIKE StoBal.SimArt NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 11.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"  NO-UNDO.

form

    StoBal.StoBal     /* COLUMN-LABEL FORMAT */
    Stock.StoName     format "x(30)"
    StoBal.Balance
    StoBal.OrdPoint
             /* COLUMN-LABEL FORMAT */
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    " Balances of Article " + p-SimArt + " "
    FRAME sel.

form
    StoBal.StoBal     /* LABEL FORMAT */
    StoBal.SimArt    /* LABEL FORMAT */
            /* LABEL FORMAT */

    WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels 1 columns
    FRAME lis.

form /* Detailed Balance */

    StoBal.DetBal[1]  label "In Stock, Arrival tested ..."
    StoBal.DetBal[2]  label "In Stock, pre-pers. ........"
    StoBal.DetBal[3]  label "* Total at This Location ..."
    StoBal.DetBal[4]  label "At Customers ..............."
    StoBal.DetBal[5]  label "In use ....................."
    StoBal.DetBal[6]  label "Discarded .................."
with centered overlay title "Det. Bal. Art " + StoBal.StoBal + " / "
    + StoBal.StoBal + " "
    ROW 5 side-labels FRAME dbal.


form /* seek Balance Record  BY  Stock */
    Stock
    help "Enter Stock Code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND SCODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Balance Record  BY SimArt */
    SimArt
    help "Enter Article Code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND ACODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FIND SimArt where SimArt.SimArt = p-SimArt AND 
     SimArt.Brand = icBrand no-lock.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Code,By 3, By 4".


FIND FIRST StoBal
where StoBal.SimArt = p-SimArt and STobal.Brand = icBrand NO-LOCK NO-ERROR.
IF AVAILABLE StoBal THEN ASSIGN
   Memory       = recid(StoBal)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No stock balances available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder > 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 37 entry(order,orders).
    END.

   IF must-add THEN DO:  /* Add a StoBal  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        DO TRANSACTION:
           CLEAR FRAME lis no-pause.
           PROMPT-FOR StoBal.StoBal
           VALIDATE
              (StoBal.StoBal = "" OR
              NOT CAN-FIND(StoBal using  StoBal.StoBal WHERE 
                          StoBal.Brand  = icBrand),
              "Balance Record " + string(INPUT StoBal.StoBal) +
              " already exists !").
           IF input StoBal.StoBal = "" THEN LEAVE ADD-ROW.
           CREATE StoBal.
           ASSIGN
           StoBal.Brand  = icBrand 
           StoBal.StoBal = INPUT FRAME lis StoBal.StoBal.

           RUN local-UPDATE-record.

           ASSIGN
           Memory = recid(StoBal)
           xrecid = Memory.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST StoBal
      where StoBal.SimArt = p-SimArt AND StoBal.Brand = icBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE StoBal THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND StoBal WHERE recid(StoBal) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE StoBal THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(StoBal).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE.
        PAUSE 0 no-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */

   /* IF a ROW was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 203  ufk[2]= 0 ufk[3]= 237 ufk[4]= 202
        ufk[5]=0 ufk[6]=0
        /*ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)*/
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.

      IF order = 1 THEN DO:
        CHOOSE ROW StoBal.StoBal {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) StoBal.StoBal WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND StoBal WHERE recid(StoBal) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE StoBal THEN
              ASSIGN FIRSTrow = i Memory = recid(StoBal).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 no-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF NOT AVAILABLE StoBal THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(StoBal)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE StoBal THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(StoBal).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND StoBal WHERE recid(StoBal) = Memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE StoBal THEN DO:
           Memory = recid(StoBal).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE StoBal THEN Memory = recid(StoBal).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
        END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND StoBal WHERE recid(StoBal) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       Stock = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE Stock WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF Stock <> "" THEN DO:
          FIND FIRST StoBal WHERE StoBal.StoBal >= Stock AND stoBal.Brand = icBrand 
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE StoBal THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some StoBal/Stock was found */
          ASSIGN order = 1 Memory = recid(StoBal) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO TRANS:  /* INIVID. CARDS */
       ufkey = TRUE.
       RUN local-find-this(FALSE).                                        

       rt_param[1] = StoBal.StoBal.
       rt_param[2] = "0".              /* ALL SIM batches */
       RUN Mm/simall.p.

       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO TRANSACTION:  /* DET. BAL */
       ufkey = TRUE. ufk = 0. ehto = 3. RUN Syst/ufkey.p.
       RUN local-find-this(FALSE).                                        
       PAUSE 0.
       DISP StoBal.DetBal[1 FOR 6] WITH FRAME dbal.
       message "Press ENTER to continue !".
       PAUSE no-message.
       HIDE FRAME dbal.
       NEXT LOOP.
     END.


/*
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       Stock.StoName StoBal.StoBal .

       RUN local-find-NEXT.
       IF AVAILABLE StoBal THEN Memory = recid(StoBal).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-prev.
          IF AVAILABLE StoBal THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(StoBal).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       Stock.StoName StoBal.StoBal .
       IF ok THEN DO:

           DELETE StoBal.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST StoBal
           where StoBal.SimArt = p-SimArt AND Stobal.Brand = icBrand ) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */
*/
     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(StoBal) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(StoBal) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:


    DEF INPUT PARAMETER exlock AS lo NO-UNDO.
    IF exlock THEN
      FIND StoBal WHERE recid(StoBal) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND StoBal WHERE recid(StoBal) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST StoBal
       where StoBal.SimArt = p-SimArt NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST StoBal USE-INDEX SimArt
       where StoBal.SimArt = p-SimArt AND Stobal.Brand = icBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST StoBal
       where StoBal.SimArt = p-SimArt AND Stobal.Brand = icBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST StoBal USE-INDEX SimArt
       where StoBal.SimArt = p-SimArt AND Stobal.Brand = icBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT StoBal
       where StoBal.SimArt = p-SimArt AND Stobal.Brand = icBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT StoBal USE-INDEX SimArt
       where StoBal.SimArt = p-SimArt AND Stobal.Brand = icBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev StoBal
       where StoBal.SimArt = p-SimArt AND Stobal.Brand = icBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev StoBal USE-INDEX SimArt
       where StoBal.SimArt = p-SimArt AND Stobal.Brand = icBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       DISPLAY
       StoBal.StoBal
       Stock.StoName
       StoBal.Balance
       StoBal.OrdPoint
       StoBal.Balance < StoBal.OrdPoint format "!/"
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND Stock WHERE Stock.Stock = StoBal.StoBal no-lock no-error.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   RUN local-find-others.
   DISP
   WITH FRAME lis.
   UPDATE
       StoBal.SimArt
   WITH FRAME lis.
END PROCEDURE.

