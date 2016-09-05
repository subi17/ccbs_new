/* ----------------------------------------------------------------------
  MODULE .......: UNREGLOG.P
  TASK .........: Display unregistered payments
  APPLICATION ..: tms
  AUTHOR .......: tk
  CREATED ......: 12-08-02
  CHANGED ......: 16.08.02/aam correct labels for find-routines etc. 
                  05.03.03/tk  tokens
  VERSIO N ......: 
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'unreglog'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF INPUT PARAMETER UrSeq    LIKE UnregPaym.UrSeq No-UNDO.

DEF VAR AccDate  LIKE UnregLog.AccDate  NO-UNDO.
DEF VAR CustNum  LIKE UnregLog.CustNum NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"  NO-UNDO.

form
    UnregLog.AccDate 
    UnregLog.Voucher 
    UnregLog.CustNum 
    UnregLog.CustBal COLUMN-LABEL "Type"  
    UnregLog.Amount
WITH ROW FrmRow  CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) 
    " Booked PAYMENTS "
    FRAME sel.

form
    UnregLog.AccDate    
    UnregLog.CustNum  
    UnregLog.Voucher  
    UnregLog.CustBal
    UnregLog.Amount
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels 1 columns
    FRAME lis.


form /* seek  BY  AccDate */
    AccDate
    help "Enter Date"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND BOOKING Date "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek   BY CustNum */
    CustNum
    help "Enter Customer Nbr"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CUSTOMER "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Date,By Customer,By 3, By 4".


FIND FIRST UnregLog 
WHERE UnregLog.UrSeq = UrSeq NO-LOCK NO-ERROR.
IF AVAILABLE UnregLog THEN ASSIGN
   memory       = recid(UnregLog)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No registered payments made from this event!" 
   VIEW-AS ALERT-BOX.
   RETURN.

END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 37 entry(order,orders).
    END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND UnregLog WHERE recid(UnregLog) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE UnregLog THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(UnregLog).
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
        ufk[1]= 28 ufk[2]= 0 /* 702 */   ufk[3]= 0 ufk[4]= 0
        ufk[5]= 0  ufk[6]= 0   ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        {Syst/uright1.i '"5,6"'}
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW UnregLog.AccDate {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) UnregLog.AccDate WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW UnregLog.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) UnregLog.CustNum WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND UnregLog WHERE recid(UnregLog) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE UnregLog THEN
              ASSIGN FIRSTrow = i memory = recid(UnregLog).
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
           IF NOT AVAILABLE UnregLog THEN DO:
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
                rtab[1] = recid(UnregLog)
                memory  = rtab[1].
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
           IF NOT AVAILABLE UnregLog THEN DO:
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
              rtab[FRAME-DOWN] = recid(UnregLog).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND UnregLog WHERE recid(UnregLog) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE UnregLog THEN DO:
           memory = recid(UnregLog).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE UnregLog THEN memory = recid(UnregLog).
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
           memory = rtab[FRAME-DOWN].
           FIND UnregLog WHERE recid(UnregLog) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       AccDate = ?.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE AccDate WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF AccDate <> ? THEN DO:
          FIND FIRST UnregLog WHERE UnregLog.AccDate >= AccDate
          AND UnregLog.UrSeq = UrSeq NO-LOCK NO-ERROR.
          IF NOT AVAILABLE UnregLog THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some UnregLog/AccDate was found */
          ASSIGN order = 1 memory = recid(UnregLog) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       CustNum = 0.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE CustNum WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CustNum <> 0 THEN DO:
          FIND FIRST UnregLog WHERE UnregLog.CustNum >= CustNum
           AND UnregLog.UrSeq = UrSeq NO-LOCK NO-ERROR.
          IF NOT AVAILABLE UnregLog THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some UnregLog/CustNum was found */
          ASSIGN order = 2 memory = recid(UnregLog) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */
     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(UnregLog) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(UnregLog) must-print = TRUE.
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
      FIND UnregLog WHERE recid(UnregLog) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND UnregLog WHERE recid(UnregLog) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST UnregLog
       WHERE UnregLog.UrSeq = UrSeq NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST UnregLog 
       WHERE UnregLog.UrSeq = UrSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST UnregLog
       WHERE UnregLog.UrSeq = UrSeq NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST UnregLog
       WHERE UnregLog.UrSeq = UrSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT UnregLog
       WHERE UnregLog.UrSeq = UrSeq NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT UnregLog 
       WHERE UnregLog.UrSeq = UrSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev UnregLog
       WHERE UnregLog.UrSeq = UrSeq NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev UnregLog 
       WHERE UnregLog.UrSeq = UrSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       DISPLAY
       UnregLog.AccDate 
       UnregLog.Voucher
       UnregLog.CustNum  /* sd */
       UnregLog.CustBal
       UnregLog.Amount
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-update-record:
   RUN local-find-others.
   DISP
   WITH FRAME lis.
   UPDATE
       UnregLog.CustNum
       UnregLog.Voucher
       UnregLog.CustBal
       UnregLog.Amount

   WITH FRAME lis.
END PROCEDURE.

