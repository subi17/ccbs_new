/* ----------------------------------------------------------------------
  MODULE .......: InvSeq.P
  TASK .........: browse InvSeq
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 08-02-2002
  CHANGED ......: 04.03.03 tk tokens
                  24.03.03 kl country check removed
                  16.09.03 aam brand
                  14.04.04 aam index InvNum removed
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invseq'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR InvSeq  LIKE InvSeq.InvSeq  NO-UNDO.
DEF VAR CustNum LIKE InvSeq.CustNum NO-UNDO.
DEF VAR InvNum  LIKE InvSeq.InvNum  NO-UNDO.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
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
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcBrand      AS CHAR                   NO-UNDO.

form
    InvSeq.InvSeq                     
    InvSeq.CustNum  
    lcBrand          FORMAT "X(5)" COLUMN-LABEL "Brand"
    InvSeq.FromDate
    InvSeq.ToDate    /* COLUMN-LABEL FORMAT */
    InvSeq.InvNum  format "zzzzzzzzz"
    InvSeq.Billed   format "*/"  Column-label "B"
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)  
    " Invoice Sequences (All Brands) "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    InvSeq.InvSeq     /* LABEL FORMAT */
    InvSeq.CustNum
    InvSeq.FromDate
    InvSeq.ToDate
    InvSeq.Billed
    InvSeq.FromDate    /* LABEL FORMAT */
    InvSeq.InvNum

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek Mobile user  BY  InvSeq */
    InvSeq
    HELP "Enter InvSeq no"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND InvSeq "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Mobile user  BY  CustNum */
    CustNum
    HELP "Enter First Name of User"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CUSTNO "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek Mobile user  BY InvNum */
    InvNum
    HELP "Enter Postal Code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND INVOICE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.


cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By InvSeq   ,By CustNo  ,By InvNum , By 4".

FIND FIRST InvSeq USE-INDEX InvSeq NO-LOCK NO-ERROR.

IF AVAILABLE InvSeq THEN ASSIGN
   memory     = recid(InvSeq)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   MESSAGE "No InvSeq records available !" VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND InvSeq WHERE recid(InvSeq) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE InvSeq THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(InvSeq).
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
        PAUSE 0 NO-MESSAGE.

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
        ufk[1]= 1840  ufk[2]= 702 ufk[3]= 0 /* 92 */ ufk[4]= 0
        ufk[5]= 1830  ufk[6]= 1831 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        {Syst/uright1.i '"7"'}
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW InvSeq.InvSeq ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) InvSeq.InvSeq WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW InvSeq.InvNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) InvSeq.InvNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW InvSeq.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) InvSeq.CustNum WITH FRAME sel.
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
        FIND InvSeq WHERE recid(InvSeq) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE InvSeq THEN
              ASSIGN FIRSTrow = i memory = recid(InvSeq).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE InvSeq THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(InvSeq)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE InvSeq THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(InvSeq).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND InvSeq WHERE recid(InvSeq) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE InvSeq THEN DO:
           memory = recid(InvSeq).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE InvSeq THEN memory = recid(InvSeq).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
        END.
     END. /* PREVious page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND InvSeq WHERE recid(InvSeq) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET InvSeq WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF InvSeq ENTERED THEN DO:
          FIND FIRST InvSeq WHERE InvSeq.InvSeq >= InvSeq
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE InvSeq THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some invseq/invseq was found */
          ASSIGN order = 1 memory = recid(InvSeq) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */


     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       SET CustNum WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CustNum ENTERED THEN DO:
          FIND FIRST InvSeq WHERE InvSeq.CustNum >= CustNum 
          USE-INDEX CustNum /*  srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE InvSeq THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some invseq/as-nro was found */
          ASSIGN order = 2 memory = recid(InvSeq) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:
        RUN local-find-this(FALSE).
        RUN mobcalli(InvSeq.InvSeq).
        ASSIGN 
           memory     = recid(InvSeq) 
           must-print = TRUE
           ufkey      = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO:
        RUN local-find-this(FALSE).
        RUN nnispul(InvSeq.InvSeq).
        ASSIGN 
           memory     = recid(InvSeq) 
           must-print = TRUE
           ufkey      = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(InvSeq) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(InvSeq) must-print = TRUE.
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
      FIND InvSeq WHERE recid(InvSeq) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND InvSeq WHERE recid(InvSeq) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST InvSeq USE-INDEX InvSeq 
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST InvSeq  USE-INDEX CustNum 
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST InvSeq USE-INDEX InvSeq 
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST InvSeq  USE-INDEX CustNum 
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT InvSeq USE-INDEX InvSeq 
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT InvSeq  USE-INDEX CustNum 
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV InvSeq USE-INDEX InvSeq 
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV InvSeq  USE-INDEX CustNum 
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       InvSeq.InvSeq
       InvSeq.CustNum
       lcBrand
       InvSeq.InvNum 
       InvSeq.FromDate
       InvSeq.ToDate
       InvSeq.Billed
       InvSeq.InvNum
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND Customer OF Invseq NO-LOCK NO-ERROR.
   IF AVAILABLE Customer THEN lcBrand = Customer.Brand.
   ELSE lcBrand = "".

END PROCEDURE.

