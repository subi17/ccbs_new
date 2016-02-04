/* ----------------------------------------------------------------------
  MODULE .......: partcred.p
  TASK .........: mark invoice lines FOR partial crediting
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 16.05.2002
  CHANGED ......: 18.09.2002 jp  find code
                  23.10.2002 aam allow change of row amount
                  24.01.2003 aam show vat in fVat also with cursor-up/down
                  24.02.2003 aam prefix to nnlrpu
                  12.09.2003 aam brand
                  19.03.2004 aam irowmcdr instead of nnlrpu2,
                                 irowffee instead of nnlrcoit,
                                 irowsfee instead of nnlrbit
                  09.05.2005 aam InvRow.Amt with 3 decimals               
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}

DEF TEMP-TABLE wMarked NO-UNDO
    FIELD Line AS INT
    FIELD Amt  AS DEC
    INDEX Line IS UNIQUE Line. 

DEF TEMP-TABLE wCredit LIKE wMarked.

DEF INPUT PARAMETER  iInvNum AS INT NO-UNDO. 

DEF input-output PARAMETER table FOR wMarked.


DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xBillCode    LIKE InvRow.BillCode       NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
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
DEF VAR BCName       AS CHAR                   NO-UNDO. 

DEF VAR llCredInt    AS LOGIC                  NO-UNDO.
DEF VAR llCredOver   AS LOGIC                  NO-UNDO. 
DEF VAR llCredAdv    AS LOGIC                  NO-UNDO. 
DEF VAR ldMarked     AS DEC                    NO-UNDO.
DEF VAR ldIntAmt     AS DEC                    NO-UNDO.
DEF VAR ldOverAmt    AS DEC                    NO-UNDO.
DEF VAR ldAdvAmt     AS DEC                    NO-UNDO.


form
    InvRow.BillCode     column-label "Prod."      format "x(8)"
    BCName              column-label "Prod.name"  format "x(12)"
    InvRow.FromDate     column-label "From."      format "99-99-99"
    InvRow.ToDate       column-label "To"         format "99-99-99"
    InvRow.Qty          column-label "Qty"        format "->>>>>9"
    InvRow.Amt          column-label "Amount"     format "->>>>9.999"
    ldMarked            column-label "Credit Amt" format "->>>>9.999"
        HELP "Credited amount" 
    InvRow.CreditInvNum column-label "Cr Inv."    format ">>>>>>>9"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " Lines for invoice nbr " + string(iInvNum) + " " 
       + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    Invoice.InterestAmt AT 2 
       label "Interest .."
       format "->>>>>9.99"
    llCredInt AT 26 
       label "Credit"
       help  "Credit the interest amount"
       format "Yes/No"
    ldIntAmt  AT 40
       LABEL "Amount"
       HELP  "Credited interest amount"
       FORMAT "->>>>>9.99"
       VALIDATE(INPUT ldIntAmt >= 0 AND
                INPUT ldIntAmt <= Invoice.InterestAmt,
                "Credited amount must be between 0 and original amount")
       SKIP
    Invoice.OverPaym AT 2
       label "Overpayment"
       format "->>>>>9.99"
    llCredOver AT 26
       label "Credit"
       help  "Credit the used overpayment amount"
       format "Yes/No"
    ldOverAmt  AT 40
       LABEL "Amount"
       HELP  "Credited overpayment amount"
       FORMAT "->>>>>9.99"
       VALIDATE(ABS(INPUT ldOverAmt) <= ABS(Invoice.OverPaym),
                "Credited amount cannot be more than the original amount")
       SKIP
    Invoice.AdvPaym AT 2
       label "Adv.payment"
       format "->>>>>9.99"
    llCredAdv AT 26
       label "Credit"
       help  "Credit the used advance payment amount"
       format "Yes/No"
    ldAdvAmt  AT 40
       LABEL "Amount"
       HELP  "Credited advance payment amount"
       FORMAT "->>>>>9.99"
       VALIDATE(ABS(INPUT ldAdvAmt) <= ABS(Invoice.AdvPaym),
                "Credited amount cannot be more than the original amount")
       SKIP

    WITH ROW 15 width 60 OVERLAY side-labels
    COLOR VALUE(cfc)
    FRAME crother.

FORM
    Invoice.VatIncl AT 2 LABEL "VAT " FORMAT "Included/Excluded" SKIP
    InvRow.VatPerc  AT 2 LABEL "VAT%" FORMAT "->>9.99" SKIP(1)
    WITH ROW 15 COL 61 WIDTH 20 OVERLAY SIDE-LABELS FRAME fVat.

form /* seek InvRow  BY  InvRow */
    xBillCode
    HELP "Enter BillCode code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code,By Name,By 3, By 4".

FIND Invoice WHERE Invoice.InvNum = iInvNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Invoice THEN DO:
    MESSAGE "Unknown invoice nbr: " iInvNum
    VIEW-AS ALERT-BOX 
    ERROR.
    RETURN.
END.

/* use a another TEMP-TABLE FOR handling -> UNDO is possible WITH F8 */
FOR EACH wMarked:
   CREATE wCredit.
   BUFFER-COPY wMarked TO wCredit.
END. 

/* Interest AND overpayment */
RUN DispOthers. 

FIND FIRST InvRow OF Invoice
NO-LOCK NO-ERROR.
IF AVAILABLE InvRow THEN ASSIGN
   memory       = recid(InvRow)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 7 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND InvRow WHERE recid(InvRow) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE InvRow THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(InvRow).
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
        ufk[1]= 35   ufk[2]= 1805 ufk[3]= 1809 ufk[4]= 1808
        ufk[5]= 1806 ufk[6]= 716  ufk[7]= 0    ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW InvRow.BillCode ;(uchoose.i
                                     &ulos = "cursor-up cursor-down";) 
        NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) InvRow.BillCode WITH FRAME sel.
      END.
      nap = keylabel(LASTKEY).

      IF rtab[FRAME-LINE] = ? AND
         LOOKUP(nap,"cursor-up,cursor-down") = 0
      THEN NEXT.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND InvRow WHERE recid(InvRow) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE InvRow THEN
              ASSIGN FIRSTrow = i memory = recid(InvRow).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE InvRow THEN DO:
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
                rtab[1] = recid(InvRow)
                memory  = rtab[1].
           END.
        END.
        ELSE DO:
           up 1.

           IF rtab[frame-line(sel)] NE ? THEN DO:
              RUN local-find-this(FALSE).
              RUN local-disp-row.
           END. 
        END.

      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE InvRow THEN DO:
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
              rtab[FRAME-DOWN] = recid(InvRow).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.

        ELSE DO:
           DOWN 1.
           IF rtab[frame-line(sel)] NE ? THEN DO:
              RUN local-find-this (FALSE).
              RUN local-disp-row.
           END. 
        END.

      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND InvRow WHERE recid(InvRow) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE InvRow THEN DO:
           memory = recid(InvRow).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE InvRow THEN memory = recid(InvRow).
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
           FIND InvRow WHERE recid(InvRow) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET xBillCode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF xBillCode ENTERED THEN DO:
          FIND FIRST InvRow WHERE
                     InvRow.invnum    = iinvnum AND
                     InvRow.BillCode  =  xBillCode
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE InvRow THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some nnlrivi/nnlrivi was found */
          ASSIGN order = 1 memory = recid(InvRow) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Interest AND overpayment */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        IF Invoice.InterestAmt = 0 AND
           Invoice.OverPaym    = 0 AND
           Invoice.AdvPaym     = 0
        THEN DO:
            MESSAGE "Invoice has neither Interest or overpayment amount."
            VIEW-AS ALERT-BOX
            TITLE " Crediting ".
            NEXT LOOP.
        END. 

        RUN UpdateOthers.

        RUN DispOthers. 

     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* accept marked lines */

        EMPTY TEMP-TABLE wMarked.

        FOR EACH wCredit:
           CREATE wMarked.
           BUFFER-COPY wCredit TO wMarked. 
        END. 

        LEAVE LOOP. 
     END.


     /* mark this line */
     ELSE IF LOOKUP(nap,"enter,return,2,f2") > 0 THEN DO:

       RUN local-find-this(FALSE).

       RUN local-update-record. 

       RUN local-disp-row.

       xrecid = recid(InvRow).

     END.

     /* unmark this line */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:

       RUN local-find-this(FALSE).

       FIND wCredit WHERE wCredit.Line = INTEGER(RECID(InvRow)) NO-ERROR.

       IF AVAILABLE wCredit THEN DELETE wCredit.

       RUN local-disp-row.

       xrecid = recid(InvRow).

     END.

     /* display calls */
     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO:

       RUN local-find-this(FALSE).

       IF AVAILABLE InvRow THEN 
       case InvRow.RowType:
           /* fixed ISValue */
           when 1 THEN DO:
              ASSIGN ufkey = TRUE.
              RUN nnlrpu(Invoice.CustNum,
                         InvRow.FromDate,
                         InvRow.ToDate,
                         InvRow.BillCode,
                         InvRow.InvNum,
                         InvRow.Prefix).
           END.
           /* mobile ISValue */
           when 2 THEN DO:
              ASSIGN ufkey = TRUE.
              RUN Ar/irowmcdr(Invoice.InvNum,
                           InvRow.FromDate,
                           InvRow.ToDate,
                           InvRow.BillCode,
                           InvRow.CLI).
           END.

           when 3  THEN DO:
              ASSIGN ufkey = TRUE.
              RUN Ar/irowffee(Invoice.InvNum,
                           InvRow.BillCode,
                           InvRow.CLI).
           END.
           when 4  THEN DO:
              ASSIGN ufkey = TRUE.
              RUN Ar/irowsfee(Invoice.InvNum,
                           InvRow.BillCode,
                           InvRow.CLI).
           END.

           otherwise DO:
              message "This is a single contract row ! ( -press ENTER- )".
              PAUSE no-message.
           END.
        END.

     END.


     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(InvRow) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(InvRow) must-print = TRUE.
        NEXT LOOP.
     END.


     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
        LEAVE LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel     NO-PAUSE.
HIDE FRAME crother NO-PAUSE. 
HIDE FRAME fVat    NO-PAUSE.

si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND InvRow WHERE recid(InvRow) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND InvRow WHERE recid(InvRow) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST InvRow OF Invoice
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST InvRow OF Invoice
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT InvRow OF Invoice
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV InvRow OF Invoice
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       InvRow.BillCode
       BCName
       InvRow.FromDate
       InvRow.ToDate
       InvRow.Qty
       InvRow.Amt
       ldMarked
       InvRow.CreditInvNum 
       WITH FRAME sel.

       DISPLAY Invoice.VATIncl
               InvRow.VatPerc WITH FRAME fVat.

END PROCEDURE.

PROCEDURE local-find-others.

    FIND BillItem WHERE 
         BillItem.Brand    = gcBrand AND
         BillItem.BillCode = InvRow.BillCode NO-LOCK NO-ERROR.
    ASSIGN BCName   = IF AVAILABLE BillItem THEN BillItem.BIName ELSE "".

    /* marked now */
    FIND wCredit WHERE wCredit.Line = INTEGER(recid(InvRow)) NO-ERROR.
    ASSIGN ldMarked = IF AVAILABLE wCredit 
                      THEN wCredit.Amt
                      ELSE 0. 

END PROCEDURE.

PROCEDURE local-update-record:

   /* already credited */
   IF InvRow.CreditInvNum NE 0 THEN DO:
      MESSAGE "Line has already been credited."
      VIEW-AS ALERT-BOX.
      RETURN. 
   END.

   FIND wCredit WHERE wCredit.Line = INTEGER(RECID(InvRow)) NO-ERROR.

   IF NOT AVAILABLE wCredit THEN DO:
      CREATE wCredit.
      ASSIGN wCredit.Line = RECID(InvRow)
             wCredit.Amt  = InvRow.Amt.
   END.

   ASSIGN ldMarked = wCredit.Amt.
   UPDATE ldMarked WITH FRAME sel.
   ASSIGN wCredit.Amt = ldMarked.

END PROCEDURE.


PROCEDURE UpdateOthers:

   PAUSE 0.
   DO WITH FRAME crother:
      IF Invoice.InterestAmt NE 0 THEN DO:
         UPDATE llCredInt. 
         IF llCredInt THEN DO:
            IF ldIntAmt = 0 THEN ldIntAmt = Invoice.InterestAmt.
            UPDATE ldIntAmt.
            IF ldIntAmt = 0 THEN llCredInt = FALSE.
         END.
      END.

      IF Invoice.OverPaym NE 0 THEN DO:
         UPDATE llCredOver. 

         IF llCredOver THEN REPEAT WITH FRAME crother:

            IF ldOverAmt = 0 THEN ldOverAmt = Invoice.OverPaym.
            UPDATE ldOverAmt.
            IF ldOverAmt = 0 THEN llCredOver = FALSE.

            IF (ldOverAmt > 0 AND Invoice.OverPaym < 0) OR
               (ldOverAmt < 0 AND Invoice.OverPaym > 0)
            THEN DO:
               MESSAGE "The given sum has a different sign than the"
                       "original overpayment sum."
               VIEW-AS ALERT-BOX.
               NEXT. 
            END.

            LEAVE.
         END.

      END.

      IF Invoice.AdvPaym NE 0 THEN DO:
         UPDATE llCredAdv. 

         IF llCredAdv THEN REPEAT WITH FRAME crother:

            IF ldAdvAmt = 0 THEN ldAdvAmt = Invoice.AdvPaym.
            UPDATE ldAdvAmt.
            IF ldAdvAmt = 0 THEN llCredAdv = FALSE.

            IF (ldAdvAmt > 0 AND Invoice.AdvPaym < 0) OR
               (ldAdvAmt < 0 AND Invoice.AdvPaym > 0)
            THEN DO:
               MESSAGE "The given sum has a different sign than the"
                       "original advance payment sum."
               VIEW-AS ALERT-BOX.
               NEXT. 
            END.

            LEAVE.

         END.

      END.
   END.

   /* Interest */
   FIND wCredit WHERE 
        wCredit.Line = -1 NO-ERROR.
   IF llCredInt = FALSE AND AVAILABLE wCredit
   THEN DELETE wCredit. 
   ELSE IF llCredInt = TRUE THEN DO:
      IF NOT AVAILABLE wCredit THEN DO:
         CREATE wCredit.
         ASSIGN wCredit.Line = -1.
      END.
      wCredit.Amt = ldIntAmt.
   END.

   /* overpayment */
   FIND wCredit WHERE 
        wCredit.Line = -2 NO-ERROR.
   IF llCredOver = FALSE AND AVAILABLE wCredit
   THEN DELETE wCredit. 
   ELSE IF llCredOver = TRUE THEN DO:
      IF NOT AVAILABLE wCredit THEN DO:
         CREATE wCredit.
         ASSIGN wCredit.Line = -2.
      END.
      wCredit.Amt = ldOverAmt.
   END.

   /* advance payment */
   FIND wCredit WHERE 
        wCredit.Line = -3 NO-ERROR.
   IF llCredAdv = FALSE AND AVAILABLE wCredit
   THEN DELETE wCredit. 
   ELSE IF llCredAdv = TRUE THEN DO:
      IF NOT AVAILABLE wCredit THEN DO:
         CREATE wCredit.
         ASSIGN wCredit.Line = -3.
      END.
      wCredit.Amt = ldAdvAmt. 
   END.


END PROCEDURE. 


PROCEDURE DispOthers:

    ASSIGN llCredInt  = FALSE
           ldIntAmt   = 0
           llCredOver = FALSE
           ldOverAmt  = 0
           llCredAdv  = FALSE
           ldAdvAmt   = 0.

    FOR EACH wCredit WHERE 
             wCredit.Line < 0:

       CASE wCredit.Line.
       WHEN -1 THEN ASSIGN llCredInt  = TRUE
                           ldIntAmt   = wCredit.Amt.
       WHEN -2 THEN ASSIGN llCredOver = TRUE
                           ldOverAmt  = wCredit.Amt.
       WHEN -3 THEN ASSIGN llCredAdv  = TRUE
                           ldAdvAmt   = wCredit.Amt.
       END CASE.
    END.

    PAUSE 0.
    DISPLAY Invoice.InterestAmt
            llCredInt
            ldIntAmt
            Invoice.OverPaym
            llCredOver
            ldOverAmt
            Invoice.AdvPaym
            llCredAdv
            ldAdvAmt
    WITH FRAME crother.

END PROCEDURE.


