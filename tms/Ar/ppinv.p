/* ----------------------------------------------------------------------
  MODULE .......: PPInv
  TASK .........: UPDATEs table PPInv
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.03.04
  CHANGED ......: 10.05.04/aam move directly to batches (ppbatch),
                               only "all invoices" allowed when ilAutoMode
                  16.03.06/aam TF version             
                  18.04.06/aam use payments.p instead of nnlasu.p
                  22.03.07 kl  new param for RUN Ar/payments.p

  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/finvbal.i}
{Func/fpaymplan.i}
{Func/fppinv.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'PPInv'}
{Ar/invdet.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhPPInv AS HANDLE NO-UNDO.
   lhPPInv = BUFFER PPInv:HANDLE.
   RUN StarEventInitialize(lhPPInv).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhPPInv).
   END.

END.
                                    
DEF INPUT  PARAMETER iiPlanID   AS INT NO-UNDO.
DEF INPUT  PARAMETER ilAutoMode AS LOG NO-UNDO.
DEF OUTPUT PARAMETER ol2Batch   AS LOG NO-UNDO. 


DEF NEW shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
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
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.

DEF VAR lcTitle      AS CHAR                   NO-UNDO. 
DEF VAR lcError      AS CHAR                   NO-UNDO. 
DEF VAR ldBal        AS DEC                    NO-UNDO. 
DEF VAR liInvNum     AS INT                    NO-UNDO. 
DEF VAR liValid      AS INT                    NO-UNDO. 

DEF BUFFER bPPInv FOR PPInv.
DEF BUFFER bPPlan FOR PaymPlan.

form
    PPInv.InvNum    COLUMN-LABEL "Invoice"
    Invoice.InvDate
    Invoice.DueDate
    Invoice.InvAmt  COLUMN-LABEL "Inv.Amount"
    PPInv.Amount    COLUMN-LABEL "PP Amount"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
    lcTitle
    FRAME sel.

form
    SKIP(1)
    PPInv.InvNum  
    SKIP(1)
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FUNCTION fPPTotAmount RETURNS LOGICAL.

   FOR EACH bPPInv OF PaymPlan NO-LOCK:
      ACCUMULATE bPPInv.Amount (TOTAL).
   END.
   
   FIND bPPlan WHERE RECID(bPPlan) = RECID(PaymPlan) EXCLUSIVE-LOCK.
   ASSIGN bPPlan.Amount = (ACCUM TOTAL bPPInv.Amount).
   RELEASE bPPlan.

END FUNCTION.


FIND PaymPlan WHERE 
     PaymPlan.PPlanID = iiPlanID NO-LOCK NO-ERROR.
IF NOT AVAILABLE PaymPlan THEN RETURN.
     
ASSIGN lcTitle     = " INVOICES IN PLAN: " +
                     STRING(PaymPlan.CustNum) + "/" +
                     STRING(PaymPlan.PPDate,"99.99.9999") + " "
       gcHelpParam = STRING(PaymPlan.CustNum) + ",TRUE".           

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

ASSIGN orders   = "  By Invoice ,    ,   , By 4"
       ol2Batch = FALSE.

RUN local-find-first.

IF AVAILABLE PPInv THEN ASSIGN
   Memory       = recid(PPInv)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a PPInv  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        
        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           RUN Help/hcustinv.p(PaymPlan.CustNum,FALSE).
                   
           IF siirto = ? THEN LEAVE.
           
           liInvNum = INTEGER(siirto).

           IF liInvNum = 0 THEN LEAVE add-row.
           
           /* check that invoice is valid for payment plan */
           liValid = fValidForPaymPlan(liInvNum,
                                       PaymPlan.PPType,
                                       9999).

           IF liValid > 0 THEN DO:

              lcError = fValidationError(liValid).

              MESSAGE lcError
              VIEW-AS ALERT-BOX ERROR.
              LEAVE add-row.
           END.
               
           FIND Invoice WHERE Invoice.InvNum = liInvNum NO-LOCK.
           
           CREATE PPInv.
           ASSIGN
           PPInv.PPlanID = iiPlanID 
           PPInv.InvNum  = liInvNum.
           PPInv.Amount  = fInvBal(BUFFER Invoice,
                                   TODAY).

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPPInv).

           fAddPPInvoice(PPInv.InvNum).
                        
           fPPTotAmount().
           
           ASSIGN
           Memory = recid(PPInv)
           xrecid = Memory.  
           
           LEAVE add-row.
        END.
              
        LEAVE.
      END.  /* ADD-ROW */

      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND PPInv WHERE recid(PPInv) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PPInv THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(PPInv).
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
         ufk[1]= 829
         ufk[2]= IF PaymPlan.PPStatus < 3 OR PaymPlan.PPStatus = 7
                 THEN 1781 ELSE 0
         ufk[3]= 0  
         ufk[4]= 1778  
         ufk[5]= (IF lcRight = "RW" AND 
                  LOOKUP(STRING(PaymPlan.PPStatus),"1,7") > 0 THEN 5 ELSE 0)
         ufk[6]= (IF lcRight = "RW" AND
                  LOOKUP(STRING(PaymPlan.PPStatus),"1,7") > 0 THEN 4 ELSE 0)
         ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         
         IF ilAutoMode THEN ASSIGN ufk[5] = 0               
                                   ufk[6] = 0.
         
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW PPInv.InvNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PPInv.InvNum WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8,2,F2") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND PPInv WHERE recid(PPInv) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE PPInv THEN
              ASSIGN FIRSTrow = i Memory = recid(PPInv).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE PPInv THEN DO:
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
                rtab[1] = recid(PPInv)
                Memory  = rtab[1].
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
           IF NOT AVAILABLE PPInv THEN DO:
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
              rtab[FRAME-DOWN] = recid(PPInv).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND PPInv WHERE recid(PPInv) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PPInv THEN DO:
           Memory = recid(PPInv).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE PPInv THEN Memory = recid(PPInv).
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
           Memory = rtab[FRAME-DOWN].
           FIND PPInv WHERE recid(PPInv) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* collect all customer's unpaid invoices */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO:
     
        IF CAN-FIND(FIRST PPInv WHERE PPInv.PPlanID = iiPlanID) THEN DO:
           MESSAGE "Plan already has invoice lines"
           VIEW-AS ALERT-BOX.
           NEXT.
        END.
        
        ok = FALSE.
        MESSAGE "All customer's unpaid invoices will be collected into"
                "this payment plan. Start collecting the invoices ?"
        VIEW-AS ALERT-BOX
        QUESTION
        BUTTONS YES-NO
        SET ok.

        IF ok THEN DO:
           fPPInvCustTot(1,
                         OUTPUT ldbal).
           MESSAGE "Total collected amount is" ldBal
           VIEW-AS ALERT-BOX.
           
           fPPTotAmount().
            
           RUN local-find-first.
           ASSIGN must-print = TRUE
                  memory     = RECID(PPInv).
           NEXT LOOP.       
               
        END.
        
     END.

     /* invoice's payments */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO:
     
        RUN local-find-this (FALSE).
        RUN Ar/payments.p(0,PPInv.InvNum,"").
        
        ufkey = TRUE.
        NEXT LOOP.
     END. 

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0
     THEN DO:  /* add */
        {Syst/uright2.i}
        IF CAN-FIND(FIRST PPBatch WHERE PPBatch.PPlanID = iiPlanID)
        THEN DO:
           MESSAGE "Plan has already been divided into batches"
           VIEW-AS ALERT-BOX.
           NEXT.
        END.
        
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* has invoice already been paid (deletion allowed only for plans
          that are not yet in use but check anyway) */
       FOR FIRST Payment NO-LOCK WHERE
                 Payment.InvNum  = PPInv.InvNum AND
                 Payment.PPlanID = PPInv.PPlanID:
          MESSAGE "A payment has already been posted to invoice"    
                  STRING(PPInv.InvNum) SKIP
                  "according to this payment plan." SKIP
                  "Deletion not allowed."
          VIEW-AS ALERT-BOX.
          NEXT.
       END. 

       /* have batches already been made */
       IF CAN-FIND(FIRST PPBatch WHERE PPBatch.PPlanID = PPInv.PPlanID)
       THEN DO:
          MESSAGE "Plan has already been divided into batches." SKIP
                  "Deletion not allowed."
          VIEW-AS ALERT-BOX.
          NEXT.
       END. 
       
       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       PPInv.InvNum PPInv.Amount .

       RUN local-find-NEXT.
       IF AVAILABLE PPInv THEN Memory = recid(PPInv).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE PPInv THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(PPInv).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       PPInv.InvNum PPInv.Amount .

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPPInv).

           fRemPPInvoice(PPInv.InvNum,
                         1,
                         0.0).
 
           DELETE PPInv.

           fPPTotAmount().

           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN 
     REPEAT TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       RUN local-find-this(FALSE).

       ehto = 5.
       RUN Syst/ufkey.p.
       
       /* show details */
       RUN pInvoiceDetails(PPInv.InvNum,
                           TRUE).

       ufkey = TRUE.
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(PPInv) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(PPInv) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8,4,F4") > 0 THEN DO:
        /* move directly to batches */    
        IF LOOKUP(nap,"4,F4") > 0 THEN ol2Batch = TRUE.
 
        LEAVE LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
ASSIGN si-recid    = xrecid
       gcHelpParam = "".


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND PPInv WHERE recid(PPInv) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND PPInv WHERE recid(PPInv) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST PPInv
        WHERE PPInv.PPlanID = iiPlanID
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST PPInv
        WHERE PPInv.PPlanID = iiPlanID
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT PPInv
        WHERE PPInv.PPlanID = iiPlanID
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV PPInv
        WHERE PPInv.PPlanID = iiPlanID
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       PPInv.InvNum 
       Invoice.InvDate WHEN AVAILABLE Invoice
       Invoice.DueDate WHEN AVAILABLE Invoice
       Invoice.InvAmt  WHEN AVAILABLE Invoice
       PPInv.Amount
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   FIND Invoice OF PPInv NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-UPDATE-record:
END PROCEDURE.

