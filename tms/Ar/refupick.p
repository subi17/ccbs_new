/* ----------------------------------------------------------------------
  MODULE .......: refupick
  TASK .........: Pick payments to be paid as refund
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 30.03.04
  CHANGED ......: 13.09.04/aam Sampo as default bank
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'payment'}
{Ar/refufile.i}

DEF NEW shared VAR siirto AS CHAR.

DEF VAR liCustNum   AS INT NO-UNDO.
DEF VAR ldAmt       AS DEC NO-UNDO. 

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 11.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
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

DEF VAR lcBankAcc    AS CHAR                   NO-UNDO.
DEF VAR ldtDate      AS DATE                   NO-UNDO.
DEF VAR ldTotal      AS DEC                    NO-UNDO. 
DEF VAR liQty        AS INT                    NO-UNDO. 
DEF VAR lcError      AS CHAR                   NO-UNDO. 

  
form
    ttPaym.CustNum    FORMAT ">>>>>>>9"      COLUMN-LABEL "Customer"
    Customer.CustName FORMAT "X(30)"         COLUMN-LABEL "Name"
    ttPaym.Amt        FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Amount"
    Payment.AccDate   FORMAT "99-99-99"      COLUMN-LABEL "Created"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " REFUND PAYMENTS "  + string(pvm,"99-99-99") + " "
    FRAME sel.

FORM
    lcBankAcc AT 2 
       LABEL "Bank Account"
       HELP "Bank account where payments are debited from"
       FORMAT "X(15)"
    BankAcc.BankOffice 
       NO-LABEL
       FORMAT "x(20)"
    ldTotal   AT 58 
       LABEL "Total"    
       FORMAT "->,>>>,>>9.99"
       SKIP
       
    ldtDate   AT 2 LABEL "Payment Date"
       HELP "Date on which payments are debited from bank account"
       FORMAT "99-99-9999"
    liQty AT 58
       LABEL "Qty ."
       FORMAT ">,>>>,>>9"
       SKIP
WITH  OVERLAY ROW 16 WIDTH 80
    COLOR VALUE(cfc)  TITLE COLOR VALUE(ctc) " PAYMENT CONFIGURATION "
    SIDE-LABELS FRAME fBank.


form
    Payment.CustNum COLON 15 
       Customer.CustName NO-LABEL SKIP
    Payment.Voucher COLON 15 SKIP
    Payment.AccDate COLON 15 SKIP
    Payment.PaymAmt COLON 15 SKIP(1)
    "Postings:" AT 5 SKIP
    Payment.AccNum[1] AT 8 NO-LABEL 
      Payment.Posting[1] AT 15 NO-LABEL SKIP
    Payment.AccNum[2] AT 8 NO-LABEL 
      Payment.Posting[2] AT 15 NO-LABEL SKIP
    Payment.AccNum[3] AT 8 NO-LABEL 
      Payment.Posting[3] AT 15 NO-LABEL SKIP
    Payment.AccNum[4] AT 8 NO-LABEL 
      Payment.Posting[4] AT 15 NO-LABEL SKIP
      
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


form /* seek  ttPaym */
    "Customer:" liCustNum
    HELP "Enter customer nbr"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Customer "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  ttPaym */
    "Amount:" ldAmt FORMAT "->,>>>,>>9.99"
    HELP "Enter amount"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Amount "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


FUNCTION fDispTotal RETURNS LOGICAL.

   ASSIGN ldTotal = 0
          liQty   = 0.
          
   FOR EACH ttPaym:
      ASSIGN ldTotal = ldTotal + ttPaym.Amt
             liQty   = liQty + 1.
   END.
   
   PAUSE 0.
   DISPLAY ldTotal liQty WITH FRAME fBank.
    
END FUNCTION.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Customer ," +
         "By Amount    ".

MESSAGE "Collect payments ..".

/* collect all undelivered refund payments */
FOR EACH Payment NO-LOCK WHERE
         Payment.Brand    = gcBrand     AND
         Payment.AccDate >= TODAY - 120 AND
         Payment.PaymType = 6           AND
         Payment.ExpStamp = 0:
         
   CREATE ttPaym.
   ASSIGN ttPaym.Voucher = Payment.Voucher
          ttPaym.CustNum = Payment.CustNum
          ttPaym.Amt     = Payment.PaymAmt.
END. 

HIDE MESSAGE NO-PAUSE.

ldtDate = TODAY.

PAUSE 0.
DISPLAY ldtDate WITH FRAME fBank.

FIND FIRST BankAcc NO-LOCK WHERE
           BankAcc.Brand = gcBrand AND
           BankAcc.BankAcc BEGINS "8" NO-ERROR.
IF AVAILABLE BankAcc THEN DO:
   lcBankAcc = BankAcc.BankAcc.
   DISPLAY lcBankAcc BankAcc.BankOffice WITH FRAME fBank.
END. 

fDispTotal().

RUN local-find-first.

IF AVAILABLE ttPaym THEN ASSIGN
   Memory       = recid(ttPaym)
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
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
   END.
   
   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttPaym WHERE recid(ttPaym) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttPaym THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttPaym).
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
        ufk[1]= 714  ufk[2]= 789  ufk[3]= 1804
        ufk[4] = 927
        ufk[5]= (IF lcRight = "RW" THEN 1757 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttPaym.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttPaym.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW ttPaym.Amt ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttPaym.Amt WITH FRAME sel.
      END.


      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
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
        FIND ttPaym WHERE recid(ttPaym) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttPaym THEN
              ASSIGN FIRSTrow = i Memory = recid(ttPaym).
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
           IF NOT AVAILABLE ttPaym THEN DO:
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
                rtab[1] = recid(ttPaym)
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
           IF NOT AVAILABLE ttPaym THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttPaym).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttPaym WHERE recid(ttPaym) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttPaym THEN DO:
           Memory = recid(ttPaym).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttPaym THEN Memory = recid(ttPaym).
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
           FIND ttPaym WHERE recid(ttPaym) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN Syst/ufcolor.
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        CLEAR FRAME f1.
        UPDATE liCustNum WITH FRAME f1.
        HIDE FRAME f1 NO-PAUSE.

        IF liCustNum > 0 THEN DO:
           FIND FIRST ttPaym WHERE 
                      ttPaym.CustNum >= liCustNum
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE ttPaym THEN DO:
              BELL.
              message "NONE FOUND !".
              PAUSE 1 no-message.
              NEXT BROWSE.
           END.

           ASSIGN order = 1 memory = recid(ttPaym) must-print = TRUE.

           NEXT LOOP.
        END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN Syst/ufcolor.
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        CLEAR FRAME f2.
        UPDATE ldAmt WITH FRAME f2.
        HIDE FRAME f2 NO-PAUSE.

        IF ldAmt > 0 THEN DO:
           FIND FIRST ttPaym WHERE 
                      ttPaym.Amt >= ldAmt
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE ttPaym THEN DO:
              BELL.
              message "NONE FOUND !".
              PAUSE 1 no-message.
              NEXT BROWSE.
           END.

           ASSIGN order = 2 memory = recid(ttPaym) must-print = TRUE.

           NEXT LOOP.
        END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:
         ASSIGN ehto = 9
                ufkey = TRUE.
         RUN Syst/ufkey.
         REPEAT WITH FRAME fBank ON ENDKEY UNDO, LEAVE:
            
            UPDATE lcBankAcc ldtDate WITH FRAME fBank EDITING:
               READKEY.

               IF keylabel(lastkey) = "F9" AND FRAME-FIELD = "lcBankAcc"
               THEN DO:
                   ASSIGN gcHelpParam = "bank"
                          si-recid    = 0.
                   RUN Mc/bankacc.
                   gcHelpParam = "".
                   
                   ehto = 9.
                   RUN Syst/ufkey.
       
                   IF si-recid > 0 THEN DO:
                      FIND BankAcc WHERE RECID(BankAcc) = si-recid 
                      NO-LOCK NO-ERROR.
                      IF AVAILABLE BankAcc THEN DO:
                         lcBankAcc = BankAcc.BankAcc.
                         DISPLAY lcBankAcc BankAcc.BankOffice
                         WITH FRAME fBank.
                      END.
                   END.
                   
                   NEXT. 
               END.
              
               ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0
               THEN DO WITH FRAME fBank:
                  PAUSE 0.                
    
                  IF FRAME-FIELD = "lcBankAcc" THEN DO:
                  
                     IF INPUT lcBankAcc > "" THEN DO:
                        FIND BankAcc WHERE
                             BankAcc.Brand = gcBrand AND
                             BankAcc.BankAcc = INPUT lcBankAcc 
                             NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE BankAcc THEN DO:
                           BELL.
                           MESSAGE "Unknown bank".
                           NEXT.
                        END.
                        DISPLAY BankAcc.BankOffice WITH FRAME fBank.
                     END.
                  END.
                  
               END.

               APPLY LASTKEY.
           
            END.
            LEAVE.
         END.
         DISPLAY lcBankAcc ldtDate WITH FRAME fBank.
         NEXT.
     END.
     
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
        RUN local-find-this (FALSE).

        RUN Mc/memo(INPUT 0,
                 INPUT "payment",
                 INPUT STRING(ttPaym.Voucher),
                 INPUT "Voucher number").

        ufkey = TRUE.
        NEXT.
     END.
     
     /* create payment file */
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW"
     THEN DO:  /* add */

        IF NOT CAN-FIND(FIRST ttPaym) THEN DO:
           MESSAGE "No payments available"
           VIEW-AS ALERT-BOX
           ERROR.
           NEXT.
        END.
        
        ok = FALSE.
        MESSAGE "Start creating a payment file ?"
        VIEW-AS ALERT-BOX
        QUESTION
        BUTTONS YES-NO
        TITLE " PAYMENT FILE TO BANK "
        SET ok.
        
        IF NOT ok THEN NEXT LOOP.
        
        RUN Ar/refufile (INPUT-OUTPUT TABLE ttPaym,
                      lcBankAcc,
                      ldtDate,
                      OUTPUT i,
                      OUTPUT lcError).
            
        IF i = 0 THEN         
        MESSAGE "Creation of payment file failed;" SKIP
                lcError
        VIEW-AS ALERT-BOX
        ERROR.
        
        ELSE MESSAGE "Payment file done for " + STRING(i) +
                     " payments." SKIP
                     lcError
        VIEW-AS ALERT-BOX
        TITLE " Done ".
        
        fDispTotal().
            
        must-print = TRUE.
        RUN local-find-first.
        memory = RECID(ttPaym).
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       ttPaym.CustNum Customer.CustName ttPaym.Amt.

       RUN local-find-NEXT.
       IF AVAILABLE ttPaym THEN Memory = recid(ttPaym).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ttPaym THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ttPaym).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ttPaym.CustNum Customer.CustName ttPaym.Amt.

       IF ok THEN DO:

           DELETE ttPaym.

           fDispTotal().
           
           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ttPaym) 
           THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0
     THEN REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       
       RUN local-find-this(FALSE).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(ttPaym).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttPaym) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttPaym) must-print = TRUE.
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
      FIND ttPaym WHERE recid(ttPaym) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttPaym WHERE recid(ttPaym) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST ttPaym USE-INDEX CustNum NO-ERROR.
   ELSE IF order = 2 THEN FIND FIRST ttPaym USE-INDEX Amt NO-ERROR.
   
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN FIND LAST ttPaym USE-INDEX CustNum NO-ERROR.
   ELSE IF order = 2 THEN FIND LAST ttPaym USE-INDEX Amt NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN FIND NEXT ttPaym USE-INDEX CustNum NO-ERROR.
   ELSE IF order = 2 THEN FIND NEXT ttPaym USE-INDEX Amt NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
 
   IF order = 1 THEN FIND PREV ttPaym USE-INDEX CustNum NO-ERROR.
   ELSE IF order = 2 THEN FIND PREV ttPaym USE-INDEX Amt NO-ERROR.
 
END PROCEDURE.

PROCEDURE local-disp-row:

       RUN local-find-others.
       
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ttPaym.CustNum
       Customer.CustName WHEN AVAILABLE Customer
       "Unknown" WHEN NOT AVAILABLE Customer @ Customer.CustName
       ttPaym.Amt
       Payment.AccDate 
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND Customer WHERE
        Customer.Brand   = gcBrand AND
        Customer.CustNum = ttPaym.CustNum NO-LOCK NO-ERROR. 
   
   FIND Payment WHERE Payment.Voucher = ttPaym.Voucher NO-LOCK.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      FIND Payment WHERE Payment.Voucher = ttPaym.Voucher NO-LOCK.
      
      DISP Payment.Voucher
           Payment.CustNum
           Customer.CustName WHEN AVAILABLE Customer
           "Unknown" WHEN NOT AVAILABLE Customer @ Customer.CustName
           Payment.AccDate
           Payment.PaymAmt 
           Payment.AccNum[1 FOR 4]
           Payment.Posting[1 FOR 4]
      WITH FRAME lis.

      PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.
   
END PROCEDURE.

