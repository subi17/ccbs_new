/*-----------------------------------------------------------------------------
  MODULE .......: UnregPay.P
  FUNCTION .....: Maintenance TO UnregPaym file 
  APPLICATION ..: 
  AUTHOR .......: lp
  CREATED ......: 20.12.01
  MODIFIED .....: 07.02.02/aam view memo AS EDITOR
                  12.02.02 lp IF NOT registered send back old VALUE 
                              UnregPaym.PaidAmt AND UnregPaym.Interest
                  21.02.02 lp added depositing 
                  28.02.02 lp added NEW FIELD  UnregPaym.State = 0 
                              IF records deleting -> UnregPaym.State = 1
                              added NEW Action -> DISPLAY registered
                  05.03.02 lp added unbooked
                  11.03.02 lp calculated deposite sum (PaidAmt - Booked)
                  28.03.02 aam references to "deposit" changed to "overpayment"
                  08.04.02 lp - Booked TO Advance Payment
                              - every registration TO UnregPaym.Memo 
                              - more AccNum numbers FOR monitoring
                              - register's sum can change
                              - deleted kohsuor1.p using, added PROCEDURE
                  03.05.02 tk  Event logging
                  03.05.02 aam use PaymVouch (fvoucher.i)
                  15.05.02 lp  IF F6 -> DELETE record (NOT State =1)
                  21.05.02 aam use PayAcc instead of RecAcc FOR overpayments
                               AND adv.payments 
                  07.06.02/aam VAT-handling FOR AdvPaym
                  18.06.02/aam mark a new payment source if posted to 
                               overpayment or adv.payment
                  01.08.02/aam mark booking day to memo ("BD:") 
                  15.08.02/tk  f3 -> unregistered payment history
                  10.09.02/aam check period lock,
                               ask accounting date 
                  18.09.02/jp  validation             
                  25.09.02/aam customer balances in table CustBal
                  11.10.02/aam delete changes state to 2,
                               fDispOrder(),
                               layout changes etc. 
                  25.10.02/aam credit loss payments
                  14.11.02/jr  New memo for payment 
                  22.11.02/aam vat handling for cl-payment
                  05.03.03 tk  tokens
                  11.09.03/aam brand
                  07.01.04/aam VatUsage
                  16.04.04/aam payment plan
                  04.05.04/aam account with 6 digits
                  03.08.04/aam eventlog for custintevent corrected
                  22.09.04/aam "CL" to UnregLog for credit loss postings       
                  12.01.05/aam CustNum to calcint.p
                  31.03.05/aam use fCredLossPaid for payment state
                  26.04.05/aam add new record (F5)
                  01.07.05/aam transfer to unregist. denied if unreglog found
                  07.12.05/aam set Payment.RefNum from UnregPaym.RefNum
                  30.05.06/aam longer format for sum and archive id 
                  29.09.06/aam vat from payment to adv.payment invoice,
                               fBalanceInvoicePaid()
                  09.03.07/aam ExtVoucher             
                  13.09.07/aam create request for payment creation
  Version ......: M15
  -------------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable UnregPaym

{Syst/commali.i}
{Func/timestamp.i}
{Syst/eventval.i}
{Func/faccper.i}
{Func/fcustbal.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'unregpaym'}
{Func/finvbal.i}
{Func/fpaymentreq.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhUnregPaym    AS HANDLE NO-UNDO.
   DEFINE VARIABLE lhCustIntEvent AS HANDLE NO-UNDO.

   lhUnregPaym    = BUFFER UnregPaym:HANDLE.
   lhCustIntEvent = BUFFER CustIntEvent:HANDLE.

   RUN StarEventInitialize(lhUnregPaym).
   RUN StarEventInitialize(lhCustIntEvent).
END.

/* Interests */
DEF NEW SHARED VAR intpro  AS DE EXTENT 10 NO-UNDO.
DEF NEW SHARED VAR intdays AS I  EXTENT 10 NO-UNDO.
DEF NEW SHARED VAR intsumm AS DE EXTENT 10 NO-UNDO.

DEF VAR ok         AS LOG FORMAT "Yes/No" NO-UNDO.
DEF VAR haku1      LIKE UnregPaym.PaymDate   NO-UNDO.
DEF VAR haku2      LIKE UnregPaym.CustName  NO-UNDO.
DEF VAR haku3      LIKE UnregPaym.PaidAmt   NO-UNDO.
DEF VAR haku4      LIKE UnregPaym.RefNum     NO-UNDO.
DEF VAR haku5      LIKE UnregPaym.BankAcc   NO-UNDO.
DEF VAR firstline  AS INT                 NO-UNDO.
DEF VAR order      AS INT                 NO-UNDO.
DEF VAR ex-order   AS INT                 NO-UNDO.
DEF VAR memory     AS RECID               NO-UNDO.

DEF VAR line       AS INT FORMAT "99"     NO-UNDO.
DEF VAR delline    AS INT                 NO-UNDO.
DEF VAR must-print AS LOG                 NO-UNDO.
DEF VAR must-add   AS LOG                 NO-UNDO.
DEF VAR ufkey      AS LOG                 NO-UNDO.
DEF VAR fr-header  AS CHAR                NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24     NO-UNDO.
DEF VAR i          AS INT                 NO-UNDO.
DEF VAR t          AS INT                 NO-UNDO.
DEF VAR xrecid     AS RECID.

DEF VAR ykorko     AS DE FORMAT "zzzzzz9.99-"   NO-UNDO.
DEF VAR qdays      AS C                         NO-UNDO.
DEF VAR qperc      AS C                         NO-UNDO.
DEF VAR b-due      AS DE FORMAT "zzzzzz9.99-"   NO-UNDO.
DEF VAR asno       LIKE Customer.CustNum    NO-UNDO.
DEF VAR asname     LIKE Customer.CustName   NO-UNDO.
DEF VAR choise     AS INT FORMAT "9"      NO-UNDO.
DEF VAR xState     LIKE UnregPaym.State     NO-UNDO INIT 0.

DEF VAR IntCalcMet AS I                   NO-UNDO.
DEF VAR PayAcc     AS I                   NO-UNDO.
DEF VAR IntAcc     AS I FORMAT ">>>>>9"   NO-UNDO.
DEF VAR ColAcc     AS I FORMAT ">>>>>9"   NO-UNDO.
DEF VAR RecAcc     AS I FORMAT ">>>>>9"   NO-UNDO.
DEF VAR ApAcc      AS I FORMAT ">>>>>9"   NO-UNDO.
DEF VAR OpAcc      AS I FORMAT ">>>>>9"   NO-UNDO.

DEF VAR booksum    LIKE UnregPaym.Booked    FORMAT "-zz,zz9.99" NO-UNDO.
DEF VAR booksumR   LIKE booksum           NO-UNDO.
DEF VAR booksumC   LIKE booksum           NO-UNDO.
DEF VAR unbooked   LIKE booksum           NO-UNDO.
DEF VAR DelInt     AS LOG                 NO-UNDO.
DEF VAR PreSum     LIKE booksum           NO-UNDO.
DEF VAR OldSum     LIKE booksum           NO-UNDO.
DEF VAR NewSum     LIKE booksum           NO-UNDO.

DEF VAR ldeAPAmt      AS   DEC            NO-UNDO. 
DEF VAR ldtAccDate    AS   DATE           NO-UNDO. 
DEF VAR lcOrder       AS   CHAR           NO-UNDO. 
DEF VAR llCredLoss    AS   LOGIC          NO-UNDO.
DEF VAR liCredLossAcc AS   INT            NO-UNDO.
DEF VAR ldCredLossAmt AS   DEC            NO-UNDO. 
DEF VAR liVatAcc      AS   INT            NO-UNDO. 
DEF VAR liCredCnt     AS   INT            NO-UNDO. 
DEF VAR ldVatAmt      AS   DEC            NO-UNDO.
DEF VAR llCredFound   AS   LOGIC          NO-UNDO.
DEF VAR liState       AS   INT            NO-UNDO. 
DEF VAR lcPref        AS   CHAR           NO-UNDO. 

DEF VAR mench AS CHARACTER FORMAT "x(26)" EXTENT 5
   INITIAL [ "    1. To invoice         ", 
             "    2. To overpayment     ",
             "    3. To advance payment ",
             "    4. To credit loss     ",
             "    5. Exit               " ].

{Func/cparam.i BankAcc       RETURN}. PayAcc        = TMSParam.IntVal.
{Func/cparam.i IntCalcMet    RETURN}. IntCalcMet    = TMSParam.IntVal.
{Func/cparam.i OTIntAcc      RETURN}. IntAcc        = TMSParam.IntVal.
{Func/cparam.i OverPayAcc    RETURN}. OpAcc         = TMSParam.IntVal.
{Func/cparam.i ClaimCostAcc  RETURN}. ColAcc        = TMSParam.IntVal.
{Func/cparam.i ReceivAcc     RETURN}. RecAcc        = TMSParam.IntVal.
{Func/cparam.i AdvPaymAcc    RETURN}. ApAcc         = TMSParam.IntVal.
{Func/cparam.i CreditLossAcc RETURN}. liCredLossAcc = TMSParam.IntVal.


FUNCTION fDispOrder RETURNS LOGICAL.

   CASE order:
   WHEN 1 THEN lcOrder = " By date      ".
   WHEN 2 THEN lcOrder = " By name      ".
   WHEN 3 THEN lcOrder = " By amount.   ".
   WHEN 4 THEN lcOrder = " By ref.num.  ".
   WHEN 5 THEN lcOrder = " By Bank Acc. ".
   OTHERWISE   lcOrder = "              ".
   END CASE.

   CASE xState:
   WHEN 0 THEN lcOrder = " Unregistered:" + lcOrder.
   WHEN 1 THEN lcOrder = "   Registered:" + lcOrder.
   WHEN 2 THEN lcOrder = "      Deleted:" + lcOrder.
   OTHERWISE   lcOrder = "              " + lcOrder.
   END CASE. 

   PUT SCREEN ROW 19 COL 29 lcOrder .

END FUNCTION.

FORM
    UnregPaym.Brand     FORMAT "X(4)" COLUMN-LABEL "Bran"
    UnregPaym.PaymDate  FORMAT "99.99.99"
    UnregPaym.CustName  FORMAT "X(15)"
    UnregPaym.PaidAmt   FORMAT "->>>>>>9.99"
    UnregPaym.RefNum    FORMAT "X(17)"
    UnregPaym.BankAcc   FORMAT "x(14)"
WITH WIDTH 80 ROW 1 OVERLAY SCROLL 1 15 DOWN COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " UNREGISTERED PAYMENTS " + STRING(pvm,"99-99-99") + " "     
    FRAME sel.

FORM                                               
    UnregPaym.CustName  LABEL "Customer name" 
    UnregPaym.RefNum    LABEL "Reference number" AT 36 SKIP

    UnregPaym.ArchiveId LABEL "Archive ID .."  FORMAT "X(18)"
    UnregPaym.PaymSrc   LABEL "Payment source ." 
                        AT 36 SKIP

    UnregPaym.InvNum    LABEL "Invoice nbr ." 
    UnregPaym.BankAcc   LABEL "Bank Account ..." AT 36 SKIP

    ldtAccDate          LABEL "Booking day ." 
                        FORMAT "99-99-9999"
                        HELP  "Booking day for posting"
    UnregPaym.PaidAmt   LABEL "Payment ........" 
                        AT 36 FORMAT "zzzzzz9.99-" SKIP

    UnregPaym.PaymDate  LABEL "Payment day ."  
                        FORMAT "99-99-9999" 
    UnregPaym.Booked    LABEL "Booked sum ....." 
                        AT 36 FORMAT "zzzzzz9.99-" SKIP

    unbooked            LABEL "Unbooked sum ..." 
                        AT 36 FORMAT "zzzzzz9.99-" SKIP(1)

    UnregPaym.AccNum    LABEL "Payment  account"
       booksum             LABEL "Sum ............" AT 36 
                           FORMAT "zzzzzz9.99-" SKIP
    RecAcc              LABEL "Receive  account" 
       booksumR            LABEL "Sum ............" AT 36 
                           FORMAT "zzzzzz9.99-" SKIP
    IntAcc              LABEL "Interest account"
    UnregPaym.Interest     LABEL "Interest ......." AT 36 
                           FORMAT "zzzzzz9.99-" SKIP
    ColAcc              LABEL "Collect. charges" 
       booksumC            LABEL "Sum ............" AT 36 
                           FORMAT "zzzzzz9.99-" SKIP
    DelInt              LABEL "Delay Interest ."  
                        FORMAT "Yes/No" 
                        HELP "Calculate delay interest from payment" SKIP
    "Memo:"  UnregPaym.Memo      NO-LABEL VIEW-AS EDITOR Size 60 BY 5
WITH  OVERLAY ROW 1 CENTERED COLOR VALUE(cfc) TITLE COLOR VALUE(ctc)
    fr-header WITH SIDE-LABELS FRAME lis.

FORM
   SKIP(1)
   mench[1] SKIP
   mench[2] SKIP
   mench[3] SKIP
   mench[4] SKIP
   mench[5]
WITH OVERLAY ROW 6 CENTERED COLOR VALUE(cfc) TITLE COLOR VALUE(ctc)
   " CHOOSE BOOKING TYPE " WITH NO-LABELS FRAME f-menu.

FORM
   asno            LABEL "Customer number "
   asname          LABEL "Customer's name "
   ldtAccDate      LABEL "Booking day ...."
                   FORMAT "99-99-9999"
                   HELP  "Booking day for posting"
   UnregPaym.AccNum LABEL "Bank Account ..."    
   OpAcc           LABEL "Overpaym.account"
   unbooked        LABEL "Unbooked sum ..."
   OldSum          LABEL "Old overpayment "
   PreSum          LABEL "Booking sum ...."
   NewSum          LABEL "New overpayment "
WITH  OVERLAY ROW 6 CENTERED COLOR VALUE(cfc) TITLE COLOR VALUE(ctc)
   " OVERPAYMENT FOR CUSTOMER " WITH SIDE-LABELS 1 COLUMNS FRAME over.

FORM
   asno            LABEL "Customer number "
   asname          LABEL "Customer's name "
   ldtAccDate      LABEL "Booking day ...."
                   FORMAT "99-99-9999"
                   HELP  "Booking day for posting"
   UnregPaym.AccNum LABEL "Bank Account ..."
   ApAcc           LABEL "Adv.Paym.account"
   unbooked        LABEL "Unbooked sum ..."
   OldSum          LABEL "Old Adv.Payment "
   PreSum          LABEL "Booking sum ...."
   NewSum          LABEL "New Adv.Payment "
WITH  OVERLAY ROW 6 CENTERED COLOR VALUE(cfc) TITLE COLOR VALUE(ctc)
   " ADVANCE PAYMENT FOR CUSTOMER " WITH SIDE-LABELS 1 COLUMNS FRAME AdvPaym.

{Func/brand.i}

FORM /* Seek a Date */
   "Brand:" lcBrand skip
   "Date :" haku1
   HELP "Give the payment date"
   WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND Date "
   COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME hayr1.

FORM /* Seek a customer's name */
   "Brand:" lcBrand skip
   "Name :" haku2
   HELP "Give the customer's name or beginning of it"
   WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND Name "
   COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME hayr2.

FORM /* Seek an amount */
   "Brand :" lcBrand skip
   "Amount:" haku3
   HELP "Give the payment amount"
   WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND AMOUNT "
   COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME hayr3.

FORM /* Seek a reference number */
   "Brand :" lcBrand skip
   "Refnum:" haku4
   HELP "Give the reference number"
   WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND REF.NUM. "
   COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME hayr4.

FORM /* Seek a Bank AccNum */
   "Brand:" lcBrand skip
   "Bank :"  haku5
   HELP "Give the bank account"
   WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND Bank Account "
   COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME hayr5.

cfc = "sel". 
RUN Syst/ufcolor. 
ASSIGN ccc = cfc.
VIEW FRAME sel.

xState = 0.
DO WHILE TRUE:
  FIND FIRST UnregPaym USE-INDEX PaymDate WHERE 
     UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.
  IF NOT AVAILABLE UnregPaym THEN xState = xState + 1.
  IF AVAILABLE UnregPaym OR xState > 2 THEN LEAVE.
END.

IF AVAILABLE UnregPaym THEN 
   ASSIGN 
   memory     = RECID(UnregPaym)
   must-print = TRUE 
   must-add   = FALSE.
ELSE ASSIGN 
   memory     = ?
   must-print = FALSE
   must-add   = FALSE.
   
ASSIGN 
xrecid    = ? 
delline   = 0 
ufkey     = TRUE 
order     = 1 
firstline = 0 .

LOOP:
REPEAT WITH FRAME sel:

   IF order <> ex-order THEN DO:
      ex-order = order.
      fDispOrder().
   END.

   IF must-add THEN DO:  /* Add a UnregPaym  */
 
      ASSIGN cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        UnregPaym.Memo:SCREEN-VALUE = "".
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE ADD-ROW:

           PROMPT-FOR UnregPaym.CustName.

           IF INPUT FRAME lis UnregPaym.CustName = ""               
           THEN LEAVE add-row.

           CREATE UnregPaym.
           ASSIGN
           UnregPaym.Brand    = lcBrand
           UnregPaym.UrSeq    = NEXT-VALUE(UrSeq)
           UnregPaym.CustName = INPUT FRAME lis UnregPaym.CustName
           UnregPaym.State    = xState
           UnregPaym.PaymSrc  = "MU"
           UnregPaym.AccNum   = PayAcc
           UnregPaym.AccDate  = TODAY
           UnregPaym.PaymDate = TODAY.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhUnregPaym).

           ASSIGN
           Memory = recid(UnregPaym)
           xrecid = Memory.  
           LEAVE.
        END.

      END.  /* ADD-ROW */

      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE UnregPaym THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   print-line:
   DO :
      IF must-print THEN DO:
         UP FRAME-LINE - 1.  
         FIND UnregPaym WHERE RECID(UnregPaym) = memory NO-LOCK NO-ERROR.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         REPEAT WITH FRAME sel:  
            IF AVAILABLE UnregPaym THEN DO:

               RUN local-disp-row.

               rtab[FRAME-LINE] = RECID(UnregPaym).

               RUN local-find-next.
            END.
            ELSE DO:
               CLEAR NO-PAUSE.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
         UP FRAME-LINE - 1.
         DOWN firstline.
         ASSIGN firstline = 0
                must-print = FALSE.
         PAUSE 0 NO-MESSAGE.

         /* one page of data has been Printed AND
         the cursor is in the UPmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:
      IF ufkey THEN DO:
         ASSIGN
         ufk = 0  
         ufk[1] = 816 ufk[2] = 1253 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         IF xState = 0  THEN 
            ASSIGN 
               ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
               ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)  
               ufk[7] = (IF lcRight = "RW" THEN 815 ELSE 0).

        
         IF xState > 0  THEN ASSIGN ufk[3] = 598 ufk[7] = 596.
         IF xState = 0  THEN ASSIGN ufk[3] = 597.
         IF xState NE 2 THEN ASSIGN ufk[4] = 1630.

         RUN Syst/ufkey.

      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN 
         CHOOSE ROW UnregPaym.PaymDate ;(uchoose.i;) NO-ERROR WITH FRAME sel.
      ELSE IF order = 2 THEN
         CHOOSE ROW UnregPaym.CustName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
      ELSE IF order = 3 THEN
         CHOOSE ROW UnregPaym.PaidAmt ;(uchoose.i;) NO-ERROR WITH FRAME sel.
      ELSE IF order = 4 THEN
         CHOOSE ROW UnregPaym.RefNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
      ELSE IF order = 5 THEN
         CHOOSE ROW UnregPaym.BankAcc ;(uchoose.i;) NO-ERROR WITH FRAME sel.
      COLOR DISPLAY VALUE(ccc) 
      UnregPaym.PaymDate UnregPaym.CustName UnregPaym.PaidAmt 
      UnregPaym.RefNum UnregPaym.BankAcc
      WITH FRAME sel.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 6 THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 5.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND UnregPaym WHERE RECID(UnregPaym) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-prev.
            IF AVAILABLE UnregPaym THEN
               ASSIGN firstline = i memory = RECID(UnregPaym).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      /* previous line */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND UnregPaym WHERE RECID(UnregPaym) = rtab[1] NO-LOCK.
            RUN local-find-prev.
            IF NOT AVAILABLE UnregPaym THEN DO:
               MESSAGE "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.                   
            ELSE DO:
               /* a previous one was found */
               SCROLL DOWN.

               RUN local-disp-row.

               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = RECID(UnregPaym)
               memory = rtab[1].
            END.
         END.
         ELSE UP 1.                      
      END. /* previous line */

      /* NEXT line */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO 
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND UnregPaym WHERE RECID(UnregPaym) = rtab[FRAME-DOWN] NO-LOCK .
            RUN local-find-next.
            IF NOT AVAILABLE UnregPaym THEN DO:          
               MESSAGE "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 NO-MESSAGE.   
               NEXT BROWSE.             
            END.
            ELSE DO:
               /* yet another record was found */
               SCROLL UP.     
               RUN local-disp-row.

               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = RECID(UnregPaym).   
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].  
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */            

      /* previous page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND UnregPaym WHERE RECID(UnregPaym) = memory NO-LOCK NO-ERROR.
         RUN local-find-prev.
         IF AVAILABLE UnregPaym THEN DO:
            memory = RECID(UnregPaym).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               RUN local-find-prev.
               IF AVAILABLE UnregPaym THEN memory = RECID(UnregPaym).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            MESSAGE "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 NO-MESSAGE.
         END.
      END. /* previous page */

      /* NEXT page */
      ELSE IF LOOKUP(nap,"NEXT-page,page-DOWN,+") > 0 THEN DO 
      WITH FRAME sel:
         /* cursor TO the DOWNmost line */
         IF rtab[FRAME-DOWN] = ? THEN DO:
            MESSAGE "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 NO-MESSAGE.
         END.
         ELSE DO: /* the DOWNmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND UnregPaym WHERE RECID(UnregPaym) = memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
         END.
      END. /* NEXT page */

      /* show more information */
      ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
      DO WITH FRAME lis TRANSAction:

         {Syst/uright2.i}
         FIND UnregPaym WHERE RECID(UnregPaym) = rtab[FRAME-LINE(sel)]
         NO-LOCK.
         ASSIGN  fr-header = " SHOW MORE INFORMATION " ufkey = TRUE.
         
         cfc = "lis". RUN Syst/ufcolor. 

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhUnregPaym).
         
         RUN local-update-record.
         HIDE FRAME lis NO-PAUSE. 

         /* IF  User Wanted TO Cancel this Change TRANSACTION */
         IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
         KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhUnregPaym).

         RUN local-disp-row.
         
      END. /* show more information */

      /* FIRST record */
      ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:

         RUN local-find-first.
         ASSIGN memory = RECID(UnregPaym) must-print = TRUE.
         NEXT LOOP.
      END.

      /* LAST record */
      ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO :
         RUN local-find-last.
         ASSIGN memory = RECID(UnregPaym) must-print = TRUE .
         NEXT LOOP.
      END.

      /***************************************
       * Search functions in a separate loop *
       ***************************************/
      IF LOOKUP(nap,"1,f1") > 0 THEN REPEAT WITH FRAME sel:
         ASSIGN 
         ufkey = TRUE ufk = 0 ehto = 1
         ufk[1] = 28  ufk[2] = 30 ufk[3] = 789 ufk[4] = 763
         ufk[5] = 813 ufk[8] = 8. 
         RUN Syst/ufkey.
         IF toimi = 8 THEN NEXT BROWSE.

         /* Seek a Date */
         IF toimi = 1 THEN DO:
            cfc = "puyr". RUN Syst/ufcolor.
            haku1 = ?.
            ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
            DISPLAY lcBrand WITH FRAME hayr1.
            UPDATE lcBrand WHEN gcAllBrand
                   haku1 WITH FRAME hayr1.
            HIDE FRAME hayr1 NO-PAUSE.

            IF haku1 <> ? THEN DO:
               FIND FIRST UnregPaym WHERE 
                  UnregPaym.Brand    = lcBrand AND
                  UnregPaym.PaymDate = INPUT haku1 AND
                  State = xState NO-LOCK NO-ERROR.

               IF NOT AVAILABLE UnregPaym THEN 
                  FIND FIRST UnregPaym WHERE 
                     UnregPaym.Brand = lcBrand AND
                     UnregPaym.PaymDate GE INPUT haku1 AND
                     State = xState NO-LOCK NO-ERROR.

               IF NOT AVAILABLE UnregPaym THEN 
                 FIND FIRST UnregPaym WHERE
                     Unregpaym.Brand  = lcBrand AND
                     UnregPaym.PaymDate LE INPUT haku1 AND
                     State = xState NO-LOCK NO-ERROR.

               IF NOT fRecFound(1) THEN NEXT BROWSE.                      

               NEXT LOOP.
            END.
         END. /* Seek a Date */

         /* Seek a customer's name */
         IF toimi = 2 THEN DO:
           cfc = "puyr". RUN Syst/ufcolor.
            haku2 = "".
            ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
            DISPLAY lcBrand WITH FRAME hayr2.
            UPDATE lcBrand WHEN gcAllBrand
                   haku2 WITH FRAME hayr2.
            HIDE FRAME hayr2 NO-PAUSE.

            IF haku2 <> "" THEN DO:
               FIND FIRST UnregPaym WHERE
                  UnregPaym.Brand    = lcBrand AND
                  UnregPaym.CustName = INPUT haku2 AND
                  State = xState NO-LOCK NO-ERROR.
               IF NOT AVAILABLE UnregPaym THEN 
                  FIND FIRST UnregPaym WHERE 
                     UnregPaym.Brand    = lcBrand AND
                     UnregPaym.CustName GE INPUT haku2 AND
                     State = xState NO-LOCK NO-ERROR.
               IF NOT AVAILABLE UnregPaym THEN 
                  FIND FIRST UnregPaym WHERE 
                     UnregPaym.Brand    = lcBrand AND
                     UnregPaym.CustName LE INPUT haku2 AND
                     State = xState NO-LOCK NO-ERROR.

               IF NOT fRecFound(2) THEN NEXT BROWSE.

               NEXT LOOP.
            END.
         END. /* Seek a customer's name */

         /* Seek an amount */
         IF toimi = 3 THEN DO:
            cfc = "puyr". RUN Syst/ufcolor.
            haku3 = 0.
            ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
            DISPLAY lcBrand WITH FRAME hayr3.
            UPDATE lcBrand WHEN gcAllBrand
                    haku3 WITH FRAME hayr3.
            HIDE FRAME hayr3 NO-PAUSE.

            IF haku3 <> 0 THEN DO:
               FIND FIRST UnregPaym WHERE 
                  UnregPaym.Brand    = lcBrand AND
                  UnregPaym.PaidAmt = INPUT haku3 AND
                  State = xState NO-LOCK NO-ERROR.
               IF NOT AVAILABLE UnregPaym THEN 
                  FIND FIRST UnregPaym WHERE 
                     UnregPaym.Brand    = lcBrand AND
                     UnregPaym.PaidAmt GE INPUT haku3 AND
                     State = xState NO-LOCK NO-ERROR.
               IF NOT AVAILABLE UnregPaym THEN 
                  FIND FIRST UnregPaym WHERE 
                     UnregPaym.Brand    = lcBrand AND
                     UnregPaym.PaidAmt LE INPUT haku3 AND
                     State = xState NO-LOCK NO-ERROR.

               IF NOT fRecFound(3) THEN NEXT BROWSE.

               NEXT LOOP.
            END.
         END. /* Seek an amount */

         /* Seek a reference number */
         IF toimi = 4 THEN DO:
            cfc = "puyr". RUN Syst/ufcolor.
            haku4 = "".
            ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
            DISPLAY lcBrand WITH FRAME hayr4.
            UPDATE lcBrand WHEN gcAllBrand
                   haku4 WITH FRAME hayr4.
            HIDE FRAME hayr4 NO-PAUSE.

            IF haku4 <> "" THEN DO:
               FIND FIRST UnregPaym WHERE 
                  UnregPaym.Brand    = lcBrand AND
                  UnregPaym.RefNum = INPUT haku4 AND
                  State = xState NO-LOCK NO-ERROR.
               IF NOT AVAILABLE UnregPaym THEN 
                  FIND FIRST UnregPaym WHERE 
                     UnregPaym.Brand    = lcBrand AND
                     UnregPaym.RefNum GE INPUT haku4 AND
                     State = xState NO-LOCK NO-ERROR. 
               IF NOT AVAILABLE UnregPaym THEN 
                  FIND FIRST UnregPaym WHERE 
                     UnregPaym.Brand    = lcBrand AND
                     UnregPaym.RefNum LE INPUT haku4 AND
                     State = xState NO-LOCK NO-ERROR.

               IF NOT fRecFound(4) THEN NEXT BROWSE.

               NEXT LOOP.
            END.
         END. /* Seek a reference number */

         /* Seek a Bank AccNum */ 
         IF toimi = 5 THEN DO:
            cfc = "puyr". RUN Syst/ufcolor.
            haku5 = "".
            ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
            DISPLAY lcBrand WITH FRAME hayr5.
            UPDATE lcBrand WHEN gcAllBrand
                   haku5 WITH FRAME hayr5.
            HIDE FRAME hayr5 NO-PAUSE.

            IF haku5 <> "" THEN DO:
               FIND FIRST UnregPaym WHERE 
                   UnregPaym.Brand    = lcBrand AND
                   UnregPaym.BankAcc = INPUT haku5 AND
                   State = xState NO-LOCK NO-ERROR.
               IF NOT AVAILABLE UnregPaym THEN 
                  FIND FIRST UnregPaym WHERE 
                     UnregPaym.Brand    = lcBrand AND
                     UnregPaym.BankAcc GE INPUT haku5 AND
                     State = xState NO-LOCK NO-ERROR.
               IF NOT AVAILABLE UnregPaym THEN 
                  FIND FIRST UnregPaym WHERE 
                     UnregPaym.Brand    = lcBrand AND
                     UnregPaym.BankAcc LE INPUT haku5 AND
                     State = xState NO-LOCK NO-ERROR. 

               IF NOT fRecFound(5) THEN NEXT BROWSE.

               NEXT LOOP.
            END.
         END. /* Seek a Bank AccNum */
      END.

      ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO:
         FIND UnregPaym WHERE RECID(UnregPaym) = rtab[FRAME-LINE(sel)]
         no-lock.
         RUN Ar/unreglog(UnregPaym.UrSeq).
         ufkey = true.
         NEXT.
      END.

      /* display unregistered/registered */
      ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:
        
         IF xState = 0 
         THEN liState = 1.
         ELSE liState = 0.

         FIND FIRST UnregPaym USE-INDEX PaymDate WHERE  
                    UnregPaym.Brand = lcBrand AND
                    UnregPaym.State = liState NO-LOCK NO-ERROR.
         IF NOT AVAILABLE UnregPaym THEN DO:
            MESSAGE "THERE AREN'T ANY"  
                    (IF liState = 0
                     THEN "UN"
                     ELSE "") + "REGISTERED PAYMENTS"
            VIEW-AS ALERT-BOX.
         END. 
         ELSE DO:
            ASSIGN memory     = RECID(UnregPaym)
                   must-print = TRUE
                   ufkey      = TRUE
                   xState     = liState
                   ex-order   = 0. 
            NEXT LOOP.
         END.
      END.

      /* display deleted */
      ELSE IF LOOKUP(nap,"4,f4") > 0 AND xState NE 2 THEN DO:

         FIND FIRST UnregPaym USE-INDEX PaymDate WHERE 
            UnregPaym.Brand = lcBrand AND 
            UnregPaym.State = 2 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE UnregPaym THEN DO:
            MESSAGE "THERE AREN'T ANY DELETED PAYMENTS"
            VIEW-AS ALERT-BOX.
         END. 
         ELSE DO:
            ASSIGN memory     = RECID(UnregPaym)
                   must-print = TRUE
                   ufkey      = TRUE
                   xState     = 2
                   ex-order   = 0. 
            NEXT LOOP.
         END.
      END.

      ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" AND ufk[5] > 0
      THEN DO:  /* add */
         {Syst/uright2.i}
         must-add = TRUE.
         NEXT LOOP.
      END.

      /* removal */
      ELSE IF LOOKUP(nap,"6,f6") > 0 AND xState = 0 AND lcRight = "RW"
      THEN DO TRANSAction: 
         {Syst/uright2.i}
         delline = FRAME-LINE.
         FIND UnregPaym WHERE RECID(UnregPaym) = rtab[FRAME-LINE] NO-LOCK.

         /* line TO be deleted is lightened */
         COLOR DISPLAY VALUE(ctc)
         UnregPaym.PaymDate
         UnregPaym.CustName 
         UnregPaym.PaidAmt
         UnregPaym.BankAcc.

         RUN local-find-next.
         IF AVAILABLE UnregPaym THEN memory = RECID(UnregPaym).
         ELSE DO:
            /* the one TO be deleted is rereaden */
            FIND UnregPaym WHERE RECID(UnregPaym) = rtab[FRAME-LINE] NO-LOCK.

            /* AND THEN the previous one */
            RUN local-find-prev.
            IF AVAILABLE UnregPaym THEN DO:
               ASSIGN
               delline = delline - 1  /* cause the LAST one is TO be deleted */
               memory = RECID(UnregPaym).
            END.
         END.

         /* 'FIND' back TO the ROW TO be deleted */
         FIND UnregPaym WHERE RECID(UnregPaym) = rtab[FRAME-LINE]
         EXCLUSIVE-LOCK.

         ASSIGN ok = FALSE.
         MESSAGE " ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
         COLOR DISPLAY VALUE(ccc)
         UnregPaym.PaymDate 
         UnregPaym.CustName 
         UnregPaym.PaidAmt
         UnregPaym.RefNum
         UnregPaym.BankAcc.

         IF ok THEN DO:

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhUnregPaym).

            UnregPaym.State = 2.  /* which means -> deleted */

            /* in the LAST record was deleted ? */
            IF NOT CAN-FIND(FIRST UnregPaym WHERE 
               UnregPaym.Brand = lcBrand AND UnregPaym.State = xState) THEN DO:
               CLEAR FRAME sel NO-PAUSE.
               PAUSE 0 NO-MESSAGE.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE delline = 0. /* wasn't the LAST one */
      END. /* removal */

      /* booking payments */
      ELSE IF LOOKUP(nap,"7,f7") > 0 AND xState = 0 AND lcRight = "RW" AND
           ufk[7] > 0
      THEN DO:


         FIND UnregPaym WHERE RECID(UnregPaym) = rtab[FRAME-LINE(sel)]
         NO-LOCK NO-ERROR.
         IF UnregPaym.PaidAmt = 0 THEN DO:       
            BELL.
            MESSAGE "Sum is 0 ! Delete this !".
            PAUSE.
            NEXT LOOP.
         END.   
         memory = rtab[FRAME-LINE(sel)].
         ASSIGN ufkey = TRUE ufk = 0 ehto = 3.
         RUN Syst/ufkey.   
         cfc = "lis".
         RUN Syst/ufcolor. 
         ON F1 BELL.
         ON F2 BELL.
         ON F4 BELL.

         llCredLoss = FALSE.

         /* CHOOSE register's type */
         DISPLAY mench WITH FRAME f-menu.
         CHOOSE FIELD mench AUTO-RETURN WITH FRAME f-menu.
         choise = FRAME-INDEX.

         /* to invoice; normal or as credit loss posted */
         IF choise = 1  OR choise = 4 THEN DO:

            llCredLoss = (choise = 4).

            post2inv:
            REPEAT WITH FRAME lis TRANSACTION ON ENDKEY UNDO, LEAVE:

            {Syst/uright2.i}
            FIND UnregPaym WHERE RECID(UnregPaym) = memory EXCLUSIVE-LOCK.

            ASSIGN 
            fr-header = " REGISTERING TO " +
                        IF llCredLoss 
                        THEN "CREDIT LOSS " 
                        ELSE "INVOICE "
            ufkey = TRUE ehto = 9.
            RUN Syst/ufkey.
            cfc = "lis". RUN Syst/ufcolor.  
            on f4 endkey. /* se toimi vain jos ei kutsutaan F9 */

            ASSIGN   
            unbooked   = UnregPaym.PaidAmt - UnregPaym.Booked
            booksum    = unbooked
            booksumR   = unbooked
            ldtAccDate = pvm
            DelInt     = NOT llCredLoss. 

            DISPLAY  UnregPaym.ArchiveId UnregPaym.CustName 
                     UnregPaym.PaymDate  UnregPaym.PaidAmt  UnregPaym.Booked
                     UnregPaym.Interest  UnregPaym.InvNum    UnregPaym.RefNum 
                     UnregPaym.AccNum    UnregPaym.BankAcc  UnregPaym.PaymSrc 
                     UnregPaym.Memo      unbooked RecAcc  IntAcc ColAcc
                     DelInt  booksum     booksumR booksumC.

            UPDATE UnregPaym.InvNum.

            IF UnregPaym.InvNum = "" OR UnregPaym.InvNum = "0" THEN DO:
               HIDE FRAME lis NO-PAUSE.
               LEAVE.
            END. 

            FIND Invoice WHERE Invoice.InvNum = INT(UnregPaym.InvNum) 
            NO-LOCK NO-ERROR.

            IF NOT AVAIL Invoice OR 
               Invoice.Brand NE gcBrand
            THEN DO:
               BELL.
               MESSAGE "Invoice number " UnregPaym.InvNum " is not available !".
               UnregPaym.InvNum = "".
               PAUSE MESSAGE "Press ENTER, this payment is to be cancelled !".
               NEXT LOOP.
            END.

            /* involved in an ongoing payment plan */
            FOR EACH PPInv OF Invoice NO-LOCK,
                FIRST PaymPlan OF PPInv NO-LOCK WHERE
                      PaymPlan.PPStatus < 4:
               
               ok = FALSE.        
               MESSAGE "Invoice is a part of an payment plan." SKIP
                       "Continue with registering payment ?" 
               VIEW-AS ALERT-BOX
               QUESTION 
               BUTTONS YES-NO
               SET ok.
               IF NOT ok THEN NEXT LOOP. 
            END.
             
            IF AVAIL Invoice THEN DO:

               IF NOT llCredLoss THEN DO:
                  /* Calculate open Balance -> b-due */
                  RUN Ar/invbal(Invoice.InvNum, OUTPUT b-due).
               END.

               /* how much has been posted as credit loss */
               ELSE DO:
                  b-due = 0.
                  FOR EACH Payment OF Invoice NO-LOCK:
                     ASSIGN ldVatAmt    = 0
                            llCredFound = FALSE.

                     DO i = 1 TO 10:
                        IF Payment.AccType[i] = 18
                        THEN ASSIGN b-due       = b-due + Payment.Posting[i]
                                    llCredFound = TRUE.
                        ELSE IF Payment.AccType[i] = 5
                        THEN ldVatAmt = ldVatAmt + Payment.Posting[i].
                     END.

                     /* add possible vat */
                      IF llCredFound THEN
                         b-due = b-due + ldVatAmt.
                  END.

                  IF b-due <= 0 THEN DO:
                     MESSAGE "Invoice has no credit loss postings."
                     VIEW-AS ALERT-BOX.
                     NEXT post2inv.
                  END.
               END.

               /* Check open balance / credit loss posting */
               IF b-due < unbooked THEN ASSIGN
                  booksum = b-due 
                  booksumR = b-due.

               DISPLAY DelInt.

               /* posting account */
               IF llCredLoss THEN DO:
                  RecAcc = liCredLossAcc.
                  FIND Account WHERE 
                     Account.Brand  = lcBrand AND
                     Account.AccNum = RecAcc
                     NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE Account OR Account.AccType NE 18
                  THEN DO:
                     MESSAGE "Default account for credit loss is not"
                             "correctly defined."
                     VIEW-AS ALERT-BOX
                     ERROR.
                     NEXT LOOP.
                  END.
               END. 
               ELSE DO:
                  RecAcc = Invoice.ArAccNum.

                  IF UnregPaym.AccNum = 0 THEN UnregPaym.AccNum = PayAcc.
               END.
               
               DISPLAY UnregPaym.AccNum
                       UnregPaym.Interest
                       RecAcc
                       booksumC.

               UPDATE
               ldtAccDate
               UnregPaym.AccNum  
               booksum  VALIDATE(INPUT booksum  <= unbooked,
                        "Check sum! It must be <= Unbooked sum.") 
               booksumR VALIDATE(INPUT booksumR  <= b-due AND 
                                 INPUT booksumR  <= INPUT booksum,
                        "Check sum! Open Balance for invoice is " +
                        STRING(b-due) + " or it must be <= Payment sum ")
               UnregPaym.Interest WHEN NOT llCredLoss 
                   VALIDATE(INPUT UnregPaym.Interest  <= INPUT booksum,
                       "Check sum! It must be <= Payment sum.")
               booksumC WHEN NOT llCredLoss 
                 VALIDATE(INPUT booksumC  <= INPUT booksum AND
                 (INPUT booksumR + INPUT UnregPaym.Interest + INPUT booksumC) =
                 INPUT booksum,"Check sum! It must be <= Payment sum" +
                 " or Payment = Receive + Interest + Collect. ").

               IF booksum = 0 THEN LEAVE.
               
               /* check period */
               IF fPeriodLocked(ldtAccDate,TRUE) THEN NEXT.

               UPDATE DelInt WHEN NOT llCredLoss.

               ASSIGN 
               booksumR         = -(booksumR)
               UnregPaym.Interest = -(UnregPaym.Interest)
               booksumC         = -(booksumC).
               DISP booksumR UnregPaym.Interest booksumC.    

               ASSIGN ok       = FALSE
                      ldeAPAmt = 0. 
               MESSAGE 
               "Register this to invoice ?" 
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               UPDATE ok.
      
               IF ok THEN DO:
                  RUN WritePayment (IF llCredLoss THEN "CL" ELSE "Inv",
                                    Invoice.InvNum,
                                    UnregPaym.AccNum,
                                    RecAcc,
                                    booksum,
                                    booksumR,
                                    UnregPaym.Interest,
                                    booksumC,
                                    DelInt).

                  ASSIGN
                  booksumC           = 0.

                  /* Check Booked sum    */
                  IF unbooked = booksum THEN
                     ASSIGN UnregPaym.State = 1. /* which means -> deleted */

                  HIDE FRAME lis NO-PAUSE.

                  /* Is the LAST record was deleted ? */
                  IF NOT CAN-FIND(FIRST UnregPaym WHERE 
                     UnregPaym.Brand = lcBrand AND UnregPaym.State = xState)
                  THEN DO:
                     CLEAR FRAME sel NO-PAUSE.
                     PAUSE 0 NO-MESSAGE.
                     LEAVE LOOP.
                  END.
                  ELSE DO:
                     RUN local-find-first.
                     ASSIGN
                     memory     = RECID(UnregPaym)
                     must-print = TRUE. 
                     NEXT LOOP.
                  END.
               END.        /* IF ok */ 

               ELSE DO:    /*  IF NOT ok */
                  HIDE FRAME lis NO-PAUSE. 
                  ASSIGN
                  UnregPaym.InvNum    = ""
                  UnregPaym.Interest = 0
                  booksumC         = 0.
                  NEXT LOOP.
               END.
            END. /* IF AVAIL Invoice */

            END. /* repeat */

            must-print = TRUE.       
            NEXT LOOP.           
         END. /* Invoice */

         /* Overpayment */
         IF choise = 2 THEN DO: 

            REPEAT WITH FRAME over TRANSAction:

            {Syst/uright2.i}
            FIND UnregPaym WHERE RECID(UnregPaym) = memory
            EXCLUSIVE-LOCK.

            ASSIGN
            unbooked   = UnregPaym.PaidAmt - UnregPaym.Booked
            ldtAccDate = pvm
            DelInt     = FALSE. 

            ASSIGN  ufkey = TRUE ehto = 9.
            RUN Syst/ufkey.  
            FIND Customer WHERE Customer.CustName = UnregPaym.CustName 
            NO-LOCK NO-ERROR.
            IF AVAIL Customer THEN ASSIGN 
               asno   = Customer.CustNum
               asname = Customer.CustName
               OldSum = fGetCustBal(Customer.CustNum,"TOTAL","OP")
               PreSum = unbooked
               NewSum = OldSum + unbooked.
            IF NOT AVAIL Customer THEN 
               ASSIGN 
               asno   = 0
               asname = ""
               OldSum = 0
               PreSum = unbooked
               NewSum = unbooked.

            IF UnregPaym.AccNum = 0 THEN UnregPaym.AccNum = PayAcc.
            DISPLAY asno asname UnregPaym.AccNum OpAcc unbooked 
                    OldSum PreSum NewSum DelInt .
            UPDATE asno validate(can-find(customer where customer.custnum = 
             asno),"Unknown Customer number!").
            IF asno  = 0 THEN LEAVE.

            FIND Customer WHERE Customer.CustNum = asno NO-LOCK.
            IF Customer.Brand NE gcBrand
            THEN DO:
               MESSAGE "Unknown customer"
               VIEW-AS ALERT-BOX.
               NEXT.
            END.

            OldSum = fGetCustBal(Customer.CustNum,"TOTAL","OP").

            DISP Customer.CustName @ asname 
                 OldSum
                 (OldSum + unbooked) @ NewSum.
            UPDATE ldtAccDate
                   UnregPaym.AccNum 
                   VALIDATE(Can-find(account where 
                        Account.Brand = lcBrand AND
                        account.accnum = unregpaym.accnum),
                        "Unknown Account Number!")
                   PreSum VALIDATE(INPUT FRAME over PreSum <= unbooked
                   AND INPUT FRAME over PreSum >= 0, 
                   "Check sum! It must be > 0 AND <= Unbooked.").

            IF UnregPaym.AccNum = 0 OR PreSum = 0 THEN LEAVE.

            /* check period */
            IF fPeriodLocked(ldtAccDate,TRUE) THEN NEXT.

            DISP (OldSum + PreSum) @ NewSum.

            ASSIGN ok = FALSE.
            MESSAGE "Register this as overpayment ?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            UPDATE ok.
        
            IF ok THEN DO:  
               fCustBal(Customer.CustNum,
                        "",
                        "OP",
                        PreSum). 
               ASSIGN  
               ldeAPAmt           = 0. 

               RUN WritePayment ("OP",
                                 0,UnregPaym.AccNum,OpAcc,PreSum,-(PreSum),
                                 0,0,
                                 DelInt).

               MESSAGE UnregPaym.CustName "Payment is Booked to overpayment."
               VIEW-AS ALERT-BOX 
               INFORMATION.

               ASSIGN UnregPaym.Booked = UnregPaym.Booked + PreSum.
               IF unbooked = PreSum THEN
                  ASSIGN UnregPaym.State = 1. /* which means -> deleted */

               /* Is the LAST record was deleted ? */
               IF NOT CAN-FIND(FIRST UnregPaym WHERE 
                 UnregPaym.Brand = lcBrand AND UnRegPaym.State = xState) 
               THEN DO:
                  CLEAR FRAME sel NO-PAUSE.
                  PAUSE 0 NO-MESSAGE.
                  HIDE FRAME lis NO-PAUSE.
                  LEAVE LOOP.
               END.
               ELSE DO:
                  RUN local-find-first.
                  ASSIGN
                  memory     = RECID(UnregPaym)
                  must-print = TRUE. 
                  NEXT LOOP.
               END.
            END.
            ELSE DO:
               ASSIGN
               memory     = RECID(UnregPaym)
               must-print = TRUE.
               NEXT LOOP.
            END.   
         END. /*repeat*/        
         must-print = TRUE.  
         NEXT LOOP.
         END. /* Overpayment */

         /* Advance Payment */    
         IF choise = 3 THEN DO:

            REPEAT WITH FRAME AdvPaym TRANSAction:

            {Syst/uright2.i}
            FIND UnregPaym WHERE RECID(UnregPaym) = memory
            EXCLUSIVE-LOCK.

            ASSIGN 
            unbooked   = UnregPaym.PaidAmt - UnregPaym.Booked
            ldtAccDate = pvm
            DelInt     = FALSE.

            ASSIGN  ufkey = TRUE ehto = 9.
            RUN Syst/ufkey.
            FIND Customer WHERE Customer.CustName = UnregPaym.CustName 
            NO-LOCK NO-ERROR.
            IF AVAIL Customer THEN
               ASSIGN 
               asno   = Customer.CustNum
               asname = Customer.CustName
               OldSum = fGetCustBal(Customer.CustNum,"TOTAL","AP")
               PreSum = unbooked
               NewSum = OldSum + unbooked.

            ELSE ASSIGN 
               asno   = 0
               asname = ""
               OldSum = 0
               PreSum = unbooked
               NewSum = unbooked.

            IF UnregPaym.AccNum = 0 THEN UnregPaym.AccNum = PayAcc.
            DISPLAY asno asname UnregPaym.AccNum ApAcc unbooked
                    OldSum PreSum NewSum DelInt.

            UPDATE asno validate(can-find(customer where customer.custnum = 
                                          asno),"Unknown Customer number!").
            IF asno = 0 THEN LEAVE.

            FIND Customer WHERE Customer.CustNum = asno NO-LOCK.
            IF Customer.Brand NE gcBrand
            THEN DO:
               MESSAGE "Unknown customer"
               VIEW-AS ALERT-BOX.
               NEXT.
            END.

            OldSum = fGetCustBal(Customer.CustNum,"TOTAL","AP").

            DISP Customer.CustName @ asname 
                 OldSum
                 (OldSum + unbooked) @ NewSum.
            UPDATE ldtAccDate
                   UnregPaym.AccNum 
                   VALIDATE(Can-find(account where 
                      Account.Brand = lcBrand AND
                      account.accnum = unregpaym.accnum),
                      "Unknown Account Number!")
                   PreSum VALIDATE(INPUT FRAME AdvPaym PreSum <= unbooked AND
                   INPUT FRAME AdvPaym PreSum >= 0, 
                   "Check sum! It must be > 0 AND <= Unbooked.").

            IF UnregPaym.AccNum = 0 OR PreSum = 0 THEN LEAVE.

            /* check period */
            IF fPeriodLocked(ldtAccDate,TRUE) THEN NEXT.

            DISP (OldSum + PreSum) @ NewSum.

            ASSIGN ok = FALSE.
            MESSAGE "Register this as advance payment ? "
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            UPDATE ok.

            IF ok THEN DO:  
               fCustBal(Customer.CustNum,
                        "",
                        "AP",
                        PreSum). 
               ASSIGN
               ldeApAmt        = PreSum. 

               RUN WritePayment ("AP",
                                 0,UnregPaym.AccNum,ApAcc,PreSum,-(PreSum),
                                 0,0,
                                 DelInt).

               MESSAGE UnregPaym.CustName 
                       "Payment is Booked to advance payment"
               VIEW-AS ALERT-BOX INFORMATION.

               ASSIGN UnregPaym.Booked = UnregPaym.Booked + PreSum.
               IF unbooked = PreSum THEN
                  ASSIGN UnregPaym.State = 1. /* which means -> deleted */

               /* Is the LAST record was deleted ? */
               IF NOT CAN-FIND(FIRST UnregPaym WHERE 
                  UnregPaym.Brand = lcBrand AND UnRegPaym.State = xState) 
               THEN DO:
                  CLEAR FRAME sel NO-PAUSE.
                  PAUSE 0 NO-MESSAGE.
                  HIDE FRAME lis NO-PAUSE.
                  LEAVE LOOP.
               END.
               ELSE DO:
                  RUN local-find-first.
                  ASSIGN
                  memory     = RECID(UnregPaym)
                  must-print = TRUE. 
                  NEXT LOOP.
               END.
            END.
            ELSE DO:
               ASSIGN
               memory     = RECID(UnregPaym)
               must-print = TRUE.
               NEXT LOOP.
            END.   
         END.  /* repeat */
         must-print = TRUE.  
         NEXT LOOP.
         END.  /* Advance Payment */

         /* Exit */
         IF choise = 5 THEN DO:
            must-print = TRUE.
            NEXT LOOP.
         END.
      END.

      ELSE IF LOOKUP(nap,"7,f7") > 0 AND 
           xState >= 1               AND
           xState <= 2               AND
           ufk[7] > 0 
      THEN DO TRANSAction:

         FIND UnregPaym WHERE RECID(UnregPaym) = rtab[FRAME-LINE(sel)]
         NO-LOCK.
         
         IF CAN-FIND(FIRST UnregLog OF UnregPaym) THEN DO:
             MESSAGE "Transfer is not allowed for this row."
             VIEW-AS ALERT-BOX INFORMATION.
             NEXT.
         END. 
                           
         ASSIGN ok = FALSE.
         MESSAGE "ARE YOU SURE YOU WANT TO TRANSFER PAYMENT BACK"
                 "TO UNREGISTERED ?" 
         VIEW-AS ALERT-BOX QUESTION
         BUTTONS YES-NO
         UPDATE ok.

         IF ok THEN DO:

            FIND CURRENT UnregPaym EXCLUSIVE-LOCK.
            IF llDoEvent THEN RUN StarEventSetoldBuffer(lhUnregPaym).

            IF UnregPaym.State = 1 THEN  ASSIGN
               UnregPaym.InvNum    = ""
               UnregPaym.Interest = 0
               UnregPaym.Booked   = 0.

            ASSIGN UnregPaym.State    = 0.

            IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhUnregPaym).

            RUN local-find-first.

            IF NOT AVAILABLE UnregPaym THEN DO:
               xState = 0.
               RUN local-find-first.
            END.

            IF AVAILABLE UnregPaym THEN
               ASSIGN
               memory     = RECID(UnregPaym)
               must-print = TRUE
               must-add   = FALSE
               ex-order   = 0
               ufkey      = TRUE. 

            IF NOT AVAILABLE UnregPaym THEN DO:
               MESSAGE "THERE AREN'T ANY UNREGISTERED PAYMENTS"
               VIEW-AS ALERT-BOX ERROR.
               RETURN.
            END.
         END.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

   END.  /* BROWSE */
END.  /* LOOP */


PROCEDURE WritePayment:

   DEF INPUT PARAMETER icType   AS CHAR              NO-UNDO.
   DEF INPUT PARAMETER iiInvNum LIKE Invoice.InvNum  NO-UNDO.
   DEF INPUT PARAMETER WPayAcc  AS INT               NO-UNDO.
   DEF INPUT PARAMETER WRecAcc  AS INT               NO-UNDO.
   DEF INPUT PARAMETER PaySum   LIKE Payment.PaymAmt NO-UNDO.
   DEF INPUT PARAMETER RecSum   LIKE PaySum          NO-UNDO.
   DEF INPUT PARAMETER IntSum   LIKE PaySum          NO-UNDO.
   DEF INPUT PARAMETER ColSum   LIKE PaySum          NO-UNDO.
   DEF INPUT PARAMETER ilIntEvent AS LOG             NO-UNDO.

   DEF VAR liPaymType AS INT  NO-UNDO. 
   DEF VAR lcPaymSrc  AS CHAR NO-UNDO.
   DEF VAR lcPosting  AS CHAR NO-UNDO.
   DEF VAR lcAccount  AS CHAR NO-UNDO.
   DEF VAR ldStamp    AS DEC  NO-UNDO.
   DEF VAR lcResult   AS CHAR NO-UNDO. 
   DEF VAR liRequest  AS INT  NO-UNDO.

   CASE icType:
   WHEN "CL" THEN liPaymType = 2.
   WHEN "OP" THEN liPaymType = 3.
   WHEN "AP" THEN liPaymType = 4.
   OTHERWISE liPaymType = 0.
   END CASE. 

   lcPaymSrc = IF UnregPaym.PaymSrc NE ""
               THEN UnregPaym.PaymSrc
               ELSE "UP".
   /* if invoice number is zero -> posted to OP or AP -> new payment source */
   IF iiInvNum = 0 THEN lcPaymSrc = SUBSTRING(lcPaymSrc,1,1) + "B".

               
   IF IntSum NE 0 THEN ASSIGN 
      lcPosting = STRING(100 * IntSum)
      lcAccount = STRING(IntAcc).
   IF ColSum NE 0 THEN ASSIGN
      lcPosting = lcPosting + (IF lcPosting > "" THEN ":" ELSE "") + 
                  STRING(100 * ColSum)
      lcAccount = lcAccount + (IF lcAccount > "" THEN ":" ELSE "") +
                  STRING(ColAcc).
   
   IF lcPosting > "" THEN ASSIGN
      lcPosting = "::" + lcPosting
      lcAccount = "::" + lcAccount.

   lcAccount = STRING(UnregPaym.AccNum) + lcAccount.
   
   IF iiInvNum > 0 THEN 
      FIND Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK.

   ldStamp = fMake2DT(ldtAccDate,IF ldtAccDate > TODAY THEN 3600 ELSE TIME).
   
   liRequest = 
      fPaymentWithPostingsRequest(Customer.CustNum,
                                  liPaymType,
                                  lcPaymSrc + "/UP:" + STRING(UnregPaym.UrSeq),
                                  UnregPaym.PaymDate,
                                  iiInvnum,
                                  PaySum,
                                  lcPosting,
                                  lcAccount,
                                  "From unregistered payment: " + 
                                     UnregPaym.Memo,     /* memo */
                                  ldStamp,                     
                                  0,                     /* control */
                                  INTEGER(ilIntEvent),   /* interest */
                                  "",                    /* creator */
                                  OUTPUT lcResult).
                                  
   IF liRequest > 0 THEN 
      MESSAGE "Request ID for payment is" liRequest
      VIEW-AS ALERT-BOX 
      TITLE " Request Created ".
   ELSE 
      MESSAGE "Request creation failed:" SKIP
              lcResult
      VIEW-AS ALERT-BOX ERROR.
    
   
END PROCEDURE.

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

PROCEDURE local-disp-row:

   DISPLAY UnregPaym.Brand 
           UnregPaym.PaymDate
           UnregPaym.CustName
           UnregPaym.PaidAmt
           UnregPaym.RefNum
           UnregPaym.BankAcc
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-next:

   IF order = 1 THEN FIND NEXT UnregPaym
      USE-INDEX PaymDate WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND NEXT UnregPaym
      USE-INDEX CustName WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = XState NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN FIND NEXT UnregPaym
      USE-INDEX PaidAmt WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.
   ELSE IF order = 4 THEN FIND NEXT UnregPaym
      USE-INDEX RefNum WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.
   ELSE IF order = 5 THEN FIND NEXT UnregPaym
      USE-INDEX BankAcc WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-prev:

   IF order = 1 THEN FIND PREV UnregPaym
      USE-INDEX PaymDate WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND PREV UnregPaym
      USE-INDEX CustName WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = XState NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN FIND PREV UnregPaym
      USE-INDEX PaidAmt WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.
   ELSE IF order = 4 THEN FIND PREV UnregPaym
      USE-INDEX RefNum WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.
   ELSE IF order = 5 THEN FIND PREV UnregPaym
      USE-INDEX BankAcc WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-first:

   IF order = 1 THEN FIND FIRST UnregPaym
      USE-INDEX PaymDate WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND FIRST UnregPaym
      USE-INDEX CustName WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = XState NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN FIND FIRST UnregPaym
      USE-INDEX PaidAmt WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.
   ELSE IF order = 4 THEN FIND FIRST UnregPaym
      USE-INDEX RefNum WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.
   ELSE IF order = 5 THEN FIND FIRST UnregPaym
      USE-INDEX BankAcc WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-last:

   IF order = 1 THEN FIND LAST UnregPaym
      USE-INDEX PaymDate WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN FIND LAST UnregPaym
      USE-INDEX CustName WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = XState NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN FIND LAST UnregPaym
      USE-INDEX PaidAmt WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.
   ELSE IF order = 4 THEN FIND LAST UnregPaym
      USE-INDEX RefNum WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.
   ELSE IF order = 5 THEN FIND LAST UnregPaym
      USE-INDEX BankAcc WHERE 
      UnregPaym.Brand = lcBrand AND UnregPaym.State = xState NO-LOCK NO-ERROR.

END PROCEDURE.


PROCEDURE local-UPDATE-record:

   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      IF UnregPaym.State = 0 THEN ASSIGN
         unbooked = UnregPaym.PaidAmt - UnregPaym.Booked
         booksum  = unbooked
         booksumR = unbooked.
      ELSE ASSIGN 
         unbooked = 0 
         booksum  = unbooked
         booksumR = unbooked.
      
      ldtAccDate = UnregPaym.AccDate.
      
      DISP 
      UnregPaym.CustName
      UnregPaym.ArchiveId 
      UnregPaym.InvNum   
      ldtAccDate
      UnregPaym.PaymDate 
      UnregPaym.AccNum   
 
      UnregPaym.RefNum  
      UnregPaym.PaymSrc  
      UnregPaym.BankAcc   
      UnregPaym.PaidAmt 
      UnregPaym.Booked   
      UnregPaym.Interest 
  
      UnregPaym.Memo  
      unbooked
      RecAcc IntAcc ColAcc 
      DelInt 
      booksum booksumR booksumC
      WITH FRAME lis.

      IF UnregPaym.State = 0 AND UnregPaym.PaymSrc = "MU" AND
         lcRight = "RW" 
      THEN DO:

         IF NEW UnregPaym THEN toimi = 1.
            
         ELSE DO:
            ASSIGN ufk    = 0
                   ufk[1] = 7
                   ufk[8] = 8
                   ehto   = 0.
                
            RUN Syst/ufkey.
         END.
         
         IF toimi = 1 THEN DO:
            
            ehto = 9. RUN Syst/ufkey.
         
            FIND CURRENT UnregPaym EXCLUSIVE-LOCK.
            
            UPDATE
            UnregPaym.CustName  WHEN NOT NEW UnregPaym
            UnregPaym.ArchiveId 
            UnregPaym.InvNum   
            ldtAccDate
            UnregPaym.PaymDate 
            UnregPaym.AccNum   
 
            UnregPaym.RefNum  
            UnregPaym.BankAcc   
            UnregPaym.PaidAmt   
            UnregPaym.Booked   
            UnregPaym.Interest 
  
            UnregPaym.Memo  
            WITH FRAME lis EDITING:

               READKEY.

               IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO 
               WITH FRAME lis:
                  PAUSE 0.

                  IF FRAME-FIELD = "AccNum" THEN DO:
                     IF INPUT FRAME lis UnregPaym.AccNum > 0 THEN DO:
                        FIND Account WHERE 
                             Account.Brand  = lcBrand AND
                             Account.AccNum =
                        INPUT FRAME lis UnregPaym.AccNum NO-LOCK NO-ERROR.
                        IF NOT AVAIL Account THEN DO:
                           BELL.
                           MESSAGE "Unknown account !".
                           NEXT.
                        END.
                     END.
                  END.
        
               END.
               APPLY LASTKEY.
            END. /* EDITING */
           
            UnregPaym.AccDate = ldtAccDate.
            
         END.
           
      END.
      
      ELSE DO:
         ehto = 5.
         RUN Syst/ufkey.
         PAUSE MESSAGE "Press ENTER to continue".
      END.
      
      LEAVE.
   END.
END PROCEDURE.


