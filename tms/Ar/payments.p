/* ----------------------------------------------------------------------------
  MODULE .......: Payments
  TASK .........: Browse table Payment
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 30.04.02/aam "kr" references removed
                  30.05.02/tk  eventlogging added
                  07.06.02/aam use Invoice.OverPaym FOR overpayment
                  12.09.02/jp  modified viewer sums
                  25.10.02/aam show InvType,
                               call also from commontt (one customer's data),
                               layouts, 
                               names for types etc.
                  12.11.02/jr  "suoritus" => "payment" in memo              
                  15.11.02 lp  - use invdet.i for invoice details
                               - if not available payments -> RETURN
                  03.03.03 tk  tokens             
                  15.09.03/aam brand
                  06.02.04 jp custnum for memo
                  01.04.04/aam PaymFile
                  24.09.04/aam cancel refund (update PaymType)
                  14.04.05/aam show eventlog 
                  12.05.05/aam new order; PaymSrc + PaymAmt,
                               disp PaymType
                  26.05.05/aam PaymArc, ImportStamp
                  19.10.05/aam remove path from PaymFile
                  21.12.05/aam payment source info in other actions
                  13.04.06/aam Payment.RefNum
                  18.04.06/aam iiInvNum
                  03.04.07/aam use RefNum index for type 7,
                               longer format for accounts
                  24.04.07/aam PaymType 8             
                  24.05.07/aam ExtInvID
                  11.09.07/aam use brand in prepaidrequest find,
                               longer format for pprequest (=CustNum)
  Version ......: M15
  --------------------------------------------------------------------------- */

&GLOBAL-DEFINE TimeStampDef NO
&GLOBAL-DEFINE TMSCodeDef NO
&GLOBAL-DEFINE BrTable Payment

{commali.i} 
{timestamp.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'payment'}
{eventval.i}
{invdet.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhPayment AS HANDLE NO-UNDO.
   lhPayment = BUFFER Payment:HANDLE.
   RUN StarEventInitialize(lhPayment).

END.


DEFINE INPUT PARAMETER iiCustNum AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER iiInvNum  AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER icRefNum  AS CHARACTER NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.
DEF VAR cu-text  AS c NO-UNDO.

DEF VAR lcExtInvID   LIKE Payment.ExtInvID     NO-UNDO.
DEF VAR CustNum      LIKE Payment.CustNum      NO-UNDO.
DEF VAR lcExtVoucher LIKE Payment.ExtVoucher   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 4.
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
DEF VAR AccName      AS C EXTENT 6             NO-UNDO. 

DEF VAR lcPaymType   AS CHAR                   NO-UNDO.
DEF VAR lcTypeName   AS CHAR EXTENT 6          NO-UNDO. 
DEF VAR lcPaymSrc    AS CHAR                   NO-UNDO.
DEF VAR ldPaymAmt    AS DEC                    NO-UNDO. 
DEF VAR lcCreated    AS CHAR                   NO-UNDO.
DEF VAR ldtDate      AS DATE                   NO-UNDO. 
DEF VAR liTime       AS INT                    NO-UNDO.
DEF VAR lcPaymFile   AS CHAR                   NO-UNDO.
DEF VAR llPPReq      AS LOGICAL                NO-UNDO.

form
    Payment.ExtVoucher FORMAT "X(12)"         COLUMN-LABEL "Voucher"
    Payment.AccDate    FORMAT "99-99-99"      COLUMN-LABEL "Date"
    Payment.ExtInvID   FORMAT "X(12)"         COLUMN-LABEL "Invoice"
    Payment.CustNum    FORMAT ">>>>>>>>>9"
    Payment.InvAmt     format "->>>>>>9.99" 
    Payment.PaymAmt    format "->>>>>>9.99"
    Payment.PaymSrc    FORMAT "x(3)"          COLUMN-LABEL "PS"
    Payment.PaymType   FORMAT ">9"            COLUMN-LABEL "T"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
          "  PAYMENTS  "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    Payment.ExtVoucher label "Voucher ...." 
    Payment.CustNum    label "Customer .."  AT 35 SKIP

    Payment.PaymDate  label "Payment Date"                         
    Payment.CustName  label "CustName .."  AT 35 SKIP 

    Payment.AccDate   label "Account.Date"  
    Payment.ExtInvID  label "Invoice ..."  AT 35 SKIP

    Payment.PaymAmt   label "Payment ...."  FORMAT "->,>>>,>>9.99" 
    Payment.InvAmt    label "Invoice Amt"  FORMAT "->,>>>,>>9.99" AT 35 SKIP

    Payment.PaymSrc   label "Paym. Source"  
    Payment.InvDate   label "Inv. Date ."  AT 35 SKIP

    Payment.PaymType  label "Payment Type"
       lcPaymType FORMAT "X(15)" NO-LABEL 
    Payment.DueDate   label "Due Date .."  AT 35 SKIP         
    
    Payment.PaymArc   LABEL "Archive ID ."
        FORMAT "X(20)" 
    lcCreated         LABEL "Created ..." AT 35 FORMAT "X(17)" SKIP

    lcPaymFile        LABEL "File (batch)"
        FORMAT "X(60)" SKIP
    Payment.RefNum    LABEL "Reference .." FORMAT "X(20)"
"----------------------------------------------------------------------------"
    SKIP

  "Account" TO 8 
  "Name"   AT 10
  "Amount" TO 47
  "Type"   AT 52
  SKIP

  Payment.AccNum[1]  FORMAT "zzzzzzz9"      NO-LABEL TO 8
  AccName[1]         FORMAT "X(25)"         NO-LABEL AT 10
  Payment.Posting[1] FORMAT "z,zzz,zz9.99-" NO-LABEL TO 48
  Payment.AccType[1] FORMAT ">>>"           NO-LABEL AT 52
  lcTypeName[1]      FORMAT "X(23)"         NO-LABEL AT 56
  SKIP

  Payment.AccNum[2]  FORMAT "zzzzzzz9"      NO-LABEL TO 8
  AccName[2]         FORMAT "X(25)"         NO-LABEL AT 10
  Payment.Posting[2] FORMAT "z,zzz,zz9.99-" NO-LABEL TO 48
  Payment.AccType[2] FORMAT ">>>"           NO-LABEL AT 52 
  lcTypeName[2]      FORMAT "X(23)"         NO-LABEL AT 56
  SKIP

  Payment.AccNum[3]  FORMAT "zzzzzzz9"      NO-LABEL TO 8
  AccName[3]         FORMAT "X(25)"         NO-LABEL AT 10
  Payment.Posting[3] FORMAT "z,zzz,zz9.99-" NO-LABEL TO 48
  Payment.AccType[3] FORMAT ">>>"           NO-LABEL AT 52 
  lcTypeName[3]      FORMAT "X(23)"         NO-LABEL AT 56
  SKIP

  Payment.AccNum[4]  FORMAT "zzzzzzz9"      NO-LABEL TO 8
  AccName[4]         FORMAT "X(25)"         NO-LABEL AT 10
  Payment.Posting[4] FORMAT "z,zzz,zz9.99-" NO-LABEL TO 48
  Payment.AccType[4] FORMAT ">>>"           NO-LABEL AT 52 
  lcTypeName[4]      FORMAT "X(23)"         NO-LABEL AT 56
  SKIP

  Payment.AccNum[5]  FORMAT "zzzzzzz9"      NO-LABEL TO 8
  AccName[5]         FORMAT "X(25)"         NO-LABEL AT 10
  Payment.Posting[5] FORMAT "z,zzz,zz9.99-" NO-LABEL TO 48
  Payment.AccType[5] FORMAT ">>>"           NO-LABEL AT 52 
  lcTypeName[5]      FORMAT "X(23)"         NO-LABEL AT 56
  SKIP

  Payment.AccNum[6]  FORMAT "zzzzzzz9"      NO-LABEL TO 8
  AccName[6]         FORMAT "X(25)"         NO-LABEL AT 10
  Payment.Posting[6] FORMAT "z,zzz,zz9.99-" NO-LABEL TO 48
  Payment.AccType[6] FORMAT ">>>"           NO-LABEL AT 52 
  lcTypeName[6]      FORMAT "X(23)"         NO-LABEL AT 56

WITH OVERLAY ROW 1 centered
     COLOR VALUE(cfc) TITLE  " Details (" + STRING(Payment.Voucher) + ") "
     side-labels FRAME lis.

{brand.i}

form /* seek  Voucher */
    "Brand .:" lcBrand skip
    "Voucher:" lcExtVoucher
    HELP "Enter Voucher number "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND VOUCHER "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  InvNum */
    "Brand .:" lcBrand skip
    "Invoice:" lcExtInvID
    HELP "Enter Invoice Number"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND INVOICE"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


form /* seek  CustNum */
    "Brand ..:" lcBrand skip
    "Customer:" CustNum
    HELP "Enter Customer Number"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CUSTOMER "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

form /* seek  source & amt */
    "Brand :" lcBrand skip
    "Source:" lcPaymSrc 
       HELP "Payment source" skip
    "Amount:" ldPaymAmt 
       HELP "Payment amount"
       FORMAT "->>>>>>9.99" skip
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND SOURCE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f4.


IF icRefNum > "" THEN DO:

   llPPReq = TRUE.
        
   RUN local-find-first.     
   
   IF NOT AVAILABLE Payment THEN DO:
      MESSAGE "No payments for this request"
      VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.
   
   FIND FIRST PrePaidRequest WHERE
              PrePaidRequest.Brand     = gcBrand AND 
              PrePaidRequest.PPRequest = INT(icRefNum)
   NO-LOCK NO-ERROR.

   ASSIGN
      MaxOrder = 1
      Order    = 2.
   
   FRAME sel:TITLE = " PAYMENTS OF REQUEST " + STRING(icRefNum) + " ".

   Payment.CustNum:LABEL  IN FRAME sel = "Req.ID".
   Payment.ExtInvID:LABEL IN FRAME sel = "MSISDN".
   Payment.InvAmt:LABEL   IN FRAME sel = "TopUpAmt".
   Payment.PaymAmt:LABEL  IN FRAME sel = "TaxAmt".

END.                  
ELSE IF iiCustNum > 0 THEN DO:

   ASSIGN
      MaxOrder = 1
      Order    = 2.

   FRAME sel:TITLE = " PAYMENTS OF CUSTOMER " + STRING(iiCustNum) + " ".

END.

ELSE IF iiInvNum > 0 THEN DO:
   ASSIGN MaxOrder = 1
          Order    = 1
          FrmRow   = 2
          FrmDown  = 13.

   FIND Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK.
   IF Invoice.PaymState = 5  AND
      /* is ePayment recent enough */
      Invoice.EPaymDate NE ? AND 
      TODAY - Invoice.EPaymDate <= liEPaymValid
   THEN DO:
      MESSAGE "There is a pending ePayment on invoice;" SKIP
              STRING(Invoice.ePaymDate,"99-99-9999") SKIP
              Invoice.ePaymAmt Invoice.Currency
      VIEW-AS ALERT-BOX
      TITLE " ePaid ".
   END.

   FRAME sel:TITLE = " PAYMENTS OF INVOICE " + Invoice.ExtInvID + " ".
END.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-FIRST.

IF AVAILABLE Payment THEN ASSIGN
   memory       = recid(Payment)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE " No payments were found! "
   VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        
        UP FRAME-LINE - 1.
        
        FIND Payment WHERE recid(Payment) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Payment THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Payment).
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
         ufk[1]= 177  ufk[2]= 92   ufk[3]= 714  ufk[4]= 1018
         ufk[5]= 927  ufk[6]= 1491 ufk[7]= 1152 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         IF iiCustNum > 0 OR iiInvNum > 0 THEN ASSIGN
            ufk[1] = 0
            ufk[3] = 0
            ufk[4] = 0.

         IF iiInvNum > 0 THEN ufk[2] = 0.
         
         RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Payment.ExtVoucher ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Payment.ExtVoucher WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Payment.ExtInvID ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Payment.ExtInvID WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW Payment.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Payment.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW Payment.PaymSrc  ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Payment.PaymSrc WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        IF MaxOrder > 1 THEN DO:
           order = order + 1. 
           IF order > maxOrder THEN order = 1.
        END. 
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        IF MaxOrder > 1 THEN DO:
           order = order - 1. 
           IF order = 0 THEN order = maxOrder.
        END. 
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND Payment WHERE recid(Payment) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Payment THEN
              ASSIGN FIRSTrow = i memory = recid(Payment).
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
           IF NOT AVAILABLE Payment THEN DO:
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
                rtab[1] = recid(Payment)
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
           IF NOT AVAILABLE Payment THEN DO:
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
              rtab[FRAME-DOWN] = recid(Payment).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND Payment WHERE recid(Payment) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Payment THEN DO:
           memory = recid(Payment).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Payment THEN memory = recid(Payment).
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
           FIND Payment WHERE recid(Payment) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              lcExtVoucher WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF lcExtVoucher > "" THEN DO:
          IF lcBrand = "*" THEN 
          FIND FIRST Payment WHERE 
                     Payment.Brand       = lcBrand AND
                     Payment.ExtVoucher >= lcExtVoucher
           NO-LOCK NO-ERROR.

          ELSE
          FIND FIRST Payment WHERE 
                     Payment.Brand       = lcBrand AND
                     Payment.ExtVoucher >= lcExtVoucher
           NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              lcExtInvID WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF lcExtInvID > "" THEN DO:
          IF iiCustNum > 0 THEN 
          FIND FIRST Payment NO-LOCK WHERE 
                     Payment.CustNum = iiCustNum AND 
                     Payment.ExtInvID >= lcExtInvID NO-ERROR.
          ELSE FIND FIRST Payment WHERE 
               Payment.Brand = lcBrand AND
               Payment.ExtInvID >= lcExtInvID
               USE-INDEX ExtInvID  NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F3.
       DISPLAY lcBrand WITH FRAME F3.
       UPDATE lcBrand WHEN gcAllBrand
              CustNum WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.

       IF CustNum > 0 THEN DO:
          FIND FIRST Payment WHERE 
              Payment.Brand = lcBrand AND
              Payment.CustNum >= CustNum
          USE-INDEX CustNum  NO-LOCK NO-ERROR.

          IF NOT fRecFound(3) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-3 */

     /* Search BY col 4 */
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F4.
       DISPLAY lcBrand WITH FRAME F4.
       UPDATE lcBrand WHEN gcAllBrand
              lcPaymSrc
              ldPaymAmt WITH FRAME f4.
       HIDE FRAME f4 NO-PAUSE.

       DO:
          FIND FIRST Payment WHERE 
              Payment.Brand    = lcBrand   AND
              Payment.PaymSrc  = lcPaymSrc AND
              Payment.PaymAmt >= ldPaymAmt 
          USE-INDEX PaymSrc NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Payment THEN 
          FIND FIRST Payment WHERE 
              Payment.Brand    = lcBrand   AND
              Payment.PaymSrc >= lcPaymSrc AND
              Payment.PaymAmt  = ldPaymAmt 
          USE-INDEX PaymSrc NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Payment THEN 
          FIND FIRST Payment WHERE 
              Payment.Brand    = lcBrand   AND
              Payment.PaymSrc >= lcPaymSrc AND
              Payment.PaymAmt >= ldPaymAmt 
          USE-INDEX PaymSrc NO-LOCK NO-ERROR.

          IF NOT fRecFound(4) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-4 */

     /* UPDATE memo */
     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO TRANS ON ENDKEY UNDO, NEXT LOOP:

        RUN local-find-this(TRUE).

        RUN memo(INPUT 0,
                 INPUT "payment",
                 INPUT STRING(Payment.Voucher),
                 INPUT "Voucher number").

        ufkey = true.
        next.
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". RUN ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY Payment.ExtInvID.

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(Payment).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO:  /* view */
        RUN local-find-this(TRUE).

        ehto = 5.
        run ufkey.
        RUN pInvoiceDetails(Payment.InvNum,
                            TRUE).
        ufkey = true.

     END.

     /* other actions */
     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN REPEAT:

        ASSIGN
           ufk    = 0
           ufkey  = true
           ufk[2] = 67 WHEN llPPReq = FALSE
           ufk[4] = 1752
           ufk[5] = 1746
           ufk[8] = 8
           ehto   = 0.
        
        RUN ufkey.

        IF toimi = 2 THEN DO TRANS:
                
           RUN local-find-this(FALSE).
           
           IF Payment.PaymType NE 6 THEN DO:
              MESSAGE "This is not a refund payment"
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           IF Payment.ExpStamp > 0 THEN DO:
              MESSAGE "Payment has already been sent to bank"
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.
           
           Ok = FALSE.
           MESSAGE "Change this payment's type into normal, and cancel"
                   "it's refunding ?" 
           VIEW-AS ALERT-BOX
           QUESTION
           BUTTONS YES-NO
           TITLE " CANCEL REFUND "
           SET Ok.

           IF ok THEN DO:
              RUN local-find-this(TRUE).
              IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPayment).
              Payment.PaymType = 0.
              IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPayment).
              RELEASE Payment.
           END.              
        END.

        /* eventlog */
        ELSE IF toimi = 4 THEN DO:
           RUN local-find-this(FALSE).
           
           IF AVAILABLE Payment THEN 
           RUN eventsel ("Payment",
                         Payment.Brand + CHR(255) +
                         STRING(Payment.CustNum) + CHR(255) +
                         STRING(Payment.InvNum)  + CHR(255) +
                         STRING(Payment.Voucher)).
        END. 

        /* payment source info */
        ELSE IF toimi = 5 THEN RUN h-paymsrc.
        
        ELSE IF toimi = 8 THEN NEXT LOOP.
     END.


     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(Payment) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(Payment) must-print = TRUE.
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
      FIND Payment WHERE recid(Payment) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND Payment WHERE recid(Payment) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF icRefNum > "" THEN 
      FIND FIRST Payment USE-INDEX RefNum WHERE
                 Payment.Brand    = gcBrand  AND
                 Payment.RefNum   = icRefNum NO-LOCK NO-ERROR.
 
   ELSE IF iiCustNum > 0 THEN DO:
      FIND FIRST Payment WHERE Payment.CustNum = iiCustNum
      NO-LOCK NO-ERROR. 
   END.

   ELSE IF iiInvNum > 0 THEN 
      FIND FIRST Payment WHERE Payment.InvNum = iiInvNum
      NO-LOCK NO-ERROR. 
   
   ELSE DO:
      IF order = 1 THEN FIND FIRST Payment USE-INDEX ExtVoucher
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN FIND FIRST Payment USE-INDEX InvNum
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN FIND FIRST Payment USE-INDEX CustNum
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
      ELSE IF order = 4 THEN FIND FIRST Payment USE-INDEX PaymSrc
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF icRefNum > "" THEN 
      FIND LAST Payment USE-INDEX RefNum WHERE
                Payment.Brand    = gcBrand  AND
                Payment.RefNum   = icRefNum NO-LOCK NO-ERROR.
 
   ELSE IF iiCustNum > 0 THEN DO:
      FIND LAST Payment WHERE Payment.CustNum = iiCustNum 
      NO-LOCK NO-ERROR. 
   END.
 
   ELSE IF iiInvNum > 0 THEN 
      FIND LAST Payment WHERE Payment.InvNum = iiInvNum
      NO-LOCK NO-ERROR. 
 
   ELSE DO:
      IF order = 1 THEN FIND LAST Payment USE-INDEX ExtVoucher
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN FIND LAST Payment USE-INDEX InvNum
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN FIND LAST Payment USE-INDEX CustNum
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
      ELSE IF order = 4 THEN FIND LAST Payment USE-INDEX PaymSrc
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF icRefNum > "" THEN 
      FIND NEXT Payment USE-INDEX RefNum WHERE
                Payment.Brand    = gcBrand  AND
                Payment.RefNum   = icRefNum NO-LOCK NO-ERROR.
 
   ELSE IF iiCustNum > 0 THEN DO:
      FIND NEXT Payment WHERE Payment.CustNum = iiCustNum 
      NO-LOCK NO-ERROR. 
   END.
 
   ELSE IF iiInvNum > 0 THEN 
      FIND NEXT Payment WHERE Payment.InvNum = iiInvNum
      NO-LOCK NO-ERROR. 

   ELSE DO:
      IF order = 1 THEN FIND NEXT Payment USE-INDEX ExtVoucher
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN FIND NEXT Payment USE-INDEX InvNum
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN FIND NEXT Payment USE-INDEX CustNum
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
      ELSE IF order = 4 THEN FIND NEXT Payment USE-INDEX PaymSrc
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF icRefNum > "" THEN 
      FIND PREV Payment USE-INDEX RefNum WHERE
                Payment.Brand    = gcBrand  AND
                Payment.RefNum   = icRefNum NO-LOCK NO-ERROR.
 
   ELSE IF iiCustNum > 0 THEN DO:
      FIND PREV Payment WHERE Payment.CustNum = iiCustNum 
      NO-LOCK NO-ERROR. 
   END.

   ELSE IF iiInvNum > 0 THEN 
      FIND PREV Payment WHERE Payment.InvNum = iiInvNum
      NO-LOCK NO-ERROR. 

   ELSE DO:
      IF order = 1 THEN FIND PREV Payment USE-INDEX ExtVoucher
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
      ELSE IF order = 2 THEN FIND PREV Payment USE-INDEX InvNum 
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
      ELSE IF order = 3 THEN FIND PREV Payment USE-INDEX CustNum
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
      ELSE IF order = 4 THEN FIND PREV Payment USE-INDEX PaymSrc
         WHERE Payment.Brand = lcBrand
      NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
   
   DISPLAY 
      Payment.ExtVoucher
      Payment.AccDate
      Payment.ExtInvID
      Payment.CustNum
      Payment.PaymAmt 
      Payment.InvAmt
      Payment.PaymSrc
      Payment.PaymType
   WITH FRAME sel.

   IF llPPReq THEN DO:

      DISPLAY
         PrePaidRequest.PPRequest @ Payment.CustNum
         PrePaidRequest.CLI            @ Payment.ExtInvID
         PrePaidRequest.TopUpAmt / 100 @ Payment.InvAmt
         PrePaidRequest.VatAmt   / 100 @ Payment.PaymAmt
      WITH FRAME sel.

   END.
       
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-update-record:

   RUN local-find-others.

   do i = 1 to 6:
      FIND Account WHERE 
           Account.Brand  = Payment.Brand AND 
           Account.AccNum = Payment.Accnum[i] 
      no-lock no-error.
      IF AVAIL Account 
      THEN Accname[i] = Account.AccName.
      ELSE Accname[i] = "".

      lcTypeName[i] = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "Account",
                                       "AccType",
                                       STRING(Payment.AccType[i])).
   end.

   lcPaymType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "Payment",
                                 "PaymType",
                                 STRING(Payment.PaymType)).
                                 
   IF Payment.ImportStamp > 0 THEN DO:
      fSplitTS(Payment.ImportStamp,
               OUTPUT ldtDate,
               OUTPUT liTime).
      lcCreated = STRING(ldtDate,"99-99-99") + " " +
                  STRING(liTime,"hh:mm:ss").
   END.
   ELSE lcCreated = "".
   
   /* remove path from file name */
   i = NUM-ENTRIES(Payment.PaymFile,"/").
   IF i > 1 
   THEN lcPaymFile = ENTRY(i,Payment.PaymFile,"/").
   ELSE lcPaymFile = Payment.PaymFile.
   
   ehto = 5.
   RUN ufkey.
   
   DISP
      Payment.ExtVoucher
      Payment.CustNum
      Payment.PaymDate
      Payment.CustName    
      Payment.AccDate
      Payment.ExtInvID      
      Payment.PaymAmt
      Payment.InvAmt
      Payment.PaymSrc
      Payment.InvDate
      Payment.PaymType
      lcPaymType
      Payment.DueDate
      lcPaymFile
      Payment.PaymArc
      lcCreated
      Payment.RefNum
      WITH FRAME lis.

   DO i = 1 TO 6 WITH FRAME lis:
      DISPLAY 
      Payment.AccNum[i]  WHEN Payment.Posting[i] NE 0
      AccName[i]         WHEN Payment.Posting[i] NE 0
      Payment.Posting[i] WHEN Payment.Posting[i] NE 0
      Payment.AccType[i] WHEN Payment.Posting[i] NE 0
      lcTypeName[i]      WHEN Payment.Posting[i] NE 0. 
                                 
   END.

   MESSAGE "Press ENTER to continue".
   PAUSE NO-MESSAGE. 

END PROCEDURE.

