/* ----------------------------------------------------------------------
  MODULE .......: PaymPlan
  TASK .........: UPDATEs table PaymPlan
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 08.03.04
  CHANGED ......: 10.05.04/aam no update when new created (if iiCust > 0),
                               directly to batches from invoice and vice versa
                  02.07.04/aam disconnect invoices if state is changed to 4
                  16.03.06/aam TF version
                  08.05.06/aam view eventlog
                  19.06.06/aam fRemPPInvoice
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable PaymPlan

{Syst/commali.i}
{Func/cparam2.i}
{Func/finvbal.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'PaymPlan'}
{Func/fpaymplan.i}
{Func/fppinv.i}
{Func/timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhPaymPlan AS HANDLE NO-UNDO.
   lhPaymPlan = BUFFER PaymPlan:HANDLE.
   RUN StarEventInitialize(lhPaymPlan).

   DEFINE VARIABLE lhPPBatch AS HANDLE NO-UNDO.
   lhPPBatch = BUFFER PPBatch:HANDLE.
   RUN StarEventInitialize(lhPPBatch).

   DEFINE VARIABLE lhPPInv AS HANDLE NO-UNDO.
   lhPPInv = BUFFER PPInv:HANDLE.
   RUN StarEventInitialize(lhPPInv).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhPaymPlan).
   END.

END.

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO.
DEF INPUT PARAMETER iiInvNum  AS INT NO-UNDO.
DEF INPUT PARAMETER iiStatus  AS INT NO-UNDO.

DEF VAR liCustNum  LIKE PaymPlan.CustNum         NO-UNDO.
DEF VAR liStatus   LIKE PaymPlan.PPStatus        NO-UNDO.
DEF VAR ldtDate    LIKE PaymPlan.PPDate          NO-UNDO. 

DEF VAR xrecid        AS RECID                           init ?.
DEF VAR FIRSTrow      AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow        AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown       AS INT                    NO-UNDO  init 15.
DEF VAR order         AS INT                    NO-UNDO  init 1.
DEF VAR orders        AS CHAR                   NO-UNDO.
DEF VAR maxOrder      AS INT                    NO-UNDO  init 3.
DEF VAR ufkey         AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow        AS INT                    NO-UNDO  init 0.
DEF VAR pr-order      AS INT                    NO-UNDO.
DEF VAR Memory        AS RECID                  NO-UNDO.
DEF VAR RowNo         AS INT                    NO-UNDO.
DEF VAR must-print    AS LOG                    NO-UNDO.
DEF VAR must-add      AS LOG                    NO-UNDO.
DEF VAR ac-hdr        AS CHAR                   NO-UNDO.
DEF VAR rtab          AS RECID EXTENT 24        NO-UNDO.
DEF VAR i             AS INT                    NO-UNDO.
DEF VAR ok            AS log format "Yes/No"    NO-UNDO.

DEF VAR lcCustName    AS CHAR                   NO-UNDO.
DEF VAR lcStatus      AS CHAR                   NO-UNDO. 
DEF VAR lcStatusLst   AS CHAR                   NO-UNDO. 
DEF VAR lcCode        AS CHAR                   NO-UNDO. 
DEF VAR lcType        AS CHAR                   NO-UNDO. 
DEF VAR lcTypeLst     AS CHAR                   NO-UNDO. 
DEF VAR liBankDays    AS INT                    NO-UNDO. 
DEF VAR liPlanID      AS INT                    NO-UNDO.
DEF VAR lcError       AS CHAR                   NO-UNDO. 
DEF VAR liDue         AS INT                    NO-UNDO. 
DEF VAR liAutoRun     AS INT                    NO-UNDO. 
DEF VAR llMoveOn      AS LOG                    NO-UNDO. 
DEF VAR liBatch       AS INT                    NO-UNDO. 
DEF VAR ldtFromDate   AS DATE                   NO-UNDO.
DEF VAR ldtToDate     AS DATE                   NO-UNDO.
DEF VAR ldtFirst      AS DATE                   NO-UNDO.
DEF VAR ldtLast       AS DATE                   NO-UNDO.
DEF VAR llCredCtrl    AS LOG                    NO-UNDO. 
DEF VAR lcPickType    AS CHAR                   NO-UNDO.
DEF VAR lcPassword    AS CHAR                   NO-UNDO. 
DEF VAR lcAdminPwd    AS CHAR                   NO-UNDO. 
DEF VAR lcAskPassWd   AS CHAR                   NO-UNDO.
DEF VAR lcCancelTxt   AS CHAR                   NO-UNDO.
DEF VAR liTargCust    AS INT                    NO-UNDO. 
DEF VAR llOrdCust     AS LOG                    NO-UNDO.
DEF VAR liOrdCustNum  AS INT                    NO-UNDO.
DEF VAR lcOrdCustName AS CHAR                   NO-UNDO.

DEF BUFFER bAgrCust FOR Customer.

form
    PaymPlan.PPlanID  
    PaymPlan.PPDate
    PaymPlan.CustNum   
    lcCustName         COLUMN-LABEL "Name" FORMAT "X(12)" 
    PaymPlan.Amount    FORMAT "->>>>>9.99"
    lcStatus           COLUMN-LABEL "Status" FORMAT "X(14)"
    lcType             COLUMN-LABEL "Type"   FORMAT "X(12)"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " PAYMENT PLANS "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form            
    PaymPlan.PPlanID    COLON 20 
    PaymPlan.CustNum     COLON 20   
       VALIDATE(INPUT PaymPlan.CustNum = 0 OR
                CAN-FIND(FIRST Customer WHERE 
                               Customer.Brand   = gcBrand AND
                               Customer.CustNum = INPUT PaymPlan.CustNum),
                "Unknown customer")
       lcCustName FORMAT "X(35)" NO-LABEL AT 40 SKIP
    PaymPlan.PPDate     COLON 20
       VALIDATE(INPUT PaymPlan.PPDate NE ?,
                "Date is mandatory")
    PaymPlan.Amount       COLON 20
       VALIDATE(INPUT PaymPlan.Amount NE ?,
                "Date is mandatory")
    PaymPlan.PPStatus  COLON 20
       VALIDATE(INPUT PaymPlan.PPStatus >= 1 AND
                INPUT PaymPlan.PPStatus <= 6,
                "Valid values are 1-6")
       lcStatus NO-LABEL FORMAT "X(35)"         
    PaymPlan.PPType  COLON 20
       VALIDATE(INPUT PaymPlan.PPType >= 1 AND
                INPUT PaymPlan.PPType <= 3,
                "Valid choices are 1-3")
       lcType NO-LABEL FORMAT "X(35)"                
    PaymPlan.BankDays  COLON 20                
    PaymPlan.Orderer COLON 20 
       FORMAT ">>>>>>>9"
    lcOrdCustName 
       NO-LABEL  AT 40
       FORMAT "X(35)"
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FORM 
   "Customer's unpaid invoices will be collected into" AT 2 SKIP
   "a new payment plan." AT 2 SKIP(1)

   liCustNum COLON 19
      LABEL "Customer" 
      FORMAT ">>>>>>>9"
      HELP "Customer to which payment plan will be made"
      VALIDATE(INPUT liCustNum = 0 OR
               CAN-FIND(Customer WHERE 
                        Customer.Brand = gcBrand AND
                        Customer.CustNum = INPUT liCustNum),
              "Unknown customer")
   lcCustName FORMAT "X(35)" 
      NO-LABEL 
   SKIP(1)

   liDue   COLON 19
      LABEL "Invoices"
      FORMAT "9"
      VALIDATE(INPUT liDue >= 1 AND INPUT liDue <= 4,
               "Valid values are 1-4")
      HELP "1=All unpaid, 2=due, 3=undue, 4=pick manually"
   lcPickType
      NO-LABEL
      FORMAT "X(20)"
      SKIP
      
   liBatch COLON 19
      LABEL "Number Of Batches"
      FORMAT ">9"
      HELP "How many batches debt will be divided into"
      SKIP
   ldtFromDate COLON 19 
      LABEL "First Due Date"
      FORMAT "99-99-9999"
      HELP "Due date for the 1. batch"
      SKIP
   ldtToDate COLON 19
      LABEL "Last Due Date"
      FORMAT "99-99-9999"
      HELP "Due date for the last batch"
      SKIP
   SKIP(1)

   llOrdCust COLON 19
      LABEL "Ordered By" 
      HELP "Is orderer the (I)nvoice or the (A)greement customer"
      FORMAT "Invoice customer/Agreement customer" SKIP
   liOrdCustNum AT 21
      NO-LABEL 
      FORMAT ">>>>>>>9"
   lcOrdCustName 
      NO-LABEL
      FORMAT "X(35)"
      SKIP
   WITH SIDE-LABELS OVERLAY ROW 4 CENTERED 
        TITLE " NEW PLAN " FRAME fCreate.
        
{Func/brand.i}

form /* seek  */
    "Brand:" lcBrand skip
    "Date :" ldtDate
    HELP "Enter payment plan date"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND DATE"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  CustNum */
    "Brand ..:" lcBrand skip
    "Customer:" liCustNum
    HELP "Enter customer number"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND customer "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek  */
    "Brand :" lcBrand skip
    "Status:" liStatus
    HELP "Enter description"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND description "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.


FUNCTION fPPStatus RETURNS LOGICAL
   (iiType AS INT,
    ilDisp AS LOG).

   IF iiType >= 1 AND iiType <= 7 
   THEN lcStatus = ENTRY(iiType,lcStatusLst).
   ELSE lcStatus = "".
   
   IF ilDisp THEN 
   DISPLAY lcStatus WITH FRAME lis.
   
   RETURN TRUE.
   
END.

FUNCTION fPPType RETURNS LOGICAL
   (iiPoint AS INT,
    ilDisp AS LOG).

   IF iiPoint >= 1 AND iiPoint <= 3 
   THEN lcType = ENTRY(iiPoint,lcTypeLst).
   ELSE lcType = "".
   
   IF ilDisp THEN 
   DISPLAY lcType WITH FRAME lis.
   
   RETURN TRUE.
   
END.

FUNCTION fPickType RETURNS LOGICAL
   (iiPickType AS INT).
   
   lcPickType = "".
   CASE iiPickType:
   WHEN 1 THEN lcPickType = "All invoices".
   WHEN 2 THEN lcPickType = "Due invoices".
   WHEN 3 THEN lcPickType = "Undue invoices".
   WHEN 4 THEN lcPickType = "Pick manually".
   END CASE.

   DISPLAY lcPickType WITH FRAME fCreate.
   
   RETURN TRUE.
   
END FUNCTION.

FUNCTION fLetterCust RETURNS INTEGER:

   DEF VAR liLetterCust AS INT NO-UNDO.

   FIND Customer WHERE Customer.CustNum = PaymPlan.CustNum NO-LOCK.
    
   liLetterCust = 0.
   
   IF PaymPlan.Orderer > 0 THEN liLetterCust = PaymPlan.Orderer.
   
   /* if made through request then get the orderer from there */
   IF liLetterCust = 0 AND 
      PaymPlan.PPType >= 1 AND PaymPlan.PPType <= 2 THEN DO:
      
      /* first check if invoice customer made the request */
      FOR EACH MsRequest NO-LOCK WHERE
               MsRequest.Brand      = gcBrand          AND
               MsRequest.ReqType    = 11               AND
               MsRequest.CustNum    = Customer.CustNum AND
               MsRequest.ReqIParam2 = PaymPlan.PPlanID:
         liLetterCust = MsRequest.CustNum.      
      END.

      /* then from agreement customer */
      IF liLetterCust = 0 THEN
      FOR EACH MsRequest NO-LOCK WHERE
               MsRequest.Brand      = gcBrand          AND
               MsRequest.ReqType    = 11               AND
               MsRequest.CustNum    = Customer.AgrCust AND
               MsRequest.ReqIParam2 = PaymPlan.PPlanID:
         liLetterCust = MsRequest.CustNum.      
      END.

   END.
      
   IF liLetterCust = 0 THEN liLetterCust = PaymPlan.CustNum.
   
   RETURN liLetterCust.

END FUNCTION.


DO i = 1 TO 7:
   lcStatusLst = lcStatusLst + 
                DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "PaymPlan","PPStatus",STRING(i)) + ",". 
   IF i <= 3 THEN 
   lcTypeLst = lcTypeLst + 
                DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "PaymPlan","PPType",STRING(i)) + ",". 
END.

liBankDays = fCParamI("PPBankDays").
IF liBankDays = ? THEN liBankDays = 0.

ASSIGN lcPassword  = fCParamC("MsAddressChg")
       lcAdminPwd  = fCParamC("PaymPlanCreditControl").
IF lcPassword = ? THEN lcPassword = "".
 
cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

ASSIGN orders    = " By Date     ," + 
                   " By Customer ," +
                   " By Status    "
       llMoveOn  = FALSE
       liAutoRun = 0.

/* directly to insert mode */
IF iiInvNum < 0 THEN ASSIGN
  liAutoRun = 9
  iiInvNum  = 0.
                 
IF iiInvNum > 0 OR iiCustNum > 0 OR iiStatus > 0 THEN MaxOrder = 1. 
    
IF lcAdminPwd > "" AND iiCustNum = 0 THEN DO:
   lcAskPassWd = "".
            
   PAUSE 0.
   UPDATE lcAskPassWd 
          BLANK
          FORMAT "X(20)" 
          LABEL "Password"
          HELP "Password for handling plans"  /* in credit control" */
   WITH OVERLAY ROW 10 CENTERED TITLE " CREDIT CONTROL "
        SIDE-LABELS FRAME fAdminPassword.
           
   HIDE FRAME fAdminPassword NO-PAUSE.           
   IF lcAskPassWd NE lcAdminPwd THEN DO:
      HIDE FRAME sel NO-PAUSE.
      RETURN.
   END. 

END.

RUN local-find-first.

IF AVAILABLE PaymPlan THEN ASSIGN
   Memory       = recid(PaymPlan)
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

   IF must-add THEN DO:  /* Add a PaymPlan  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           IF iiCustNum > 0 THEN DISPLAY iiCustNum @ PaymPlan.CustNum.
           ELSE PROMPT-FOR PaymPlan.CustNum.

           IF INPUT FRAME lis PaymPlan.CustNum = 0  
           THEN LEAVE add-row.

           CREATE PaymPlan.
           ASSIGN
           PaymPlan.Brand    = gcBrand
           PaymPlan.PPlanID  = NEXT-VALUE(PPlan)
           PaymPlan.CustNum  = INPUT FRAME lis PaymPlan.CustNum
           PaymPlan.PPDate   = TODAY
           PaymPlan.BankDays = liBankDays
           PaymPlan.PPStatus = 1
           PaymPlan.PPType   = 3
           liAutoRun         = 1. 

           IF iiCustNum = 0 THEN 
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPaymPlan).

           ASSIGN
           Memory = recid(PaymPlan)
           xrecid = Memory.  
           LEAVE.
        END.
        
        LEAVE.
        
      END.  /* ADD-ROW */

      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE PaymPlan THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND PaymPlan WHERE recid(PaymPlan) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PaymPlan THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(PaymPlan).
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
         ufk   = 0
         ufk[1]= 816  
         ufk[2]= (IF lcRight = "RW" THEN 1782 ELSE 0)  
         ufk[3]= 1777 
         ufk[4]= 1778
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7]= 927
         ufk[8]= 8 ufk[9]= 1
         ehto = 3. 
        
         IF iiStatus > 0 THEN ASSIGN 
            ufk[1] = 0
            ufk[2] = 0
            ufk[5] = 0
            ufk[6] = 0.
            
         IF iiCustNum > 0 THEN ufk[1] = 0.

         IF iiInvNum > 0 THEN ASSIGN 
            ufk[1] = 0
            ufk[2] = 0.
        
         IF liAutoRun = 0 THEN DO:
            ufkey = FALSE.
            RUN Syst/ufkey.
         END.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF liAutoRun = 0 THEN DO:
         IF order = 1 THEN DO:
           CHOOSE ROW PaymPlan.PPDate {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
           COLOR DISPLAY VALUE(ccc) PaymPlan.PPDate WITH FRAME sel.
         END.
         ELSE IF order = 2 THEN DO:
           CHOOSE ROW PaymPlan.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
           COLOR DISPLAY VALUE(ccc) PaymPlan.CustNum WITH FRAME sel.
         END.
         ELSE IF order = 3 THEN DO:
           CHOOSE ROW lcStatus {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
           COLOR DISPLAY VALUE(ccc) lcStatus WITH FRAME sel.
         END.

         nap = keylabel(LASTKEY).
      END.
      ELSE DO:
         CASE liAutoRun:
         WHEN 1 THEN nap = "3".
         WHEN 2 THEN nap = "4".
         WHEN 9 THEN nap = "2".
         OTHERWISE ASSIGN liAutoRun = 0
                          nap       = "9".
         END CASE.                  
      END.
      
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
        FIND PaymPlan WHERE recid(PaymPlan) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE PaymPlan THEN
              ASSIGN FIRSTrow = i Memory = recid(PaymPlan).
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
           IF NOT AVAILABLE PaymPlan THEN DO:
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
                rtab[1] = recid(PaymPlan)
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
           IF NOT AVAILABLE PaymPlan THEN DO:
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
              rtab[FRAME-DOWN] = recid(PaymPlan).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND PaymPlan WHERE recid(PaymPlan) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PaymPlan THEN DO:
           Memory = recid(PaymPlan).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE PaymPlan THEN Memory = recid(PaymPlan).
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
           FIND PaymPlan WHERE recid(PaymPlan) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* search */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 THEN DO:

        ASSIGN
        ufk = 0
        ufk[1]= 28  ufk[2]= 702  ufk[3]= 559
        ufk[8]= 8 
        ehto = 0
        ufkey = TRUE.
        
        IF iiCustNum > 0 OR iiInvNum > 0 THEN ASSIGN 
           ufk[2] = 0
           ufk[3] = 0.
         
        RUN Syst/ufkey.p.
     
        /* Search BY column 1 */
        IF toimi = 1 THEN DO ON ENDKEY UNDO, NEXT LOOP:
           cfc = "puyr". RUN Syst/ufcolor.
           ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
           CLEAR FRAME f1.
           DISPLAY lcBrand WITH FRAME F1.
           UPDATE lcBrand WHEN gcAllBrand
                  ldtDate WITH FRAME f1.
           HIDE FRAME f1 NO-PAUSE.

           IF ldtDate NE ? THEN DO:
              FIND FIRST PaymPlan WHERE 
                         PaymPlan.Brand  = lcBrand AND
                         PaymPlan.PPDate = ldtDate
              NO-LOCK NO-ERROR.

              IF NOT AVAILABLE PaymPlan THEN 
              FIND LAST PaymPlan WHERE 
                        PaymPlan.Brand  = lcBrand AND
                        PaymPlan.PPDate > ldtDate
              NO-LOCK NO-ERROR.
              
              IF NOT AVAILABLE PaymPlan THEN 
              FIND FIRST PaymPlan WHERE 
                         PaymPlan.Brand  = lcBrand AND
                         PaymPlan.PPDate < ldtDate
              NO-LOCK NO-ERROR.

              IF NOT fRecFound(1) THEN NEXT BROWSE.

              NEXT LOOP.
           END.
           
        END. /* Search-1 */

        /* Search BY column 2 */
        ELSE IF toimi = 2 THEN DO ON ENDKEY UNDO, NEXT LOOP:
           cfc = "puyr". RUN Syst/ufcolor.
           ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
           CLEAR FRAME f2.
           DISPLAY lcBrand WITH FRAME F2.
           UPDATE lcBrand WHEN gcAllBrand
                  liCustNum WITH FRAME f2.
           HIDE FRAME f2 NO-PAUSE.

           IF liCustNum > 0 THEN DO:
              FIND FIRST PaymPlan WHERE 
                         PaymPlan.Brand = lcBrand AND
                         PaymPlan.CustNum >= liCustNum
              NO-LOCK NO-ERROR.

              IF NOT fRecFound(2) THEN NEXT BROWSE.

              NEXT LOOP.

           END.
        END. /* Search-2 */

        /* Search BY column 3 */
        ELSE IF toimi = 3 THEN DO ON ENDKEY UNDO, NEXT LOOP:
           cfc = "puyr". RUN Syst/ufcolor.
           ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
           CLEAR FRAME f3.
           DISPLAY lcBrand WITH FRAME F3.
           UPDATE lcBrand WHEN gcAllBrand
                  liStatus WITH FRAME f3.
           HIDE FRAME f3 NO-PAUSE.

           IF liStatus > 0 THEN DO:
              FIND FIRST PaymPlan WHERE 
                         PaymPlan.Brand     = lcBrand AND
                         PaymPlan.PPStatus >= liStatus
              NO-LOCK NO-ERROR.

              IF NOT fRecFound(3) THEN NEXT BROWSE.

              NEXT LOOP.

           END.
        END. /* Search-3 */

     END.

     /* automatic creation of payment plan */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0 THEN DO TRANS:  
       {Syst/uright2.i}

       ASSIGN ldtFromDate = ?
              ldtToDate   = ?
              liBatch     = 0
              liDue       = 1
              liAutoRun   = 0.
              
       CLEAR FRAME fCreate NO-PAUSE.
       PAUSE 0.
       VIEW FRAME fCreate.
       
       fPickType(liDue).
       
       ASSIGN liCustNum    = iiCustNum
              liOrdCustNum = iiCustNum
              llOrdCust    = TRUE.

       IF liCustNum > 0 THEN DO:
          FIND Customer WHERE Customer.CustNum = liCustNum NO-LOCK.
          lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                        BUFFER Customer).
          lcOrdCustName = lcCustName.                              
          DISPLAY liCustNum lcCustName WITH FRAME fCreate.
       END.
       
       DISPLAY llOrdCust
               liOrdCustNum
               lcOrdCustName WITH FRAME fCreate.
               
       ASSIGN liBatch     = 0  
              ldtFromDate = ?
              ldtToDate   = ?
              ehto        = 9
              ufkey       = TRUE.
       RUN Syst/ufkey.
        
       REPEAT ON ENDKEY UNDO, LEAVE:
       
          UPDATE liCustNum WHEN iiCustNum = 0 
                 liDue
                 liBatch
                 ldtFromDate
                 ldtToDate VALIDATE(INPUT ldtToDate >= INPUT ldtFromDate,
                                    "Date of last batch cannot be earlier")
                 llOrdCust                   
          WITH FRAME fCreate EDITING:
             READKEY.
          
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
             THEN DO WITH FRAME fCreate:
             
                PAUSE 0.
                
                IF FRAME-FIELD = "liCustNum" AND 
                   INPUT FRAME fCreate liCustNum > 0 
                THEN DO:
                   FIND Customer WHERE 
                        Customer.Brand = gcBrand AND
                        Customer.CustNum = INPUT FRAME fCreate liCustNum 
                   NO-LOCK NO-ERROR.
                   IF AVAILABLE Customer THEN DO:
                      lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                    BUFFER Customer).
                   
                      DISPLAY lcCustName WITH FRAME fCreate.
                      
                      IF llOrdCust THEN ASSIGN
                         liOrdCustNum  = Customer.CustNum
                         lcOrdCustName = lcCustName.
                      ELSE DO:
                         FIND bAgrCust WHERE 
                              bAgrCust.CustNum = Customer.AgrCust NO-LOCK.
                         ASSIGN 
                            liOrdCustNum  = Customer.AgrCust
                            lcOrdCustName = DYNAMIC-FUNCTION("fDispCustName"
                                                             IN ghFunc1,
                                                             BUFFER bAgrCust).
                      END.
                   
                      DISPLAY liOrdCustNum lcOrdCustName WITH FRAME fCreate.  
                   END.
                    
                END.
                
                ELSE IF FRAME-FIELD = "liDue" THEN DO:
                   fPickType(INPUT INPUT liDue).
                END.
             END.

             APPLY LASTKEY.
       
          END. 

          IF llOrdCust THEN ASSIGN
             liOrdCustNum  = Customer.CustNum
             lcOrdCustName = lcCustName.
          ELSE DO:
             FIND bAgrCust WHERE 
                  bAgrCust.CustNum = Customer.AgrCust NO-LOCK.
             ASSIGN 
                liOrdCustNum  = Customer.AgrCust
                lcOrdCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                 BUFFER bAgrCust).
          END.
                   
          DISPLAY liOrdCustNum lcOrdCustName WITH FRAME fCreate.  
          
          LEAVE.
       END.
         
       IF liCustNum > 0 AND liBatch > 0 AND 
          ldtFromDate NE ? AND ldtToDate NE ?
       THEN DO:

          ok = FALSE.
          MESSAGE "Start creating a payment plan ?"
          VIEW-AS ALERT-BOX
          QUESTION
          BUTTONS YES-NO
          SET ok.
          
          HIDE FRAME fCreate NO-PAUSE.
            
          IF ok THEN DO:
             RUN Ar/pplancre (liCustNum,
                           0,
                           liDue,
                           liBatch,
                           "",
                           ldtFromDate,
                           ldtToDate,
                           3,
                           OUTPUT liPlanID,
                           OUTPUT lcError).
                             
             IF lcError > "" OR liPlanID = 0
             THEN MESSAGE "Creation failed;" SKIP
                          lcError
                  VIEW-AS ALERT-BOX.

             ELSE DO:
                /* user has to accept new plan manually */
                FIND PaymPlan WHERE PaymPlan.PPlanID = liPlanID EXCLUSIVE-LOCK.
                ASSIGN PaymPlan.PPStatus = 1
                       PaymPlan.Orderer  = liOrdCustNum.
                
                MESSAGE "Payment plan was created"
                VIEW-AS ALERT-BOX.
               
                ASSIGN must-print = TRUE
                       memory     = RECID(PaymPlan).
                       
                /* pick invoices manually */
                IF liDue = 4 THEN liAutoRun = 1.
                
                NEXT LOOP.
             END.
             
          END.
       END.
       
       HIDE FRAME fCreate NO-PAUSE.
          
     END.
 
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:  /* Invoices */
       {Syst/uright2.i}
       RUN local-find-this (FALSE).
       liAutoRun = 0.
       IF AVAILABLE PaymPlan 
       THEN RUN Ar/ppinv (PaymPlan.PPlanID,
                       FALSE, /* automatic mode */
                       OUTPUT llMoveOn). 
       ASSIGN gcHelpParam = ""
              ufkey = true.
       IF llMoveOn THEN liAutoRun = 2.       
       RUN local-disp-row.           
     END.

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:  /* batches */
       {Syst/uright2.i}
       RUN local-find-this (FALSE).
       liAutoRun = 0.
       IF AVAILABLE PaymPlan 
       THEN RUN Ar/ppbatch (PaymPlan.PPlanID,
                         liBatch,
                         ldtFromDate,
                         ldtToDate,
                         OUTPUT llMoveOn). 

       ASSIGN liBatch     = 0
              ldtFromDate = ?
              ldtToDate   = ?.
       
       IF llMoveOn THEN liAutoRun = 1.                  
       ufkey = true. 
       RUN local-disp-row.           
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF PaymPlan.PPStatus > 1 AND 
          (CAN-FIND(FIRST PPInv   OF PaymPlan) OR
           CAN-FIND(FIRST PPBatch OF PaymPlan)) 
       THEN DO:
          MESSAGE "There are invoices / batches defined for this plan."
                  "Deletion is not allowed."
          VIEW-AS ALERT-BOX
          ERROR.
          NEXT LOOP.
       END.
       
       RUN local-find-NEXT.

       IF AVAILABLE PaymPlan THEN Memory = recid(PaymPlan).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE PaymPlan THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(PaymPlan).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       PaymPlan.Amount PaymPlan.CustNum PaymPlan.PPDate lcStatus .

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       PaymPlan.Amount
       PaymPlan.CustNum PaymPlan.PPDate lcStatus .

       IF ok THEN DO:

           FOR EACH PPBatch OF PaymPlan EXCLUSIVE-LOCK:
              IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPPBatch).
              DELETE PPBatch.
           END.
 
           FOR EACH PPInv OF PaymPlan EXCLUSIVE-LOCK:
              IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPPInv).
              fRemPPInvoice(PPInv.InvNum,
                            1,
                            0.0).
              DELETE PPInv.
           END.
            
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPaymPlan).

           DELETE PaymPlan.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE PaymPlan THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     /* memo */
     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:
     
        RUN local-find-this(FALSE).
        
        IF NOT AVAILABLE PaymPlan THEN NEXT.
        
        ufkey = TRUE.
     
        RUN Mc/memo(PaymPlan.CustNum,
                 "PaymPlan",
                 STRING(PaymPlan.PPlanID),
                 "Payment Plan").
     END.
 
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPaymPlan).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY PaymPlan.CustNum.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPaymPlan).

       RUN local-disp-row.
       xrecid = recid(PaymPlan).

       RELEASE PaymPlan.
       
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(PaymPlan) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(PaymPlan) must-print = TRUE.
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
      FIND PaymPlan WHERE recid(PaymPlan) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND PaymPlan WHERE recid(PaymPlan) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiInvNum > 0 THEN DO:
       FOR EACH PPInv NO-LOCK WHERE
                PPInv.InvNum = iiInvNum,
          FIRST PaymPlan OF PPInv NO-LOCK
       BY PaymPlan.PPStatus: /* get first active one */
          LEAVE.
       END.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
       FIND FIRST PaymPlan NO-LOCK WHERE
                  PaymPlan.Brand   = gcBrand AND
                  PaymPlan.CustNum = iiCustNum NO-ERROR.
   END.
   ELSE IF iiStatus > 0 THEN DO:
       FIND FIRST PaymPlan NO-LOCK WHERE
                  PaymPlan.Brand    = gcBrand AND
                  PaymPlan.PPStatus = iiStatus NO-ERROR.
   END. 
   ELSE DO:
       IF order = 1 THEN FIND FIRST PaymPlan USE-INDEX PPDate
          WHERE PaymPlan.Brand = gcBrand
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST PaymPlan 
          WHERE PaymPlan.Brand = gcBrand USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST PaymPlan USE-INDEX PPStatus
          WHERE PaymPlan.Brand = gcBrand
        NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF iiInvNum > 0 THEN DO:
       FOR EACH PPInv NO-LOCK WHERE
                PPInv.InvNum = iiInvNum,
          FIRST PaymPlan OF PPInv NO-LOCK
       BY PaymPlan.PPStatus: /* get first active one */
          LEAVE.
       END.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
       FIND LAST PaymPlan NO-LOCK WHERE
                 PaymPlan.Brand   = gcBrand AND
                 PaymPlan.CustNum = iiCustNum NO-ERROR.
   END.
   ELSE IF iiStatus > 0 THEN DO:
       FIND LAST PaymPlan NO-LOCK WHERE
                 PaymPlan.Brand    = gcBrand AND
                 PaymPlan.PPStatus = iiStatus NO-ERROR.
   END. 
   ELSE DO:
       IF order = 1 THEN FIND LAST PaymPlan USE-INDEX PPDate
          WHERE PaymPlan.Brand = gcBrand
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST PaymPlan 
          WHERE PaymPlan.Brand = gcBrand USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST PaymPlan USE-INDEX PPStatus
          WHERE PaymPlan.Brand = gcBrand
        NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF iiInvNum > 0 THEN DO:
       FIND FIRST PaymPlan WHERE PaymPlan.PPlanID = -1 NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
       FIND NEXT PaymPlan NO-LOCK WHERE
                 PaymPlan.Brand   = gcBrand AND
                 PaymPlan.CustNum = iiCustNum NO-ERROR.
   END.
   ELSE IF iiStatus > 0 THEN DO:
       FIND NEXT PaymPlan NO-LOCK WHERE
                 PaymPlan.Brand    = gcBrand AND
                 PaymPlan.PPStatus = iiStatus NO-ERROR.
   END. 
   ELSE DO:
       IF order = 1 THEN FIND NEXT PaymPlan USE-INDEX PPDate
          WHERE PaymPlan.Brand = gcBrand
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT PaymPlan 
          WHERE PaymPlan.Brand = gcBrand USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT PaymPlan USE-INDEX PPStatus
          WHERE PaymPlan.Brand = gcBrand
        NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF iiInvNum > 0 THEN DO:
       FIND FIRST PaymPlan WHERE PaymPlan.PPlanID = -1 NO-LOCK NO-ERROR.
   END.
   ELSE IF iiCustNum > 0 THEN DO:
       FIND PREV PaymPlan NO-LOCK WHERE
                 PaymPlan.Brand   = gcBrand AND
                 PaymPlan.CustNum = iiCustNum NO-ERROR.
   END.
   ELSE IF iiStatus > 0 THEN DO:
       FIND PREV PaymPlan NO-LOCK WHERE
                 PaymPlan.Brand    = gcBrand AND
                 PaymPlan.PPStatus = iiStatus NO-ERROR.
   END. 
   
   ELSE DO:
       IF order = 1 THEN FIND PREV PaymPlan USE-INDEX PPDate
          WHERE PaymPlan.Brand = gcBrand
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV PaymPlan 
          WHERE PaymPlan.Brand = gcBrand USE-INDEX CustNum
        NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV PaymPlan USE-INDEX PPStatus
          WHERE PaymPlan.Brand = gcBrand
        NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       PaymPlan.PPlanID
       PaymPlan.CustNum 
       lcCustName 
       PaymPlan.PPDate
       PaymPlan.Amount
       lcStatus
       lcType
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

    FIND Customer OF PaymPlan NO-LOCK NO-ERROR. 
    lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                  BUFFER Customer).
                                        
    fPPStatus(PaymPlan.PPStatus,FALSE).
    fPPType(PaymPlan.PPType,FALSE).
    
END PROCEDURE.

PROCEDURE local-UPDATE-record:
 
   DEF VAR liOldStatus AS INT NO-UNDO.
   
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      fPPType(PaymPlan.PPType,TRUE).
      
      liOldStatus = PaymPlan.PPStatus.

      DISP PaymPlan.PPlanID
           PaymPlan.CustNum
           lcCustName 
           PaymPlan.PPDate
           PaymPlan.Amount
           PaymPlan.PPStatus
           lcStatus
           PaymPlan.PPType
           lcType
           PaymPlan.BankDays
           PaymPlan.Orderer
      WITH FRAME lis.

      IF NEW(PaymPlan) THEN LEAVE.
        
      HandlePlan:
      REPEAT:
      
         fPPStatus(PaymPlan.PPStatus,TRUE).
               
         lcOrdCustName = "".
         IF PaymPlan.Orderer > 0 THEN DO:
            FIND Customer WHERE Customer.CustNum = PaymPlan.Orderer NO-LOCK.
            lcOrdCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                             BUFFER Customer).
         END.
   
         DISP PaymPlan.PPStatus lcType 
              PaymPlan.Orderer lcOrdCustName
         WITH FRAME lis.

         ASSIGN ufk    = 0 
                ehto   = 0
                ufk[3] = 1752
                ufk[8] = 8.
       
         /* change */
         IF PaymPlan.PPStatus < 4 THEN ufk[1] = 7.
         ELSE ufk[1] = 1068.
         
         /* print letter */
         IF PaymPlan.PPStatus = 3 THEN ufk[4] = 938.
         /* accept */
         IF PaymPlan.PPStatus < 3 OR
            (PaymPlan.PPStatus = 7 AND iiStatus = 7)
         THEN ufk[6] = 1062.
         /* cancel plan */
         IF PaymPlan.PPStatus < 4 OR
            (PaymPlan.PPStatus = 7 AND iiStatus = 7)
         THEN ufk[7] = 1063.
       
         RUN Syst/ufkey.
         
         IF toimi = 1 THEN DO:
        
            IF lcPassword > "" THEN DO:
             
               lcAskPassWd = "".
            
               PAUSE 0.
               UPDATE lcAskPassWd 
                  BLANK
                  FORMAT "X(20)" 
                  LABEL "Password"
                  HELP "Password for changing plan"
                  WITH OVERLAY ROW 10 CENTERED TITLE " PLAN CHANGE "
                       SIDE-LABELS FRAME fPassword.
               
               HIDE FRAME fPassword NO-PAUSE.
               
               IF lcAskPassWd NE lcPassword THEN NEXT. 
            END.

            ehto = 9. RUN Syst/ufkey.
      
            FIND CURRENT PaymPlan EXCLUSIVE-LOCK.
            
            UPDATE
            PaymPlan.PPStatus
            PaymPlan.BankDays
            PaymPlan.Orderer
            WITH FRAME lis EDITING:
         
               READKEY.
          
               IF KEYLABEL(LASTKEY) = "F9" THEN DO:
            
                  IF FRAME-FIELD = "PPStatus" THEN DO:
               
                     RUN Help/h-tmscodes.p(INPUT "PaymPlan",     /* TableName */
                                          "PPStatus",     /* FieldName */
                                          "AccRec",       /* GroupCode */
                                    OUTPUT lcCode).
              
                     IF lcCode ne "" AND lcCode NE ? THEN DO:
                        fPPStatus(INTEGER(lcCode),TRUE).
                        DISPLAY INTEGER(lcCode) ;& PaymPlan.PPStatus
                        WITH FRAME lis.   
                     END.
                  END.
                                      
                  ELSE IF FRAME-FIELD = "PPType" THEN DO:
               
                     RUN Help/h-tmscodes.p(INPUT "PaymPlan",    /* TableName */
                                          "PPType",      /* FieldName */
                                          "AccRec",      /* GroupCode */
                                    OUTPUT lcCode).
              
                     IF lcCode ne "" AND lcCode NE ? THEN DO:
                        fPPType(INTEGER(lcCode),TRUE).
                        DISPLAY INTEGER(lcCode) ;& PaymPlan.PPType
                        WITH FRAME lis.   
                     END.
                  END.
               
                  ehto = 9.
                  RUN Syst/ufkey.
                  NEXT. 
               END.

               ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
               THEN DO WITH FRAME lis:
             
                  PAUSE 0.
                
                  IF FRAME-FIELD = "PPStatus" THEN DO:
                     fPPStatus(INPUT INPUT FRAME lis PaymPlan.PPStatus,TRUE).
                     IF lcStatus = "" THEN DO:
                        BELL.
                        MESSAGE "Unknown status".
                        NEXT.
                     END.
                  END.
                
                  ELSE IF FRAME-FIELD = "PPType" THEN DO:
                     fPPType(INPUT INPUT FRAME lis PaymPlan.PPType,TRUE).
                     IF lcType = "" THEN DO:
                        BELL.
                        MESSAGE "Unknown status".
                        NEXT.
                     END. 
                  END.

                  ELSE IF FRAME-FIELD = "Orderer" THEN DO:
                     FIND Customer WHERE Customer.CustNum = 
                        INPUT FRAME lis PaymPlan.Orderer NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE Customer THEN DO:
                        BELL.
                        MESSAGE "Unknown customer" VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                  END.
               END.
          
               APPLY LASTKEY.
      
            END. /* EDITING */

            /* cancel payment plan */
            IF PaymPlan.PPStatus = 4 AND liOldStatus NE 4 THEN DO:
            
               ok = TRUE.
               MESSAGE "You are about to cancel this payment plan." SKIP
                       "Shall all invoices connected to this plan be"
                       "disconnected from it ?"
               VIEW-AS ALERT-BOX
               QUESTION
               BUTTONS YES-NO
               TITLE " CANCEL PAYMENT PLAN "
               SET ok.
            
               IF ok THEN
               FOR EACH PPInv OF PaymPlan NO-LOCK:
                  fRemPPInvoice(PPInv.InvNum,
                                1,
                                0.0).
               END.
               /* cancel cancellation */
               ELSE PaymPlan.PPStatus = liOldStatus.
            END.
         
         END.

         /* eventlog */
         ELSE IF toimi = 3 THEN DO:
         
           IF AVAILABLE PaymPlan THEN 
           RUN Mc/eventsel ("PaymPlan",
                         STRING(PaymPlan.PPlanID)).
         END.
         
         /* print letter */
         ELSE IF toimi = 4 THEN DO:

            ok = FALSE. 

            IF PaymPlan.PPStatus = 7 THEN DO:
               MESSAGE "Plan is in credit control. Printing not allowed."
               VIEW-AS ALERT-BOX INFORMATION.
               NEXT.
            END. 
            
            IF PaymPlan.PPStatus > 1 THEN DO:

               MESSAGE "Plan has already been accepted." SKIP
                       "Do You want to print the confirmation letter anyway?" 
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               TITLE " LETTER "
               SET ok.

               IF NOT ok THEN NEXT.  
            END.
            
            IF NOT ok THEN    
               MESSAGE "Print a confirmation letter to EPL?"
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               TITLE " LETTER "
               SET ok.
           
            IF ok THEN DO:

               liTargCust = fLetterCust().

               IF liTargCust > 0 THEN DO: 
                  RUN Ar/prinpplan(PaymPlan.PPlanID,
                                liTargCust,
                                OUTPUT lcError).
                          
                  IF lcError > "" THEN 
                    MESSAGE "Printing failed;" SKIP
                             lcError
                     VIEW-AS ALERT-BOX ERROR.
                  ELSE MESSAGE "Confirmation letter has been printed"
                       VIEW-AS ALERT-BOX 
                       TITLE " DONE ".
               END.
               ELSE MESSAGE "Printing cancelled" 
                    VIEW-AS ALERT-BOX INFORMATION.
            END.        
         END.           

         /* accept */
         ELSE IF toimi = 6 THEN DO:

            ldtLast = TODAY.
            FOR EACH PPBatch OF PaymPlan NO-LOCK:
               ACCUMULATE PPBatch.Amount (TOTAL).
               
               IF PPBatch.DueDate = ? THEN ldtLast = ?.
               
               ldtLast = MAX(ldtLast,PPBatch.DueDate).
            END.
              
            IF (ACCUM TOTAL PPBatch.Amount) NE PaymPlan.Amount OR
               ldtLast = ?
            THEN DO:
               MESSAGE "Plan has not been divided into batches properly"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END. 
             
            ASSIGN ok         = FALSE
                   llCredCtrl = FALSE 
                   ldtFirst   = ldtLast.    
                   
            /* latest batch is assigned too far to future */
            FOR EACH PPInv OF PaymPlan NO-LOCK,
               FIRST Invoice OF PPInv NO-LOCK:
               ldtFirst = MIN(ldtFirst,Invoice.DueDate).
            END. 

            IF PaymPlan.PPStatus NE 7 AND ldtLast > ldtFirst + 90 THEN DO:
               MESSAGE "Latest due date is assigned to be over 90 days from"
                       "oldest original due date." SKIP
                       "Plan will be marked to status 7, credit control." SKIP
                       "Continue with accepting?"
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               TITLE " TO CREDIT CONTROL "
               SET ok.

               IF NOT ok THEN NEXT.
               
               llCredCtrl = TRUE.
            END.
            
            IF NOT ok THEN 
               MESSAGE "Mark plan as accepted?" 
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               TITLE " ACCEPT PLAN "
               SET ok.
                     
            IF ok THEN DO:

               IF (PaymPlan.PPStatus < 2 AND NOT llCredCtrl) OR
                   PaymPlan.PPStatus = 7
               THEN DO:
                  MESSAGE "Print a confirmation letter to orderer?"  
                  VIEW-AS ALERT-BOX QUESTION
                  BUTTONS YES-NO
                  TITLE " PRINT LETTER "
                  SET ok.
                    
                  IF ok THEN DO:
                    
                     liTargCust = fLetterCust().
                       
                     IF liTargCust > 0 THEN DO:
                        RUN Ar/prinpplan(PaymPlan.PPlanID,
                                      liTargCust, 
                                      OUTPUT lcError).
                                      
                        IF lcError > "" THEN
                           MESSAGE "Printing failed;" SKIP
                                    lcError
                           VIEW-AS ALERT-BOX ERROR.
                        ELSE
                           MESSAGE "Confirmation letter has been printed."
                           VIEW-AS ALERT-BOX INFORMATION.
                     END.
                     ELSE DO:
                        MESSAGE "Printing and accepting cancelled" 
                        VIEW-AS ALERT-BOX INFORMATION.
                        NEXT.                           
                     END. 
                  END.
                  ELSE IF PaymPlan.PPType = 3 THEN DO:
                     MESSAGE "Printing and accepting cancelled" 
                     VIEW-AS ALERT-BOX INFORMATION.
                     NEXT.                           
                  END. 
               END.
               
               FIND CURRENT PaymPlan EXCLUSIVE-LOCK.
               IF llCredCtrl 
               THEN PaymPlan.PPStatus = 7.
               ELSE PaymPlan.PPStatus = 3.

               IF NOT llCredCtrl THEN DO: 
               
                  ok = TRUE.
                  
                  MESSAGE "Create a fee for activation?"  
                  VIEW-AS ALERT-BOX QUESTION
                  BUTTONS YES-NO
                  TITLE " CREATE FEE "
                  SET ok.
               
                  IF ok THEN RUN Mc/creasfee.p (PaymPlan.CustNum,
                                           0,
                                           TODAY,
                                           "PaymPlan",
                                           "Activate",
                                           1,
                                           ?,
                                           "",          /* memo */
                                           TRUE,        /* messages to screen */
                                           katun,
                                           "",
                                           0,
                                           "",
                                           "",
                                           OUTPUT lcError).
               END.  
               
               MESSAGE "Plan has been marked to status" PaymPlan.PPStatus 
               VIEW-AS ALERT-BOX 
               TITLE (IF PaymPlan.PPStatus = 3 
                      THEN " ACCEPTED "
                      ELSE " CREDIT CONTROL ").
            END.
                     
         END. 
         
         /* cancel plan */
         ELSE IF toimi = 7 THEN DO:
            ok = FALSE.
            
            MESSAGE "Payment plan will be cancelled, i.e. invoices will be"
                    "detached from it and the plan cannot be handled"
                    "any further." SKIP
                    "Continue with cancellation ?"
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO
            SET ok. 
            
            IF ok THEN DO:
               REPEAT WITH FRAME fCancel ON ENDKEY UNDO, NEXT HandlePlan:
                  ehto = 9.
                  RUN Syst/ufkey.
                  
                  PAUSE 0.
                  UPDATE lcCancelTxt 
                     FORMAT "X(60)" HELP "Reason for cancellation"
                  WITH NO-LABELS TITLE " CANCELLATION " 
                       OVERLAY ROW 10 CENTERED 1 DOWN FRAME fCancel.
                  
                  IF lcCancelTxt = "" THEN DO:
                     MESSAGE "Explanation for cancellation is mandatory"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  
                  HIDE FRAME fCancel NO-PAUSE.
                  LEAVE.
               END.
               
               FIND CURRENT PaymPlan EXCLUSIVE-LOCK.
                
               PaymPlan.PPStatus = 4.

               /* detach invoices */ 
               FOR EACH PPInv OF PaymPlan NO-LOCK:
                  fRemPPInvoice(PPInv.InvNum,
                                1,
                                0.0).
               END.
               
               CREATE Memo.
               ASSIGN Memo.Brand     = gcBrand
                      Memo.HostTable = "PaymPlan"
                      Memo.KeyValue  = STRING(PaymPlan.PPlanID)
                      Memo.CustNum   = PaymPlan.CustNum
                      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
                      Memo.CreUser   = katun 
                      Memo.MemoTitle = "Plan Cancelled"
                      Memo.MemoText  = lcCancelTxt.
                      Memo.CreStamp  = fMakeTS().
            
               MESSAGE "Plan has been marked to status 4"
               VIEW-AS ALERT-BOX
               TITLE " CANCELLED ".
               
            END.    
            
         END.
 
         ELSE IF toimi = 8 THEN LEAVE.
         
      END.
      
      LEAVE.
   END.
   
END PROCEDURE.

