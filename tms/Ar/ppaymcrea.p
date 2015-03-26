/* ------------------------------------------------------
  MODULE .......: ppaymcrea.p
  FUNCTION .....: divide invoice into 2 payment batches 
                  (-> create a payment plan)
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 15.03.06
  MODIFIED .....: 11.05.06/aam 2.batch from original duedate, not from 1.batch
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{cparam2.i}
{timestamp.i}
{fctserval.i}
{fctchange.i}
{fmakemsreq.i}
{fduedate.i}
{finvbal.i}
{fpaymplan.i}

DEF INPUT PARAMETER iiInvNum AS INT  NO-UNDO. 

DEF VAR llOk          AS LOG  NO-UNDO.
DEF VAR ldDebt        AS DEC  NO-UNDO.
DEF VAR ldtDueDate    AS DATE NO-UNDO EXTENT 2.
DEF VAR ldAmount      AS DEC  NO-UNDO EXTENT 2.
DEF VAR liAddDays     AS INT  NO-UNDO.
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR liCreated     AS INT  NO-UNDO.
DEF VAR ldDivide      AS DEC  NO-UNDO. 
DEF VAR liValid       AS INT  NO-UNDO. 
DEF VAR llCreateFees  AS LOG  NO-UNDO.
DEF VAR llOrdCust     AS LOG  NO-UNDO.
DEF VAR liCustNum     AS INT  NO-UNDO.
DEF VAR lcCustName    AS CHAR NO-UNDO.

DEF BUFFER bInvoice FOR Invoice.
DEF BUFFER bAgrCust FOR Customer.


FORM
   SKIP(1)
   Invoice.InvNum  COLON 20 
      LABEL "Invoice Nbr" SKIP
   Invoice.InvDate COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999" SKIP
   Invoice.InvAmt  COLON 20 
      LABEL "Invoice Amount"
      FORMAT "->>>>>>9.99" SKIP
   ldDebt          COLON 20 
      LABEL "Current Debt"
      FORMAT "->>>>>>9.99" SKIP
   Invoice.DueDate COLON 20
      LABEL "Due Date"
      FORMAT "99-99-9999" SKIP(1)
      
   ldtDueDate[1] COLON 20
      LABEL "Due Date, Batch 1"
      FORMAT "99-99-9999" SKIP
   ldAmount[1] COLON 20
      LABEL "Amount, Batch 1"
      FORMAT "->>>>>>9.99" SKIP(1)
   ldtDueDate[2] COLON 20
      LABEL "Due Date, Batch 2"
      FORMAT "99-99-9999" SKIP
   ldAmount[2] COLON 20
      LABEL "Amount, Batch 2"
      FORMAT "->>>>>>9.99" 
   SKIP(1)
   llCreateFees    COLON 20
      LABEL "Create Fee"
      HELP "Create a fee for part payment activation"
      FORMAT "Yes/No"
   llOrdCust       COLON 20
      LABEL "Ordered By" 
      HELP "Is orderer the (I)nvoice or the (A)greement customer"
      FORMAT "Invoice customer/Agreement customer" SKIP

   liCustNum COLON 20
      NO-LABEL 
      FORMAT ">>>>>>>9"
   lcCustName 
      NO-LABEL
      FORMAT "X(35)"
      SKIP
   WITH ROW 2 OVERLAY SIDE-LABELS CENTERED  
        TITLE " PART PAYMENT, INVOICE " + STRING(iiInvNum) + " " 
        FRAME fCriter.

FIND Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Invoice THEN DO:
   MESSAGE "Unknown Invoice"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

/* latest x days from original due date */
liAddDays = fCParamI("PPDueDateLimit").
IF liAddDays = ? THEN liAddDays = 0.

/* check that invoice is valid for payment plan */
liValid = fValidForPaymPlan(Invoice.InvNum,
                            2,
                            liAddDays).

IF liValid > 0 THEN DO:

   lcError = fValidationError(liValid).

   MESSAGE lcError
   VIEW-AS ALERT-BOX ERROR.
   
   RETURN.
END. 


/* how many days forward */
liAddDays = fCParamI("PPaymBatch2Due").
IF liAddDays = ? OR liAddDays = 0 THEN DO:
   MESSAGE "Due date for batch 2 has not been defined"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

/* division rule for amount */
ldDivide = fCParamDE("PPaymBatch1Amt").
IF ldDivide = ? OR ldDivide = 0 THEN DO:
   MESSAGE "Rule for dividing amount to batches has not been defined"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

RUN invbal(Invoice.InvNum, OUTPUT ldDebt).

ASSIGN ldtDueDate[1] = MAX(Invoice.DueDate,TODAY)
       /* add defined days and check that new date is a banking day */
       ldtDueDate[2] = fChkDueDate(Invoice.DueDate + liAddDays)
       
       ldAmount[1]   = ROUND(ldDivide * ldDebt,2)
       ldAmount[2]   = ldDebt - ldAmount[1]
       llCreateFees  = TRUE
       llOrdCust     = TRUE.

PAUSE 0.
DISPLAY Invoice.InvNum 
        Invoice.InvDate
        Invoice.InvAmt
        ldDebt
        Invoice.DueDate
WITH FRAME fCriter.

FIND Customer WHERE Customer.CustNum = Invoice.CustNum  NO-LOCK.
FIND bAgrCust WHERE bAgrCust.CustNum = Customer.AgrCust NO-LOCK.

lDueDate:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO lDueDate, NEXT lDueDate:

   IF llOrdCust THEN DO:
      liCustNum = Invoice.CustNum.
      lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                    BUFFER Customer).
   END.                                 
 
   ELSE DO:
      liCustNum = Customer.AgrCust.
   
      lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                    BUFFER bAgrCust).
   END.                                 
 
   PAUSE 0.
   DISPLAY ldtDueDate 
           ldAmount
           llCreateFees
           llOrdCust 
           liCustNum
           lcCustName   WITH FRAME fCriter.

   ASSIGN
      ufk    = 0 
      ufk[1] = 7
      ufk[5] = 1027  
      ufk[8] = 8 
      ehto   = 0.
   RUN ufkey.

   IF toimi = 1 THEN DO:
   
      REPEAT WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
         
         ehto = 9. RUN ufkey.
         
         UPDATE ldAmount[1]
                llCreateFees
                llOrdCust
         WITH FRAME fCriter EDITING:

            READKEY.
            
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
            
               IF FRAME-FIELD = "ldAmount" THEN DO:
               
                  IF INPUT ldAmount[1] < ROUND(ldDivide * ldDebt,2) THEN DO:
                     MESSAGE "Minimum amount for batch 1 is" 
                             TRIM(STRING(ROUND(ldDivide * ldDebt,2),
                                  "->>>>>>>9.99"))
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  
                  IF INPUT ldAmount[1] >= ldDebt THEN DO:
                     MESSAGE "Amount for batch 1 should be less than"
                             "current debt"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
            END.
            
            APPLY LASTKEY.
           
         END.
    
         ldAmount[2] = ldDebt - ldAmount[1].
         LEAVE.
      END. 
            
   END. 
   
   ELSE IF toimi = 5 THEN DO:

      llOk = FALSE.
      MESSAGE "A part payment plan will be created for invoice." SKIP
              "Continue with request creation ?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO 
      SET llOk.
      
      IF NOT llOk THEN NEXT.
      
      liCreated = fPaymPlanRequest(Invoice.InvNum,
                                   liCustNum,
                                   "partpaym",
                                   STRING(ldtDueDate[1]) + ";" + 
                                   STRING(ldAmount[1]) + ";" + 
                                   STRING(ldtDueDate[2]) + ";" + 
                                   STRING(ldAmount[2]), 
                                   llCreateFees,   /* create fees */
                                   "",            /* creator     */
                                   OUTPUT lcError).
                                      
      IF liCreated > 0 THEN 
         MESSAGE "Request was created with ID" liCreated
         VIEW-AS ALERT-BOX INFORMATION.
         
      ELSE 
         MESSAGE "Request could not be created;" SKIP
                 lcError
         VIEW-AS ALERT-BOX ERROR.

      LEAVE.
   END.
   
   ELSE IF toimi = 8 THEN LEAVE.

END. /* lDueDate */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

