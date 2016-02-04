/* ------------------------------------------------------
  MODULE .......: duedatechg.p
  FUNCTION .....: set a new due date for invoice (-> create a payment plan)
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 14.03.06
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/fctserval.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
{Func/fduedate.i}
{Func/finvbal.i}
{Func/fpaymplan.i}

DEF INPUT PARAMETER iiInvNum AS INT  NO-UNDO. 

DEF VAR llOk          AS LOG  NO-UNDO.
DEF VAR ldDebt        AS DEC  NO-UNDO.
DEF VAR ldtNewDueDate AS DATE NO-UNDO.
DEF VAR liAddDays     AS INT  NO-UNDO.
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR liCreated     AS INT  NO-UNDO.
DEF VAR liValid       AS INT  NO-UNDO.
DEF VAR llCreateFees  AS LOG  NO-UNDO.
DEF VAR llOrdCust     AS LOG  NO-UNDO.
DEF VAR liCustNum     AS INT  NO-UNDO.
DEF VAR lcCustName    AS CHAR NO-UNDO.

DEF BUFFER bInvoice FOR Invoice.
DEF BUFFER bAgrCust FOR Customer.


FORM
   SKIP(1)
   Invoice.InvNum  COLON 18 
      LABEL "Invoice Nbr" SKIP
   Invoice.InvDate COLON 18 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999" SKIP
   Invoice.InvAmt  COLON 18 
      LABEL "Invoice Amount"
      FORMAT "->>>>>>9.99" SKIP
   ldDebt          COLON 18 
      LABEL "Current Debt"
      FORMAT "->>>>>>9.99" SKIP
   Invoice.DueDate COLON 18
      LABEL "Due Date"
      FORMAT "99-99-9999" SKIP(1)
   ldtNewDueDate   COLON 18
      LABEL "New Due Date"
      FORMAT "99-99-9999" SKIP
   llCreateFees    COLON 18
      LABEL "Create Fee"
      HELP "Create a fee for due date change"
      FORMAT "Yes/No"
   llOrdCust       COLON 18
      LABEL "Ordered By" 
      HELP "Is orderer the (I)nvoice or the (A)greement customer"
      FORMAT "Invoice customer/Agreement customer" SKIP

   liCustNum COLON 18
      NO-LABEL 
      FORMAT ">>>>>>>9"
   lcCustName 
      NO-LABEL
      FORMAT "X(35)"
      SKIP
   SKIP(1)
   WITH ROW 5 OVERLAY SIDE-LABELS CENTERED  
        TITLE " CHANGE DUE DATE, INVOICE " + STRING(iiInvNum) + " " 
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
                            1,
                            liAddDays).

IF liValid > 0 THEN DO:

   lcError = fValidationError(liValid).

   MESSAGE lcError
   VIEW-AS ALERT-BOX ERROR.
   
   RETURN.
END. 

/* how many days forward */
liAddDays = fCParamI("PPDueDateChg").
IF liAddDays = ? OR liAddDays = 0 THEN DO:
   MESSAGE "Due date transfer has not been defined"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

       /* add defined days and check that new date is a banking day */
ASSIGN ldtNewDueDate = fChkDueDate(Invoice.DueDate + liAddDays)
       llCreateFees  = TRUE
       llOrdCust     = TRUE.

RUN Ar/invbal(Invoice.InvNum, OUTPUT ldDebt).
 
PAUSE 0.
DISPLAY Invoice.InvNum 
        Invoice.InvDate
        Invoice.InvAmt
        ldDebt
        Invoice.DueDate
        ldtNewDueDate
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
   DISPLAY llOrdCust 
           liCustNum
           lcCustName
           llCreateFees 
   WITH FRAME fCriter.

   ASSIGN
      ufk    = 0 
      ufk[1] = 7
      ufk[5] = 1027  
      ufk[8] = 8 
      ehto   = 0.
   RUN Syst/ufkey.

   IF toimi = 1 THEN DO:
   
      REPEAT WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
         
         ehto = 9. RUN Syst/ufkey.
         
         UPDATE llCreateFees llOrdCust WITH FRAME fCriter.
         LEAVE.
      END. 

   END.
   
   IF toimi = 5 THEN DO:

      llOk = FALSE.
      MESSAGE "A new due date will be set for invoice." SKIP
              "Continue with request creation ?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO 
      SET llOk.
      
      IF NOT llOk THEN NEXT.
      
      liCreated = fPaymPlanRequest(Invoice.InvNum,
                                   liCustNum,
                                   "duedate",
                                   STRING(ldtNewDueDate) + ";" +
                                   STRING(ldDebt),
                                   llCreateFees, 
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

