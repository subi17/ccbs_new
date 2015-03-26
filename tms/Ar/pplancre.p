/* ----------------------------------------------------------------------
  MODULE .......: pplancre
  TASK .........: Create a payment plan from customer's unpaid invoices
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 10.03.04
  CHANGED ......: 10.05.04/aam mark as sent if reminder based
                  16.03.06/aam iiBatchQty, idtFromDate, idtToDate, iiInvNum
                  19.06.06/aam fRemPPInvoice
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{finvbal.i}
{fpaymplan.i}
{fppbatch.i}
{fppinv.i}
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhPaymPlan AS HANDLE NO-UNDO.
   lhPaymPlan = BUFFER PaymPlan:HANDLE.
   RUN StarEventInitialize(lhPaymPlan).

END.

DEF INPUT  PARAMETER iiCustNum   AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiInvNum    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiDue       AS INT  NO-UNDO. 
DEF INPUT  PARAMETER iiBatchQty  AS INT  NO-UNDO.
DEF INPUT  PARAMETER icAmounts   AS CHAR NO-UNDO. 
DEF INPUT  PARAMETER idtFromDate AS DATE NO-UNDO. 
DEF INPUT  PARAMETER idtToDate   AS DATE NO-UNDO. 
DEF INPUT  PARAMETER iiType      AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER oiPlanID    AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocError     AS CHAR NO-UNDO.

DEF VAR liBankDays AS INT  NO-UNDO.
DEF VAR llInvFound AS LOG  NO-UNDO. 


/* default for banking days */
liBankDays = fCParamI("PPBankDays").
IF liBankDays = ? THEN liBankDays = 0.


FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer OR Customer.Brand NE gcBrand THEN DO:
   ocError = "Unknown customer " + STRING(iiCustNum).
   RETURN.
END.

CREATE PaymPlan.
ASSIGN PaymPlan.Brand    = gcBrand
       PaymPlan.PPlanID  = NEXT-VALUE(PPlan)
       PaymPlan.CustNum  = iiCustNum
       PaymPlan.PPDate   = TODAY
       PaymPlan.BankDays = liBankDays
       PaymPlan.PPStatus = 0  /* created */
       PaymPlan.PPType   = iiType.

IF iiDue NE 4 THEN DO:

   /* if invoice nbr given then use only that */
   IF iiInvNum > 0 
   THEN llInvFound = fPPSingleInvoice(iiInvNum,
                                      OUTPUT PaymPlan.Amount).
 
   /* get unpaid invoices 
      iiDue 1:all
            2:due
            3:undue 
            4:pick manually
   */
   ELSE llInvFound = fPPInvCustTot(iiDue,
                                   OUTPUT PaymPlan.Amount).

   IF NOT llInvFound THEN DO:
      DELETE PaymPlan.
      ocError = "No invoices found".
      RETURN.
   END.

   /* divide into batches */
   IF NOT fPaymPlanBatches(iiBatchQty,
                           icAmounts,
                           idtFromDate,
                           idtToDate,
                           OUTPUT ocError) 
   THEN DO:
   
      FOR EACH PPInv OF PaymPlan:
         fRemPPInvoice(PPInv.InvNum,
                       1,
                       0.0).
         DELETE PPInv.
      END. 
      FIND CURRENT PaymPlan EXCLUSIVE-LOCK.
      DELETE PaymPlan.

      ocError = "Batches: " + ocError.
      RETURN.
   END.

END.

ASSIGN PaymPlan.PPStatus = 1
       oiPlanId          = PaymPlan.PPlanID.

IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPaymPlan).

RELEASE PaymPlan.

