/* ----------------------------------------------------------------------
  MODULE .......: ddpaymentco.p
  TASK .........: Collect dd invoices to be paid  
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 23.04.07
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

DEFINE INPUT  PARAMETER icInvGrp       AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iiCustNum1     AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiCustNum2     AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER icInvID1       AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER icInvID2       AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER idtDueDate     AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER iiInvType      AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER icBillRun      AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER idtPaymDate    AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER oiInvCount     AS INT  NO-UNDO.
DEFINE OUTPUT PARAMETER ocError        AS CHAR NO-UNDO. 

DEF VAR liPicked    AS INT  NO-UNDO. 
DEF VAR ldtFrom     AS DATE NO-UNDO.
DEF VAR ldtTo       AS DATE NO-UNDO.

{Ar/ddpaymentt.i}


FUNCTION fMakeTemp RETURNS LOGICAL.

    /* already paid */
    IF Invoice.PaymState > 0 THEN RETURN FALSE.

    /* charge type is not direct debit */
    IF Invoice.ChargeType NE 2 THEN RETURN FALSE. 

    /* not sent to bank */
    IF Invoice.DDState = 0 THEN RETURN FALSE. 
    
    /* claiming status already marked */
    IF Invoice.ClaimStatus NE "" AND
       Invoice.ClaimStatus NE "0.0" THEN RETURN FALSE. 
    
    /* no negative invoices */
    IF Invoice.InvAmt <= 0 THEN RETURN FALSE. 
    
    /* no partly paid invoices */
    IF Invoice.PaidAmt NE 0 THEN RETURN FALSE. 
    
    /* printing denied */
    IF Invoice.InvCfg[1] = TRUE THEN RETURN FALSE.

    IF iiInvType > 0 AND Invoice.InvType NE iiInvType 
    THEN RETURN FALSE.

    IF icBillRun > "" AND NOT Invoice.BillRun BEGINS icBillRun 
    THEN RETURN FALSE. 

    CREATE ttInvoice.
    ASSIGN ttInvoice.InvNum  = Invoice.InvNum
           liPicked          = liPicked + 1. 

    RETURN TRUE.
    
END FUNCTION.


IF idtDueDate NE ? THEN ASSIGN 
   ldtFrom = idtDueDate - 40
   ldtTo   = idtDueDate.
ELSE ASSIGN 
   ldtFrom = 1/1/7
   ldtTo   = 12/31/50.

IF icInvID1 = icInvID2 THEN 
FOR FIRST Invoice NO-LOCK WHERE
          Invoice.Brand    = gcBrand AND
          Invoice.ExtInvID = icInvID1,
    FIRST Customer OF Invoice NO-LOCK:
   
   icInvGrp = Customer.InvGroup.
   
   fMakeTemp().

END.

ELSE
FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE               
         Invoice.Brand    = gcBrand    AND
         Invoice.InvDate  >= ldtFrom   AND
         Invoice.InvDate  <= ldtTo     AND
         Invoice.ExtInvID >= icInvID1  AND      
         Invoice.ExtInvID <= icInvID2  AND   
         Invoice.CustNum >= iiCustNum1 AND
         Invoice.CustNum <= iiCustNum2 AND
         (IF idtDueDate NE ?
          THEN Invoice.DueDate = idtDueDate
          ELSE TRUE),
   FIRST Customer OF Invoice NO-LOCK WHERE
         (IF icInvGrp NE "" 
          THEN Customer.InvGroup = icInvGrp 
          ELSE TRUE):

   fMakeTemp().

   IF SESSION:BATCH = FALSE AND liPicked MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY liPicked FORMAT ">>>>>>>9"
      WITH NO-LABELS OVERLAY ROW 10 CENTERED
           TITLE " Collecting " FRAME fColl2.
   END.
   
END. 


RUN Ar/ddpayment (INPUT-OUTPUT TABLE ttInvoice,  
               idtPaymDate,
               liPicked,
               OUTPUT oiInvCount). 

ocError = ocError + (IF ocError > "" THEN ", " ELSE "") + 
          RETURN-VALUE.

 
