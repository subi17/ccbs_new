/* fppbatch.i       10.03.04/aam 
                    15.03.06/aam batch qty and dates to fPaymPlanBatches

   divide a payment plan into batches 
   
   cparam2.i needed,
   PaymPlan should be in buffer
*/

{Func/refcode.i}
{Func/frefnum.i}
{Func/fduedate.i}

DEF VAR lcBatchAmt   AS CHAR                   NO-UNDO.
DEF VAR liBatchAmt   AS INT                    NO-UNDO.
DEF VAR lcPaymTerm   AS CHAR                   NO-UNDO.
DEF VAR liTermQty    AS INT                    NO-UNDO. 
DEF VAR lcBatchFee   AS CHAR                   NO-UNDO.
DEF VAR liFeeQty     AS INT                    NO-UNDO. 

ASSIGN lcBatchAmt = fCParamC("PPBatchAmt")
       lcPaymTerm = fCParamC("PPPaymTerm").
       
IF lcBatchAmt = ? OR
   lcBatchFee = ? OR
   lcPaymTerm = ?
THEN ASSIGN lcBatchAmt = ""
            lcBatchFee = ""
            lcPaymTerm = "".

ASSIGN liTermQty = NUM-ENTRIES(lcPaymTerm,"/")
       liFeeQty  = 0.


/* divide automatically to batches according to params */
FUNCTION fPaymPlanBatches RETURNS LOGICAL
   (INPUT  iiBatchQty  AS INT,
    INPUT  icAmounts   AS CHAR,
    INPUT  idtFromDate AS DATE,
    INPUT  idtToDate   AS DATE,
    OUTPUT ocError     AS CHAR). 

   DEF VAR ldPlanAmt    AS DEC  NO-UNDO. 
   DEF VAR ldTotPlanAmt AS DEC  NO-UNDO. 
   DEF VAR ldPlanBal    AS DEC  NO-UNDO. 
   DEF VAR liPPCnt      AS INT  NO-UNDO. 
   DEF VAR lcDueDates   AS CHAR NO-UNDO.

   ocError = "".
   
   IF PaymPlan.PPStatus > 1 THEN DO:
      ocError = "Status of plan is already " + STRING(PaymPlan.PPStatus).
      RETURN FALSE.
   END.
  
   IF CAN-FIND(FIRST PPBatch OF PaymPlan) THEN DO:
      ocError = "Plan already has batches".
      RETURN FALSE.
   END. 
   
   /* amount to be divided */
   FOR EACH PPInv OF PaymPlan NO-LOCK:
   
      ACCUMULATE PPInv.Amount (TOTAL).

      /* check that invoice is valid for payment plan */
      FIND Invoice OF PPInv NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Invoice THEN DO:
         ocError = "Unknown invoice " + STRING(PPInv.InvNum).
         RETURN FALSE.
      END.
              
      IF Invoice.PaymState > 1 AND Invoice.PaymState NE 4 THEN DO:
         ocError = "Invoice " + STRING(PPInv.InvNum) +
                   " is no longer valid for a payment plan".
         RETURN FALSE.
      END.

      ldPlanBal = fInvBal(BUFFER Invoice,
                          TODAY).
                            
      IF ldPlanBal NE PPInv.Amount THEN DO:
         ocError = "Balance of invoice " + STRING(PPInv.InvNum) + 
                   " has changed after it was picked for this payment plan".
         RETURN FALSE.
      END.
      
   END.
           
   ASSIGN ldPlanAmt    = (ACCUM TOTAL PPInv.Amount)
          ldTotPlanAmt = ldPlanAmt.

   /* how many batches should be made */
   IF iiBatchQty = 0 THEN DO:
      liBatchAmt = 0.
      DO liPPCnt = NUM-ENTRIES(lcBatchAmt,"/") TO 1 BY -1:
         IF ldPlanAmt >= DECIMAL(ENTRY(liPPCnt,lcBatchAmt,"/")) THEN DO:
            liBatchAmt = liPPCnt.
            LEAVE. 
         END.   
      END.
   END.
   ELSE liBatchAmt = iiBatchQty.
   
   /* divide total amount into batches, add possible extra fees */
   IF ldPlanAmt > 0 AND liBatchAmt > 0 THEN DO TRANS:
           
      /* default dates from cparam if not given */      
      IF idtFromDate = ? THEN 
         idtFromDate = PaymPlan.PPDate + INTEGER(ENTRY(1,lcPaymTerm,"/")).
      IF idtToDate   = ? THEN ASSIGN 
         idtToDate   = PaymPlan.PPDate +
                       INTEGER(ENTRY(MAX(liBatchAmt,liTermQty),
                               lcPaymTerm,"/")).

      DO liPPCnt = 1 TO liBatchAmt:

         CREATE PPBatch.
         ASSIGN 
         PPBatch.PPlanID  = PaymPlan.PPlanID
         PPBatch.PPBatch  = liPPCnt
         PPBatch.BatchFee = IF liFeeQty >= liPPCnt AND PaymPlan.PPType NE 1
                            THEN DECIMAL(ENTRY(liPPCnt,lcBatchFee,"/"))
                            ELSE 0
         PPBatch.PBStatus = 0.
         
         IF PaymPlan.PPType = 3 THEN    
            PPBatch.RefNum   = fFormRefNum(PaymPlan.CustNum,
                                           PaymPlan.PPlanID,
                                           -1 * PPBatch.PPBatch).
         ELSE 
            PPBatch.RefNum   = fFormRefNum(PaymPlan.CustNum,
                                           0,
                                           0).
                                            
          /* for last batch use all that is left */
         IF liPPCnt = liBatchAmt THEN PPBatch.Amount = ldPlanAmt.

         ELSE DO:
            /* amounts have been given */
            IF icAmounts > "" AND NUM-ENTRIES(icAmounts,";") >= liPPCnt
            THEN PPBatch.Amount = DECIMAL(ENTRY(liPPCnt,icAmounts,";")).
            /* divide total amount evenly */            
            ELSE PPBatch.Amount = ROUND(ldTotPlanAmt / liBatchAmt,2).
         END.

         IF PPBatch.Amount = 0 THEN DO:
            DELETE PPBatch.
            NEXT.
         END. 

         ldPlanAmt = ldPlanAmt - PPBatch.Amount.
 
         /* divide batches evenly between begin and end date */
         IF liPPCnt = 1 THEN 
            PPBatch.DueDate = idtFromDate.
         ELSE IF liPPCnt = liBatchAmt THEN 
            PPBatch.DueDate = idtToDate.
         ELSE 
            PPBatch.DueDate = idtFromDate + 
                              (liPPCnt - 1) * (idtToDate - idtFromDate + 1) / 
                                              (liBatchAmt - 1).
         /* move pass weekends and holidays */
         PPBatch.DueDate = fChkDueDate(PPBatch.DueDate).   

      END.
           
      RETURN TRUE.
              
   END.

   ELSE ocError = "No batches could be created".
   
   RETURN FALSE.
   
END FUNCTION.

