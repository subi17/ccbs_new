/* fpaymplan.i       16.03.06/aam

   changes:          16.06.06/aam ClaimState instead of ClaimQty

*/

{finvbal.i}

/* check that invoice is valid for payment plan */
FUNCTION fValidForPaymPlan RETURNS INTEGER
   (iiInvNum   AS INT,
    iiPlanType AS INT,   /* 1=duedate change, 2=part payment, 3=payment plan */
    iiOverDue  AS INT):  /* how may days over due date are allowed */
   
   DEF BUFFER bChkInv   FOR Invoice.
   DEF BUFFER bChkPlan  FOR PaymPlan.
   DEF BUFFER bChkBatch FOR PPBatch.
   DEF BUFFER bChkPPInv FOR PPInv.
   
   DEF VAR ldDebt AS DEC NO-UNDO.
   
   /* errors:
      1= invoice not available
      2= paid already 
      3= already overdue (considering given limit, iiOverDue) 
      4= customer's first invoice
      5= booked to credit loss
      6= claimed
      7= deposit/advance payment invoice
      8= customer has another already due invoice
      9= direct debit
      10= old payment plan failed
      11= invoice customer has another active payment plan
      51= payment plan exists (due date chg)
      52= payment plan exists (part payment)
      53= payment plan exists (payment plan)
   */
      
   FIND bChkInv WHERE bChkInv.InvNum = iiInvNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bChkInv THEN RETURN 1.

   /* invoice must be unpaid (a positive balance) */
   ldDebt = fInvBal(BUFFER bChkInv,
                    TODAY).

   IF ldDebt <= 0 THEN RETURN 2. 

   /* not checked for 'free' payment plan */
   IF iiPlanType NE 3 THEN DO:

      /* too many days over original due date */
      IF bChkInv.DueDate + iiOverDue < TODAY THEN RETURN 3.

      /* it cannot be the first invoice for customer */
      IF NOT CAN-FIND(FIRST Invoice WHERE
                            Invoice.Brand   = bChkInv.Brand   AND
                            Invoice.CustNum = bChkInv.CustNum AND
                            Invoice.InvDate < bChkInv.InvDate)
      THEN RETURN 4. 

      /* another already due invoice */
      IF CAN-FIND(FIRST Invoice WHERE
                        Invoice.Brand     = bChkInv.Brand    AND
                        Invoice.CustNum   = bChkInv.CustNum  AND
                        Invoice.PaymState < 2                AND
                        Invoice.DueDate   < TODAY            AND
                        Invoice.InvAmt - Invoice.PaidAmt > 0 AND
                        Invoice.InvNum   NE bChkInv.InvNum)
      THEN RETURN 8.
   END.
   
   /* credit loss */
   IF bChkInv.PaymState = 3 THEN RETURN 5.  

   /* already in claiming process */
   IF bChkInv.ClaimState >= 6 AND bChkInv.ClaimState <= 10 THEN RETURN 6. 
   
   /* deposit / adv.payment invoice */
   IF bChkInv.InvType = 3 OR bChkInv.InvType = 4 THEN RETURN 7.
                    
   /* direct debit */
   IF bChkInv.ChargeType = 2 AND bChkInv.DueDate >= TODAY THEN RETURN 9.
   
   FOR EACH bChkPPInv NO-LOCK WHERE
            bChkPPInv.InvNum = iiInvNum,
      FIRST bChkPlan OF bChkPPInv NO-LOCK:
   
      /* cannot be involved in an active payment plan */
      IF bChkPlan.PPStatus < 4 THEN RETURN bChkPlan.PPType + 50.
   
      /* if previous plan has failed, over 3 months must have passed
         from that */
      ELSE IF bChkPlan.PPStatus = 6 AND iiPlanType NE 3 THEN DO:
   
         FOR EACH bChkBatch OF bChkPlan NO-LOCK
         BY bChkBatch.DueDate:
      
            IF bChkBatch.PaidAmt < bChkBatch.Amount AND
               bChkBatch.DueDate + bChkPlan.BankDays > TODAY - 90 
            THEN RETURN 10.
         END.
      END.   
   END.

   /* has invoice customer another payment plan */
   IF iiPlanType NE 3 THEN 
   FOR EACH bChkPlan NO-LOCK WHERE
            bChkPlan.Brand   = gcBrand         AND
            bChkPlan.CustNum = bChkInv.CustNum AND
            bChkPlan.PPStatus > 0              AND 
            bChkPlan.PPStatus < 4:
      RETURN 11.
   END. 
      
   /* ok */
   RETURN 0.
   
END FUNCTION.

FUNCTION fValidationError RETURNS CHAR
   (iiErrorCode AS INT):
   
   CASE iiErrorCode:
   WHEN 1  THEN RETURN "Unknown invoice".
   WHEN 2  THEN RETURN "Unpaid balance should be greater than zero".
   WHEN 3  THEN RETURN "Too many days have passed from due date".
   WHEN 4  THEN RETURN "Function is not allowed for first invoice".
   WHEN 5  THEN RETURN "Booked to credit loss".
   WHEN 6  THEN RETURN "Already claimed".
   WHEN 7  THEN RETURN "Not allowed for deposit/advance payment invoices".
   WHEN 8  THEN RETURN "Customer has another already due invoice".
   WHEN 9  THEN RETURN "Invoice will be charged using direct debit".
   WHEN 10 THEN RETURN "A previous payment plan has failed recently".
   WHEN 11 THEN RETURN "Customer already has an active payment plan".
   WHEN 51 OR
   WHEN 52 OR
   WHEN 53 THEN RETURN "Invoice is already part of an active payment plan".
   OTHERWISE    RETURN "Unknown error".
   END CASE. 
   
END FUNCTION.



