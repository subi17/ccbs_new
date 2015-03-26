/* fppinv.i     10.03.04/aam

   collect invoices to a payment plan
   
   PaymPlan should be in buffer

   changes:      01.07.04/aam check that invoice is printed
                 04.10.04/aam also unprinted invoices are taken
                 16.03.06/aam fPPSingleInvoice()
                 27.03.06/aam use fValidForPaymPlan for checking invoice
                              (fPPInvChk removed)
                 18.04.06/aam return invoices to last claim state when 
                              plan fails
                 16.06.06/aam ClaimState instead of ClaimQty,
                              fAddPPInvoice and fRemPPInvoice
*/

DEF BUFFER bChkPPInv    FOR PPInv.
DEF BUFFER bChkPaymPlan FOR PaymPlan.
DEF BUFFER bChkInvoice  FOR Invoice.

/* add invoice to plan */
FUNCTION fAddPPInvoice RETURNS LOGICAL
   (iiInvNum AS INT).
 
   FOR FIRST bChkInvoice WHERE 
             bChkInvoice.InvNum = iiInvNum EXCLUSIVE-LOCK,
       FIRST Customer OF bChkInvoice NO-LOCK:
                 
      /* invoice added to plan */              
      ASSIGN bChkInvoice.PaymState = 4
             bChkInvoice.ClaimPerm = FALSE.
   END.
   
END FUNCTION.
 
/* remove invoice from plan */
FUNCTION fRemPPInvoice RETURNS LOGICAL
   (iiInvNum AS INT,
    iiType   AS INT,
    idState  AS DEC).
 
   /* iiType 1=remove invoice from plan
             2=plan failed, remove and mark special claim status 
   */
             
   FOR FIRST bChkInvoice WHERE 
             bChkInvoice.InvNum = iiInvNum EXCLUSIVE-LOCK,
       FIRST Customer OF bChkInvoice NO-LOCK:
                 
      ASSIGN 
         bChkInvoice.PaymState = IF bChkInvoice.PaidAmt = bChkInvoice.InvAmt 
                                 THEN 2
                                 ELSE IF bChkInvoice.PaidAmt = 0 
                                      THEN 0
                                      ELSE 1
         bChkInvoice.ClaimPerm = Customer.ClaimPerm. 
   END. 
     
END FUNCTION.

/* collect all customer's unpaid invoices */ 
FUNCTION fPPInvCustTot RETURNS LOGICAL
   (iiDue AS INT,
    OUTPUT odBal AS DEC).

   /* iiDue 1=all
            2=due
            3=undue  */
            
   DEF VAR lcErr    AS CHAR NO-UNDO.
   DEF VAR ldInvBal AS DEC  NO-UNDO. 
   
   odBal = 0.
   
   FOR EACH Invoice NO-LOCK WHERE
            Invoice.Brand   = gcBrand          AND
            Invoice.CustNum = PaymPlan.CustNum AND
            Invoice.PaymState NE 2             AND 
            Invoice.InvType NE 3               AND
            Invoice.InvType NE 4:
       
      /*  due/undue invoices */
      IF (iiDue = 2 AND Invoice.DueDate  > PaymPlan.PPDate) OR
         (iiDue = 3 AND Invoice.DueDate <= PaymPlan.PPDate)
      THEN NEXT. 
        
      /* check that invoice is valid for payment plan */
      IF fValidForPaymPlan(Invoice.InvNum,
                           PaymPlan.PPType,
                           9999) > 0 THEN NEXT. 

      CREATE PPInv.
      ASSIGN PPInv.PPlanID = PaymPlan.PPlanID
             PPInv.InvNum  = Invoice.InvNum.
             PPInv.Amount  = fInvBal(BUFFER Invoice,
                                     TODAY).
             odBal         = odBal + PPInv.Amount.
 
      /* mark new payment state for invoices */
      fAddPPInvoice(Invoice.InvNum).
                       
   END.
            
   RETURN (CAN-FIND(FIRST PPInv OF PaymPlan)).
   
END FUNCTION.

/* collect one invoice */ 
FUNCTION fPPSingleInvoice RETURNS LOGICAL
   (iiInvNum AS INT,
    OUTPUT odBal AS DEC).

   DEF VAR lcErr    AS CHAR NO-UNDO.
   DEF VAR ldInvBal AS DEC  NO-UNDO. 
   
   odBal = 0.
   
   FOR FIRST Invoice NO-LOCK WHERE
             Invoice.InvNum = iiInvNUm:
       
      /* check that invoice is valid for payment plan */
      IF fValidForPaymPlan(Invoice.InvNum,
                           PaymPlan.PPType,
                           9999) > 0 THEN NEXT. 

      CREATE PPInv.
      ASSIGN PPInv.PPlanID = PaymPlan.PPlanID
             PPInv.InvNum  = Invoice.InvNum.
             PPInv.Amount  = fInvBal(BUFFER Invoice,
                                     TODAY).
             odBal         = odBal + PPInv.Amount.

      /* mark new payment state for invoices */
      fAddPPInvoice(Invoice.InvNum).
                       
   END.
            
   RETURN (odBal > 0).
   
END FUNCTION.


