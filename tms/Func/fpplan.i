/* fpplan.i     08.04.04/aam
 
   update payment plan's payment status
   
   changes:     23.03.06/aam  epayment 
                08.08.06/aam  don't close plan if batch with status 5 exists    
*/
   
FUNCTION fPaymPlanPaid RETURNS LOGICAL
   (iiCustNum AS INT,
    iiInvNum  AS INT,
    idAmt     AS DEC).
    
    DEF VAR ldPaid  AS DEC NO-UNDO.
    DEF VAR ldBatch AS DEC NO-UNDO.
    
    FOR EACH PaymPlan EXCLUSIVE-LOCK WHERE
             PaymPlan.Brand   = gcBrand   AND
             PaymPlan.CustNum = iiCustNum AND
             PaymPlan.PPStatus < 4,
       FIRST PPInv OF PaymPlan NO-LOCK WHERE
             PPInv.InvNum = iiInvNum,
        EACH PPBatch OF PaymPlan EXCLUSIVE-LOCK WHERE
             (PPBatch.PBStatus < 2 OR PPBatch.PBStatus = 5)
     BY PaymPlan.PPlanID
     BY PPBatch.PPBatch:
    
        /* epayment */
        IF PPBatch.PBStatus = 5 
        THEN ldBatch = 0.
        ELSE ldBatch = PPBatch.PaidAmt.
        
        ASSIGN ldPaid           = MIN(idAmt,PPBatch.Amount - ldBatch)
               PPBatch.PaidAmt  = ldBatch + ldPaid
               PPBatch.PBStatus = IF PPBatch.PaidAmt = PPBatch.Amount
                                  THEN 2
                                  ELSE IF PPBatch.PaidAmt = 0
                                       THEN 0
                                       ELSE 1
               idAmt            = idAmt - ldPaid.
               
        /* mark plan as closed if all batches are paid */
        IF NOT CAN-FIND(FIRST PPBatch OF PaymPlan WHERE 
                              PPBatch.PBStatus < 2) AND
           NOT CAN-FIND(FIRST PPBatch OF PaymPlan WHERE 
                              PPBatch.PBStatus = 5) 
        THEN PaymPlan.PPStatus = 5. 
        
        IF idAmt = 0 THEN LEAVE.       
    END.
     
END FUNCTION.
    
    
