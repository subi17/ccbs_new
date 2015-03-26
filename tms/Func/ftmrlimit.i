/* ftmrlimit.i      12.05.08/aam
 
*/


/* copy default limits to customer */
FUNCTION fTMRLimit2Customer RETURNS LOGIC
   (iiCustNum AS INT):
   
   FOR EACH TMRule NO-LOCK WHERE 
            TMRule.Brand     = gcBrand AND
            TMRule.ToDate   >= TODAY   AND
            TMRule.FromDate <= TODAY   AND
            TMRule.LimitSource = 1     AND 
            TMRule.NewCustomer = TRUE,
       EACH TMRLimit OF TMRule NO-LOCK WHERE
            TMRLimit.ToDate   >= TODAY AND
            TMRLimit.FromDate <= TODAY:
               
      IF CAN-FIND(FIRST Limit USE-INDEX CustNum WHERE
                        Limit.CustNum   = iiCustNum        AND
                        Limit.LimitType = 1                AND
                        Limit.TMRuleSeq = TMRule.TMRuleSeq AND
                        Limit.LimitID   = TMRLimit.LimitID AND
                        Limit.ToDate   >= TODAY)
      THEN NEXT. 
         
      CREATE Limit.
      BUFFER-COPY TMRLimit TO Limit.
      ASSIGN 
         Limit.CustNum   = iiCustNum
         Limit.LimitType = 1
         Limit.DefValue  = TRUE.
   END.

END FUNCTION.

/* copy default limits to subscription */
FUNCTION fTMRLimit2Subscription RETURNS LOGIC
   (iiMsSeq AS INT):
   
   FOR EACH TMRule NO-LOCK WHERE 
            TMRule.Brand     = gcBrand AND
            TMRule.ToDate   >= TODAY   AND
            TMRule.FromDate <= TODAY   AND
            TMRule.LimitSource = 2     AND 
            TMRule.NewCustomer = TRUE,
       EACH TMRLimit OF TMRule NO-LOCK WHERE
            TMRLimit.ToDate   >= TODAY AND
            TMRLimit.FromDate <= TODAY:
               
      IF CAN-FIND(FIRST Limit USE-INDEX MsSeq WHERE
                        Limit.MsSeq     = iiMsSeq          AND
                        Limit.LimitType = 1                AND
                        Limit.TMRuleSeq = TMRule.TMRuleSeq AND
                        Limit.LimitID   = TMRLimit.LimitID AND
                        Limit.ToDate   >= TODAY)
      THEN NEXT. 
         
      CREATE Limit.
      BUFFER-COPY TMRLimit TO Limit.
      ASSIGN 
         Limit.MsSeq     = iiMsSeq
         Limit.LimitType = 1
         Limit.DefValue  = TRUE.
   END.

END FUNCTION.
  
