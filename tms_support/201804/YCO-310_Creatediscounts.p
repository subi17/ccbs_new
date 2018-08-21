/* 
YCO-310  
Create new discounts:
CONVDISC_CS10F12 -> "Cross Sell 10% 12 months - Fixed Line Discount"
CONVDISC_CS10M12 -> "Cross Sell 10% 12 months - Mobile Line Discount"
*/ 

DEFINE VARIABLE liSequence AS INTEGER NO-UNDO.

DEFINE BUFFER bDiscountPlan FOR DiscountPlan.
DEFINE BUFFER bDPSubject    FOR DPSubject.  
DEFINE BUFFER bDPTarget     FOR DPTarget.

/* CONVDISC_CS10F12 */
IF NOT CAN-FIND(FIRST DiscountPlan WHERE 
                      DiscountPlan.Brand    EQ "1"      AND 
                      DiscountPlan.DPRuleId EQ "CONVDISC_CS10F12")
THEN DO:
   liSequence = 1.
   FIND LAST DiscountPlan EXCLUSIVE-LOCK 
        USE-INDEX DPId NO-ERROR.
   IF AVAILABLE DiscountPlan THEN 
      liSequence = DiscountPlan.DPId + 1.   
   CREATE DiscountPlan.
   ASSIGN
      DiscountPlan.Brand          = "1"                       
      DiscountPlan.DPId           = liSequence                        
      DiscountPlan.Subject        = "Contract Target"          
      DiscountPlan.TargetType     = "List"            
      DiscountPlan.DPUnit         = "Percentage"            
      DiscountPlan.Priority       = 1          
      DiscountPlan.ProcessStopper = FALSE             
      DiscountPlan.DPCurrency     = ""          
      DiscountPlan.BillCode       = "CONVDISC_CS10F12"           
      DiscountPlan.ValidFrom      = TODAY              
      DiscountPlan.ValidTo        = 12/31/2049             
      DiscountPlan.DPRuleId       = "CONVDISC_CS10F12"             
      DiscountPlan.DPName         = "Cross Sell 10% 12 months - Fixed Line Discount"             
      DiscountPlan.DPMemo         = ""               
      DiscountPlan.SubjectType    = "List"             
      DiscountPlan.MaxAmount      = 0             
      DiscountPlan.MinBaseAmount  = 0             
      DiscountPlan.CCDisplay      = 1             
      DiscountPlan.ValidPeriods   = 12             
      DiscountPlan.DPCategory     = "Cross sell".  
      
   CREATE DPRate.
   ASSIGN
      DPRate.DPId      = DiscountPlan.DPId 
      DPRate.DiscValue = 10
      DPRate.ValidFrom = TODAY 
      DPRate.ValidTo   = 12/31/2049.          
      
   /* Same subjects and targets as CONVDISC_CS20F12*/
   FIND FIRST bDiscountPlan NO-LOCK WHERE 
              bDiscountPlan.Brand    EQ "1" AND 
              bDiscountPlan.DPRuleId EQ "CONVDISC_CS20F12" 
        NO-ERROR.
   IF AVAILABLE bDiscountPlan THEN DO:
      /* Subjects */
      FOR EACH bDPSubject NO-LOCK WHERE  
               bDPSubject.DPId    EQ bDiscountPlan.DPId AND 
               bDPSubject.ValidTo EQ 12/31/2049:
         CREATE DPSubject.
         ASSIGN 
            DPSubject.DPId      = DiscountPlan.DPId
            DPSubject.DPSubject = bDPSubject.DPSubject
            DPSubject.ValidFrom = TODAY 
            DPSubject.ValidTo   = 12/31/49.                                            
      END.     
      /* Targets */
      FOR EACH bDPTarget NO-LOCK WHERE  
               bDPTarget.DPId    EQ bDiscountPlan.DPId AND 
               bDPTarget.ValidTo EQ 12/31/2049:
         CREATE DPTarget.
         ASSIGN 
            DPTarget.DPId        = DiscountPlan.DPId
            DPTarget.ValidFrom   = TODAY 
            DPTarget.ValidTo     = 12/31/2049
            DPTarget.TargetTable = bDPTarget.TargetTable
            DPTarget.TargetKey   = bDPTarget.TargetKey
            DPTarget.Included    = bDPTarget.Included.                               
      END.
   END.                  
END.                      
           
/* CONVDISC_CS10M12 */
IF NOT CAN-FIND(FIRST DiscountPlan WHERE 
                      DiscountPlan.Brand    EQ "1"      AND 
                      DiscountPlan.DPRuleId EQ "CONVDISC_CS10M12")
THEN DO:
   liSequence = 1.
   FIND LAST DiscountPlan EXCLUSIVE-LOCK 
        USE-INDEX DPId NO-ERROR.
   IF AVAILABLE DiscountPlan THEN 
      liSequence = DiscountPlan.DPId + 1.   
   CREATE DiscountPlan.
   ASSIGN
      DiscountPlan.Brand          = "1"                       
      DiscountPlan.DPId           = liSequence                        
      DiscountPlan.Subject        = "Contract Target"          
      DiscountPlan.TargetType     = "List"            
      DiscountPlan.DPUnit         = "Percentage"            
      DiscountPlan.Priority       = 1          
      DiscountPlan.ProcessStopper = FALSE             
      DiscountPlan.DPCurrency     = ""          
      DiscountPlan.BillCode       = "CONVDISC_CS10M12"           
      DiscountPlan.ValidFrom      = TODAY              
      DiscountPlan.ValidTo        = 12/31/2049             
      DiscountPlan.DPRuleId       = "CONVDISC_CS10M12"             
      DiscountPlan.DPName         = "Cross Sell 10% 12 months - Mobile Line Discount"             
      DiscountPlan.DPMemo         = ""               
      DiscountPlan.SubjectType    = "List"             
      DiscountPlan.MaxAmount      = 0             
      DiscountPlan.MinBaseAmount  = 0             
      DiscountPlan.CCDisplay      = 1             
      DiscountPlan.ValidPeriods   = 12             
      DiscountPlan.DPCategory     = "Cross sell".  
      
   CREATE DPRate.
   ASSIGN
      DPRate.DPId      = DiscountPlan.DPId 
      DPRate.DiscValue = 10
      DPRate.ValidFrom = TODAY 
      DPRate.ValidTo   = 12/31/2049.          
      
   /* Same subjects and targets as CONVDISC_CS20M12*/
   FIND FIRST bDiscountPlan NO-LOCK WHERE 
              bDiscountPlan.Brand    EQ "1" AND 
              bDiscountPlan.DPRuleId EQ "CONVDISC_CS20M12" 
        NO-ERROR.
   IF AVAILABLE bDiscountPlan THEN DO:
      /* Subjects */
      FOR EACH bDPSubject NO-LOCK WHERE  
               bDPSubject.DPId    EQ bDiscountPlan.DPId AND 
               bDPSubject.ValidTo EQ 12/31/2049:
         CREATE DPSubject.
         ASSIGN 
            DPSubject.DPId      = DiscountPlan.DPId
            DPSubject.DPSubject = bDPSubject.DPSubject
            DPSubject.ValidFrom = TODAY 
            DPSubject.ValidTo   = 12/31/49.                                            
      END.     
      /* Targets */
      FOR EACH bDPTarget NO-LOCK WHERE  
               bDPTarget.DPId    EQ bDiscountPlan.DPId AND 
               bDPTarget.ValidTo EQ 12/31/2049:
         CREATE DPTarget.
         ASSIGN 
            DPTarget.DPId        = DiscountPlan.DPId
            DPTarget.ValidFrom   = TODAY 
            DPTarget.ValidTo     = 12/31/2049
            DPTarget.TargetTable = bDPTarget.TargetTable
            DPTarget.TargetKey   = bDPTarget.TargetKey
            DPTarget.Included    = bDPTarget.Included.                               
      END.
   END.                  
END.             
           
           




            
       
