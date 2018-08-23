/*---------------------------------------------------------------------- 
YCO-1023. Adding existing discounts to new tariffs 
/tms_support/201808/YCO-1023_otherdiscounts.p 
jotorres 23.08.2018
-----------------------------------------------------------------------*/

DEFINE VARIABLE lcDPSubjects AS CHARACTER NO-UNDO.  
DEFINE VARIABLE lcDPTargets  AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCount      AS INTEGER   NO-UNDO.

ASSIGN 
   lcDPSubjects = "CONT36,CONT37,CONT38,CONT39"                      
   lcDPTargets  = "CONT36MF,CONT37MF,CONT38MF,CONT39MF".
   
/* PRODISC20_6 ---------------------------------------*/   
FIND DiscountPlan NO-LOCK WHERE
     DiscountPlan.Brand    EQ  Syst.Var:gcBrand AND   
     DiscountPlan.DPRuleId EQ "PRODISC20_6" NO-ERROR.
IF NOT AVAILABLE DiscountPlan THEN 
   RETURN.     
   
/* DPSubjects. (CliTypes to assign the discount) */
DO liCount = 1 TO NUM-ENTRIES(lcDPSubjects):
   IF NOT CAN-FIND(FIRST DPSubject WHERE 
                         DPSubject.DPId      EQ DiscountPlan.DPId AND                 
                         DPSubject.DPSubject EQ ENTRY(liCount, lcDPSubjects))
   THEN DO:        
      CREATE DPSubject.
      ASSIGN
         DPSubject.DPId       = DiscountPlan.DPId                 
         DPSubject.DPSubject  = ENTRY(liCount, lcDPSubjects)                 
         DPSubject.ValidFrom  = TODAY                  
         DPSubject.ValidTo    = 12/31/2049.
   END.       
END.       

/* DPTargets. (BillItems to apply the discount) */
DO liCount = 1 TO NUM-ENTRIES(lcDPTargets):
   IF NOT CAN-FIND(FIRST DPTarget WHERE 
                         DPTarget.DPId        EQ DiscountPlan.DPId AND 
                         DPTarget.TargetTable EQ "BillItem"        AND 
                         DPTarget.TargetKey   EQ ENTRY(liCount, lcDPTargets))
   THEN DO:                                                   
      CREATE DPTarget.
      ASSIGN 
         DPTarget.DPId        = DiscountPlan.DPId
         DPTarget.ValidFrom   = TODAY
         DPTarget.ValidTo     = 12/31/2049
         DPTarget.TargetTable = "BillItem"
         DPTarget.TargetKey   = ENTRY(liCount, lcDPTargets)
         DPTarget.Included    = YES.
   END.           
END.     

/* RETDISC_MOB ---------------------------------------*/
FIND DiscountPlan NO-LOCK WHERE
     DiscountPlan.Brand    EQ  Syst.Var:gcBrand AND   
     DiscountPlan.DPRuleId EQ "RETDISC_MOB" NO-ERROR.
IF NOT AVAILABLE DiscountPlan THEN 
   RETURN.     
   
/* DPSubjects. (CliTypes to assign the discount) */
DO liCount = 1 TO NUM-ENTRIES(lcDPSubjects):
   IF NOT CAN-FIND(FIRST DPSubject WHERE 
                         DPSubject.DPId      EQ DiscountPlan.DPId AND                 
                         DPSubject.DPSubject EQ ENTRY(liCount, lcDPSubjects))
   THEN DO:        
      CREATE DPSubject.
      ASSIGN
         DPSubject.DPId       = DiscountPlan.DPId                 
         DPSubject.DPSubject  = ENTRY(liCount, lcDPSubjects)                 
         DPSubject.ValidFrom  = TODAY                  
         DPSubject.ValidTo    = 12/31/2049.
   END.       
END.       

/* DPTargets. (BillItems to apply the discount) */
DO liCount = 1 TO NUM-ENTRIES(lcDPTargets):
   IF NOT CAN-FIND(FIRST DPTarget WHERE 
                         DPTarget.DPId        EQ DiscountPlan.DPId AND 
                         DPTarget.TargetTable EQ "BillItem"        AND 
                         DPTarget.TargetKey   EQ ENTRY(liCount, lcDPTargets))
   THEN DO:                                                   
      CREATE DPTarget.
      ASSIGN 
         DPTarget.DPId        = DiscountPlan.DPId
         DPTarget.ValidFrom   = TODAY
         DPTarget.ValidTo     = 12/31/2049
         DPTarget.TargetTable = "BillItem"
         DPTarget.TargetKey   = ENTRY(liCount, lcDPTargets)
         DPTarget.Included    = YES.
   END.           
END.  

/* LRDISC2 ---------------------------------------*/
/* Note: For LRDISC2, Subject Type = All, so no need to add DPSubjects */ 
FIND DiscountPlan NO-LOCK WHERE
     DiscountPlan.Brand    EQ  Syst.Var:gcBrand AND   
     DiscountPlan.DPRuleId EQ "LRDISC2" NO-ERROR.
IF NOT AVAILABLE DiscountPlan THEN 
   RETURN.
   
/* DPTargets. (BillItems to apply the discount) */
DO liCount = 1 TO NUM-ENTRIES(lcDPTargets):
   IF NOT CAN-FIND(FIRST DPTarget WHERE 
                         DPTarget.DPId        EQ DiscountPlan.DPId AND 
                         DPTarget.TargetTable EQ "BillItem"        AND 
                         DPTarget.TargetKey   EQ ENTRY(liCount, lcDPTargets))
   THEN DO:                                                   
      CREATE DPTarget.
      ASSIGN 
         DPTarget.DPId        = DiscountPlan.DPId
         DPTarget.ValidFrom   = TODAY
         DPTarget.ValidTo     = 12/31/2049
         DPTarget.TargetTable = "BillItem"
         DPTarget.TargetKey   = ENTRY(liCount, lcDPTargets)
         DPTarget.Included    = YES.
   END.           
END.  
