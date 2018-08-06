/*--------------------------------------------------------------
YCO-1002. Adding more ClyTypes allowed for discount RETDISC_CONV
/tms_support/201808/YCO-1002.p
--------------------------------------------------------------*/

DEFINE VARIABLE lcDPSubjects AS CHARACTER NO-UNDO.  
DEFINE VARIABLE lcDPTargets  AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCount      AS INTEGER   NO-UNDO.

ASSIGN 
   lcDPSubjects = "CONTFHNB3G_300,CONTFHNB7G_300,CONTFHNB69_300,"  +
                  "CONTFHNB109_300,CONTFHNB62_300,CONTFHNB58_300," +
                  "CONTFHNB45_300,CONTDSL2G,CONTDSL35,CONTDSL39,"  +
                  "CONTDSL45,CONTDSL48,CONTDSL3G,CONTDSL7G,CONTDSL99"
                      
   lcDPTargets  = "CONTDSLMF,CONTFH300MF,CONTFHNB109_300PRO,"              +
                  "CONTFHNB45_300PRO,CONTFHNB58_300PRO,CONTFHNB62_300PRO," + 
                  "CONTFHNB69_300PRO,CONTDSL35PRO,CONTDSL39PRO,"           + 
                  "CONTDSL48PRO,CONTDSL99PRO".
   
FIND DiscountPlan NO-LOCK WHERE
     DiscountPlan.Brand    EQ  Syst.Var:gcBrand AND   
     DiscountPlan.DPRuleId EQ "RETDISC_CONV" NO-ERROR.
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
         DPSubject.ValidFrom  = 05/24/2018                  
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
         DPTarget.ValidFrom   = 05/24/2018
         DPTarget.ValidTo     = 12/31/2049
         DPTarget.TargetTable = "BillItem"
         DPTarget.TargetKey   = ENTRY(liCount, lcDPTargets)
         DPTarget.Included    = YES.
   END.           
END.   
       