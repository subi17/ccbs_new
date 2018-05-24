/*--------------------------------------------------------------
YCO-388
Creating new discount PRODISC20_6 "20 % discount 6 months PRO"
jotorres. 19/05/2018
--------------------------------------------------------------*/

DEFINE VARIABLE lcResult AS CHARACTER NO-UNDO. 

PROCEDURE pCreateDiscount:
  
   /* Parameters */
   DEFINE INPUT  PARAMETER ilcDPRule         AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ilcDPName         AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ilcDPUnit         AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ildDiscValue      AS DECIMAL   NO-UNDO.
   DEFINE INPUT  PARAMETER ilValidPeriods    AS INTEGER   NO-UNDO.
   DEFINE INPUT  PARAMETER ilcDPSubjectsList AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ilcDPTargetsList  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ilcDPCategory     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER olcResult         AS CHARACTER NO-UNDO.
        
   DEFINE VARIABLE liCont     AS INTEGER NO-UNDO.
   DEFINE VARIABLE liSequence AS INTEGER NO-UNDO.

   /* Checking if DiscountPlan already exists. */
   IF CAN-FIND(FIRST DiscountPlan WHERE 
                     DiscountPlan.Brand    EQ "1" AND 
                     DiscountPlan.DPRuleId EQ ilcDPRule) THEN DO:
      olcResult = "DiscountPlan " + ilcDPRule + " already exists --> ERROR".
      RETURN.
   END.

   /* Getting next sequence for DiscountPlan. */
   liSequence = 1.
   FIND LAST DiscountPlan EXCLUSIVE-LOCK USE-INDEX DPId NO-ERROR.
   IF AVAILABLE DiscountPlan THEN 
      liSequence = DiscountPlan.DPId + 1.
         
   CREATE DiscountPlan.
   ASSIGN
      DiscountPlan.Brand          = "1"                       
      DiscountPlan.DPId           = liSequence                        
      DiscountPlan.Subject        = "Contract Target"          
      DiscountPlan.TargetType     = "List"            
      DiscountPlan.DPUnit         = ilcDPUnit            
      DiscountPlan.Priority       = 1          
      DiscountPlan.ProcessStopper = FALSE             
      DiscountPlan.DPCurrency     = ""          
      DiscountPlan.BillCode       = ilcDPRule           
      DiscountPlan.ValidFrom      = TODAY              
      DiscountPlan.ValidTo        = 12/31/2049             
      DiscountPlan.DPRuleId       = ilcDPRule             
      DiscountPlan.DPName         = ilcDPName             
      DiscountPlan.DPMemo         = ""               
      DiscountPlan.SubjectType    = "List"             
      DiscountPlan.MaxAmount      = 0             
      DiscountPlan.MinBaseAmount  = 0             
      DiscountPlan.CCDisplay      = 1             
      DiscountPlan.ValidPeriods   = ilValidPeriods             
      DiscountPlan.DPCategory     = ilcDPCategory.

   /* DPRate */
   CREATE DPRate.
   ASSIGN
      DPRate.DPId      = DiscountPlan.DPId 
      DPRate.DiscValue = ildDiscValue
      DPRate.ValidFrom = TODAY 
      DPRate.ValidTo   = 12/31/2049. 
  
   /* DPSubjects. (Clitypes to assign the discount) */
   DO liCont = 1 TO NUM-ENTRIES(ilcDPSubjectsList):
      CREATE DPSubject.
      ASSIGN
         DPSubject.DPId       = DiscountPlan.DPId                 
         DPSubject.DPSubject  = ENTRY(liCont, ilcDPSubjectsList)                 
         DPSubject.ValidFrom  = TODAY                  
         DPSubject.ValidTo    = 12/31/2049. 
   END.
   
   /* DPTargets. (BillItems to apply the discount) */
   DO liCont = 1 TO NUM-ENTRIES(ilcDPTargetsList):
      CREATE DPTarget.
      ASSIGN 
         DPTarget.DPId        = DiscountPlan.DPId
         DPTarget.ValidFrom   = TODAY
         DPTarget.ValidTo     = 12/31/2049
         DPTarget.TargetTable = "BillItem"
         DPTarget.TargetKey   = ENTRY(liCont, ilcDPTargetsList)
         DPTarget.Included    = YES.
   END.      

   olcResult = "Created discount " + ilcDPRule + " --> OK".

   RELEASE DiscountPlan.
 
END.   

/* --- MAIN BLOCK --- */
RUN pCreateDiscount 
   (INPUT "PRODISC20_6",
    INPUT "20 % discount 6 months PRO",
    INPUT "Percentage", 
    INPUT 20,
    INPUT 6,
    INPUT "CONTDSL48,CONTDSL52,CONTDSL59,CONTDSL99,CONTFH109_300," + 
          "CONTFH129_1000,CONTFH48_50,CONTFH52_50,CONTFH58_300,"   + 
          "CONTFH59_50,CONTFH62_300,CONTFH69_300,CONTFH76_1000,"   + 
          "CONTFH82_1000,CONTFH89_1000,CONTFH99_50",
    INPUT "CONT15MF,CONT25MF,CONT26MF,CONT30MF,CONTDSL48PRO,"     + 
          "CONTDSL52PRO,CONTDSL59PRO,CONTDSL99PRO,CONTDSLMF,"     + 
          "CONTFH1000MF,CONTFH109_300PRO,CONTFH129_1000PRO,"      + 
          "CONTFH300MF,CONTFH48_50PRO,CONTFH50MF,CONTFH52_50PRO," + 
          "CONTFH58_300PRO,CONTFH59_50PRO,CONTFH62_300PRO,"       + 
          "CONTFH69_300PRO,CONTFH76_1000PRO,CONTFH82_1000PRO,"    + 
          "CONTFH89_1000PRO,CONTFH99_50PRO",
    INPUT "Retention", 
    OUTPUT lcResult).
                     
DISPLAY lcResult FORMAT "X(50)".

