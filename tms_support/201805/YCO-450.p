/*----------------------------------------- 
YCO-450. Creating 2 new discounts.
/tms_support/201805/YCO-450.p 
jotorres 23.05.2018
-----------------------------------------*/

DEFINE VARIABLE lcDPSubjects AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDPTargets  AS CHARACTER NO-UNDO.    
DEFINE VARIABLE lcResult     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInfo       AS CHARACTER NO-UNDO.

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
      olcResult = "Discount " + ilcDPRule + " already exists --> ERROR".
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
  
   /* DPSubjects. (CliTypes to assign the discount) */
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

/* RETDISC_MOB */
ASSIGN
   lcDPSubjects = "CONT15,CONT25,CONT33,CONT34"
   lcDPTargets  = "CONT15MF,CONT25MF,CONT33MF,CONT34MF".
RUN pCreateDiscount (INPUT "RETDISC_MOB",
                     INPUT "CVM discount - Mobile Line",
                     INPUT "Percentage",
                     INPUT 10,
                     INPUT 24,
                     INPUT lcDPSubjects,
                     INPUT lcDPTargets,
                     INPUT "Retention",
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

/* RETDISC_CONV */
ASSIGN
   lcDPSubjects = 
      "CONTFH2G_50,CONTFH2G_300,CONTFH2G_1000,CONTFH39_50,"   + 
      "CONTFH49_300,CONTFH69_1000,CONTFH48_50,CONTFH58_300,"  + 
      "CONTFH76_1000,CONTFH3G_50,CONTFH3G_300,CONTFH3G_1000," +
      "CONTFH7G_50,CONTFH7G_300,CONTFH7G_1000,CONTFH59_50,"   +
      "CONTFH69_300,CONTFH89_1000,CONTFH99_50,CONTFH109_300," +
      "CONTFH129_1000,CONTFH35_50,CONTFH45_300,CONTFH65_1000"
   lcDPTargets = 
      "CONT10MF,CONT15MF,CONT25MF,CONT30MF,CONT32MF,CONT33MF,CONT34MF," +
      "CONTFH50MF,CONTFH300MF,CONTFH1000MF,"               +
      "CONTFH39_50PRO,CONTFH49_300PRO,CONTFH69_1000PRO,"   + 
      "CONTFH48_50PRO,CONTFH58_300PRO,CONTFH76_1000PRO,"   +
      "CONTFH59_50PRO,CONTFH69_300PRO,CONTFH89_1000PRO,"   + 
      "CONTFH99_50PRO,CONTFH109_300PRO,CONTFH129_1000PRO," +
      "CONTFH35_50PRO,CONTFH45_300PRO,CONTFH65_1000PRO".
RUN pCreateDiscount (INPUT "RETDISC_CONV",
                     INPUT "CVM discount - Convergent Line",
                     INPUT "Percentage",                     
                     INPUT 10,
                     INPUT 24,
                     INPUT lcDPSubjects,
                     INPUT lcDPTargets,
                     INPUT "Retention",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

MESSAGE lcInfo VIEW-AS ALERT-BOX.
