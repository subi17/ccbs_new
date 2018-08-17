/*----------------------------------------- 
YCO-700. New discount CONT_DISC_TB_20.
/tms_support/201806/YCO-700.p 
jotorres 27.06.2018
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
      DiscountPlan.ValidTo        = 12/31/2018             
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
      DPRate.ValidTo   = 12/31/2018. 
  
   /* DPSubjects. (CliTypes to assign the discount) */
   DO liCont = 1 TO NUM-ENTRIES(ilcDPSubjectsList):
      CREATE DPSubject.
      ASSIGN
         DPSubject.DPId       = DiscountPlan.DPId                 
         DPSubject.DPSubject  = ENTRY(liCont, ilcDPSubjectsList)                 
         DPSubject.ValidFrom  = TODAY                  
         DPSubject.ValidTo    = 12/31/2018. 
   END.
   
   /* DPTargets. (BillItems to apply the discount) */
   DO liCont = 1 TO NUM-ENTRIES(ilcDPTargetsList):
      CREATE DPTarget.
      ASSIGN 
         DPTarget.DPId        = DiscountPlan.DPId
         DPTarget.ValidFrom   = TODAY
         DPTarget.ValidTo     = 12/31/2018
         DPTarget.TargetTable = "BillItem"
         DPTarget.TargetKey   = ENTRY(liCont, ilcDPTargetsList)
         DPTarget.Included    = YES.
   END.      

   olcResult = "Created discount " + ilcDPRule + " --> OK".

   RELEASE DiscountPlan.
 
END.   

/* CONT_DISC_TB_20 */
ASSIGN
   lcDPSubjects = 
      "CONTDSLTB59,CONTFHTB59_50,CONTFHTB69_300,CONTFHTB89_1000,CONTFHNBTB69_300"
/*
   lcDPTargets = 
      "CONT35MF,"                                          +
      "CONTDSLMF,CONTFH50MF,CONTFH300MF,CONTFH1000MF,"     + 
      "CONTDSLTB59PRO,CONTFHTB59_50PRO,CONTFHTB69_300PRO," + 
      "CONTFHTB89_1000PRO,CONTFHNBTB69_300PRO".
*/
   lcDPTargets = "CONT35MF". 

RUN pCreateDiscount (INPUT "CONT_DISC_TB_20",
                     INPUT "Descuento Final Try&Buy - Convergente",
                     INPUT "Fixed",                     
                     INPUT 16.529,
                     INPUT 0,
                     INPUT lcDPSubjects,
                     INPUT lcDPTargets,
                     INPUT "Tactical",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

MESSAGE lcInfo VIEW-AS ALERT-BOX.
