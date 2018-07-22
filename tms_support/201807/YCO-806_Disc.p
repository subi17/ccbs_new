/*----------------------------------------- 
YCO-806. New discounts BackToSchool 2018.
/tms_support/201807/YCO-806_Disc.p 
jotorres 22.07.2018
-----------------------------------------*/

DEFINE VARIABLE lcDPSubjects AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDPTargets  AS CHARACTER NO-UNDO.    
DEFINE VARIABLE lcResult     AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInfo       AS CHARACTER NO-UNDO.

PROCEDURE pCreateDiscount:
  
   /* Parameters */
   DEFINE INPUT  PARAMETER icDPRuleId       AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER icDPName         AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER icDPUnit         AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER idDiscValue      AS DECIMAL   NO-UNDO.
   DEFINE INPUT  PARAMETER iiValidPeriods   AS INTEGER   NO-UNDO.
   DEFINE INPUT  PARAMETER icSubjectType    AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER icDPSubjectsList AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER icTargetType     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER icDPTargetsList  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER icDPCategory     AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER ocResult         AS CHARACTER NO-UNDO.
        
   DEFINE VARIABLE liCont     AS INTEGER NO-UNDO.
   DEFINE VARIABLE liSequence AS INTEGER NO-UNDO.

   /* Checking if DiscountPlan already exists. */
   IF CAN-FIND(FIRST DiscountPlan WHERE 
                     DiscountPlan.Brand    EQ Syst.Var:gcBrand AND 
                     DiscountPlan.DPRuleId EQ icDPRuleId) THEN DO:
      ocResult = "Discount " + icDPRuleId + " already exists --> WARNING".
      RETURN.
   END.

   /* Getting next sequence for DiscountPlan. */
   liSequence = 1.
   FIND LAST DiscountPlan EXCLUSIVE-LOCK USE-INDEX DPId NO-ERROR.
   IF AVAILABLE DiscountPlan THEN 
      liSequence = DiscountPlan.DPId + 1.
         
   CREATE DiscountPlan.
   ASSIGN
      DiscountPlan.Brand          = Syst.Var:gcBrand                       
      DiscountPlan.DPId           = liSequence                        
      DiscountPlan.Subject        = "Contract Target"          
      DiscountPlan.TargetType     = icTargetType            
      DiscountPlan.DPUnit         = icDPUnit            
      DiscountPlan.Priority       = 1          
      DiscountPlan.ProcessStopper = FALSE             
      DiscountPlan.DPCurrency     = ""          
      DiscountPlan.BillCode       = icDPRuleId           
      DiscountPlan.ValidFrom      = TODAY              
      DiscountPlan.ValidTo        = 12/31/2049             
      DiscountPlan.DPRuleId       = icDPRuleId             
      DiscountPlan.DPName         = icDPName             
      DiscountPlan.DPMemo         = ""               
      DiscountPlan.SubjectType    = icSubjectType             
      DiscountPlan.MaxAmount      = 0             
      DiscountPlan.MinBaseAmount  = 0             
      DiscountPlan.CCDisplay      = 1             
      DiscountPlan.ValidPeriods   = iiValidPeriods             
      DiscountPlan.DPCategory     = icDPCategory.

   /* DPRate */
   CREATE DPRate.
   ASSIGN
      DPRate.DPId      = DiscountPlan.DPId 
      DPRate.DiscValue = idDiscValue
      DPRate.ValidFrom = TODAY 
      DPRate.ValidTo   = 12/31/2049. 
  
   /* DPSubjects. (CliTypes to assign the discount) */
   DO liCont = 1 TO NUM-ENTRIES(icDPSubjectsList):
      CREATE DPSubject.
      ASSIGN
         DPSubject.DPId       = DiscountPlan.DPId                 
         DPSubject.DPSubject  = ENTRY(liCont, icDPSubjectsList)                 
         DPSubject.ValidFrom  = TODAY                  
         DPSubject.ValidTo    = 12/31/2049. 
   END.
   
   /* DPTargets. (BillItems to apply the discount) */
   DO liCont = 1 TO NUM-ENTRIES(icDPTargetsList):
      CREATE DPTarget.
      ASSIGN 
         DPTarget.DPId        = DiscountPlan.DPId
         DPTarget.ValidFrom   = TODAY
         DPTarget.ValidTo     = 12/31/2049
         DPTarget.TargetTable = "BillItem"
         DPTarget.TargetKey   = ENTRY(liCont, icDPTargetsList)
         DPTarget.Included    = YES.
   END.      

   ocResult = "Created discount " + icDPRuleId + " --> OK".

   RELEASE DiscountPlan.
 
END.   

/* CONT_DISC_BackToSchool_M --------------------------------------------- */

/* CliTypes allowed for the discount. */
ASSIGN lcDPSubjects = 
   "CONT,CONT2,CONT4,CONT5,CONT6,CONT7,CONT8,CONT9,CONT10,CONT15,CONT23,CONT24," +
   "CONT25,CONT26,CONT27,CONT31,CONT32,CONT33,CONT34,CONTM,CONTF10,CONTF20,"     +
   "CONTF20D,CONTF30,CONTF40,CONTF55,CONTF8,CONTF11,CONTS12,CONTS15,CONTS16,"    +
   "CONTS20,CONTS21,CONTS25,CONTS26,CONTS30,CONTS32,CONTS35,CONTS39".

/* All Monthly fees to apply the discount (monthly fee for mobile, for fixed and for PRO) */
ASSIGN lcDPTargets = "". 

RUN pCreateDiscount (INPUT "CONT_DISC_BackToSchool_M",           /* DPRuleId                                    */
                     INPUT "Descuento Vuelta al Cole  - Móvil",  /* DPName                                      */
                     INPUT "Percentage",                         /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 20,                                   /* DiscValue                                   */
                     INPUT 6,                                    /* Valid Periods                               */  
                     INPUT "List",                               /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                         /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "All",                                /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                          /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Promotion",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).


/* CONT_DISC_BackToSchool --------------------------------------------- */

/* CliTypes allowed for the discount. */
ASSIGN lcDPSubjects = 
   "CONTDSL39,CONTFH39_50,CONTFH49_300,CONTFH69_1000,CONTDSL48,CONTFH48_50,"                          +
   "CONTFH58_300,CONTFH76_1000,CONTDSL3G,CONTFH3G_50,CONTFH3G_300,CONTFH3G_1000,"                     +
   "CONTDSL52,CONTFH52_50,CONTFH62_300,CONTFH82_1000,CONTDSL7G,CONTFH7G_50,CONTFH7G_300,"             +
   "CONTFH7G_1000,CONTDSL2G,CONTFH2G_50,CONTFH2G_300,CONTFH2G_1000,CONTDSL59,CONTFH59_50,"            +
   "CONTFH69_300,CONTFH89_1000,CONTDSL35,CONTFH35_50,CONTFH45_300,CONTFH65_1000,CONTFHNB62_300,"      +
   "CONTFHNB3G_300,CONTFHNB7G_300,CONTFHNB69_300,CONTFHNB58_300,CONTFHNB2G_300,CONTFHNB109_300,"      +
   "CONTFHNB45_300,CONTFH129_1000,CONTFH109_300,CONTFH99_50,CONTDSL99,CONTFHTB89_1000,"               +
   "CONTFHTB69_300,CONTFHNBTB69_300,CONTFHTB59_50,CONTDSLTB59".

/* All Monthly fees to apply the discount (monthly fee for mobile, for fixed and for PRO) */
ASSIGN lcDPTargets = "". 

RUN pCreateDiscount (INPUT "CONT_DISC_BackToSchool",                /* DPRuleId                                    */
                     INPUT "Descuento Vuelta al Cole  - Convergente", /* DPName                                      */
                     INPUT "Percentage",                              /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 20,                                        /* DiscValue                                   */
                     INPUT 6,                                         /* Valid Periods                               */  
                     INPUT "List",                                    /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                              /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "All",                                     /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                               /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Promotion",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

MESSAGE lcInfo VIEW-AS ALERT-BOX.
