/*-------------------------------------
YCO-1092. Creating 4 new discounts.
/tms_support/201808/YCO-1092_discount.p
jotorres 22.08.2018
-------------------------------------*/

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
      DiscountPlan.CCDisplay      = 0    /* ¡¡CHANGED!! */         
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

/* ---------------- MAIN ---------------- */

/* PRODISC2_12 --------------------------*/

/* CliTypes allowed for the discount. */
ASSIGN lcDPSubjects =  "CONTDSL52,CONTFH52_50".

/* All Monthly fees to apply the discount (monthly fee for mobile, for fixed and for PRO) */
ASSIGN lcDPTargets = "CONTDSL52PRO,CONTFH52_50PRO". 

RUN pCreateDiscount (INPUT "PRODISC2_12",                     /* DPRuleId                                    */
                     INPUT "2 euros discount 12 months PRO",  /* DPName                                      */
                     INPUT "Fixed",                           /* DPUnit ["Percentage"|"Fixed"]               */
                     INPUT 2,                                 /* DiscValue                                   */
                     INPUT 12,                                /* Valid Periods                               */  
                     INPUT "List",                            /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                      /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                            /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                       /* DPTargets (empty when TargetType = "All")   */
                     INPUT "",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

 /* PRODISC3_12 --------------------------*/

/* CliTypes allowed for the discount. */
ASSIGN lcDPSubjects =  "CONTFH62_300,CONTFHNB62_300,CONTFH82_1000".

/* All Monthly fees to apply the discount (monthly fee for mobile, for fixed and for PRO) */
ASSIGN lcDPTargets = "CONTFH62_300PRO,CONTFHNB62_300PRO,CONTFH82_1000PRO". 

RUN pCreateDiscount (INPUT "PRODISC3_12",                     /* DPRuleId                                    */
                     INPUT "3 euros discount 12 months PRO",  /* DPName                                      */
                     INPUT "Fixed",                           /* DPUnit ["Percentage"|"Fixed"]               */
                     INPUT 3,                                 /* DiscValue                                   */
                     INPUT 12,                                /* Valid Periods                               */  
                     INPUT "List",                            /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                      /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                            /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                       /* DPTargets (empty when TargetType = "All")   */
                     INPUT "",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

/* PRODISC4_12 --------------------------*/

/* CliTypes allowed for the discount. */
ASSIGN lcDPSubjects =  "CONTDSL99,CONTFH99_50,CONTDSL59,CONTDSLTB59,CONTFH59_50,CONTFHTB59_50".

/* All Monthly fees to apply the discount (monthly fee for mobile, for fixed and for PRO) */
ASSIGN lcDPTargets = "CONTDSL99PRO,CONTFH99_50PRO,CONTDSL59PRO,CONTDSLTB59PRO,CONTFH59_50PRO,CONTFHTB59_50PRO". 

RUN pCreateDiscount (INPUT "PRODISC4_12",                     /* DPRuleId                                    */
                     INPUT "4 euros discount 12 months PRO",  /* DPName                                      */
                     INPUT "Fixed",                           /* DPUnit ["Percentage"|"Fixed"]               */
                     INPUT 4,                                 /* DiscValue                                   */
                     INPUT 12,                                /* Valid Periods                               */  
                     INPUT "List",                            /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                      /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                            /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                       /* DPTargets (empty when TargetType = "All")   */
                     INPUT "",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

/* PRODISC5_12 --------------------------*/

/* CliTypes allowed for the discount. */
ASSIGN lcDPSubjects = "CONTFH69_300,CONTFHTB69_300,CONTFHNB69_300,CONTFHNBTB69_300,CONTFH89_1000,CONTFHTB89_1000,CONTFH109_300,CONTFHNB109_300,CONTFH129_1000".

/* All Monthly fees to apply the discount (monthly fee for mobile, for fixed and for PRO) */
ASSIGN lcDPTargets = "CONTFH69_300PRO,CONTFHTB69_300PRO,CONTFHNB69_300PRO,CONTFHNBTB69_300PRO,CONTFH89_1000PRO,CONTFHTB89_1000PRO,CONTFH109_300PRO,CONTFHNB109_300PRO,CONTFH129_1000PRO". 

RUN pCreateDiscount (INPUT "PRODISC5_12",                     /* DPRuleId                                    */
                     INPUT "5 euros discount 12 months PRO",  /* DPName                                      */
                     INPUT "Fixed",                           /* DPUnit ["Percentage"|"Fixed"]               */
                     INPUT 5,                                 /* DiscValue                                   */
                     INPUT 12,                                /* Valid Periods                               */  
                     INPUT "List",                            /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                      /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                            /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                       /* DPTargets (empty when TargetType = "All")   */
                     INPUT "",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

MESSAGE lcInfo VIEW-AS ALERT-BOX.
