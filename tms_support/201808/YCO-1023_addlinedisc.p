/*------------------------------------------------------------------- 
YCO-1023. Additional line discounts for new Mobile Only PRO tariffs.
/tms_support/201808/YCO-1023_addlinedisc.p 
jotorres 20.08.2018
--------------------------------------------------------------------*/

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

/*--------*/
/* CONT36 */
/*--------*/

/* CliTypes allowed for the discount. */
ASSIGN lcDPSubjects = "CONT36".

/* All Monthly fees to apply the discount (monthly fee for mobile, for fixed and for PRO) */
ASSIGN lcDPTargets = "CONT36MF". 

RUN pCreateDiscount (INPUT "DISCCONT36",                   /* DPRuleId                                    */
                     INPUT "Additional Line CONT36 disc", /* DPName                                      */
                     INPUT "Percentage",                  /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 20,                            /* DiscValue                                   */
                     INPUT 0,                             /* Valid Periods                               */  
                     INPUT "List",                        /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                  /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                        /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                   /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Additional line",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

RUN pCreateDiscount (INPUT "DISCCONT36H",                                   /* DPRuleId                                    */
                     INPUT "Convergent - Additional Line CONT36 disc 50%", /* DPName                                      */
                     INPUT "Percentage",                                   /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 50,                                             /* DiscValue                                   */
                     INPUT 0,                                              /* Valid Periods                               */  
                     INPUT "List",                                         /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                                   /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                                         /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                                    /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Additional line",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

RUN pCreateDiscount (INPUT "DISCCONT36HM",                                   /* DPRuleId                                    */
                     INPUT "Mobile only - Additional Line CONT36 disc 50%", /* DPName                                      */
                     INPUT "Percentage",                                   /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 50,                                             /* DiscValue                                   */
                     INPUT 0,                                              /* Valid Periods                               */  
                     INPUT "List",                                         /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                                   /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                                         /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                                    /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Additional line",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

/*--------*/
/* CONT37 */
/*--------*/

/* CliTypes allowed for the discount. */
ASSIGN lcDPSubjects = "CONT37".

/* All Monthly fees to apply the discount (monthly fee for mobile, for fixed and for PRO) */
ASSIGN lcDPTargets = "CONT37MF". 

RUN pCreateDiscount (INPUT "DISCCONT37",                   /* DPRuleId                                    */
                     INPUT "Additional Line CONT37 disc", /* DPName                                      */
                     INPUT "Percentage",                  /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 20,                            /* DiscValue                                   */
                     INPUT 0,                             /* Valid Periods                               */  
                     INPUT "List",                        /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                  /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                        /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                   /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Additional line",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

RUN pCreateDiscount (INPUT "DISCCONT37H",                                   /* DPRuleId                                    */
                     INPUT "Convergent - Additional Line CONT37 disc 50%", /* DPName                                      */
                     INPUT "Percentage",                                   /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 50,                                             /* DiscValue                                   */
                     INPUT 0,                                              /* Valid Periods                               */  
                     INPUT "List",                                         /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                                   /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                                         /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                                    /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Additional line",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

RUN pCreateDiscount (INPUT "DISCCONT37HM",                                   /* DPRuleId                                    */
                     INPUT "Mobile only - Additional Line CONT37 disc 50%", /* DPName                                      */
                     INPUT "Percentage",                                   /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 50,                                             /* DiscValue                                   */
                     INPUT 0,                                              /* Valid Periods                               */  
                     INPUT "List",                                         /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                                   /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                                         /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                                    /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Additional line",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

/*--------*/
/* CONT38 */
/*--------*/

/* CliTypes allowed for the discount. */
ASSIGN lcDPSubjects = "CONT38".

/* All Monthly fees to apply the discount (monthly fee for mobile, for fixed and for PRO) */
ASSIGN lcDPTargets = "CONT38MF". 

RUN pCreateDiscount (INPUT "DISCCONT38",                   /* DPRuleId                                    */
                     INPUT "Additional Line CONT38 disc", /* DPName                                      */
                     INPUT "Percentage",                  /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 20,                            /* DiscValue                                   */
                     INPUT 0,                             /* Valid Periods                               */  
                     INPUT "List",                        /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                  /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                        /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                   /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Additional line",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

RUN pCreateDiscount (INPUT "DISCCONT38H",                                   /* DPRuleId                                    */
                     INPUT "Convergent - Additional Line CONT38 disc 50%", /* DPName                                      */
                     INPUT "Percentage",                                   /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 50,                                             /* DiscValue                                   */
                     INPUT 0,                                              /* Valid Periods                               */  
                     INPUT "List",                                         /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                                   /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                                         /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                                    /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Additional line",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

RUN pCreateDiscount (INPUT "DISCCONT38HM",                                   /* DPRuleId                                    */
                     INPUT "Mobile only - Additional Line CONT38 disc 50%", /* DPName                                      */
                     INPUT "Percentage",                                   /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 50,                                             /* DiscValue                                   */
                     INPUT 0,                                              /* Valid Periods                               */  
                     INPUT "List",                                         /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                                   /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                                         /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                                    /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Additional line",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

/*--------*/
/* CONT39 */
/*--------*/

/* CliTypes allowed for the discount. */
ASSIGN lcDPSubjects = "CONT39".

/* All Monthly fees to apply the discount (monthly fee for mobile, for fixed and for PRO) */
ASSIGN lcDPTargets = "CONT39MF". 

RUN pCreateDiscount (INPUT "DISCCONT39",                   /* DPRuleId                                    */
                     INPUT "Additional Line CONT39 disc", /* DPName                                      */
                     INPUT "Percentage",                  /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 20,                            /* DiscValue                                   */
                     INPUT 0,                             /* Valid Periods                               */  
                     INPUT "List",                        /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                  /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                        /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                   /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Additional line",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

RUN pCreateDiscount (INPUT "DISCCONT39H",                                   /* DPRuleId                                    */
                     INPUT "Convergent - Additional Line CONT39 disc 50%", /* DPName                                      */
                     INPUT "Percentage",                                   /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 50,                                             /* DiscValue                                   */
                     INPUT 0,                                              /* Valid Periods                               */  
                     INPUT "List",                                         /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                                   /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                                         /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                                    /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Additional line",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

RUN pCreateDiscount (INPUT "DISCCONT39HM",                                   /* DPRuleId                                    */
                     INPUT "Mobile only - Additional Line CONT39 disc 50%", /* DPName                                      */
                     INPUT "Percentage",                                   /* DPUnit ["Percentage"|"Fix"]                 */
                     INPUT 50,                                             /* DiscValue                                   */
                     INPUT 0,                                              /* Valid Periods                               */  
                     INPUT "List",                                         /* SubjectType ["All"|"List"]                  */ 
                     INPUT lcDPSubjects,                                   /* DPSubjects (empty when SubjectType = "All") */
                     INPUT "List",                                         /* TargetType ["All"|"List"]                   */ 
                     INPUT lcDPTargets,                                    /* DPTargets (empty when TargetType = "All")   */
                     INPUT "Additional line",                     
                     OUTPUT lcResult).
lcInfo = lcInfo + lcResult + CHR(10).

MESSAGE lcInfo VIEW-AS ALERT-BOX.
