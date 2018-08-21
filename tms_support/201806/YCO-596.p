/* YCO-596.p                             */
/* Discounts allowed for Try&Buy tariffs */
/* /tms_support/201806/YCO-596.p         */
/* jotorres. 25.06.2018                  */      
 
DEFINE VARIABLE lcOldClitypes AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcNewClitypes AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCount       AS INTEGER   NO-UNDO.
 
DEFINE BUFFER bDPSubject FOR DPSubject.
DEFINE BUFFER bDPTarget  FOR DPTarget.
 
ASSIGN 
   lcOldClitypes = "CONTDSL59,CONTFH59_50,CONTFH69_300,CONTFH89_1000,CONTFHNB69_300"
   lcNewCliTypes = "CONTDSLTB59,CONTFHTB59_50,CONTFHTB69_300,CONTFHTB89_1000,CONTFHNBTB69_300".
 
DO liCount = 1 TO NUM-ENTRIES(lcOldClitypes):
   
   /* DPSubjects */
   FOR EACH bDPSubject NO-LOCK WHERE
            bDPSubject.DPSubject EQ ENTRY(liCount, lcOldClitypes) AND 
            bDPSubject.ValidTo   EQ 12/31/2049:
 
      IF NOT CAN-FIND(FIRST DPSubject WHERE 
                            DPSubject.DPId       EQ bDPSubject.DPId AND            
                            DPSubject.DPSubject  EQ ENTRY(liCount, lcNewCliTypes))
      THEN DO:
         CREATE DPSubject.
         ASSIGN
            DPSubject.DPId       = bDPSubject.DPId                 
            DPSubject.DPSubject  = ENTRY(liCount, lcNewCliTypes)                 
            DPSubject.ValidFrom  = TODAY                  
            DPSubject.ValidTo    = 12/31/2049.        
      END.                      
   END.       
   
   /* DPTargets */
   FOR EACH bDPTarget NO-LOCK WHERE 
            bDPTarget.TargetTable EQ "BillItem" AND  
            bDPTarget.TargetKey   EQ ENTRY(liCount, lcOldClitypes) + "PRO":
               
      IF NOT CAN-FIND(FIRST DPTarget WHERE
                            DPTarget.DPId        EQ bDPTarget.DPId AND
                            DPTarget.TargetTable EQ "BillItem"     AND    
                            DPTarget.TargetKey   EQ (ENTRY(liCount, lcNewCliTypes) + "PRO"))                     
      THEN DO:                 
         CREATE DPTarget.
         ASSIGN 
            DPTarget.DPId        = bDPTarget.DPId
            DPTarget.ValidFrom   = TODAY 
            DPTarget.ValidTo     = 12/31/2049
            DPTarget.TargetTable = "BillItem" 
            DPTarget.TargetKey   = ENTRY(liCount, lcNewCliTypes) + "PRO".
      END.
   END.                
END.    
