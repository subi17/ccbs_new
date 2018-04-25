/* YTS-12791. Updating Discount Plans - DPTargets  */

DEF VAR lcDiscountList AS CHAR NO-UNDO INITIAL "CONVDISC,CONVDISC_CS10M12,CONVDISC_CS20M12,DISCWINBACK".
DEF VAR lcBillItemList AS CHAR NO-UNDO INITIAL "CONT32MF,CONT33MF,CONT34MF".

DEF VAR lii AS INT NO-UNDO.
DEF VAR lij AS INT NO-UNDO.

DO lii = 1 TO NUM-ENTRIES(lcDiscountList):
   FIND DiscountPlan NO-LOCK WHERE 
        DiscountPlan.Brand    EQ "1"                        AND 
        DiscountPlan.DPRuleId EQ ENTRY(lii, lcDiscountList) AND 
        DiscountPlan.ValidTo  EQ 12/31/49 
        NO-ERROR.
   IF AVAILABLE DiscountPlan THEN 
      DO lij = 1 TO NUM-ENTRIES(lcBillItemList):     
         FIND DPTarget NO-LOCK WHERE
              DPTarget.DPId        EQ DiscountPlan.DPId          AND 
              DPTarget.TargetTable EQ "BillItem"                 AND 
              DPTarget.TargetKey   EQ ENTRY(lij, lcBillItemList) AND  
              DPTarget.ValidTo     EQ 12/31/49
              NO-ERROR.
         IF NOT AVAILABLE DPTarget THEN DO:
            CREATE DPTarget.
            ASSIGN
               DPTarget.DPId        =  DiscountPlan.DPId      
               DPTarget.ValidFrom   = TODAY  
               DPTarget.ValidTo     = 12/31/49
               DPTarget.TargetTable = "BillItem"    
               DPTarget.TargetKey   = ENTRY(lij, lcBillItemList)       
               DPTarget.Included    = YES.     
            END.                                   
      END.                  
END.     
        
