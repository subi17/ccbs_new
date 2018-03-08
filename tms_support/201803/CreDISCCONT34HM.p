/* ----------------------------------------------------------------------
  MODULE .......: CONT34DISC.p
  TASK .........: Creating Discount plan for CONT34
  APPLICATION ..: tms
  AUTHOR .......:  
  CREATED ......:  
  ---------------------------------------------------------------------- */

DEFINE VARIABLE mySequence AS INTEGER NO-UNDO INITIAL 1.

FIND LAST DiscountPlan USE-INDEX DPId EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE DiscountPlan THEN 
    mySequence = DiscountPlan.DPId + 1.

CREATE DiscountPlan.
   ASSIGN
   DiscountPlan.Brand          = "1"                     
   DiscountPlan.DPId           = mySequence
   DiscountPlan.Subject        = "Contract Target"
   DiscountPlan.TargetType     = "List"
   DiscountPlan.DPUnit         ="Percentage"
   DiscountPlan.Priority       = 1
   DiscountPlan.ProcessStopper = no
   DiscountPlan.DPCurrency     = ""         
   DiscountPlan.BillCode       = "DISCCONT34HM"
   DiscountPlan.ValidFrom      = 03/07/2018
   DiscountPlan.ValidTo        = 12/31/2049
   DiscountPlan.DPRuleId       = "DISCCONT34HM"
   DiscountPlan.DPName         = "Mobile only - Additional Line CONT34 50%"
   DiscountPlan.DPMemo         = "" 
   DiscountPlan.SubjectType    = "List"
   DiscountPlan.MaxAmount      = 0 
   DiscountPlan.MinBaseAmount  = 0 
   DiscountPlan.CCDisplay      = 1 
   DiscountPlan.ValidPeriods   = 0.

CREATE DPRate.
ASSIGN
   DPRate.DPId      = DiscountPlan.DPId 
   DPRate.DiscValue = 50
   DPRate.ValidFrom = 03/07/2018 
   DPRate.ValidTo   = 12/31/2049.

CREATE DPSubject.
ASSIGN
   DPSubject.DPId       = DiscountPlan.DPId                 
   DPSubject.DPSubject  = "CONT34"                 
   DPSubject.ValidFrom  = 03/07/2018                  
   DPSubject.ValidTo    = 12/31/2049.                   

CREATE DPTarget.
ASSIGN
   DPTarget.DPId        = DiscountPlan.DPId
   DPTarget.ValidFrom   = 03/07/2018
   DPTarget.ValidTo     = 12/31/2049
   DPTarget.TargetTable = "BillItem"
   DPTarget.TargetKey   = "CONT34MF"
   DPTarget.Included    = yes.

disp DiscountPlan.DPId.