/* ----------------------------------------------------------------------
  MODULE .......: CONT31DISC.p
  TASK .........: Creating Discount plan for Watch Tariff
  APPLICATION ..: tms
  AUTHOR .......: jotorres
  CREATED ......: 14.12.2017
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
   DiscountPlan.DPUnit         ="Percenta"
   DiscountPlan.Priority       = 1
   DiscountPlan.ProcessStopper = no
   DiscountPlan.DPCurrency     = ""         
   DiscountPlan.BillCode       = "CONT31DISC"
   DiscountPlan.ValidFrom      = 12/18/2017
   DiscountPlan.ValidTo        = 12/31/2049
   DiscountPlan.DPRuleId       = "CONT31DISC"
   DiscountPlan.DPName         = "Descuento promoción"
   DiscountPlan.DPMemo         = "" 
   DiscountPlan.SubjectType    = "List"
   DiscountPlan.MaxAmount      = 0 
   DiscountPlan.MinBaseAmount  = 0 
   DiscountPlan.CCDisplay      = 0 
   DiscountPlan.ValidPeriods   = 0.

CREATE DPRate.
ASSIGN
   DPRate.DPId      = DiscountPlan.DPId 
   DPRate.DiscValue = 0
   DPRate.ValidFrom = 12/18/2017 
   DPRate.ValidTo   = 12/31/2049.

CREATE DPSubject.
ASSIGN
   DPSubject.DPId       = DiscountPlan.DPId                 
   DPSubject.DPSubject  = "CONT31"                 
   DPSubject.ValidFrom  = 12/18/2017                  
   DPSubject.ValidTo    = 12/31/2049.                   

CREATE DPTarget.
ASSIGN
   DPTarget.DPId        = DiscountPlan.DPId
   DPTarget.ValidFrom   = 12/18/2017
   DPTarget.ValidTo     = 12/31/2049
   DPTarget.TargetTable = "BillItem"
   DPTarget.TargetKey   = "CONT31MF"
   DPTarget.Included    = yes.
