{commali.i}

DEFINE TEMP-TABLE ttDuration NO-UNDO 
   FIELD CallCase     AS CHARACTER
   FIELD BDest        AS CHARACTER
   FIELD FromDate     AS DATE
   FIELD ToDate       AS DATE
   FIELD FirstLimit   AS INTEGER
   FIELD FLimitCycle  AS INTEGER
   FIELD SLimitCycle  AS INTEGER
   INDEX CallCase CallCase BDest.
   
CREATE ttDuration.
ASSIGN 
   ttDuration.CallCase    = "CALL"
   ttDuration.FirstLimit  = 0
   ttDuration.FLimitCycle = 60
   ttDuration.SLimitCycle = 30
   ttDuration.FromDate    = 1/1/06
   ttDuration.ToDate      = 12/31/49.

FOR EACH BDestConf NO-LOCK WHERE
         BDestConf.Brand     = gcBrand AND
         BDestConf.GroupType = 1,
    EACH BDestConfItem NO-LOCK WHERE
         BDestConfItem.Brand = gcBrand AND
         BDestConfItem.BDCGroup = BDestConf.BDCGroup:
         
   CREATE ttDuration.
   ASSIGN 
      ttDuration.CallCase    = STRING(BDestConfItem.RateCCN)
      ttDuration.BDest       = BDestConfItem.BDest
      ttDuration.FromDate    = MAX(BDestConf.FromDate,BDestConfItem.FromDate)
      ttDuration.ToDate      = MIN(BDestConf.ToDate,BDestConfItem.ToDate)
      ttDuration.FirstLimit  = BDestConf.ConfigValue1
      ttDuration.FLimitCycle = 0
      ttDuration.SLimitCycle = 1.
      
   IF ttDuration.FromDate = ? OR ttDuration.ToDate = ? THEN 
      DELETE ttDuration.
END.     


