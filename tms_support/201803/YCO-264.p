/* YCO-264: Changing PRO prices for "La Combinada Interminable"/"La Sinfín Infinitos GB" */

FIND FIRST FMItem WHERE
           FMItem.Brand     = "1"             AND
           FMItem.feemodel  = "CONTDSLMF"     AND           
           FMItem.pricelist = "PRO_CONTDSL99" AND           
           FMItem.billcode  = "CONTDSL99PRO"  AND
           FMItem.ToDate    = 12/31/49 
     EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE FMItem THEN 
   ASSIGN FMItem.ToDate = (TODAY - 1).
              
CREATE FMItem.
ASSIGN
   FMItem.Amount            = 8.19
   FMItem.BillCode          = "CONTDSL99PRO"
   FMItem.BillCycle         = 2
   FMItem.BillMethod        = FALSE
   FMItem.BillType          = "MF"
   FMItem.Brand             = "1"
   FMItem.BrokenRental      = 1
   FMItem.FeeModel          = "CONTDSLMF"
   FMItem.FromDate          = TODAY
   FMItem.Interval          = 1
   FMItem.PriceList         = "PRO_CONTDSL99"
   FMItem.ServiceLimitGroup = ""
   FMItem.ToDate            = 12/31/49.
 
/*----------------------------------*/ 
 FIND FIRST FMItem WHERE
           FMItem.Brand     = "1"               AND
           FMItem.feemodel  = "CONTFH50MF"      AND           
           FMItem.pricelist = "PRO_CONTFH99_50" AND           
           FMItem.billcode  = "CONTFH99_50PRO"  AND
           FMItem.ToDate    = 12/31/49 
     EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE FMItem THEN 
   ASSIGN FMItem.ToDate = (TODAY - 1).
              
CREATE FMItem.
ASSIGN
   FMItem.Amount            = 8.19
   FMItem.BillCode          = "CONTFH99_50PRO"
   FMItem.BillCycle         = 2
   FMItem.BillMethod        = FALSE
   FMItem.BillType          = "MF"
   FMItem.Brand             = "1"
   FMItem.BrokenRental      = 1
   FMItem.FeeModel          = "CONTFH50MF"
   FMItem.FromDate          = TODAY
   FMItem.Interval          = 1
   FMItem.PriceList         = "PRO_CONTFH99_50"
   FMItem.ServiceLimitGroup = ""
   FMItem.ToDate            = 12/31/49.
/*----------------------------------*/ 
 FIND FIRST FMItem WHERE
           FMItem.Brand     = "1"                 AND
           FMItem.feemodel  = "CONTFH300MF"       AND           
           FMItem.pricelist = "PRO_CONTFH109_300" AND           
           FMItem.billcode  = "CONTFH109_300PRO"  AND
           FMItem.ToDate    = 12/31/49 
     EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE FMItem THEN 
   ASSIGN FMItem.ToDate = (TODAY - 1).
              
CREATE FMItem.
ASSIGN
   FMItem.Amount            = 8.92
   FMItem.BillCode          = "CONTFH109_300PRO"
   FMItem.BillCycle         = 2
   FMItem.BillMethod        = FALSE
   FMItem.BillType          = "MF"
   FMItem.Brand             = "1"
   FMItem.BrokenRental      = 1
   FMItem.FeeModel          = "CONTFH300MF"
   FMItem.FromDate          = TODAY
   FMItem.Interval          = 1
   FMItem.PriceList         = "PRO_CONTFH109_300"
   FMItem.ServiceLimitGroup = ""
   FMItem.ToDate            = 12/31/49.
/*----------------------------------*/ 
 FIND FIRST FMItem WHERE
           FMItem.Brand     = "1"                  AND
           FMItem.feemodel  = "CONTFH1000MF"       AND           
           FMItem.pricelist = "PRO_CONTFH129_1000" AND           
           FMItem.billcode  = "CONTFH129_1000PRO"  AND
           FMItem.ToDate    = 12/31/49 
     EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE FMItem THEN 
   ASSIGN FMItem.ToDate = (TODAY - 1).
              
CREATE FMItem.
ASSIGN
   FMItem.Amount            = 9.4
   FMItem.BillCode          = "CONTFH129_1000PRO"
   FMItem.BillCycle         = 2
   FMItem.BillMethod        = FALSE
   FMItem.BillType          = "MF"
   FMItem.Brand             = "1"
   FMItem.BrokenRental      = 1
   FMItem.FeeModel          = "CONTFH1000MF"
   FMItem.FromDate          = TODAY
   FMItem.Interval          = 1
   FMItem.PriceList         = "PRO_CONTFH129_1000"
   FMItem.ServiceLimitGroup = ""
   FMItem.ToDate            = 12/31/49.

