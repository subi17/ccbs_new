/* YCO-310. Creating Periodical Contract - DayCampaign */
/* FTERM12-140 / FTERMPERIOD140 */

FIND FIRST DayCampaign NO-LOCK WHERE 
           DayCampaign.Brand   EQ "1" AND
           DayCampaign.DCEvent EQ "FTERM12-140" 
           NO-ERROR.
IF NOT AVAILABLE DayCampaign THEN DO:
   CREATE DayCampaign.
   ASSIGN
      DayCampaign.DCEvent         = "FTERM12-140"                  
      DayCampaign.ValidFrom       = TODAY      
      DayCampaign.InclUnit        = 0       
      DayCampaign.ValidTo         = 12/31/49       
      DayCampaign.BillCode        = ""      
      DayCampaign.MaxChargeIncl   = 0        
      DayCampaign.DCTarget        = ""    
      DayCampaign.Weekday         = ""        
      DayCampaign.DCName          = "FTERM12-140 fixed line permanency"      
      DayCampaign.CCN             = 0        
      DayCampaign.MaxChargeExcl   = 0          
      DayCampaign.Brand           = "1"     
      DayCampaign.DCType          = "3"      
      DayCampaign.CalcMethod      = 1        
      DayCampaign.DurType         = 3       
      DayCampaign.DurMonths       = 12      
      DayCampaign.Renewal         = 0     
      DayCampaign.FeeModel        = ""        
      DayCampaign.ModifyFeeModel  = ""    
      DayCampaign.TermFeeModel    = "FTERMPERIOD140"        
      DayCampaign.DurUnit         = 0        
      DayCampaign.Effective       = 1            
      DayCampaign.TermFeeCalc     = 2             
      DayCampaign.InclStartCharge = YES          
      DayCampaign.InstanceLimit   = 1            
      DayCampaign.FirstMonthCalc  = 0            
      DayCampaign.LastMonthCalc   = 0            
      DayCampaign.StatusCode      = 1            
      DayCampaign.BundleUpsell    = ""         
      DayCampaign.DSSPriority     = 0           
      DayCampaign.PayType         = 0            
      DayCampaign.BundleTarget    = 0. 
       
   CREATE FeeModel.
   ASSIGN
      FeeModel.FeeModel = "FTERMPERIOD140"
      FeeModel.FeeName  = "Fixed line contract termination"
      FeeModel.Brand    = "1" 
      FeeModel.FMGroup  = 0.

   CREATE FMItem.
   ASSIGN
      FMItem.FeeModel            = "FTERMPERIOD140"                   
      FMItem.PriceList           = "COMMON"         
      FMItem.BillCode            = "FTERMPERIOD"
      FMItem.BillMethod          = YES 
      FMItem.Interval            = 0      
      FMItem.Amount              = 140.66
      FMItem.BillType            = "PF"      
      FMItem.BillCycle           = 1    
      FMItem.FFItemQty           = 0     
      FMItem.FFEndDate           = ?   
      FMItem.InclAmt             = 0       
      FMItem.FromDate            = TODAY      
      FMItem.ToDate              = 12/31/49         
      FMItem.InclBillCode        = ""     
      FMItem.InclUnit            = 0       
      FMItem.ServiceLimitGroup   = ""      
      FMItem.Brand               = "1"         
      FMItem.BrokenRental        = 1         
      FMItem.FirstMonthBR        = 0.   
END.

    
