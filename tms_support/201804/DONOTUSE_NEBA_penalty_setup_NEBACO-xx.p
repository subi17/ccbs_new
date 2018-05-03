/* YCO-310. Creating Periodical Contract - DayCampaign */
/* FTERM12-140 / FTERMPERIOD140 */

fPermanency("NBTERM12-160",
            "NBTERMPERIOD",
            
            110.0).

FUNCTION fPermanency RETURNS CHAR
   (icDCEvent      AS CHAR, /* FTERM12-140 */
    icTermFeeModel AS CHAR, /* FTERMPERIOD140 */
    idAmt          AS DEC): /* 110.0 */
   DEF VAR lcDCName AS CHAR NO-UNDO.



   lcDCName = icDCEvent + " fixed line permanency".

   FIND FIRST DayCampaign NO-LOCK WHERE 
              DayCampaign.Brand   EQ "1" AND
              DayCampaign.DCEvent EQ icDCEvent 
              NO-ERROR.
   IF NOT AVAILABLE DayCampaign THEN DO:
      CREATE DayCampaign.
      ASSIGN
         DayCampaign.DCEvent         = icDCEvent                   
         DayCampaign.ValidFrom       = TODAY      
         DayCampaign.InclUnit        = 0       
         DayCampaign.ValidTo         = 12/31/49       
         DayCampaign.BillCode        = ""      
         DayCampaign.MaxChargeIncl   = 0        
         DayCampaign.DCTarget        = ""    
         DayCampaign.Weekday         = ""        
         DayCampaign.DCName          = lcDCName
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
         DayCampaign.TermFeeModel    = icTermFeeModel         
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
   END.
   ELSE DO:
      message "daycampaign already exists" VIEW-AS ALERT-BOX.
      RETURN "".
   END.
      FIND FIRST FeeModel NO-LOCK WHERE 
                 FeeModel.FeeModel eq icTermFeeModel NO-ERROR.
      IF NOT AVAIL FeeModel THEN DO:
         CREATE FeeModel.
         ASSIGN
            FeeModel.FeeModel = icTermFeeModel 
            FeeModel.FeeName  = "Neba line contract termination"
            FeeModel.Brand    = "1" 
            FeeModel.FMGroup  = 0.
     END.
     ELSE DO:
        message "daycampaign already exists" VIEW-AS ALERT-BOX.
        RETURN "".
     END.
     FIND FIRST FmItem NO-LOCK WHERE
                fmItem.Teemodel eq icTermFeeModel NO-ERROR.
     IF NOT AVAIL fMItem THEN DO:           
        CREATE FMItem.
        ASSIGN
         FMItem.FeeModel            = icTermFeeModel                    
         FMItem.PriceList           = "COMMON"         
         FMItem.BillCode            = icTermFeeModel
         FMItem.BillMethod          = YES 
         FMItem.Interval            = 0      
         FMItem.Amount              = idAmt
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
   ELSE DO:
      message "fmitem already found" VIEW-AS ALERT-BOX.
      RETURN "".
   END.


END FUNCTION.   
