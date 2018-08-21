/* Creating Periodical Contract for permanency */

FUNCTION fPermanency RETURNS CHAR
   (icDCEvent      AS CHAR, /* DayCampaign.DCEvent (Periodical contract)          */
    icTermFeeModel AS CHAR, /* Fee Model                                          */
    icBillItem     AS CHAR, /* BillItem (should be created apart if doesn' exist) */ 
    idAmt          AS DEC): /* 110.0 */
    
   DEF VAR lcDCName AS CHAR NO-UNDO.
   DEF VAR lcFeeName AS CHAR NO-UNDO.
   DEF VAR liDurUnit AS INT NO-UNDO.

   liDurUnit = 2. /* DurUnit 2 for FTERM */
   lcDCName = icDCEvent +  " periodical contract".
   lcFeeName = "Fixed line contract termination".
   IF icDCEvent BEGINS "NEBTERM" THEN DO:
      liDurUnit = 0.
      lcDCName = icDCEvent + " fixed line permanency".
      lcFeeName = "Neba line contract termination".
   END.

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
         DayCampaign.DurUnit         = liDurUnit
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
      MESSAGE "DayCampaign already exists" VIEW-AS ALERT-BOX.
      RETURN "".
   END.
   
   FIND FIRST FeeModel NO-LOCK WHERE 
              FeeModel.Brand    EQ "1" AND 
              FeeModel.FeeModel EQ icTermFeeModel NO-ERROR.
   IF NOT AVAIL FeeModel THEN DO:
      CREATE FeeModel.
      ASSIGN
         FeeModel.FeeModel = icTermFeeModel 
         FeeModel.FeeName  = lcFeeName
         FeeModel.Brand    = "1" 
         FeeModel.FMGroup  = 0.
   END.
   ELSE DO:
      MESSAGE "Feemodel already exists" VIEW-AS ALERT-BOX.
      RETURN "".
   END.
   FIND FIRST FMItem NO-LOCK WHERE
              FMItem.Brand    EQ "1" AND  
              FMItem.FeeModel EQ icTermFeeModel NO-ERROR.
   IF NOT AVAIL fMItem THEN DO:           
      CREATE FMItem.
      ASSIGN
         FMItem.FeeModel            = icTermFeeModel                    
         FMItem.PriceList           = "COMMON"         
         FMItem.BillCode            = icBillItem
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
      MESSAGE "FMItem already exists" VIEW-AS ALERT-BOX.
      RETURN "".
   END.
END FUNCTION.
 
fPermanency("NEBTERM12-160",    /* DayCampaign.DCEvent (Periodical contract)              */
            "NEBTERMPERIOD160", /* FeeModel.FeeModel                                      */ 
            "NEBTERMPERIOD",    /* BillItem (should be created apart if it doesn't exist) */       
            110.0).            /* Price */
            
fPermanency("NEBTERM12-237",    /* DayCampaign.DCEvent (Periodical contract)              */
            "NEBTERMPERIOD237", /* FeeModel.FeeModel                                      */ 
            "NEBTERMPERIOD",    /* BillItem (should be created apart if it doesn't exist) */       
            187.36).            /* Price */

fPermanency("NEBTERM12-293",    /* DayCampaign.DCEvent (Periodical contract)              */
            "NEBTERMPERIOD293", /* FeeModel.FeeModel                                      */ 
            "NEBTERMPERIOD",    /* BillItem (should be created apart if it doesn't exist) */       
            243.88).            /* Price */                        
            
fPermanency("FTERM12-110",    /* DayCampaign.DCEvent (Periodical contract)              */
            "FTERMPERIOD110", /* FeeModel.FeeModel                                      */
            "FTERMPERIOD",    /* BillItem (should be created apart if it doesn't exist) */
            110.0).           /* Price */

fPermanency("FTERM12-187",    /* DayCampaign.DCEvent (Periodical contract)              */
            "FTERMPERIOD187", /* FeeModel.FeeModel                                      */
            "FTERMPERIOD",    /* BillItem (should be created apart if it doesn't exist) */
            187.36).          /* Price */

fPermanency("FTERM12-243",    /* DayCampaign.DCEvent (Periodical contract)              */
            "FTERMPERIOD243", /* FeeModel.FeeModel                                      */
            "FTERMPERIOD",    /* BillItem (should be created apart if it doesn't exist) */
            243.88).          /* Price */


      
