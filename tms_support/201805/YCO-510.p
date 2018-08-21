DEFINE VARIABLE lcPaytermList AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCont        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcAux         AS CHARACTER NO-UNDO.

 FUNCTION fPayterm RETURNS CHAR
   (icDCEvent      AS CHAR, /* DayCampaign.DCEvent (Periodical contract)          */
    icBillItem     AS CHAR, /* BillItem (should be created apart if doesn' exist) */
    iiDurMonths    AS INT,  /* Duration                                           */  
    idAmt          AS DEC): /* Price                                              */

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
         DayCampaign.BillCode        = icBillItem      
         DayCampaign.MaxChargeIncl   = 0        
         DayCampaign.DCTarget        = ""    
         DayCampaign.Weekday         = ""        
         DayCampaign.DCName          = "Installment"
         DayCampaign.CCN             = 0        
         DayCampaign.MaxChargeExcl   = 0          
         DayCampaign.Brand           = "1"     
         DayCampaign.DCType          = "5"      
         DayCampaign.CalcMethod      = 1        
         DayCampaign.DurType         = 3       
         DayCampaign.DurMonths       = iiDurMonths      
         DayCampaign.Renewal         = 0     
         DayCampaign.FeeModel        = icDCEvent        
         DayCampaign.ModifyFeeModel  = ""    
         DayCampaign.TermFeeModel    = "TERMPAYTERM"         
         DayCampaign.DurUnit         = 2        
         DayCampaign.Effective       = 1            
         DayCampaign.TermFeeCalc     = 2             
         DayCampaign.InclStartCharge = YES          
         DayCampaign.InstanceLimit   = 1            
         DayCampaign.FirstMonthCalc  = 0            
         DayCampaign.LastMonthCalc   = 0            
         DayCampaign.StatusCode      = 1            
         DayCampaign.BundleUpsell    = ""         
         DayCampaign.DSSPriority     = 0           
         DayCampaign.PayType         = 1            
         DayCampaign.BundleTarget    = 0
         DayCampaign.BundleType      = 0. 
   END.
   ELSE DO:
      MESSAGE "DayCampaign already exists" VIEW-AS ALERT-BOX.
      RETURN "".
   END.
   
   FIND FIRST FeeModel NO-LOCK WHERE 
              FeeModel.Brand    EQ "1" AND 
              FeeModel.FeeModel EQ icDCEvent NO-ERROR.
   IF NOT AVAIL FeeModel THEN DO:
      CREATE FeeModel.
      ASSIGN
         FeeModel.FeeModel = icDCEvent 
         FeeModel.FeeName  = "Periodical contract installment"
         FeeModel.Brand    = "1" 
         FeeModel.FMGroup  = 0.
   END.
   ELSE DO:
      MESSAGE "Feemodel already exists" VIEW-AS ALERT-BOX.
      RETURN "".
   END.
   FIND FIRST FMItem NO-LOCK WHERE
              FMItem.Brand    EQ "1" AND  
              FMItem.FeeModel EQ icDCEvent NO-ERROR.
   IF NOT AVAIL fMItem THEN DO:           
      CREATE FMItem.
      ASSIGN
         FMItem.FeeModel            = icDCEvent                    
         FMItem.PriceList           = "COMMON"         
         FMItem.BillCode            = "PAYTERM"
         FMItem.BillMethod          = FALSE 
         FMItem.Interval            = 1      
         FMItem.Amount              = idAmt
         FMItem.BillType            = "MF"      
         FMItem.BillCycle           = 2    
         FMItem.FFItemQty           = 24     
         FMItem.FFEndDate           = ?   
         FMItem.InclAmt             = 0       
         FMItem.FromDate            = TODAY      
         FMItem.ToDate              = 12/31/49         
         FMItem.InclBillCode        = ""     
         FMItem.InclUnit            = 0       
         FMItem.ServiceLimitGroup   = ""      
         FMItem.Brand               = "1"         
         FMItem.BrokenRental        = 1         
         FMItem.FirstMonthBR        = 1.   
   END.
   ELSE DO:
      MESSAGE "FMItem already exists" VIEW-AS ALERT-BOX.
      RETURN "".
   END.
END FUNCTION.
 
 
lcPaytermList = 
   "PAYTERM24_7,PAYTERM24_9,PAYTERM24_11,PAYTERM24_13,PAYTERM24_14,"   +
   "PAYTERM24_16,PAYTERM24_17,PAYTERM24_19,PAYTERM24_21,PAYTERM24_23," +
   "PAYTERM24_24,PAYTERM24_26,PAYTERM24_27,PAYTERM24_28,PAYTERM24_29," + 
   "PAYTERM24_31,PAYTERM24_32,PAYTERM24_33,PAYTERM24_34,PAYTERM24_35," +
   "PAYTERM24_36,PAYTERM24_37,PAYTERM24_38,PAYTERM24_39,PAYTERM24_40".   
 
 DO liCont = 1 TO NUM-ENTRIES(lcPaytermList):
    lcAux = ENTRY(liCont, lcPaytermList).    
    fPayterm(
             lcAux,  /* DayCampaign.DCEvent (Periodical contract) */
             "PAYTERM24", /* BillItem (should be created apart if it doesn't exist) */   
             24, /* Duration */
             DECIMAL(ENTRY(2, lcAux, "_")) /* Price */
             ).             
 END.   
  
