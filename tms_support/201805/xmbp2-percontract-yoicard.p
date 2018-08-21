{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/create_eventlog.i}  

&GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

{Func/lib/eventlog.i}   

DEFINE VARIABLE lhDayCampaign   AS HANDLE NO-UNDO.
   
lhDayCampaign  = BUFFER DayCampaign:HANDLE.

RUN StarEventInitialize(lhDayCampaign).

FIND FIRST DayCampaign NO-LOCK WHERE 
           DayCampaign.Brand   EQ "1" AND
           DayCampaign.DCEvent EQ "YOICARD" 
           NO-ERROR.
           
IF NOT AVAILABLE DayCampaign THEN DO:
   CREATE DayCampaign.
   ASSIGN
      DayCampaign.DCEvent         = "YOICARD"                  
      DayCampaign.ValidFrom       = TODAY      
      DayCampaign.InclUnit        = 0       
      DayCampaign.ValidTo         = 12/31/49       
      DayCampaign.BillCode        = ""      
      DayCampaign.MaxChargeIncl   = 0        
      DayCampaign.DCTarget        = ""    
      DayCampaign.Weekday         = ""        
      DayCampaign.DCName          = "Credit Card"      
      DayCampaign.CCN             = 0        
      DayCampaign.MaxChargeExcl   = 0          
      DayCampaign.Brand           = "1"     
      DayCampaign.DCType          = "7"      
      DayCampaign.CalcMethod      = 0        
      DayCampaign.DurType         = 3       
      DayCampaign.DurMonths       = 1      
      DayCampaign.Renewal         = 0     
      DayCampaign.FeeModel        = ""        
      DayCampaign.ModifyFeeModel  = ""    
      DayCampaign.TermFeeModel    = ""        
      DayCampaign.DurUnit         = 0        
      DayCampaign.Effective       = 1            
      DayCampaign.TermFeeCalc     = 0             
      DayCampaign.InclStartCharge = NO          
      DayCampaign.InstanceLimit   = 1            
      DayCampaign.FirstMonthCalc  = 0            
      DayCampaign.LastMonthCalc   = 0            
      DayCampaign.StatusCode      = 1            
      DayCampaign.BundleUpsell    = ""         
      DayCampaign.DSSPriority     = 0           
      DayCampaign.PayType         = 0 
      DayCampaign.BundleType      = 0            
      DayCampaign.BundleTarget    = 4.
END.
       
RUN StarEventMakeCreateEvent(lhDayCampaign).
 
Syst.TMSRelation:mAddRelation("DCCLi",
                              "SkipMatrix",
                              "YOICARD",
                              "YOICARD",
                              "ChildValue",
                               YES).
 
 
 