/* YCO-276-3Gb-Upsell-creation.p 
   Purpose: create a total of 4 upsells with the same 3Gb size 
*/
DEF VAR liUpsells     AS INTEGER NO-UNDO.
DEF VAR liSLSeq       AS INTEGER NO-UNDO.
DEF VAR liMXSeq       AS INTEGER NO-UNDO.
DEF VAR liPackageID   AS INTEGER NO-UNDO.
DEF VAR liComponentID AS INTEGER NO-UNDO.
DEF VAR liBDLValue    AS INTEGER NO-UNDO.
DEF VAR liPrior       AS INTEGER NO-UNDO.
DEF VAR lSuccess      AS LOGICAL NO-UNDO.
DEF VAR dfrom         AS DATE FORMAT "99/99/99" NO-UNDO.
DEF VAR dto           AS DATE FORMAT "99/99/99" NO-UNDO.
DEF VAR iMx           AS INTEGER NO-UNDO.

DEF BUFFER b_MXItem FOR MXItem.

/* List of valid tariffs for this upsell */
DEF VAR cValidList AS CHAR INITIAL
   "CONTFH2G_50,CONTFH2G_300,CONTFH2G_1000,CONTFH39_50,CONTFH49_300,CONTFH69_1000,CONTFH48_50,CONTFH58_300,CONTFH76_1000,CONTFH3G_50,CONTFH3G_300,CONTFH3G_1000,CONTFH7G_50,CONTFH7G_300,CONTFH7G_1000,CONTFH59_50,CONTFH69_300,CONTFH89_1000,CONTFH99_50,CONTFH109_300,CONTFH129_1000,CONT34,CONT15,CONT33,CONT25,CONTFH35_50,CONTFH45_300,CONTFH65_1000".

/* List of upsells with description and feemodel id */
DEF VAR cUpsell_Id      AS CHARACTER NO-UNDO INITIAL 
    "FID3GB_R_UPSELL,FID3GB_3m_R_UPSELL,FID3GB_6m_R_UPSELL,FID3GB_12m_R_UPSELL".
DEF VAR cUpsell_Desc    AS CHARACTER NO-UNDO INITIAL "Bono de datos 3Gb adicionales 1 mes,Bono de datos 3Gb adicionales 3 meses,Bono de datos 3Gb adicionales 6 meses,Bono de datos 3Gb adicionales 12 meses".
DEF VAR cUpsell_Fee     AS CHARACTER NO-UNDO INITIAL "FID3GBMFUPS,FID3GBMFUPS_3m,FID3GBMFUPS_6m,FID3GBMFUPS_12m".

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".

FUNCTION fGetNextMXSeq RETURNS INTEGER ():
   DEFINE BUFFER bf_Matrix FOR Matrix.

   FOR EACH bf_Matrix NO-LOCK BY bf_Matrix.MXSeq DESCENDING:
     RETURN bf_Matrix.MXSeq + 1.
   END.

   RETURN 1.
END FUNCTION.

FUNCTION fGetNextMatrixPriority RETURNS INTEGER (icKey AS CHARACTER):
   DEFINE BUFFER bf_Matrix FOR Matrix.

   FIND LAST bf_Matrix WHERE bf_Matrix.mxkey = icKey NO-LOCK NO-ERROR.
   IF AVAILABLE bf_Matrix THEN 
      RETURN (bf_Matrix.Prior + 1).
   ELSE 
      RETURN 1.  
END FUNCTION.


ASSIGN
   dfrom = TODAY
   dto   = DATE(12,31,2049).

FORM
  SKIP "This program will create upsells for YCO-276: 3G with codes" SKIP(1)
  "FID3GB_R_UPSELL,FID3GB_3m_R_UPSELL,FID3GB_6m_R_UPSELL and FID3GB_12m_R_UPSELL" SKIP
  "The below dates will be used as the valid from and to dates" skip
  "for all records created as part of these upsells." 
  SKIP(2)
  "Upsell Valid from: " dfrom SKIP
  "Upsell Valid to  : " dto
  WITH OVERLAY CENTERED ROW 6 TITLE " Add Upsells for YCO-276 " NO-LABELS
  FRAME f-yco276.

UPDATE dfrom
       dto WITH FRAME f-yco276.

MESSAGE "Are you sure you want to proceed with the process?" VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO UPDATE lGo AS LOGICAL.
   
IF lGo = FALSE THEN
DO:
   MESSAGE "Process cancelled by user" VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.
     
blk-upsell:
DO TRANSACTION ON ERROR UNDO blk-upsell, LEAVE blk-upsell
               ON STOP  UNDO blk-upsell, LEAVE blk-upsell:
                  
  lsuccess = FALSE.

  /* Check if the program has already being executed */
  FIND FIRST daycampaign WHERE daycampaign.Brand   = Syst.Var:gcBrand
                           AND daycampaign.DCEVent = "FID3GB_R_UPSELL"
                         NO-LOCK NO-ERROR. 
  IF AVAILABLE daycampaign THEN
  DO:
     MESSAGE "YCO-276 Upsell 3Gb creation program has already being executed"
       VIEW-AS ALERT-BOX.
     RETURN.
  END.


  DO liUpsells = 1 TO 4:
   
     CREATE daycampaign.
     ASSIGN 
        daycampaign.Brand           = Syst.Var:gcBrand                      /* Brand                           */
        daycampaign.DCEvent         = ENTRY(liUpsells,cUpsell_Id)           /* Periodical Term                 */
        daycampaign.DCName          = ENTRY(liUpsells,cUpsell_Desc)         /* Name                            */
        daycampaign.PayType         = 1                                     /* PaymentType                     */
        daycampaign.ValidFrom       = dfrom                                 /* Valid From                      */
        daycampaign.ValidTo         = dto                                   /* Valid To                        */
        daycampaign.DCType          = "6"                                   /* Campaign Type: 6 Pool Rating    */
        daycampaign.BillCode        = ""                                    /* Billing Item: it can be left blank for new upsells */
        daycampaign.InclUnit        = 4                                     /* Included Unit: 4 megabyte       */
        daycampaign.MaxChargeExcl   = 0                                     /* Max. Charge Excl. VAT           */
        daycampaign.MaxChargeIncl   = 0                                     /* Max. Charge Incl. VAT           */
        daycampaign.DCTarget        = ""                                    /* Target                          */     
        daycampaign.Weekday         = ""                                    /* Weekday                         */
        daycampaign.CCN             = 93                                    /* Report CCN that is marked to CDRs: 93 GPRS National (MO) */
        daycampaign.CalcMethod      = 1                                     /* Calculation Method: 1 Special charge before limit        */
        daycampaign.DurType         = 2                                     /* Type of duration: 2 Determinate with counters            */
        daycampaign.DurMonths       = 1                                     /* Duration In Months              */   
        daycampaign.Renewal         = 0                                     /* Renewal Method                  */
        daycampaign.Effective       = 1                                     /* Effective                       */
        daycampaign.InclStartCharge = yes                                   /* Include Start Charge            */
        daycampaign.InstanceLimit   = 100                                   /* Instance Limit                  */
        daycampaign.FirstMonthCalc  = 0                                     /* First Month Calculation         */
        daycampaign.LastMonthCalc   = 0                                     /* Last Month Calculation          */
        daycampaign.DSSPriority     = 0                                     /* DSSPriority                     */
        daycampaign.BundleTarget    = 0                                     /* Bundle Target                   */   
        daycampaign.BundleUpsell    = ""                                    /* Bundle Upsell                   */  
        daycampaign.StatusCode      = 1                                     /* Status: 1 (Active)              */
        daycampaign.FeeModel        = ENTRY(liUpsells,cUpsell_Fee)          /* Fee Model                       */
        daycampaign.ModifyFeeModel  = ""                                    /* Modification Fee Model          */
        daycampaign.TermFeeModel    = ""                                    /* Termination Fee Model           */
        daycampaign.TermFeeCalc     = 0                                     /* Term. Fee Calculation           */
        daycampaign.DurUnit         = 0.                                    /* Duration Unit                   */
     
     /* Add SERVICE PACKAGE (SHAPER Service Pack is in table mobile.ServPac) (Note: Apparently DSS do not have a Service Package)
        DAYCAMPAIGN -> DCSERVICEPACKAGE 0:2? */
     FIND LAST DCServicePackage USE-INDEX DCServicePackageID NO-LOCK NO-ERROR.
     IF AVAILABLE DCServicePackage THEN 
         liPackageID = DCServicePackage.DCServicePackageID + 1.
     ELSE 
         liPackageID = 1.
     
     CREATE DCServicePackage.
     ASSIGN 
        DCServicePackage.brand               = daycampaign.Brand
        DCServicePackage.DCEVent             = daycampaign.DCEvent
        DCServicePackage.DCServicePackageID  = liPackageID
        DCServicePackage.ServPac             = "SHAPER"
        DCServicePackage.FromDate            = daycampaign.ValidFrom
        DCServicePackage.ToDate              = daycampaign.ValidTo.
   
     /* Add SERVICE COMPONENT (DCServiceComponent has to be created for SHAPER DCServicePackages) 
        DCSERVICEPACKAGE -> DCSERVICECOMPONENT 0:2? */
     FIND LAST DCServiceComponent USE-INDEX DCServiceComponentID NO-LOCK NO-ERROR.
     IF AVAILABLE DCServiceComponent THEN 
        liComponentID = DCServiceComponent.DCServiceComponentID + 1.
     ELSE 
        liComponentID = 1.    
   
     CREATE DCServiceComponent.
     ASSIGN 
        DCServiceComponent.DCServicePackageID   = DCServicePackage.DCServicePackageID 
        DCServiceComponent.DCServiceComponentID = liComponentID      
        DCServiceComponent.ServCom              = DCServicePackage.ServPac
        DCServiceComponent.DefValue             = 1
        DCServiceComponent.DefParam             = "3221225472#UPSELL,GRACE=0,TEMPLATE=HSPA" /* nGb x 1024 x 1024 x 1024 = n bytes */
        DCServiceComponent.FromDate             = DCServicePackage.FromDate 
        DCServiceComponent.ToDate               = DCServicePackage.ToDate.
        
     /* Add Fee Model 
        DAYCAMPAIGN -> FEEMODEL 1:1 */
     CREATE Feemodel.
     ASSIGN
        Feemodel.brand    = daycampaign.Brand                                    /* Brand           */
        Feemodel.feemodel = daycampaign.FeeModel                                 /* Billing Event   */  
        Feemodel.feename  = ENTRY(liUpsells,cUpsell_Id) + " upsell monthly fee"  /* Name            */
        Feemodel.fmgroup  = 0.                                                   /* Fee Model Group */
   
     /* Add Fee Item 
        FEEMODEL -> FMITEM 1:N */
     CREATE FMItem.
     ASSIGN     
        FMItem.Brand             = daycampaign.Brand        /* Brand                                                       */
        FMItem.Feemodel          = Feemodel.feemodel        /* Billing Event                                               */
        FMItem.PriceList         = "COMMON"                 /* Price List                                                  */
        FMItem.BillCode          = Feemodel.feemodel        /* Bill code - equal to feemodel for upsells                   */  
        FMItem.BillMethod        = FALSE                    /* Type: single fee (FALSE) / Fixed fee (TRUE)                 */
        FMItem.FromDate          = daycampaign.ValidFrom       
        FMItem.ToDate            = daycampaign.ValidTo
        FMItem.Interval          = 0                        /* Interval: 1 = every month, 12 = every year. 0 = an upsell is not a regular fee. */
        FMItem.Amount            = 0                        /* Price: YCO-276 These upsell are free of charge              */   
        FMItem.BillType          = "TR"                     /* Billing type:                                               */                  
        FMItem.BillCycle         = 1                        /* Bill cycle: 1 - before, 2 - during, 3 - after               */                   
        FMItem.FFItemQty         = 0                        /* Quantity of fixed fee items                                 */
        FMItem.FFEndDate         = ?                        /* End date for fixed fee                                      */
        FMItem.InclAmt           = 0                        /* Amount of billable material that is included in this fee    */
        FMItem.InclBillCode      = ""                       /* Billing item that included amount concerns                  */
        FMItem.InclUnit          = 0                        /* Unit of included material                                   */
        FMItem.ServiceLimitGroup = ""                       /* Group code of Service Limit: used only with prepaid upsells */
        FMItem.BrokenRental      = 0                        /* Broker Rental                                               */
        FMItem.FirstMonthBR      = 0.                       /* Broken rental for first month                               */
   
     /* Add Billing concept 
        FMITEM -> BILLITEM 1:1 */
     CREATE BillItem.
     ASSIGN
        BillItem.Brand             = daycampaign.Brand                             /* Brand                                     */
        BillItem.BillCode          = FMItem.BillCode                               /* Billing Item                              */
        BillItem.BIName            = ENTRY(liUpsells,cUpsell_Desc) + " upselling"  /* Bill. Item Name                           */ 
        BillItem.AccNum            = 70514100                                      /* Account                                   */
        BillItem.BIGroup           = "3"                                           /* Bill.Item Group: 3 internet               */
        BillItem.DispMPM           = No                                            /* Display MPM                               */ 
        BillItem.EUAccNum          = 70514100                                      /* EU Sales Account                          */ 
        BillItem.FSAccNum          = 70514100                                      /* Foreign Sales Account                     */
        BillItem.BillType          = ""                                            /* Billing type                              */ 
        BillItem.ISOrder           = 0                                             /* Section Order                             */
        BillItem.VATCode           = 1                                             /* VAT code                                  */
        BillItem.TB1AccNum         = 0                                             /* TB1                                       */
        BillItem.TB2AccNum         = 0                                             /* TB2                                       */
        BillItem.InvSect           = ""                                            /* Invoice Section                           */ 
        BillItem.EUVATCode         = 0                                             /* EU VAT Code                               */ 
        BillItem.EUConAccNum       = 70514100                                      /* EU Cons. Sales                            */ 
        BillItem.CostCentre        = "SL"                                          /* Cost Centre                               */
        BillItem.AltAccNum         = 70514100                                      /* Own Use Account                           */
        BillItem.TaxClass          = "1"                                           /* Tax Class                                 */ 
        BillItem.SAPRid            = "051"                                         /* SAP Reporting ID                          */ 
        BillItem.VIPAccNum         = 70514100                                      /* VIP Account                               */
        BillItem.OrderChannelOrder = 0                                             /* Presentation order in Order Channel       */ 
        BillItem.Active            = YES                                           /* Active                                    */
        BillItem.ItemType          = 0.                                            /* Item Type: 0 for mobile (or mobile part of convergent), 1 for fixed line */
          
     /* Add MXITem and MATRIX
        DAYCAMPAIGN -> MXITem 1:1?
        MATRIX -> MXITem 1:1 */
     liMxSeq = fGetNextMXSeq().
     liPrior = fGetNextMatrixPriority("PERCONTR").
   
     CREATE Matrix.
     ASSIGN 
        Matrix.Brand  = daycampaign.Brand
        Matrix.MXName = daycampaign.DCEvent + " Per.contract usage"    /* Name of the Matrix                   */
        Matrix.MXKey  = "PERCONTR"              /* Matrix Key                           */
        Matrix.MXSeq  = liMxSeq                 /* Matrix Sequence                      */
        Matrix.MXRes  = 1                       /* Matrix Response: 0 Denied, 1 Allowed */
        Matrix.Prior  = liPrior.                /* Priority                             */          
   
     CREATE MXItem.
     ASSIGN 
        MXItem.MXSeq   = Matrix.MXSeq           /* Matrix Sequence */
        MXItem.MXName  = "PerContract"          /* Name MXItem     */
        MXItem.MXValue = daycampaign.dcevent.   /* Matrix Value    */
 
     DO iMx = 1 TO NUM-ENTRIES(cValidList):
        CREATE b_MXItem.
        ASSIGN 
           b_MXItem.MXSeq   = MXItem.MXSeq            /* Matrix Sequence */
           b_MXItem.MXName  = "SubsTypeTo"            /* Name MXItem     */
           b_MXItem.MXValue = ENTRY(iMx,cValidList).  /* Matrix Value    */
     END.
              
     /* Add Service Limit group (This record has to exists for daycampaign records with a DCType = 1 or 4 or 6 or 8)
        DAYCAMPAIGN -> SERVICELIMITGROUP 1:1 */
     CREATE ServiceLimitGroup.
     ASSIGN 
        ServiceLimitGroup.brand     = daycampaign.Brand
        ServiceLimitGroup.groupcode = daycampaign.DCEvent
        ServiceLimitGroup.Groupname = "Mobile Data Usage Upsell 3G"
        ServiceLimitGroup.validfrom = daycampaign.ValidFrom
        ServiceLimitGroup.validto   = daycampaign.ValidTo.
        
     /* Add Service Limit - Creating DATA only for an upsell
        SERVICELIMITGROUP -> SERVICELIMIT 1:N (data/voice) */
     FIND LAST ServiceLimit NO-LOCK USE-INDEX SLSeq NO-ERROR.               
     IF AVAILABLE ServiceLimit THEN 
         liSLSeq = ServiceLimit.SLSeq + 1.          
     ELSE 
         liSLSeq = 1.
     
     CREATE ServiceLimit.
     ASSIGN
        ServiceLimit.ToTS           = 0                            /* ToTimestamp                                    */
        ServiceLimit.groupcode      = ServiceLimitGroup.groupcode  /* GroupCode                                      */
        ServiceLimit.SLSeq          = liSLSeq                      /* SLSeq                                          */
        ServiceLimit.WebDisp        = 0                            /* Web (legacy field, unused. Used to be 0 or 1.) */
        ServiceLimit.DialType       = 7                            /* Dialling Type: 7 Data                          */
        ServiceLimit.InclUnit       = 4                            /* Included Unit                                  */
        ServiceLimit.SLCode         = ServiceLimitGroup.groupcode  /* Code of Service Limit                          */
        ServiceLimit.SLName         = ServiceLimitGroup.Groupname  /* Service Limit Name                             */
        ServiceLimit.InclAmt        = 3072                         /* Included Amount: 3 x 1024 = 3072Mb = 3GB       */
        ServiceLimit.ValidFrom      = ServiceLimitGroup.validfrom  /* Valid From                                     */
        ServiceLimit.ValidTo        = ServiceLimitGroup.validto    /* Valid To                                       */
        ServiceLimit.FirstMonthCalc = 0                            /* First Month Calculation                        */  
        ServiceLimit.LastMonthCalc  = 0                            /* Last Month Calculation                         */
        ServiceLimit.BDestLimit     = 0.                           /* BDestLimit                                     */
     
     /* Add Service limit target (Upsell are linked to Service Limit Target rather than ProgLimit) 
        SERVICELIMIT -> SERVICELIMITTARGET 1:N */
     CREATE ServiceLimitTarget. 
     ASSIGN 
            ServiceLimitTarget.Slseq          = ServiceLimit.SLSeq            /* Sequence for Service Limit: ServiceLimit.SLSeq */
            ServiceLimitTarget.ServiceLMember = "14100001"                    /* 14100001 for DATA */
            ServiceLimitTarget.ServiceLimitMt = 0
            ServiceLimitTarget.InsideRate     = "GPRS_" + ServiceLimitGroup.groupcode /* Limit rate: GPRS_FID3GB_R_UPSELL, GPRS_FID3GB_3m_R_UPSELL, ... */
            ServiceLimitTarget.outsideRate    = "".
   
     /* Add B-Destination 
        SERVICELIMITTARGET -> BDest 1:1 */
     FIND LAST BDest USE-INDEX BDestID NO-LOCK NO-ERROR.
     IF AVAILABLE BDest THEN 
         liBDLValue = BDest.BDestID + 1.
     ELSE
         liBDLValue = 1.
        
     CREATE BDest. 
     ASSIGN 
        BDest.Brand    = daycampaign.Brand               /* Brand                               */
        BDest.BDestID  = liBDLValue                      /* BDestination ID                     */
        BDest.BDest    = ServiceLimitTarget.InsideRate   /* B-subNo (Link to ServiceLimit Target or ProgLimit records) */
        BDest.BDName   = "GRPS Data"                     /* BDest name                          */
        BDest.DestType = 0                               /* B-subscriber type                   */
        BDest.CCN      = 93                              /* Country/Service number: 93 for DATA */
        BDest.Class    = 1                               /* Class: 1 A-sub pays                 */
        BDest.FromDate = daycampaign.ValidFrom           /* Valid from                          */
        BDest.ToDate   = daycampaign.ValidTo.            /* Valid to                            */

  END.  /* DO liUpsells */
  
  /* Process OK */
  lSuccess = TRUE.
END.

IF lSuccess THEN
    MESSAGE "Upsells successfully created" VIEW-AS ALERT-BOX.
ELSE 
    MESSAGE "ERROR: Failed to create the upsells" VIEW-AS ALERT-BOX.

