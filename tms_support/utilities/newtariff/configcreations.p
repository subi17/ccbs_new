/*------------------------------------------------------------------------
  MODULE .......: configcreations.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: Tue Feb 10 15:36:57 EET 2015
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.
/* ***************************  Definitions  ************************** */
{commpaa.i}
katun = "Cron".
gcBrand = "1".
{cparam2.i}
{eventlog.i}
{ftransdir.i}
{tariffconfig.i}
{tariffcons.i}
{fixedlinefunc.i}

/* ********************  Functions  ******************** */
FUNCTION fTMSCodeValue RETURNS CHARACTER 
             (INPUT lcTableName AS CHARACTER,
              INPUT lcFieldName AS CHARACTER,
              INPUT lcCodeGroup AS CHARACTER):

   FIND FIRST TMSCodes NO-LOCK WHERE
              TMSCodes.TableName = TableName AND
              TMSCodes.FieldName = FieldName AND
              TMSCodes.CodeGroup = CodeGroup AND
              TMSCodes.InUse     > 0         NO-ERROR.
   
   IF AVAILABLE TMSCodes THEN 
      RETURN TMSCodes.CodeValue.           
   ELSE RETURN "".   
              
END FUNCTION.                

FUNCTION fTMSCValue RETURNS CHARACTER
   (iTableName AS CHAR,
    iFieldName AS CHAR,
    iCodeName  AS CHAR).

    FIND FIRST TMSCodes NO-LOCK WHERE
               TMSCodes.TableName = iTableName AND
               TMSCodes.FieldName = iFieldName AND
          TRIM(TMSCodes.CodeName) = iCodeName  NO-ERROR.

    IF AVAILABLE TMSCodes THEN RETURN TMSCodes.CodeValue.
    ELSE RETURN "".

END FUNCTION.  

FUNCTION fGetNextMatrixPriority RETURNS INTEGER 
   (icKey AS CHARACTER):

   DEFINE BUFFER bf_Matrix FOR Matrix.

   FIND LAST bf_Matrix WHERE bf_Matrix.mxkey = icKey NO-LOCK NO-ERROR.
   IF AVAILABLE bf_Matrix THEN 
      RETURN (bf_Matrix.Prior + 1).
   ELSE 
      RETURN 1.  

END FUNCTION.    


FUNCTION fGetNextRequestActionSequence RETURNS INTEGER 
   ():
   DEFINE BUFFER bf_RequestAction FOR RequestAction.

   FIND LAST bf_RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.
   IF AVAILABLE bf_RequestAction THEN 
      RETURN (bf_RequestAction.RequestActionID + 1).
   ELSE 
      RETURN 1.

END FUNCTION.    

FUNCTION fPrepareRequestActionParam RETURNS LOGICAL
  (INPUT icMobileBundles            AS CHARACTER,
   INPUT icFixedLineBundles         AS CHARACTER,
   INPUT icBundlesForActivateOnSTC  AS CHARACTER,
   INPUT icServicesRecreated        AS CHARACTER,
   OUTPUT ocReqTypeList             AS CHARACTER,
   OUTPUT ocActionList              AS CHARACTER,
   OUTPUT ocActionKeyList           AS CHARACTER,
   OUTPUT ocActionTypeList          AS CHARACTER):

   DEFINE VARIABLE liCnt             AS INTEGER   NO-UNDO.   
   DEFINE VARIABLE liCount           AS INTEGER   NO-UNDO.   

   IF icMobileBundles > "" THEN 
   DO liCount = 1 TO NUM-ENTRIES(icMobileBundles):    

      DO liCnt = 1 TO 2:
        ASSIGN 
           ocReqTypeList     = ocReqTypeList + (IF ocReqTypeList <> "" THEN "," ELSE "") + (IF liCnt = 1 THEN 
                                                                                               STRING({&REQTYPE_SUBSCRIPTION_CREATE}) 
                                                                                            ELSE 
                                                                                               STRING({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE})) 
           ocActionList      = ocActionList      + (IF ocActionList      <> "" THEN "," ELSE "") + "CREATE"
           ocActionKeyList   = ocActionKeyList   + (IF ocActionKeyList   <> "" THEN "," ELSE "") + ENTRY(liCount, icMobileBundles)
           ocActionTypeList  = ocActionTypeList  + (IF ocActionTypeList  <> "" THEN "," ELSE "") + "DayCampaign".
      END.   

   END.

   IF icFixedLineBundles > "" THEN 
   DO liCount = 1 TO NUM-ENTRIES(icFixedLineBundles):    

      DO liCnt = 1 TO 2: 
         ASSIGN 
            ocReqTypeList = ocReqTypeList + (IF ocReqTypeList <> "" THEN "," ELSE "") + (IF liCnt = 1 THEN 
                                                                                            STRING({&REQTYPE_FIXED_LINE_CREATE}) 
                                                                                         ELSE 
                                                                                            STRING({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}))  
            ocActionList      = ocActionList      + (IF ocActionList      <> "" THEN "," ELSE "") + "CREATE"
            ocActionKeyList   = ocActionKeyList   + (IF ocActionKeyList   <> "" THEN "," ELSE "") + ENTRY(liCount, icFixedLineBundles)
            ocActionTypeList  = ocActionTypeList  + (IF ocActionTypeList  <> "" THEN "," ELSE "") + "DayCampaign".
      END.   
   END.   

   IF icBundlesForActivateOnSTC > "" THEN 
   DO liCount = 1 TO NUM-ENTRIES(icBundlesForActivateOnSTC):
      ASSIGN 
            ocReqTypeList = ocReqTypeList + (IF ocReqTypeList <> "" THEN "," ELSE "") + STRING({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE})  
            ocActionList      = ocActionList      + (IF ocActionList      <> "" THEN "," ELSE "") + "CREATE"
            ocActionKeyList   = ocActionKeyList   + (IF ocActionKeyList   <> "" THEN "," ELSE "") + ENTRY(liCount, icBundlesForActivateOnSTC)
            ocActionTypeList  = ocActionTypeList  + (IF ocActionTypeList  <> "" THEN "," ELSE "") + "DayCampaign".
   END.

   IF icServicesRecreated > "" THEN 
   DO liCount = 1 TO NUM-ENTRIES(icServicesRecreated):
      ASSIGN 
            ocReqTypeList = ocReqTypeList + (IF ocReqTypeList <> "" THEN "," ELSE "") + STRING({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE})  
            ocActionList      = ocActionList      + (IF ocActionList      <> "" THEN "," ELSE "") + "RECREATE"
            ocActionKeyList   = ocActionKeyList   + (IF ocActionKeyList   <> "" THEN "," ELSE "") + ENTRY(liCount, icServicesRecreated)
            ocActionTypeList  = ocActionTypeList  + (IF ocActionTypeList  <> "" THEN "," ELSE "") + "CTServPac".
   END.

   RETURN TRUE.

END FUNCTION.
/* ***************************  Main Block  *************************** */
PROCEDURE pRatePlan:
    DEFINE INPUT PARAMETER icRatePlan          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icRPName            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icReferenceRatePlan AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icRatePlanAction    AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf_RatePlanCopyFrom  FOR RatePlan.
    DEFINE BUFFER bf_PListConfCopyFrom FOR PListConf.
    DEFINE BUFFER bPListConf           FOR PListConf.
    DEFINE BUFFER bPriceList           FOR PriceList.
    DEFINE BUFFER bTariff              FOR Tariff.

    FIND FIRST bf_RatePlanCopyFrom WHERE bf_RatePlanCopyFrom.Brand = gcBrand AND bf_RatePlanCopyFrom.RatePlan = icReferenceRatePlan NO-LOCK NO-ERROR.
    IF AVAIL bf_RatePlanCopyFrom THEN 
    DO:
        FIND FIRST RatePlan WHERE RatePlan.Brand = gcBrand AND RatePlan.RatePlan = icRatePlan NO-LOCK NO-ERROR.
        IF NOT AVAIL RatePlan THEN 
        DO:
            CREATE RatePlan.
            BUFFER-COPY bf_RatePlanCopyFrom EXCEPT RatePlan RPName TO RatePlan
                ASSIGN 
                    RatePlan.RatePlan = icRatePlan
                    RatePlan.RPName   = icRPName.        
                
            FOR EACH bf_PListConfCopyFrom WHERE bf_PListConfCopyFrom.Brand    = gcBrand                      AND 
                                                bf_PListConfCopyFrom.RatePlan = bf_RatePlanCopyFrom.RatePlan AND
                                                bf_PListConfCopyFrom.dFrom   <= TODAY                        AND
                                                bf_PListConfCopyFrom.dTo     >= TODAY                        NO-LOCK:

                IF icRatePlanAction = "New" AND (bf_PListConfCopyFrom.RatePlan EQ bf_PListConfCopyFrom.PriceList)  THEN 
                DO:
                    /* A copy of existing rateplan's pricelist and related tariffs is created */
                    FIND FIRST PriceList WHERE PriceList.Brand = gcBrand AND PriceList.PriceList = bf_PListConfCopyFrom.PriceList NO-LOCK NO-ERROR.
                    IF AVAIL PriceList THEN 
                    DO:
                        CREATE bPriceList.
                        BUFFER-COPY PriceList EXCEPT PriceList PLName TO bPriceList
                            ASSIGN 
                                bPriceList.PriceList = RatePlan.RatePlan
                                bPriceList.PLName    = RatePlan.RPName.

                        FOR EACH Tariff WHERE Tariff.Brand = gcBrand AND Tariff.PriceList = PriceList.PriceList NO-LOCK:                     
                            CREATE bTariff.
                            BUFFER-COPY Tariff EXCEPT TariffNum PriceList ValidFrom TO bTariff
                                ASSIGN 
                                    bTariff.TariffNum = NEXT-VALUE(Tariff)
                                    bTariff.PriceList = bPriceList.PriceList
                                    bTariff.ValidFrom = TODAY.                      
                        END.                

                        CREATE bPListConf.
                        BUFFER-COPY bf_PListConfCopyFrom EXCEPT PriceList RatePlan dFrom dTo TO bPListConf
                            ASSIGN 
                                bPListConf.PriceList = RatePlan.RatePlan
                                bPListConf.RatePlan  = RatePlan.RatePlan
                                bPListConf.dFrom     = TODAY 
                                bPListConf.dTo       = DATE(12,31,2049).
                    END.        
                END.            
                ELSE
                DO: 
                    /* Share existing rateplan's pricelist and related tariffs */                               
                    CREATE bPListConf.
                    BUFFER-COPY bf_PListConfCopyFrom EXCEPT RatePlan TO bPListConf
                        ASSIGN bPListConf.RatePlan = RatePlan.RatePlan.                                                  
                END.
            END.                                      
        END. /* IF NOT AVAIL RatePlan THEN */
    END.

    RETURN "".

END PROCEDURE.


PROCEDURE pCustomRates:    
    DEFINE PARAMETER BUFFER ttTariff FOR ttTariff.    

    FIND FIRST Tariff WHERE Tariff.Brand      = gcBrand            AND 
                            Tariff.PriceList  = ttTariff.PriceList AND 
                            Tariff.CCN        = INT(ttTariff.CCN)  AND 
                            Tariff.BDest      = ttTariff.BDest     AND 
                            Tariff.ValidFrom <= TODAY              AND 
                            Tariff.ValidTo   >= TODAY              NO-LOCK NO-ERROR.                                    
    IF NOT AVAIL Tariff THEN 
    DO:
        CREATE Tariff.
        ASSIGN 
            Tariff.Brand          = gcBrand
            Tariff.TariffNum      = NEXT-VALUE(Tariff)
            Tariff.PriceList      = ttTariff.PriceList
            Tariff.CCN            = INT(ttTariff.CCN)
            Tariff.BDest          = ttTariff.BDest
            Tariff.BillCode       = ttTariff.BillItem
            Tariff.DataType       = INT(ttTariff.PriceUnit)
            Tariff.Discount[4]    = Yes
            Tariff.TZName[1]      = "Off Peak"
            Tariff.DayType[1]     = 1
            Tariff.TZFrom[1]      = "0000" 
            Tariff.TZTo[1]        = "2400"
            Tariff.Price[1]       = DECIMAL(ttTariff.Price)
            Tariff.StartCharge[1] = DECIMAL(ttTariff.SetupFee)
            Tariff.TZName[2]      = "Peak"
            Tariff.DayType[2]     = 0
            Tariff.TZFrom[2]      = "0000" 
            Tariff.TZTo[2]        = "0000"
            Tariff.Price[2]       = 0
            Tariff.StartCharge[2] = 0
            Tariff.TZName[3]      = "Off Peak"
            Tariff.DayType[3]     = 0
            Tariff.TZFrom[3]      = "0000" 
            Tariff.TZTo[3]        = "0000"
            Tariff.Price[3]       = 0
            Tariff.StartCharge[3] = 0
            Tariff.ValidFrom      = TODAY 
            Tariff.Validto        = DATE(12,31,2049).
    END.            

    RETURN "".

END PROCEDURE.

PROCEDURE pCLIType:
   DEFINE PARAMETER BUFFER ttCliType FOR ttCliType.
   
   DEFINE VARIABLE liFinalBT  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liFinalCR  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcRatePlan AS CHARACTER NO-UNDO.

   IF NOT CAN-FIND(FIRST CLIType WHERE CLIType.Brand = gcBrand AND CLIType.CLIType = ttCliType.CliType) THEN 
   DO:     
      /* Find highest value of billing target and contrtype */    
      FOR EACH CLIType NO-LOCK:
         IF CLIType.BillTarget > liFinalBT THEN 
            liFinalBT = CLIType.BillTarget.

         IF Clitype.ContrType > liFinalCR THEN 
            liFinalCR = CLIType.ContrType.        
      END.
      
      /* In case of tariff bundle subscription type creation,  
         main parent tariff billing target value will be assigned directly. 
         Else, already available value has to be incremented by 1 */
      IF ttCliType.TariffBundle NE "" THEN 
      DO:   
         FIND FIRST CLIType WHERE CLIType.Brand = gcBrand AND CLIType.CLIType = ttCLIType.ParentTariff NO-LOCK NO-ERROR.                
         IF AVAILABLE CLIType THEN
            ASSIGN lcRatePlan = CLIType.PricePlan
                   liFinalBT  = CLIType.BillTarget.               
      END.
       
      CREATE CLIType.
      ASSIGN 
         CLIType.Brand             = gcBrand
         CLIType.CLIType           = ttCliType.CliType
         CLIType.CLIName           = ttCliType.CliName
         CLIType.BaseBundle        = ttCliType.BaseBundle
         CLIType.PayType           = ttCliType.PayType
         CLIType.UsageType         = ttCliType.UsageType
         CLIType.PricePlan         = (IF lcRatePlan NE "" THEN 
                                          lcRatePlan 
                                      ELSE IF ttCliType.RatePlan NE "" THEN 
                                          ttCliType.RatePlan 
                                      ELSE 
                                          REPLACE(ttCliType.CliType,"CONT","CONTRATO")) 
         CLIType.ServicePack       = (IF ttCliType.PayType EQ 1 THEN "11" ELSE "12")
         ClIType.ServiceClass      = ttCliType.ServiceClass
         CLIType.BillTarget        = (liFinalBT + 1) 
         CLIType.ContrType         = (IF CLIType.PayType EQ 1 THEN (liFinalCR + 1) ELSE 1)
         CLIType.ARAccNum          = (IF CLIType.PayType = 1 THEN 43000000 ELSE IF CLIType.PayType = 2 THEN 43001000 ELSE 0)
         CLIType.CommercialFee     = ttCliType.CommercialFee
         CLIType.CompareFee        = ttCliType.CompareFee
         CliType.BundleType        = ttCliType.BundleType
         CLIType.WebStatusCode     = ttCliType.WebStatusCode
         CLIType.StatusCode        = ttCliType.StatusCode         
         CLIType.LineType          = ttCliType.LineType
         CLIType.FixedLineType     = ttCliType.FixedLineType 
         CliType.FixedLineDownload = ttCliType.FixedLineDownload
         CliType.FixedLineUpload   = ttCliType.FixedLineUpload.

      IF ttCliType.CopyServicesFromCliType > "" THEN 
         RUN pCTServPac(ttCliType.CliType, ttCliType.CopyServicesFromCliType).      
      
      IF ttCliType.AllowedBundles > "" THEN
      DO:    
          RUN pMatrix(ttCliType.CliType, 
                      ((IF ttCliType.BaseBundle > "" THEN (ttCliType.BaseBundle + ",") ELSE "") + (IF ttCliType.FixedLineBaseBundle > "" THEN (ttCliType.FixedLineBaseBundle + ",") ELSE "") + ttCliType.AllowedBundles)).

      END.

      IF ttCliType.TariffBundle = "" THEN    
         RUN pSLGAnalyse(ttCliType.CliType, ttCliType.CopyServicesFromCliType, ttCliType.BaseBundle, ttCliType.FixedLineBaseBundle, ttCliType.AllowedBundles).         

      IF ttCliType.BundleType = False THEN     
         RUN pRequestAction(ttCliType.CliType, 
                            ttCliType.BaseBundle, 
                            ttCliType.FixedLineBaseBundle, 
                            ttCliType.BundlesForActivateOnSTC, 
                            ttCliType.ServicesForReCreateOnSTC).      

      IF ttCLIType.PayType = 1 AND NOT (CliType.FixedLineDownload > "" OR CliType.FixedLineUpload > "") THEN /* Non-convergent */
      DO:
         RUN pUpdateTMSParam("ALL_POSTPAID_CONTRACTS",ttCliType.CliType).  
         RUN pUpdateTMSParam("POSTPAID_DATA_CONTRACTS", ttCliType.CliType).
         /*TODO: Parent of Tariff bundle needs to be excluded */
         RUN pUpdateTMSParam("BB_PROFILE_1",ttCliType.CliType).
      END.
      
      IF ttCliType.MobileBaseBundleDataLimit > 0 OR CliType.FixedLineDownload > "" OR CliType.FixedLineUpload > "" THEN       
         RUN pUpdateTMSParam("DATA_BUNDLE_BASED_CLITYPES", ttCliType.CliType).

      IF ttCLIType.PayType = 1 AND ttCLIType.UsageType = 1 THEN 
         RUN pUpdateTMSParam("POSTPAID_VOICE_TARIFFS", ttCliType.CliType).
      ELSE IF ttCLIType.PayType = 2 AND ttCLIType.UsageType = 1 THEN 
         RUN pUpdateTMSParam("PREPAID_VOICE_TARIFFS", ttCliType.CliType).         
   END.
     
   RETURN "".

END PROCEDURE.


PROCEDURE pCTServPac:
   DEFINE INPUT PARAMETER icCLIType                 AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icCopyServicesFromCliType AS CHARACTER NO-UNDO.   
   
   DEFINE BUFFER bf_CTServPac_CopyFrom  FOR CTServPac.
   DEFINE BUFFER bf_CTServEl_CopyFrom   FOR CTServEl.
   DEFINE BUFFER bf_CTServAttr_CopyFrom FOR CTServAttr.
   
   FOR EACH bf_CTServPac_CopyFrom WHERE bf_CTServPac_CopyFrom.Brand = gcBrand AND bf_CTServPac_CopyFrom.CLIType = icCopyServicesFromCliType NO-LOCK
       ON ERROR UNDO, THROW:

      IF LOOKUP(bf_CTServPac_CopyFrom.ServPac,"Y_HURP,D_HOTL") = 0 AND 
         (bf_CTServPac_CopyFrom.ServPac BEGINS "C_" OR bf_CTServPac_CopyFrom.ServPac BEGINS "Y_" OR bf_CTServPac_CopyFrom.ServPac BEGINS "D_") THEN 
         NEXT.

      CREATE CTServPac.
      BUFFER-COPY bf_CTServPac_CopyFrom EXCEPT CLIType TO CTServPac
         ASSIGN CTServPac.CLIType  = icCLIType.            
          
      FOR EACH bf_CTServEl_CopyFrom WHERE bf_CTServEl_CopyFrom.Brand = gcBrand AND bf_CTServEl_CopyFrom.CLIType = bf_CTServPac_CopyFrom.CLIType AND bf_CTServEl_CopyFrom.ServPac = bf_CTServPac_CopyFrom.ServPac NO-LOCK
          ON ERROR UNDO, THROW:

         CREATE CTServEl.
         BUFFER-COPY bf_CTServEl_CopyFrom EXCEPT CTServEl CLIType TO CTServEl
            ASSIGN 
               CTServEl.CTServEl = NEXT-VALUE(CTServEl)
               CTServEl.CLIType  = icCLIType.

         FIND ServCom WHERE ServCom.Brand = gcBrand AND ServCom.ServCom = CTServEl.ServCom NO-LOCK NO-ERROR.
         IF AVAILABLE ServCom AND ServCom.ServAttr = TRUE THEN
         DO:
             FOR EACH bf_CTServAttr_CopyFrom WHERE bf_CTServAttr_CopyFrom.CTServEl = bf_CTServEl_CopyFrom.CTServEl NO-LOCK
                 ON ERROR UNDO, THROW:
                CREATE CTServAttr.
                BUFFER-COPY bf_CTServAttr_CopyFrom EXCEPT CTServEl TO CTServAttr
                   ASSIGN CTServAttr.CTServEl = CTServEl.CTServEl.
             END.
         END. 
      END.
   END.  
      
   RETURN "".
                             
END PROCEDURE.

PROCEDURE pSLGAnalyse:
    DEFINE INPUT PARAMETER icCliType             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icCliTypeCopyFrom     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icMobileBaseBundle    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icFixedLineBaseBundle AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icAllowedBundles      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE liCnt                         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE liCount                       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcSubsTypePrefix              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcBillCodeList                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcConvergentBillCodeList      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcMxValue                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcBaseBundleOfCopyFromCliType AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf_Matrix     FOR Matrix.
    DEFINE BUFFER bf_MxItem     FOR MxItem.
    DEFINE BUFFER bf_SLGAnalyse FOR SLGAnalyse.
    DEFINE BUFFER bf_CliTypeCF  FOR CliType.

    ASSIGN
        lcBillCodeList           = "14100001,10100001,10100003,10100005,CFOTHER,CFYOIGO" 
        lcConvergentBillCodeList = "F10100003,F10100005"
        lcSubsTypePrefix = (IF icCliType BEGINS "CONTDSL" THEN 
                                "CONTDSL*,CONT*" 
                            ELSE IF icCliType BEGINS "CONTFH" THEN 
                                (icCliType + ",CONT*")     
                            ELSE IF icCliType BEGINS "CONT" THEN 
                                "CONT*" 
                            ELSE IF icCliType BEGINS "TRAJ" THEN 
                                "TRAJ*" 
                            ELSE "").

    FIND FIRST bf_CliTypeCF WHERE bf_CliTypeCF.Brand = gcBrand AND bf_CliTypeCF.ClIType = icCliTypeCopyFrom NO-LOCK NO-ERROR.    
    IF AVAIL bf_CliTypeCF THEN 
        ASSIGN lcBaseBundleOfCopyFromCliType = bf_CliTypeCF.BaseBundle.    

    /* Get list of all allowed bundles/bonos for the subscription type */    
    IF lcSubsTypePrefix > "" THEN
    DO liCount = 1 TO NUM-ENTRIES(lcSubsTypePrefix)
       ON ERROR UNDO, THROW:
        FOR EACH bf_Matrix WHERE bf_Matrix.Brand = gcBrand AND bf_Matrix.MXKey = "PERCONTR" NO-LOCK By bf_Matrix.Prior
            ON ERROR UNDO, THROW:
            
            IF bf_Matrix.MXRes <> 1 THEN 
                NEXT.                                           
            
            ASSIGN lcMxValue = ENTRY(liCount,lcSubsTypePrefix).
            
            FOR EACH bf_MxItem WHERE bf_MxItem.MxSeq = bf_Matrix.MxSeq AND bf_MxItem.MxName = "SubsTypeTo" AND bf_MxItem.MxValue = lcMxValue NO-LOCK
                ON ERROR UNDO, THROW:
                FOR EACH MxItem WHERE MxItem.MxSeq = bf_MxItem.MxSeq AND MxItem.MXName = "PerContract" NO-LOCK
                    ON ERROR UNDO, THROW:
                    FIND FIRST DayCampaign WHERE Daycampaign.Brand = gcBrand AND Daycampaign.DCEvent = MxItem.MxValue NO-LOCK NO-ERROR.
                    IF AVAIL DayCampaign AND LOOKUP(DayCampaign.DcType, {&PERCONTRACT_RATING_PACKAGE} + ",6") > 0 AND LOOKUP(DayCampaign.DCEvent, icAllowedBundles) = 0 THEN                 
                        ASSIGN icAllowedBundles = icAllowedBundles + (IF icAllowedBundles <> "" THEN "," ELSE "") + DayCampaign.DCEvent.
                END.
            END.

        END.
    END.    
    
    IF icAllowedBundles > "" THEN 
    DO liCount = 1 TO NUM-ENTRIES(icAllowedBundles)
       ON ERROR UNDO, THROW:

        IF ENTRY(liCount,icAllowedBundles) = "CONTDSL" THEN
        DO liCnt = 1 TO NUM-ENTRIES(lcConvergentBillCodeList)
           ON ERROR UNDO, THROW:

            FIND FIRST bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand             = gcBrand                               AND 
                                           bf_SLGAnalyse.ServiceLimitGroup = "CONTDSL"                             AND
                                           bf_SLGAnalyse.CLIType           = icCliTypeCopyFrom                     AND
                                           bf_SLGAnalyse.ValidTo           >= TODAY                                AND 
                                           bf_SLGAnalyse.BillCode          = ENTRY(liCnt,lcConvergentBillCodeList) NO-LOCK NO-ERROR.
            IF AVAIL bf_SLGAnalyse THEN
            DO:
                CREATE SLGAnalyse.
                BUFFER-COPY bf_SLGAnalyse EXCEPT CliType ValidFrom ValidTo TO SLGAnalyse
                    ASSIGN
                        SLGAnalyse.CliType   = icCliType
                        SLGAnalyse.ValidFrom = TODAY
                        SLGAnalyse.ValidTo   = DATE(12,31,2049).
            END.
        END.          
        ELSE IF ENTRY(liCount,icAllowedBundles) BEGINS "CONTFH" THEN
        DO liCnt = 1 TO NUM-ENTRIES(lcConvergentBillCodeList)
           ON ERROR UNDO, THROW:

            FIND FIRST bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand             = gcBrand                               AND 
                                           bf_SLGAnalyse.ServiceLimitGroup BEGINS "CONTFH"                         AND
                                           bf_SLGAnalyse.CLIType           = icCliTypeCopyFrom                     AND
                                           bf_SLGAnalyse.ValidTo           >= TODAY                                AND 
                                           bf_SLGAnalyse.BillCode          = ENTRY(liCnt,lcConvergentBillCodeList) NO-LOCK NO-ERROR.
            IF AVAIL bf_SLGAnalyse THEN
            DO:
                CREATE SLGAnalyse.
                BUFFER-COPY bf_SLGAnalyse EXCEPT CliType ValidFrom ValidTo TO SLGAnalyse
                    ASSIGN
                        SLGAnalyse.CliType              = icCliType
                        SLGAnalyse.ServiceLimitGroup    = icFixedLineBaseBundle 
                        SLGAnalyse.ValidFrom            = TODAY
                        SLGAnalyse.ValidTo              = DATE(12,31,2049).
            END.
        END.          
        ELSE IF ENTRY(liCount,icAllowedBundles) = "VOICE100" THEN
        DO liCnt = 1 TO NUM-ENTRIES(lcBillCodeList)
           ON ERROR UNDO, THROW:
            
            IF ENTRY(liCount,lcBillCodeList) = "14100001" THEN 
                NEXT.

            FIND FIRST bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand            = gcBrand                       AND 
                                           bf_SLGAnalyse.BillCode         = ENTRY(liCnt,lcBillCodeList) AND
                                           bf_SLGAnalyse.CLIType          = icCliTypeCopyFrom             AND
                                           bf_SLGAnalyse.ValidTo          >= TODAY                        AND 
                                           bf_SLGAnalyse.ServiceLimitGroup = "VOICE100"                   NO-LOCK NO-ERROR.
            IF AVAIL bf_SLGAnalyse THEN
            DO:
                CREATE SLGAnalyse.
                BUFFER-COPY bf_SLGAnalyse EXCEPT CliType ValidFrom ValidTo TO SLGAnalyse
                    ASSIGN
                        SLGAnalyse.CliType           = icCliType
                        SLGAnalyse.ValidFrom         = TODAY
                        SLGAnalyse.ValidTo           = DATE(12,31,2049).
            END.
        END.
        ELSE IF ENTRY(liCount,icAllowedBundles) = "FREE100MINUTES" THEN
        DO liCnt = 1 TO NUM-ENTRIES(lcBillCodeList)
           ON ERROR UNDO, THROW:
            
            IF ENTRY(liCount,lcBillCodeList) = "14100001" THEN 
                NEXT.

            FIND FIRST bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand            = gcBrand                       AND 
                                           bf_SLGAnalyse.BillCode         = ENTRY(liCnt,lcBillCodeList) AND
                                           bf_SLGAnalyse.CLIType          = icCliTypeCopyFrom             AND
                                           bf_SLGAnalyse.ValidTo          >= TODAY                        AND 
                                           bf_SLGAnalyse.ServiceLimitGroup = "FREE100MINUTES"             NO-LOCK NO-ERROR.
            IF AVAIL bf_SLGAnalyse THEN
            DO:
                CREATE SLGAnalyse.
                BUFFER-COPY bf_SLGAnalyse EXCEPT CliType ValidFrom ValidTo TO SLGAnalyse
                    ASSIGN
                        SLGAnalyse.CliType           = icCliType
                        SLGAnalyse.ValidFrom         = TODAY
                        SLGAnalyse.ValidTo           = DATE(12,31,2049).
            END.
        END.
        ELSE
        DO:
            /* Default bundles for CONT* or TRAJ* subscription types */
            FIND FIRST bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand             = gcBrand                         AND 
                                           bf_SLGAnalyse.ServiceLimitGroup = ENTRY(liCount,icAllowedBundles) AND
                                           bf_SLGAnalyse.CLIType           = icCliTypeCopyFrom               AND
                                           bf_SLGAnalyse.ValidTo           >= TODAY                          NO-LOCK NO-ERROR.            
            IF AVAIL bf_SLGAnalyse THEN
            DO:
                CREATE SLGAnalyse.
                BUFFER-COPY bf_SLGAnalyse EXCEPT CliType ValidFrom ValidTo TO SLGAnalyse
                    ASSIGN
                        SLGAnalyse.CliType   = icCliType                        
                        SLGAnalyse.ValidFrom = TODAY
                        SLGAnalyse.ValidTo   = DATE(12,31,2049).
            END.
        END.                                         
    END.    

    /* Voice national related */
    DO liCount = 1 TO NUM-ENTRIES(lcBillCodeList)
       ON ERROR UNDO, THROW:

        IF (LOOKUP("FREE100MINUTES",icAllowedBundles) > 0 OR LOOKUP("VOICE100",icAllowedBundles) > 0) AND ENTRY(liCount,lcBillCodeList) <> "14100001" THEN 
            NEXT.

        FIND FIRST bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand             = gcBrand                       AND 
                                       bf_SLGAnalyse.BillCode          = ENTRY(liCount,lcBillCodeList) AND
                                       bf_SLGAnalyse.CLIType           = icCliTypeCopyFrom             AND    
                                       (IF lcBaseBundleOfCopyFromCliType > "" THEN 
                                           bf_SLGAnalyse.ServiceLimitGroup = lcBaseBundleOfCopyFromCliType 
                                        ELSE 
                                           TRUE)                                                      AND                                  
                                       bf_SLGAnalyse.ValidTo          >= TODAY                        NO-LOCK NO-ERROR.
        IF AVAIL bf_SLGAnalyse THEN
        DO:
            CREATE SLGAnalyse.
            BUFFER-COPY bf_SLGAnalyse EXCEPT CliType ServiceLimitGroup ValidFrom ValidTo TO SLGAnalyse
                ASSIGN
                    SLGAnalyse.CliType           = icCliType 
                    SLGAnalyse.ServiceLimitGroup = (IF icMobileBaseBundle > "" THEN icMobileBaseBundle ELSE icCliType)
                    SLGAnalyse.ValidFrom         = TODAY
                    SLGAnalyse.ValidTo           = DATE(12,31,2049).
        END.
    END.

    RETURN "".

END PROCEDURE.


PROCEDURE pDayCampaign:
   DEFINE PARAMETER BUFFER ttDayCampaign FOR ttDayCampaign.   
   
   IF NOT CAN-FIND(FIRST DayCampaign WHERE DayCampaign.Brand = gcBrand AND DayCampaign.DCEvent = ttDayCampaign.DCEvent) THEN
   DO:
      CREATE DayCampaign.
      ASSIGN 
         DayCampaign.Brand           = gcBrand
         DayCampaign.DCEvent         = ttDayCampaign.DCEvent 
         DayCampaign.DCName          = ttDayCampaign.DCName
         DayCampaign.PayType         = ttDayCampaign.PayType
         DayCampaign.ValidFrom       = TODAY 
         DayCampaign.ValidTo         = DATE(12,31,2049)
         DayCampaign.StatusCode      = 1           /* Default value Active */
         DayCampaign.DCType          = (IF ttDayCampaign.DCType = "ServicePackage" THEN "1" ELSE IF ttDayCampaign.DCType = "PackageWithCounter" THEN "4" ELSE "7")
         DayCampaign.CCN             = (IF ttDayCampaign.DCType = "PackageWithCounter" THEN 93 ELSE 0)         
         DayCampaign.InstanceLimit   = 1
         DayCampaign.BillCode        = ttDayCampaign.BillCode          
         DayCampaign.InclUnit        = (IF ttDayCampaign.DCType = "PackageWithCounter" THEN 4 ELSE 1) 
         DayCampaign.CalcMethod      = (IF ttDayCampaign.DCType = "PackageWithCounter" THEN 4 ELSE 1)  
         DayCampaign.InclStartCharge = YES                          
         DayCampaign.MaxChargeIncl   = 0                            
         DayCampaign.MaxChargeExcl   = 0
         DayCampaign.Effective       = INTEGER(fTMSCodeValue("Daycampaign","Effective","PerContr"))         
         DayCampaign.DurType         = (IF ttDayCampaign.SLCreated THEN 1 ELSE 4)
         DayCampaign.DurMonth        = 0
         DayCampaign.DurUnit         = (IF ttDayCampaign.DCType = "PackageWithCounter" THEN 0 ELSE 1)
         DayCampaign.WeekDay         = ""
         DayCampaign.BundleUpsell    = ttDayCampaign.UpSell
         DayCampaign.FeeModel        = ttDayCampaign.BillCode
         DayCampaign.ModifyFeeModel  = ""                          
         DayCampaign.TermFeeModel    = ""                          
         DayCampaign.TermFeeCalc     = 0.
         
      IF ttDayCampaign.DataLimit > 0 THEN   
         RUN pDCServicePackage(ttDayCampaign.DCEvent, ttDayCampaign.BonoSupport).
   END.

   RETURN "".    
   
END PROCEDURE.


PROCEDURE pDCServicePackage:
   DEFINE INPUT PARAMETER icDCEvent     AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER ilBonoSupport AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE liPackageID   AS INTEGER NO-UNDO.
   DEFINE VARIABLE liComponentID AS INTEGER NO-UNDO.    
    
   FIND LAST DCServicePackage USE-INDEX DCServicePackageID NO-LOCK NO-ERROR.    
   IF AVAILABLE DCServicePackage THEN 
      ASSIGN liPackageID = DCServicePackage.DCServicePackageID + 1.
   ELSE 
      ASSIGN liPackageID = 1.
   
   FIND LAST DCServiceComponent USE-INDEX DCServiceComponentID NO-LOCK NO-ERROR.
   IF AVAILABLE DCServiceComponent THEN 
      liComponentID = DCServiceComponent.DCServiceComponentID + 1.
   ELSE 
      liComponentID = 1.    

   CREATE DCServicePackage.
   ASSIGN                            
      DCServicePackage.Brand              = gcBrand 
      DCServicePackage.DCEvent            = icDCEvent 
      DCServicePackage.DCServicePackageID = liPackageID
      DCServicePackage.ServPac            = "SHAPER"                    
      DCServicePackage.FromDate           = TODAY 
      DCServicePackage.ToDate             = DATE(12,31,2049).   
   
   CREATE DCServiceComponent.
   ASSIGN 
      DCServiceComponent.DCServicePackageID   = DCServicePackage.DCServicePackageID 
      DCServiceComponent.DCServiceComponentID = liComponentID      
      DCServiceComponent.ServCom              = DCServicePackage.ServPac
      DCServiceComponent.DefValue             = 1
      DCServiceComponent.DefParam             = (IF ilBonoSupport THEN (icDCEvent + "#ADDBUNDLE") ELSE icDCEvent)
      DCServiceComponent.FromDate             = TODAY 
      DCServiceComponent.ToDate               = DATE(12,31,2049).   
       
    RETURN "".
       
END PROCEDURE.


PROCEDURE pFeeModel:
   DEFINE INPUT  PARAMETER icFeeModel     AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER icFeeModelName AS CHARACTER NO-UNDO.   
   DEFINE OUTPUT PARAMETER olCreated      AS LOGICAL   NO-UNDO.

   FIND FIRST FeeModel WHERE FeeModel.Brand = gcBrand AND FeeModel.FeeModel = icFeeModel NO-LOCK NO-ERROR.    
   IF NOT AVAILABLE FeeModel THEN
   DO:
      CREATE FeeModel.
      ASSIGN 
         FeeModel.Brand    = gcBrand
         FeeModel.FeeModel = icFeeModel
         FeeModel.FeeName  = icFeeModelName               
         FeeModel.FMGroup  = 0
         olCreated         = True.
   END.

   RETURN "".
                 
END PROCEDURE. 

   
PROCEDURE pFMItem:   
   DEFINE PARAMETER BUFFER ttFMItem  FOR ttFMItem.   
   DEFINE PARAMETER BUFFER ttCliType FOR ttCliType.   
   
   DEFINE VARIABLE lcRatePlan AS CHARACTER NO-UNDO.   
   
   IF ttCliType.TariffBundle NE "" THEN 
   DO:
      FIND FIRST CLIType WHERE CLIType.Brand = gcBrand AND CLIType.CLIType = ttCliType.ParentTariff NO-LOCK NO-ERROR.
      IF AVAILABLE CLIType THEN
         ASSIGN lcRatePlan = CLIType.PricePlan.
   END.
   
   ASSIGN lcRatePlan = (IF lcRatePlan NE "" THEN lcRatePlan ELSE REPLACE(ttCliType.CliType,"CONT","CONTRATO")).

   FIND FIRST RatePlan WHERE RatePlan.Brand = gcBrand AND RatePlan.RatePlan = lcRatePlan NO-LOCK NO-ERROR.           
   IF AVAIL RatePlan THEN   
      FIND FIRST PListConf WHERE PListConf.Brand = gcBrand AND PListConf.RatePlan = RatePlan.RatePlan AND PListConf.PriceList BEGINS "CONTRATO" NO-LOCK NO-ERROR.   
   
   CREATE FMItem. 
   ASSIGN     
      FMItem.Brand             = gcBrand
      FMItem.FeeModel          = ttFMItem.FeeModel
      FMItem.BillCode          = ttFMItem.BillCode
      FMItem.PriceList         = PListConf.PriceList WHEN AVAILABLE PListConf 
      FMItem.FromDate          = TODAY       
      FMItem.ToDate            = DATE(12,31,2049)
      FMItem.BillType          = "MF"                  
      FMItem.Interval          = 1                   
      FMItem.BillCycle         = 2                   
      FMItem.FFItemQty         = 0 
      FMItem.FFEndDate         = ? 
      FMItem.Amount            = ttFMItem.Amount   
      FMItem.FirstMonthBR      = ttFMItem.FirstMonthBR
      FMItem.BrokenRental      = ttFMItem.BrokenRental
      FMItem.ServiceLimitGroup = "".

   RETURN "".
   
END PROCEDURE.    


PROCEDURE pServiceLimitGroup:
   DEFINE PARAMETER BUFFER ttServiceLimitGroup FOR ttServiceLimitGroup.   

   FIND FIRST ServiceLimitGroup WHERE ServiceLimitGroup.Brand = gcBrand AND ServiceLimitGroup.GroupCode = ttServiceLimitGroup.GroupCode NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ServiceLimitGroup THEN
   DO:
      CREATE ServiceLimitGroup.
      ASSIGN 
         ServiceLimitGroup.Brand     = gcBrand
         ServiceLimitGroup.GroupCode = ttServiceLimitGroup.GroupCode    
         ServiceLimitGroup.GroupName = ttServiceLimitGroup.GroupName    
         ServiceLimitGroup.ValidFrom = TODAY 
         ServiceLimitGroup.ValidTo   = DATE(12,31,2049).
   END.  

   RETURN "".
   
END PROCEDURE.
 

PROCEDURE pServiceLimit:
   DEFINE PARAMETER BUFFER ttServiceLimit FOR ttServiceLimit.      
   DEFINE OUTPUT PARAMETER oiSLSeq      AS INTEGER   NO-UNDO.

   DEFINE VARIABLE liInclUnit AS INTEGER NO-UNDO.
   
   DEFINE BUFFER bf_ServiceLimit FOR ServiceLimit.

   IF NUM-ENTRIES(ttServiceLimit.SLCode,"_") = 1 OR ENTRY(2,ttServiceLimit.SLCode,"_") EQ "DATA" THEN 
      liInclUnit = 4. 
   ELSE IF ENTRY(2,ttServiceLimit.SLCode,"_") EQ "QTY" THEN
      liInclUnit = 7. 
   ELSE 
      liInclUnit = 1.
   
   FIND FIRST ServiceLimit WHERE ServiceLimit.GroupCode = ttServiceLimit.GroupCode AND 
                                 ServiceLimit.SLCode    = ttServiceLimit.SLCode    AND 
                                 ServiceLimit.DialType  = ttServiceLimit.DialType  AND 
                                 ServiceLimit.ValidFrom <= TODAY                   AND
                                 ServiceLimit.ValidTo   >= TODAY                   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ServiceLimit THEN 
   DO:                                  
      FIND LAST bf_ServiceLimit NO-LOCK USE-INDEX SLSeq NO-ERROR.               
      IF AVAILABLE bf_ServiceLimit THEN 
         ASSIGN oiSLSeq = bf_ServiceLimit.SLSeq + 1.          
      ELSE 
         ASSIGN oiSLSeq = 1.
                  
      CREATE ServiceLimit.
      ASSIGN 
         ServiceLimit.GroupCode      = ttServiceLimit.GroupCode
         ServiceLimit.SLCode         = ttServiceLimit.SLCode                                                    
         ServiceLimit.SLSeq          = oiSLSeq 
         ServiceLimit.SLName         = ttServiceLimit.SLName                             
         ServiceLimit.DialType       = ttServiceLimit.DialType
         ServiceLimit.InclAmt        = ttServiceLimit.InclAmt                  
         ServiceLimit.InclUnit       = liInclUnit
         ServiceLimit.BDestLimit     = 0
         ServiceLimit.ValidFrom      = TODAY 
         ServiceLimit.ValidTo        = DATE(12,31,2049)
         ServiceLimit.FirstMonthCalc = ttServiceLimit.FirstMonthCalc
         ServiceLimit.LastMonthCalc  = ttServiceLimit.LastMonthCalc
         Servicelimit.Web            = 1.
   END.
   
   RETURN "".
      
END PROCEDURE.


PROCEDURE pServiceLimitTarget:
    DEFINE PARAMETER BUFFER ttServiceLimitTarget FOR ttServiceLimitTarget.          
    DEFINE INPUT PARAMETER iiSLSeq       AS INTEGER   NO-UNDO. 
    
    FIND FIRST ServiceLimitTarget WHERE ServiceLimitTarget.SLSeq = iiSLSeq AND ServiceLimitTarget.ServiceLMember = ttServiceLimitTarget.ServiceLMember NO-LOCK NO-ERROR.
    IF NOT AVAIL ServiceLimitTarget THEN 
    DO:    
        CREATE ServiceLimitTarget. 
        ASSIGN 
            ServiceLimitTarget.Slseq          = iiSLSeq
            ServiceLimitTarget.ServiceLMember = ttServiceLimitTarget.ServiceLMember
            ServiceLimitTarget.InsideRate     = ttServiceLimitTarget.InSideRate    
            ServiceLimitTarget.outsideRate    = ttServiceLimitTarget.OutSideRate.
    END.

    RETURN "".

END PROCEDURE.


PROCEDURE pProgLimit:
    DEFINE PARAMETER BUFFER ttProgLimit FOR ttProgLimit.
    DEFINE INPUT PARAMETER iiSLSeq       AS INTEGER   NO-UNDO. 

    FIND FIRST ProgLimit WHERE ProgLimit.GroupCode = ttProgLimit.GroupCode AND ProgLimit.SLSeq = iiSLSeq AND ProgLimit.BDest = ttProgLimit.BDest NO-LOCK NO-ERROR.
    IF NOT AVAIL ProgLimit THEN 
    DO:
        CREATE ProgLimit.
        ASSIGN 
            ProgLimit.GroupCode = ttProgLimit.GroupCode
            ProgLimit.SLSeq     = iiSLSeq
            ProgLimit.ValidFrom = TODAY
            ProgLimit.ValidTo   = DATE(12,31,2049)
            ProgLimit.LimitFrom = ttProgLimit.LimitFrom
            ProgLimit.LimitTo   = ttProgLimit.LimitTo
            ProgLimit.BDest     = ttProgLimit.BDest.
    END.

    RETURN "".

END PROCEDURE.


PROCEDURE pBDestination:
    DEFINE PARAMETER BUFFER ttBDest FOR ttBDest.       

    DEFINE VARIABLE liBDLValue     AS INTEGER   NO-UNDO.
    DEFINE BUFFER bf_BDest FOR BDest.

    FIND FIRST BDest WHERE BDest.Brand = gcBrand AND BDest.BDest = ttBDest.BDest NO-LOCK NO-ERROR. 
    IF NOT AVAILABLE BDest THEN 
    DO:    
        FIND LAST bf_BDest USE-INDEX BDestID NO-LOCK NO-ERROR.    
        IF AVAILABLE bf_BDest THEN 
            liBDLValue = bf_BDest.BDestID + 1.
        ELSE 
            liBDLValue = 1.
     
        CREATE BDest. 
        ASSIGN 
            BDest.Brand    = gcBrand    
            BDest.BDestID  = liBDLValue
            BDest.BDest    = ttBDest.BDest
            BDest.BDName   = ttBDest.BDName
            BDest.DestType = 0 
            BDest.CCN      = ttBDest.CCN
            BDest.Class    = 1
            BDest.FromDate = TODAY 
            BDest.ToDate   = DATE(12,31,2049).      
    END. 
    
    RETURN "".
   
END PROCEDURE.


PROCEDURE pRequestAction:
   DEFINE INPUT PARAMETER icCLIType                  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icMobileBundles            AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icFixedLineBundles         AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icBundlesForActivateOnSTC  AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER icServicesRecreated        AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE liCount           AS INTEGER   NO-UNDO.   
   DEFINE VARIABLE liRequestType     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcRequestTypeList AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE liAction          AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcAction          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcActionType      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcActionKey       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcActionList      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcActionKeyList   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcActionTypeList  AS CHARACTER NO-UNDO.

   /* TODO: SMS related configuration */
   FIND FIRST CliType WHERE ClIType.Brand = gcBrand AND CliType.ClIType = icCLIType NO-LOCK NO-ERROR.
   IF NOT AVAIL CliType THEN 
      UNDO, THROW NEW Progress.Lang.AppError("CLIType doesn't exists for creating request actions", 1).

   /* This is to add mobile packages and fixed line packages to RequestType's subscription creation, fixedline activation & STC */    
   fPrepareRequestActionParam(icMobileBundles,
                              icFixedLineBundles,
                              icBundlesForActivateOnSTC,
                              icServicesRecreated,
                              OUTPUT lcRequestTypeList,
                              OUTPUT lcActionList,
                              OUTPUT lcActionKeyList,
                              OUTPUT lcActionTypeList).      

   IF lcActionTypeList = "" THEN
      UNDO, THROW NEW Progress.Lang.AppError('Failed to add configurations to request action', 1).      

   DO liCount = 1 TO NUM-ENTRIES(lcRequestTypeList)
      ON ERROR UNDO, THROW:

      ASSIGN
         lcAction      = ENTRY(liCount, lcActionList)
         liAction      = (IF lcAction = "CREATE" THEN 
                             1 
                          ELSE IF lcAction = "TERMINATE" THEN 
                             2 
                          ELSE IF lcAction = "RECREATE" THEN 
                             3 
                          ELSE 
                             1)
         lcActionKey   = ENTRY(liCount, lcActionKeyList)
         lcActionType  = ENTRY(liCount, lcActionTypeList)
         liRequestType = INT(ENTRY(liCount, lcRequestTypeList)).

      FIND FIRST RequestAction WHERE RequestAction.Brand      = gcBrand         AND
                                     RequestAction.ReqType    = liRequestType   AND
                                     RequestAction.CliType    = CliType.CliType AND
                                     RequestAction.ActionType = lcActionType    AND
                                     RequestAction.ActionKey  = lcActionKey     AND
                                     RequestAction.Action     = liAction        AND
                                     RequestAction.ValidFrom <= TODAY           AND
                                     RequestAction.ValidTo   >= TODAY           NO-LOCK NO-ERROR.
      IF NOT AVAIL RequestAction THEN
      DO:
         CREATE RequestAction.
         ASSIGN     
            RequestAction.Brand           = gcBrand
            RequestAction.RequestActionID = fGetNextRequestActionSequence()
            RequestAction.ReqType         = liRequestType
            RequestAction.CLIType         = CliType.CliType
            RequestAction.PayType         = 0
            RequestAction.Action          = liAction
            RequestAction.ActionKey       = lcActionKey
            RequestAction.ActionType      = lcActionType           
            RequestAction.ValidFrom       = TODAY
            RequestAction.ValidTo         = DATE(12,31,2049). 
      END.
   END.  

   FIND FIRST RequestAction WHERE RequestAction.Brand      = gcBrand         AND 
                                  RequestAction.PayType    = 1               AND 
                                  RequestAction.ReqType    = 0               AND 
                                  RequestAction.Action     = 13              AND 
                                  RequestAction.ActionType = "SMS"           AND 
                                  RequestAction.ActionKey  = "STC_Requested" AND 
                                  RequestAction.ValidTo   >= TODAY           NO-LOCK NO-ERROR.
   IF AVAIL RequestAction THEN 
   DO:
       FIND FIRST RequestActionRule WHERE RequestActionRule.RequestActionID = RequestAction.RequestActionID AND RequestActionRule.ParamField = "ReqCParam2" AND RequestActionRule.ToDate >= TODAY EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
       IF AVAIL RequestActionRule AND LOOKUP(icMobileBundles, RequestActionRule.ParamValue) = 0 THEN 
           ASSIGN RequestActionRule.ParamValue = RequestActionRule.ParamValue + (IF RequestActionRule.ParamValue <> '' THEN "," ELSE "") + icMobileBundles.
   END.

   RETURN "".

END PROCEDURE.


PROCEDURE pMatrix:
   DEFINE INPUT PARAMETER icCLIType          AS CHARACTER NO-UNDO.    
   DEFINE INPUT PARAMETER icAllowedBundles   AS CHARACTER NO-UNDO.

   DEFINE VARIABLE liCount AS INTEGER NO-UNDO.

   FIND FIRST Matrix WHERE Matrix.Brand = gcBrand AND Matrix.MXKey = "PERCONTR" AND Matrix.MxName = icCLIType NO-LOCK NO-ERROR.
   IF NOT AVAIL Matrix THEN 
   DO:
       CREATE Matrix.
       ASSIGN
          Matrix.Brand  = gcBrand
          Matrix.MXSeq  = NEXT-VALUE(imsi)
          Matrix.mxkey  = "PERCONTR"
          Matrix.mxname = icCLIType
          Matrix.prior  = fGetNextMatrixPriority("PERCONTR")
          Matrix.mxres  = 1.
       
       CREATE MXItem.
       ASSIGN
          MXItem.MxSeq   = Matrix.MXSeq
          MXItem.MxValue = icCLIType
          MXItem.MxName  = "SubsTypeTo".   
   END.
      
   DO liCount = 1 TO NUM-ENTRIES(icAllowedBundles)
      ON ERROR UNDO, THROW:
      
      IF LOOKUP(ENTRY(liCount,icAllowedBundles), "CONTDSL") > 0 THEN 
          NEXT.
      
      FIND FIRST MxItem WHERE MxItem.MxSeq = Matrix.MXSeq AND MxItem.MxName = "PerContract" AND MxItem.MxValue = ENTRY(liCount,icAllowedBundles) NO-LOCK NO-ERROR.
      IF NOT AVAIL MxItem THEN 
      DO:      
         CREATE MXItem.
         ASSIGN
            MXItem.MxSeq   = Matrix.MXSeq
            MXItem.MxValue = ENTRY(liCount,icAllowedBundles)
            MXItem.MxName  = "PerContract".   
      END.   

   END.

   RETURN "".

END PROCEDURE.

PROCEDURE pTMRItemValue:
    DEFINE PARAMETER BUFFER ttTMRItemValue FOR ttTMRItemValue.
    
    FIND FIRST TMRItemValue WHERE TMRItemValue.TMRuleSeq         = ttTMRItemValue.TMRuleSeq                            AND 
                                  TMRItemValue.CounterItemValues = ttTMRItemValue.BDest + "," + ttTMRItemValue.CliType AND
                                  TMRItemValue.ToDate           >= TODAY                                               NO-LOCK NO-ERROR.     
    IF NOT AVAIL TMRItemValue THEN 
    DO:                                  
        CREATE TMRItemValue.
        ASSIGN 
            TMRItemValue.TMRuleSeq         = ttTMRItemValue.TMRuleSeq
            TMRItemValue.CounterItemValues = ttTMRItemValue.BDest + "," + ttTMRItemValue.CliType
            TMRItemValue.FromDate          = TODAY
            TMRItemValue.ToDate            = DATE(12,31,2049).        
    END.

    RETURN "".

END PROCEDURE.


PROCEDURE pUpdateTMSParam:
   DEFINE INPUT PARAMETER icParamCode AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER icCLIType   AS CHARACTER NO-UNDO.   

   FIND FIRST TMSParam WHERE TMSParam.Brand = gcBrand  AND TMSParam.ParamCode EQ icParamCode NO-LOCK NO-ERROR.
   IF AVAIL TMSParam THEN 
   DO:
      IF LOOKUP(icCliType, TMSParam.CharVal) = 0 THEN 
      DO:
         FIND CURRENT TMSParam EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF LOCKED TMSParam THEN 
            UNDO, THROW NEW Progress.Lang.AppError(icParamCode + ' TMSParam failed to update', 1).

         IF AVAIL TMSParam THEN      
            ASSIGN TMSParam.CharVal = TMSParam.CharVal + (IF TMSParam.CharVal <> "" THEN "," ELSE "") + icCLIType. 
      END.         
   END.

   RETURN "".

END PROCEDURE.
