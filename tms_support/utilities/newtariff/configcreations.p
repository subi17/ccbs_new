/*------------------------------------------------------------------------
  MODULE .......: configcreations.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: Tue Feb 10 15:36:57 EET 2015
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/
BLOCK-LEVEL ON ERROR UNDO, THROW.
/* ***************************  Definitions  ************************** */
{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/ftransdir.i}
{utilities/newtariff/tariffconfig.i}
{utilities/newtariff/tariffcons.i}
{Func/fixedlinefunc.i}

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

FUNCTION fGetNextMXSeq RETURNS INTEGER ():

   DEFINE BUFFER Matrix FOR Matrix.

   FOR EACH Matrix NO-LOCK BY Matrix.MXSeq DESCENDING:
     RETURN Matrix.MXSeq + 1.
   END.

   RETURN 1.

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
  (INPUT icCliType                  AS CHARACTER,
   INPUT ilPostpaid                 AS LOGICAL  ,
   INPUT icMobileBundles            AS CHARACTER,
   INPUT icFixedLineBundles         AS CHARACTER,
   INPUT icBundlesForActivateOnSTC  AS CHARACTER,
   INPUT icServicesRecreated        AS CHARACTER,
   OUTPUT ocReqTypeList             AS CHARACTER,
   OUTPUT ocActionList              AS CHARACTER,
   OUTPUT ocActionKeyList           AS CHARACTER,
   OUTPUT ocActionTypeList          AS CHARACTER):

   DEFINE VARIABLE liCnt             AS INTEGER   NO-UNDO.   
   DEFINE VARIABLE liCount           AS INTEGER   NO-UNDO.   

   IF ilPostpaid THEN
   DO:
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
                ocReqTypeList     = ocReqTypeList + (IF ocReqTypeList <> "" THEN "," ELSE "") + STRING({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE})  
                ocActionList      = ocActionList      + (IF ocActionList      <> "" THEN "," ELSE "") + "CREATE"
                ocActionKeyList   = ocActionKeyList   + (IF ocActionKeyList   <> "" THEN "," ELSE "") + ENTRY(liCount, icBundlesForActivateOnSTC)
                ocActionTypeList  = ocActionTypeList  + (IF ocActionTypeList  <> "" THEN "," ELSE "") + "DayCampaign".
       END.
   
       IF icServicesRecreated > "" THEN 
       DO liCount = 1 TO NUM-ENTRIES(icServicesRecreated):
          ASSIGN 
                ocReqTypeList     = ocReqTypeList + (IF ocReqTypeList <> "" THEN "," ELSE "") + STRING({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE})  
                ocActionList      = ocActionList      + (IF ocActionList      <> "" THEN "," ELSE "") + "RECREATE"
                ocActionKeyList   = ocActionKeyList   + (IF ocActionKeyList   <> "" THEN "," ELSE "") + ENTRY(liCount, icServicesRecreated)
                ocActionTypeList  = ocActionTypeList  + (IF ocActionTypeList  <> "" THEN "," ELSE "") + "CTServPac".
       END.
   END.
   ELSE
   DO:
      DO liCnt = 1 TO 3:
           ASSIGN 
              ocReqTypeList     = ocReqTypeList     + (IF ocReqTypeList <> "" THEN "," ELSE "") + (IF liCnt = 1 THEN 
                                                                                                      STRING({&REQTYPE_CONTRACT_ACTIVATION}) 
                                                                                                   ELSE IF liCnt = 2 THEN 
                                                                                                      STRING({&REQTYPE_CONTRACT_TERMINATION}) 
                                                                                                   ELSE 
                                                                                                      STRING({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}))   
              ocActionList      = ocActionList      + (IF ocActionList      <> "" THEN "," ELSE "") + "SEND-SMS"
              ocActionKeyList   = ocActionKeyList   + (IF ocActionKeyList   <> "" THEN "," ELSE "") + (IF liCnt = 1 THEN 
                                                                                                           "UpsellTARJ7Act" 
                                                                                                       ELSE IF liCnt = 2 THEN 
                                                                                                           icCliType + "DeAct|" 
                                                                                                       ELSE 
                                                                                                           icCliType + "STC|")
              ocActionTypeList  = ocActionTypeList  + (IF ocActionTypeList  <> "" THEN "," ELSE "") + "SMS".
      END.

      IF icServicesRecreated > "" THEN 
      DO liCount = 1 TO NUM-ENTRIES(icServicesRecreated):
         DO liCnt = 1 TO 3:
             ASSIGN 
                ocReqTypeList     = ocReqTypeList     + (IF ocReqTypeList <> "" THEN "," ELSE "") + (IF liCnt = 1 THEN 
                                                                                                        STRING({&REQTYPE_CONTRACT_ACTIVATION}) 
                                                                                                     ELSE IF liCnt = 2 THEN 
                                                                                                        STRING({&REQTYPE_CONTRACT_TERMINATION}) 
                                                                                                     ELSE 
                                                                                                        STRING({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}))   
                ocActionList      = ocActionList      + (IF ocActionList      <> "" THEN "," ELSE "") + (IF liCnt = 1 THEN 
                                                                                                            "RECREATE" 
                                                                                                         ELSE IF liCnt = 2 THEN 
                                                                                                            "TERMINATE" 
                                                                                                         ELSE 
                                                                                                            "TERMINATE")
                ocActionKeyList   = ocActionKeyList   + (IF ocActionKeyList   <> "" THEN "," ELSE "") + ENTRY(liCount, icServicesRecreated)
                ocActionTypeList  = ocActionTypeList  + (IF ocActionTypeList  <> "" THEN "," ELSE "") + "CTServPac".
         END.
      END.

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

    FIND FIRST bf_RatePlanCopyFrom WHERE bf_RatePlanCopyFrom.Brand = Syst.Var:gcBrand AND bf_RatePlanCopyFrom.RatePlan = icReferenceRatePlan NO-LOCK NO-ERROR.
    IF AVAIL bf_RatePlanCopyFrom THEN 
    DO:
        FIND FIRST RatePlan WHERE RatePlan.Brand = Syst.Var:gcBrand AND RatePlan.RatePlan = icRatePlan NO-LOCK NO-ERROR.
        IF NOT AVAIL RatePlan THEN 
        DO:
            CREATE RatePlan.
            BUFFER-COPY bf_RatePlanCopyFrom EXCEPT RatePlan RPName TO RatePlan
                ASSIGN 
                    RatePlan.RatePlan = icRatePlan
                    RatePlan.RPName   = icRPName.        
                
            FOR EACH bf_PListConfCopyFrom WHERE bf_PListConfCopyFrom.Brand    = Syst.Var:gcBrand                      AND 
                                                bf_PListConfCopyFrom.RatePlan = bf_RatePlanCopyFrom.RatePlan AND
                                                bf_PListConfCopyFrom.dFrom   <= TODAY                        AND
                                                bf_PListConfCopyFrom.dTo     >= TODAY                        NO-LOCK:

                IF icRatePlanAction = "New" AND (bf_PListConfCopyFrom.RatePlan EQ bf_PListConfCopyFrom.PriceList)  THEN 
                DO:
                    /* A copy of existing rateplan's pricelist and related tariffs is created */
                    FIND FIRST PriceList WHERE PriceList.Brand = Syst.Var:gcBrand AND PriceList.PriceList = bf_PListConfCopyFrom.PriceList NO-LOCK NO-ERROR.
                    IF AVAIL PriceList THEN 
                    DO:
                        CREATE bPriceList.
                        BUFFER-COPY PriceList EXCEPT PriceList PLName TO bPriceList
                            ASSIGN 
                                bPriceList.PriceList = RatePlan.RatePlan
                                bPriceList.PLName    = RatePlan.RPName.

                        FOR EACH Tariff WHERE Tariff.Brand = Syst.Var:gcBrand AND Tariff.PriceList = PriceList.PriceList NO-LOCK:                     
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

    FIND FIRST Tariff WHERE Tariff.Brand      = Syst.Var:gcBrand            AND 
                            Tariff.PriceList  = ttTariff.PriceList AND 
                            Tariff.CCN        = INT(ttTariff.CCN)  AND 
                            Tariff.BDest      = ttTariff.BDest     AND 
                            Tariff.ValidFrom <= TODAY              AND 
                            Tariff.ValidTo   >= TODAY              NO-LOCK NO-ERROR.                                    
    IF NOT AVAIL Tariff THEN 
    DO:
        CREATE Tariff.
        ASSIGN 
            Tariff.Brand          = Syst.Var:gcBrand
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
   
   DEFINE VARIABLE liFinalBT   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liFinalCR   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcRatePlan  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcActionKey AS CHARACTER NO-UNDO.

   DEFINE BUFFER bf_RequestAction     FOR RequestAction.
   DEFINE BUFFER bf_RequestActionRule FOR RequestActionRule.
   DEFINE BUFFER bf_MxItem            FOR MxItem.

   IF NOT CAN-FIND(FIRST CLIType WHERE CLIType.Brand = Syst.Var:gcBrand AND CLIType.CLIType = ttCliType.CliType) THEN 
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
         FIND FIRST CLIType WHERE CLIType.Brand = Syst.Var:gcBrand AND CLIType.CLIType = ttCLIType.ParentTariff NO-LOCK NO-ERROR.                
         IF AVAILABLE CLIType THEN
            ASSIGN lcRatePlan = CLIType.PricePlan
                   liFinalBT  = CLIType.BillTarget.               
      END.
       
      CREATE CLIType.
      ASSIGN 
         CLIType.Brand             = Syst.Var:gcBrand
         CLIType.CLIType           = ttCliType.CliType
         CLIType.CLIName           = ttCliType.CliName
         CLIType.BaseBundle        = ttCliType.BaseBundle
         CliType.FixedBundle       = ttCliType.FixedLineBaseBundle
         CLIType.PayType           = ttCliType.PayType
         CLIType.UsageType         = ttCliType.UsageType
         CLIType.PricePlan         = (IF lcRatePlan NE "" THEN 
                                          lcRatePlan 
                                      ELSE IF ttCliType.RatePlan NE "" THEN 
                                          ttCliType.RatePlan 
                                      ELSE 
                                          REPLACE(ttCliType.CliType,"CONT","CONTRATO")) 
         CLIType.ServicePack       = (IF ttCliType.PayType EQ 1 THEN "41" ELSE "42")
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
         RUN pCTServPac(ttCliType.BaseBundle, ttCliType.CliType, ttCliType.CopyServicesFromCliType).
      
      IF ttCliType.AllowedBundles > "" THEN
      DO:
          RUN pMatrix(ttCliType.CliType, 
                      ((IF ttCliType.BaseBundle          > "" THEN (ttCliType.BaseBundle          + ",") ELSE "") + 
                       (IF ttCliType.FixedLineBaseBundle > "" THEN (ttCliType.FixedLineBaseBundle + ",") ELSE "") + ttCliType.AllowedBundles)).

          /* Matrix for additional lines */
          FOR EACH MxItem WHERE MxItem.MxSeq   > 0                                 AND 
                                MxItem.MxName  = "SubsTypeFrom"                    AND 
                                MxItem.MxValue = ttCliType.CopyServicesFromCliType NO-LOCK:

              IF NOT CAN-FIND(FIRST bf_MxItem WHERE bf_MxItem.MxSeq   = MxItem.MXSeq      AND 
                                                    bf_MxItem.MxName  = MxItem.MxName     AND 
                                                    bf_MxItem.MxValue = ttCliType.CliType NO-LOCK) THEN

              DO:      
                 CREATE bf_MXItem.
                 ASSIGN
                    bf_MXItem.MxSeq   = MxItem.MXSeq
                    bf_MXItem.MxValue = ttCliType.CliType
                    bf_MXItem.MxName  = MxItem.MxName.   
              END.   
          END.
      END.

      IF ttCliType.TariffBundle = "" THEN    
         RUN pSLGAnalyse(ttCliType.CliType, 
                         ttCliType.CopyServicesFromCliType, 
                         ttCliType.BaseBundle, 
                         ttCliType.FixedLineBaseBundle, 
                         ttCliType.AllowedBundles,
                         (ttCliType.PayType = 2)).         

      IF ttCliType.BundleType = False THEN
      DO:
         RUN pRequestAction(ttCliType.CliType, 
                            (ttCliType.PayType = 1),
                            ttCliType.BaseBundle, 
                            ttCliType.FixedLineBaseBundle, 
                            ttCliType.BundlesForActivateOnSTC, 
                            ttCliType.ServicesForReCreateOnSTC). 

         /* Copy request action rules */
         FOR EACH Requestaction NO-LOCK WHERE Requestaction.Clitype = ttCliType.CopyServicesFromCliType:

             ASSIGN lcActionKey = Requestaction.ActionKey. 
             IF Requestaction.ActionType = "DayCampaign" AND Requestaction.ActionKey BEGINS "CONTFH" THEN 
                 ASSIGN lcActionKey = ttCliType.FixedLineBaseBundle.

             IF NOT CAN-FIND(FIRST bf_RequestAction WHERE bf_RequestAction.Brand      = Syst.Var:gcBrand                  AND
                                                          bf_RequestAction.CliType    = ttCliType.CliType        AND
                                                          bf_RequestAction.ReqType    = Requestaction.ReqType    AND
                                                          bf_RequestAction.ValidTo   >= TODAY                    AND 
                                                          bf_RequestAction.PayType    = Requestaction.PayType    AND
                                                          bf_RequestAction.ActionType = Requestaction.ActionType AND
                                                          bf_RequestAction.ActionKey  = lcActionKey              AND
                                                          bf_RequestAction.Action     = Requestaction.Action     NO-LOCK) THEN
             DO:
                 CREATE bf_RequestAction.
                 ASSIGN     
                    bf_RequestAction.Brand           = Syst.Var:gcBrand
                    bf_RequestAction.RequestActionID = fGetNextRequestActionSequence()
                    bf_RequestAction.ReqType         = Requestaction.ReqType
                    bf_RequestAction.CLIType         = ttCliType.CliType
                    bf_RequestAction.PayType         = Requestaction.PayType
                    bf_RequestAction.Action          = Requestaction.Action
                    bf_RequestAction.ActionKey       = lcActionKey
                    bf_RequestAction.ActionType      = Requestaction.ActionType           
                    bf_RequestAction.ValidFrom       = TODAY
                    bf_RequestAction.ValidTo         = DATE(12,31,2049). 

                 FOR EACH RequestActionRule WHERE RequestActionRule.RequestActionID = Requestaction.RequestActionID NO-LOCK:

                     IF NOT CAN-FIND(FIRST bf_RequestActionRule WHERE 
                                           bf_RequestActionRule.RequestActionid = RequestActionRule.RequestActionID AND 
                                           bf_RequestActionRule.ParamField      = RequestActionRule.ParamField      AND 
                                           bf_RequestActionRule.ToDate          >= TODAY                            NO-LOCK) THEN 
                     DO:                                                                     
                         CREATE bf_RequestActionRule.
                         ASSIGN
                             bf_RequestActionRule.RequestActionid = RequestActionRule.RequestActionID
                             bf_RequestActionRule.ParamField      = RequestActionRule.ParamField
                             bf_RequestActionRule.ParamValue      = RequestActionRule.ParamValue
                             bf_RequestActionRule.ExclParamValue  = RequestActionRule.ExclParamValue
                             bf_RequestActionRule.FromDate        = TODAY
                             bf_RequestActionRule.ToDate          = DATE(12,31,2049).
                     END.
                         
                 END.    
             END.
         END.

      END.   

      IF NOT (CliType.FixedLineDownload > "" OR CliType.FixedLineUpload > "") THEN /* Non-convergent */
      DO:
         IF ttCLIType.PayType = 1 THEN 
         DO: 
             RUN pUpdateTMSParam("ALL_POSTPAID_CONTRACTS",ttCliType.CliType).  
             RUN pUpdateTMSParam("POSTPAID_DATA_CONTRACTS", ttCliType.CliType).
         END.
         /*TODO: Parent of Tariff bundle needs to be excluded */
         RUN pUpdateTMSParam("BB_PROFILE_1",ttCliType.CliType).
      END.
      ELSE IF INDEX(ttCliType.CliName,"Azul") > 0 OR INDEX(ttCliType.CliName,"Morada") > 0 THEN 
         RUN pUpdateTMSParam("Extra_MainLine_CLITypes",ttCliType.CliType). 
      
      IF ttCLIType.PayType = 1 THEN 
      DO:
          IF LOOKUP("DSS2",ttCliType.AllowedBundles) > 0 THEN 
          DO:
              RUN pUpdateTMSParam("DSS2_PRIMARY_SUBS_TYPE", ttCliType.CliType).
              RUN pUpdateTMSParam("DSS2_SUBS_TYPE"        , ttCliType.CliType).
          END.    

          IF (ttCliType.MobileBaseBundleDataLimit > 0 OR CliType.FixedLineDownload > "" OR CliType.FixedLineUpload > "") THEN       
              RUN pUpdateTMSParam("DATA_BUNDLE_BASED_CLITYPES", ttCliType.CliType).
      END.
         
      IF ttCLIType.PayType = 1 AND ttCLIType.UsageType = 1 THEN 
         RUN pUpdateTMSParam("POSTPAID_VOICE_TARIFFS", ttCliType.CliType).
      ELSE IF ttCLIType.PayType = 2 AND ttCLIType.UsageType = 1 THEN 
      DO:
         RUN pUpdateTMSParam("PREPAID_VOICE_TARIFFS", ttCliType.CliType).  
         
         IF ttCliType.MobileBaseBundleDataLimit > 0 THEN 
            RUN pUpdateTMSParam("PREPAID_DATA_CONTRACTS", ttCliType.CliType).
      END.   
      ELSE IF ttCliType.PayType = 2 AND ttCliType.UsageType = 2 THEN
         RUN pUpdateTMSParam("PREPAID_DATA_CONTRACTS", ttCliType.CliType).          
   END.
     
   RETURN "".

END PROCEDURE.


PROCEDURE pCTServPac:

   DEFINE INPUT PARAMETER icBaseBundle              AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icCLIType                 AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER icCopyServicesFromCliType AS CHARACTER NO-UNDO.   
   
   DEFINE BUFFER bf_CTServPac_CopyFrom  FOR CTServPac.
   DEFINE BUFFER bf_CTServEl_CopyFrom   FOR CTServEl.
   DEFINE BUFFER bf_CTServAttr_CopyFrom FOR CTServAttr.
   
   FOR EACH bf_CTServPac_CopyFrom WHERE bf_CTServPac_CopyFrom.Brand = Syst.Var:gcBrand AND bf_CTServPac_CopyFrom.CLIType = icCopyServicesFromCliType NO-LOCK
       ON ERROR UNDO, THROW:

      IF LOOKUP(bf_CTServPac_CopyFrom.ServPac,"Y_HURP,D_HOTL") = 0 AND 
         (bf_CTServPac_CopyFrom.ServPac BEGINS "C_" OR bf_CTServPac_CopyFrom.ServPac BEGINS "Y_" OR bf_CTServPac_CopyFrom.ServPac BEGINS "D_") THEN 
         NEXT.

      CREATE CTServPac.
      BUFFER-COPY bf_CTServPac_CopyFrom EXCEPT CLIType TO CTServPac
         ASSIGN CTServPac.CLIType  = icCLIType.            
          
      FOR EACH bf_CTServEl_CopyFrom WHERE bf_CTServEl_CopyFrom.Brand = Syst.Var:gcBrand AND bf_CTServEl_CopyFrom.CLIType = bf_CTServPac_CopyFrom.CLIType AND bf_CTServEl_CopyFrom.ServPac = bf_CTServPac_CopyFrom.ServPac NO-LOCK
          ON ERROR UNDO, THROW:

         CREATE CTServEl.
         BUFFER-COPY bf_CTServEl_CopyFrom EXCEPT CTServEl CLIType TO CTServEl
            ASSIGN 
               CTServEl.CTServEl = NEXT-VALUE(CTServEl)
               CTServEl.CLIType  = icCLIType.

         IF bf_CTServEl_CopyFrom.ServPac = "TMSSERVICE" AND
            bf_CTServEl_CopyFrom.ServCom = "SHAPER_STP"
         THEN CTServEl.DefParam = icBaseBundle.

         FIND ServCom WHERE ServCom.Brand = Syst.Var:gcBrand AND ServCom.ServCom = CTServEl.ServCom NO-LOCK NO-ERROR.
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
    DEFINE INPUT PARAMETER ilPrepaid             AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE liCnt                         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE liCount                       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcSubsTypePrefix              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcBillCodeList                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcSMSBillCodeList             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcConvergentBillCodeList      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcMxValue                     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcBaseBundleOfCopyFromCliType AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf_Matrix     FOR Matrix.
    DEFINE BUFFER bf_MxItem     FOR MxItem.
    DEFINE BUFFER bf_SLGAnalyse FOR SLGAnalyse.
    DEFINE BUFFER bf_CliTypeCF  FOR CliType.

    ASSIGN
        lcBillCodeList           = "14100001,10100001,10100003,10100005,CFOTHER,CFYOIGO,14104019,10104013" 
        lcSMSBillCodeList        = "12100001,12100002,12100003,12100004"
        lcConvergentBillCodeList = "F10100003,F10100005"
        lcSubsTypePrefix = (IF icCliType BEGINS "CONTDSL" THEN 
                                "CONTDSL*,CONT*" 
                            ELSE IF icCliType BEGINS "CONTFH" THEN 
                                "CONT*"     
                            ELSE IF icCliType BEGINS "CONT" THEN 
                                "CONT*" 
                            ELSE IF icCliType BEGINS "TARJ" THEN 
                                "TARJ*" 
                            ELSE "").

    ASSIGN lcSubsTypePrefix = lcSubsTypePrefix + (IF lcSubsTypePrefix <> "" THEN "," ELSE "") + icCliType.

    FIND FIRST bf_CliTypeCF WHERE bf_CliTypeCF.Brand = Syst.Var:gcBrand AND bf_CliTypeCF.ClIType = icCliTypeCopyFrom NO-LOCK NO-ERROR.    
    IF AVAIL bf_CliTypeCF THEN 
        ASSIGN lcBaseBundleOfCopyFromCliType = bf_CliTypeCF.BaseBundle.    

    /* Get list of all allowed bundles/bonos for the subscription type */    
    IF lcSubsTypePrefix > "" THEN
    DO liCount = 1 TO NUM-ENTRIES(lcSubsTypePrefix)
       ON ERROR UNDO, THROW:
        FOR EACH bf_Matrix WHERE bf_Matrix.Brand = Syst.Var:gcBrand AND bf_Matrix.MXKey = "PERCONTR" NO-LOCK By bf_Matrix.Prior
            ON ERROR UNDO, THROW:
            
            IF bf_Matrix.MXRes <> 1 THEN 
                NEXT.                                           
            
            ASSIGN lcMxValue = ENTRY(liCount,lcSubsTypePrefix).
            
            FOR EACH bf_MxItem WHERE bf_MxItem.MxSeq = bf_Matrix.MxSeq AND bf_MxItem.MxName = "SubsTypeTo" AND bf_MxItem.MxValue = lcMxValue NO-LOCK
                ON ERROR UNDO, THROW:
                FOR EACH MxItem WHERE MxItem.MxSeq = bf_MxItem.MxSeq AND MxItem.MXName = "PerContract" NO-LOCK
                    ON ERROR UNDO, THROW:
                    FIND FIRST DayCampaign WHERE Daycampaign.Brand = Syst.Var:gcBrand AND Daycampaign.DCEvent = MxItem.MxValue NO-LOCK NO-ERROR.
                    IF AVAIL DayCampaign AND LOOKUP(DayCampaign.DcType, {&PERCONTRACT_RATING_PACKAGE} + ",6") > 0 AND LOOKUP(DayCampaign.DCEvent, icAllowedBundles) = 0 THEN                 
                        ASSIGN icAllowedBundles = icAllowedBundles + (IF icAllowedBundles <> "" THEN "," ELSE "") + DayCampaign.DCEvent.
                END.
            END.

        END.
    END.    
  
    IF LOOKUP(icFixedLineBaseBundle, icAllowedBundles) = 0 THEN 
        ASSIGN icAllowedBundles = icAllowedBundles + (IF icAllowedBundles <> "" THEN "," ELSE "") + icFixedLineBaseBundle.

    IF icAllowedBundles > "" THEN 
    DO liCount = 1 TO NUM-ENTRIES(icAllowedBundles)
       ON ERROR UNDO, THROW:

        IF ENTRY(liCount,icAllowedBundles) = "CONTDSL" THEN
        DO liCnt = 1 TO NUM-ENTRIES(lcConvergentBillCodeList)
           ON ERROR UNDO, THROW:

            FIND FIRST bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand             = Syst.Var:gcBrand                               AND 
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

            FIND FIRST bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand             = Syst.Var:gcBrand                               AND 
                                           bf_SLGAnalyse.ServiceLimitGroup BEGINS "CONTFH"                         AND
                                           bf_SLGAnalyse.CLIType           = icCliTypeCopyFrom                     AND
                                           bf_SLGAnalyse.ValidTo           >= TODAY                                AND 
                                           bf_SLGAnalyse.BillCode          = ENTRY(liCnt,lcConvergentBillCodeList) NO-LOCK NO-ERROR.
            IF AVAIL bf_SLGAnalyse THEN
            DO:
                IF NOT CAN-FIND(FIRST SLGAnalyse WHERE SLGAnalyse.Brand             = Syst.Var:gcBrand                AND
                                                       SLGAnalyse.BelongTo          = bf_SLGAnalyse.BelongTo AND
                                                       SLGAnalyse.CliType           = icCliType              AND 
                                                       SLGAnalyse.BillCode          = bf_SLGAnalyse.BillCode AND
                                                       SLGAnalyse.CCN               = bf_SLGAnalyse.CCN      AND
                                                       SLGAnalyse.BDest             = bf_SLGAnalyse.BDest    AND 
                                                       SLGAnalyse.Prior             = bf_SLGAnalyse.Prior    AND
                                                       SLGAnalyse.ValidTo           >= TODAY                 AND 
                                                       SLGAnalyse.ServiceLimitGroup = icFixedLineBaseBundle  AND 
                                                       SLGAnalyse.SLGAType          = bf_SLGAnalyse.SLGAType NO-LOCK) THEN 
                DO:
                    CREATE SLGAnalyse.
                    BUFFER-COPY bf_SLGAnalyse EXCEPT CliType ServiceLimitGroup ValidFrom ValidTo TO SLGAnalyse
                        ASSIGN
                            SLGAnalyse.CliType              = icCliType
                            SLGAnalyse.ServiceLimitGroup    = icFixedLineBaseBundle 
                            SLGAnalyse.ValidFrom            = TODAY
                            SLGAnalyse.ValidTo              = DATE(12,31,2049).
                END.        
            END.
        END.   
        ELSE IF ENTRY(liCount,icAllowedBundles) = "SMS5000" THEN
        DO liCnt = 1 TO NUM-ENTRIES(lcSMSBillCodeList)
           ON ERROR UNDO, THROW:

           FIND FIRST bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand            = Syst.Var:gcBrand                        AND 
                                          bf_SLGAnalyse.BillCode         = ENTRY(liCnt,lcSMSBillCodeList) AND
                                          bf_SLGAnalyse.CLIType          = icCliTypeCopyFrom              AND
                                          bf_SLGAnalyse.ValidTo          >= TODAY                         AND 
                                          bf_SLGAnalyse.ServiceLimitGroup = "SMS5000"                     NO-LOCK NO-ERROR.
           IF AVAIL bf_SLGAnalyse THEN
           DO:
               IF NOT CAN-FIND(FIRST SLGAnalyse WHERE SLGAnalyse.Brand             = Syst.Var:gcBrand                         AND
                                                      SLGAnalyse.BelongTo          = bf_SLGAnalyse.BelongTo          AND
                                                      SLGAnalyse.CliType           = icCliType                       AND 
                                                      SLGAnalyse.BillCode          = bf_SLGAnalyse.BillCode          AND
                                                      SLGAnalyse.CCN               = bf_SLGAnalyse.CCN               AND
                                                      SLGAnalyse.BDest             = bf_SLGAnalyse.BDest             AND 
                                                      SLGAnalyse.Prior             = bf_SLGAnalyse.Prior             AND
                                                      SLGAnalyse.ValidTo           >= TODAY                          AND 
                                                      SLGAnalyse.ServiceLimitGroup = bf_SLGAnalyse.ServiceLimitGroup AND 
                                                      SLGAnalyse.SLGAType          = bf_SLGAnalyse.SLGAType          NO-LOCK) THEN 
               DO: 
                   CREATE SLGAnalyse.
                   BUFFER-COPY bf_SLGAnalyse EXCEPT CliType ValidFrom ValidTo TO SLGAnalyse
                       ASSIGN
                           SLGAnalyse.CliType           = icCliType
                           SLGAnalyse.ValidFrom         = TODAY
                           SLGAnalyse.ValidTo           = DATE(12,31,2049).
               END.        
           END.

        END.   
        ELSE IF ENTRY(liCount,icAllowedBundles) = "VOICE100" THEN
        DO liCnt = 1 TO NUM-ENTRIES(lcBillCodeList)
           ON ERROR UNDO, THROW:
            
            IF ENTRY(liCount,lcBillCodeList) = "14100001" THEN 
                NEXT.

            FIND FIRST bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand            = Syst.Var:gcBrand                       AND 
                                           bf_SLGAnalyse.BillCode         = ENTRY(liCnt,lcBillCodeList) AND
                                           bf_SLGAnalyse.CLIType          = icCliTypeCopyFrom             AND
                                           bf_SLGAnalyse.ValidTo          >= TODAY                        AND 
                                           bf_SLGAnalyse.ServiceLimitGroup = "VOICE100"                   NO-LOCK NO-ERROR.
            IF AVAIL bf_SLGAnalyse THEN
            DO:
                IF NOT CAN-FIND(FIRST SLGAnalyse WHERE SLGAnalyse.Brand             = Syst.Var:gcBrand                         AND
                                                       SLGAnalyse.BelongTo          = bf_SLGAnalyse.BelongTo          AND
                                                       SLGAnalyse.CliType           = icCliType                       AND 
                                                       SLGAnalyse.BillCode          = bf_SLGAnalyse.BillCode          AND
                                                       SLGAnalyse.CCN               = bf_SLGAnalyse.CCN               AND
                                                       SLGAnalyse.BDest             = bf_SLGAnalyse.BDest             AND 
                                                       SLGAnalyse.Prior             = bf_SLGAnalyse.Prior             AND
                                                       SLGAnalyse.ValidTo           >= TODAY                          AND 
                                                       SLGAnalyse.ServiceLimitGroup = bf_SLGAnalyse.ServiceLimitGroup AND 
                                                       SLGAnalyse.SLGAType          = bf_SLGAnalyse.SLGAType          NO-LOCK) THEN 
                DO:
                    CREATE SLGAnalyse.
                    BUFFER-COPY bf_SLGAnalyse EXCEPT CliType ValidFrom ValidTo TO SLGAnalyse
                        ASSIGN
                            SLGAnalyse.CliType           = icCliType
                            SLGAnalyse.ValidFrom         = TODAY
                            SLGAnalyse.ValidTo           = DATE(12,31,2049).
                END.        
            END.
        END.
        ELSE IF ENTRY(liCount,icAllowedBundles) = "FREE100MINUTES" THEN
        DO liCnt = 1 TO NUM-ENTRIES(lcBillCodeList)
           ON ERROR UNDO, THROW:
            
            IF ENTRY(liCount,lcBillCodeList) = "14100001" THEN 
                NEXT.

            FIND FIRST bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand            = Syst.Var:gcBrand                       AND 
                                           bf_SLGAnalyse.BillCode         = ENTRY(liCnt,lcBillCodeList) AND
                                           bf_SLGAnalyse.CLIType          = icCliTypeCopyFrom             AND
                                           bf_SLGAnalyse.ValidTo          >= TODAY                        AND 
                                           bf_SLGAnalyse.ServiceLimitGroup = "FREE100MINUTES"             NO-LOCK NO-ERROR.
            IF AVAIL bf_SLGAnalyse THEN
            DO:
                IF NOT CAN-FIND(FIRST SLGAnalyse WHERE SLGAnalyse.Brand             = Syst.Var:gcBrand                         AND
                                                       SLGAnalyse.BelongTo          = bf_SLGAnalyse.BelongTo          AND
                                                       SLGAnalyse.CliType           = icCliType                       AND 
                                                       SLGAnalyse.BillCode          = bf_SLGAnalyse.BillCode          AND
                                                       SLGAnalyse.CCN               = bf_SLGAnalyse.CCN               AND
                                                       SLGAnalyse.BDest             = bf_SLGAnalyse.BDest             AND 
                                                       SLGAnalyse.Prior             = bf_SLGAnalyse.Prior             AND
                                                       SLGAnalyse.ValidTo           >= TODAY                          AND 
                                                       SLGAnalyse.ServiceLimitGroup = bf_SLGAnalyse.ServiceLimitGroup AND 
                                                       SLGAnalyse.SLGAType          = bf_SLGAnalyse.SLGAType          NO-LOCK) THEN 
                DO:
                    CREATE SLGAnalyse.
                    BUFFER-COPY bf_SLGAnalyse EXCEPT CliType ValidFrom ValidTo TO SLGAnalyse
                        ASSIGN
                            SLGAnalyse.CliType           = icCliType
                            SLGAnalyse.ValidFrom         = TODAY
                            SLGAnalyse.ValidTo           = DATE(12,31,2049).
                END.
            END.
        END.
        ELSE
        DO:
            /* Default bundles for CONT* or TRAJ* subscription types */
            FOR EACH bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand             = Syst.Var:gcBrand                         AND 
                                         bf_SLGAnalyse.ServiceLimitGroup = ENTRY(liCount,icAllowedBundles) AND
                                         bf_SLGAnalyse.CLIType           = icCliTypeCopyFrom               AND
                                         bf_SLGAnalyse.ValidTo           >= TODAY                          NO-LOCK:

                IF NOT CAN-FIND(FIRST SLGAnalyse WHERE SLGAnalyse.Brand             = Syst.Var:gcBrand                         AND
                                                       SLGAnalyse.BelongTo          = bf_SLGAnalyse.BelongTo          AND
                                                       SLGAnalyse.CliType           = icCliType                       AND 
                                                       SLGAnalyse.BillCode          = bf_SLGAnalyse.BillCode          AND
                                                       SLGAnalyse.CCN               = bf_SLGAnalyse.CCN               AND
                                                       SLGAnalyse.BDest             = bf_SLGAnalyse.BDest             AND 
                                                       SLGAnalyse.Prior             = bf_SLGAnalyse.Prior             AND
                                                       SLGAnalyse.ValidTo           >= TODAY                          AND 
                                                       SLGAnalyse.ServiceLimitGroup = bf_SLGAnalyse.ServiceLimitGroup AND 
                                                       SLGAnalyse.SLGAType          = bf_SLGAnalyse.SLGAType          NO-LOCK) THEN 
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
    END.    

    /* Voice national related */
    IF NOT ilPrepaid THEN 
    DO liCount = 1 TO NUM-ENTRIES(lcBillCodeList)
       ON ERROR UNDO, THROW:

        IF (LOOKUP("FREE100MINUTES",icAllowedBundles) > 0 OR LOOKUP("VOICE100",icAllowedBundles) > 0) AND ENTRY(liCount,lcBillCodeList) <> "14100001" THEN 
            NEXT.

        FIND FIRST bf_SLGAnalyse WHERE bf_SLGAnalyse.Brand             = Syst.Var:gcBrand                       AND 
                                       bf_SLGAnalyse.BillCode          = ENTRY(liCount,lcBillCodeList) AND
                                       bf_SLGAnalyse.CLIType           = icCliTypeCopyFrom             AND    
                                       (IF lcBaseBundleOfCopyFromCliType > "" THEN 
                                           bf_SLGAnalyse.ServiceLimitGroup = lcBaseBundleOfCopyFromCliType 
                                        ELSE 
                                           TRUE)                                                      AND                                  
                                       bf_SLGAnalyse.ValidTo          >= TODAY                        NO-LOCK NO-ERROR.
        IF AVAIL bf_SLGAnalyse THEN
        DO:
            IF NOT CAN-FIND(FIRST SLGAnalyse WHERE SLGAnalyse.Brand             = Syst.Var:gcBrand                         AND
                                                   SLGAnalyse.BelongTo          = bf_SLGAnalyse.BelongTo          AND
                                                   SLGAnalyse.CliType           = icCliType                       AND 
                                                   SLGAnalyse.BillCode          = bf_SLGAnalyse.BillCode          AND
                                                   SLGAnalyse.CCN               = bf_SLGAnalyse.CCN               AND
                                                   SLGAnalyse.BDest             = bf_SLGAnalyse.BDest             AND 
                                                   SLGAnalyse.Prior             = bf_SLGAnalyse.Prior             AND
                                                   SLGAnalyse.ValidTo           >= TODAY                          AND 
                                                   SLGAnalyse.ServiceLimitGroup = bf_SLGAnalyse.ServiceLimitGroup AND 
                                                   SLGAnalyse.SLGAType          = bf_SLGAnalyse.SLGAType          NO-LOCK) THEN 
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
    END.

    RETURN "".

END PROCEDURE.
   
PROCEDURE pFMItem:   

   DEFINE PARAMETER BUFFER ttCliType FOR ttCliType.
   DEFINE INPUT  PARAMETER icBundle            AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER icPriceList         AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ideAmount           AS DECIMAL   NO-UNDO.
   DEFINE INPUT  PARAMETER iiFirstMonthFeeCalc AS INTEGER   NO-UNDO.
   DEFINE INPUT  PARAMETER iiLastMonthFeeCalc  AS INTEGER   NO-UNDO.
   
   DEFINE VARIABLE lcRatePlan AS CHARACTER NO-UNDO.   
   
   IF AVAIL ttCliType THEN 
   DO:
       IF ttCliType.TariffBundle NE "" THEN 
       DO:
          FIND FIRST CLIType WHERE CLIType.Brand = Syst.Var:gcBrand AND CLIType.CLIType = ttCliType.ParentTariff NO-LOCK NO-ERROR.
          IF AVAILABLE CLIType THEN
             ASSIGN lcRatePlan = CLIType.PricePlan.
       END.
       
       ASSIGN lcRatePlan = (IF lcRatePlan NE "" THEN lcRatePlan ELSE REPLACE(ttCliType.CliType,"CONT","CONTRATO")).

       FIND FIRST RatePlan WHERE RatePlan.Brand = Syst.Var:gcBrand AND RatePlan.RatePlan = lcRatePlan NO-LOCK NO-ERROR.           
       IF AVAIL RatePlan THEN   
          FIND FIRST PListConf WHERE PListConf.Brand = Syst.Var:gcBrand AND PListConf.RatePlan = RatePlan.RatePlan AND PListConf.PriceList BEGINS "CONTRATO" NO-LOCK NO-ERROR.   
   END.

   DEFINE VARIABLE lcPriceList AS CHARACTER NO-UNDO.
   
   lcPriceList = IF icPriceList <> ""
                 THEN icPriceList 
                 ELSE IF AVAIL PListConf AND PListConf.PriceList <> ""
                 THEN PListConf.PriceList 
                 ELSE "".

   FIND FIRST FMItem NO-LOCK WHERE
      FMItem.Brand     = Syst.Var:gcBrand AND
      FMItem.FeeModel  = icBundle         AND
      FMItem.PriceList = lcPriceList      AND
      FMItem.BillCode  = icBundle         AND
      FMItem.FromDate <= TODAY            AND
      FMItem.ToDate   >= TODAY
   NO-ERROR.

   IF AVAILABLE FMItem
   THEN UNDO, THROW NEW Progress.Lang.AppError
         (SUBSTITUTE("FMItem having FeeModel=&1, PriceList=&2, BillCode=&3 " +
                     "is already defined and active",
                     icBundle, lcPriceList, icBundle), 1). 

   CREATE FMItem. 
   ASSIGN     
      FMItem.Brand             = Syst.Var:gcBrand
      FMItem.FeeModel          = icBundle
      FMItem.BillCode          = icBundle
      FMItem.PriceList         = lcPriceList
      FMItem.FromDate          = TODAY       
      FMItem.ToDate            = DATE(12,31,2049)
      FMItem.BillType          = "MF"                  
      FMItem.Interval          = 1                   
      FMItem.BillCycle         = 2                   
      FMItem.FFItemQty         = 0 
      FMItem.FFEndDate         = ? 
      FMItem.Amount            = ideAmount
      FMItem.FirstMonthBR      = iiFirstMonthFeeCalc
      FMItem.BrokenRental      = iiLastMonthFeeCalc
      FMItem.ServiceLimitGroup = "".

   RETURN "".
   
END PROCEDURE.

PROCEDURE pRequestAction:
   DEFINE INPUT PARAMETER icCLIType                  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ilPostpaid                 AS LOGICAL   NO-UNDO.
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
   FIND FIRST CliType WHERE ClIType.Brand = Syst.Var:gcBrand AND CliType.ClIType = icCLIType NO-LOCK NO-ERROR.
   IF NOT AVAIL CliType THEN 
      UNDO, THROW NEW Progress.Lang.AppError("CLIType doesn't exists for creating request actions", 1).

   /* This is to add mobile packages and fixed line packages to RequestType's subscription creation, fixedline activation & STC */    
   fPrepareRequestActionParam(icCLIType,
                              ilPostpaid,
                              icMobileBundles,
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
                          ELSE IF lcAction = "SEND-SMS" THEN 
                             13    
                          ELSE 
                             1)
         lcActionKey   = ENTRY(liCount, lcActionKeyList)
         lcActionType  = ENTRY(liCount, lcActionTypeList)
         liRequestType = INT(ENTRY(liCount, lcRequestTypeList)).

      FIND FIRST RequestAction WHERE RequestAction.Brand      = Syst.Var:gcBrand         AND
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
            RequestAction.Brand           = Syst.Var:gcBrand
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

   IF ilPostpaid THEN 
   DO:
       RUN pUpdateRequestActionRule("",
                                    {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE},
                                    1,
                                    13,
                                    "SMS",
                                    "STC_Requested",
                                    "ReqCParam2",
                                    icMobileBundles,
                                    "").   
   END.
   ELSE
   DO:
       /* STC */
       FIND FIRST RequestAction WHERE RequestAction.Brand      = Syst.Var:gcBrand                             AND 
                                      RequestAction.CliType    = CliType.CliType                     AND   
                                      RequestAction.ReqType    = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND 
                                      RequestAction.PayType    = 0                                   AND 
                                      RequestAction.Action     = 13                                  AND 
                                      RequestAction.ActionType = "SMS"                               AND 
                                      RequestAction.ActionKey  BEGINS (icCLIType + "STC|")           AND 
                                      RequestAction.ValidTo   >= TODAY                               NO-LOCK NO-ERROR.
       IF AVAIL RequestAction THEN
       DO:
          CREATE RequestActionRule.
          ASSIGN
              RequestActionRule.RequestActionid = RequestAction.RequestActionID
              RequestActionRule.ParamField      = "ReqStatus"
              RequestActionRule.ParamValue      = "2"
              RequestActionRule.FromDate        = TODAY
              RequestActionRule.ToDate          = DATE(12,31,2049).
       END.

       /* Activation */ 
       FIND FIRST RequestAction WHERE RequestAction.Brand      = Syst.Var:gcBrand                        AND 
                                      RequestAction.CliType    = CliType.CliType                AND
                                      RequestAction.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION} AND 
                                      RequestAction.PayType    = 0                              AND
                                      RequestAction.Action     = 13                             AND 
                                      RequestAction.ActionType = "SMS"                          AND 
                                      RequestAction.ActionKey  = "UpsellTARJ7Act"               AND 
                                      RequestAction.ValidTo   >= TODAY                          NO-LOCK NO-ERROR.
       IF AVAIL RequestAction THEN
       DO:
          CREATE RequestActionRule.
          ASSIGN
              RequestActionRule.RequestActionid = RequestAction.RequestActionID
              RequestActionRule.ParamField      = "ReqCParam3"
              RequestActionRule.ParamValue      = "+,TARJ7_UPSELL"
              RequestActionRule.FromDate        = TODAY
              RequestActionRule.ToDate          = DATE(12,31,2049).

          CREATE RequestActionRule.
          ASSIGN
              RequestActionRule.RequestActionid = RequestAction.RequestActionID
              RequestActionRule.ParamField      = "ReqStatus"
              RequestActionRule.ParamValue      = "+,2"
              RequestActionRule.FromDate        = TODAY
              RequestActionRule.ToDate          = DATE(12,31,2049).    
       END.

       /* Deactivation */
       FIND FIRST RequestAction WHERE RequestAction.Brand      = Syst.Var:gcBrand                         AND 
                                      RequestAction.CliType    = CliType.CliType                 AND
                                      RequestAction.ReqType    = {&REQTYPE_CONTRACT_TERMINATION} AND 
                                      RequestAction.PayType    = 0                               AND
                                      RequestAction.Action     = 13                              AND 
                                      RequestAction.ActionType = "SMS"                           AND 
                                      RequestAction.ActionKey  = "UpsellTARJ7Act"                AND 
                                      RequestAction.ValidTo   >= TODAY                           NO-LOCK NO-ERROR.
       IF AVAIL RequestAction THEN
       DO:
          CREATE RequestActionRule.
          ASSIGN
              RequestActionRule.RequestActionid = RequestAction.RequestActionID
              RequestActionRule.ParamField      = "ReqCParam2"
              RequestActionRule.ParamValue      = "+," + CliType.CliType
              RequestActionRule.FromDate        = TODAY
              RequestActionRule.ToDate          = DATE(12,31,2049).

          CREATE RequestActionRule.
          ASSIGN
              RequestActionRule.RequestActionid = RequestAction.RequestActionID
              RequestActionRule.ParamField      = "ReqStatus"
              RequestActionRule.ParamValue      = "+,2"
              RequestActionRule.FromDate        = TODAY
              RequestActionRule.ToDate          = DATE(12,31,2049).    
       END.

       /* STC */ 
       RUN pUpdateRequestActionRule("",
                                    {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE},
                                    2,
                                    13,
                                    "SMS",
                                    "STC_Requested",
                                    "ReqCParam2",
                                    CliType.CliType,
                                    "").

       RUN pUpdateRequestActionRule("",
                                    {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE},
                                    2,
                                    13,
                                    "SMS",
                                    "STC_DONE",
                                    "ReqCParam2",
                                    "+",
                                    CliType.CliType).

       RUN pUpdateRequestActionRule("TARJ",
                                    {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE},
                                    0,
                                    2,
                                    "CTServPac",
                                    "BB",
                                    "ReqCParam1",
                                    CliType.CliType,
                                    "").

       RUN pUpdateRequestActionRule("TARJ4",
                                    {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE},
                                    0,
                                    2,
                                    "CTServPac",
                                    "BB",
                                    "ReqCParam1",
                                    CliType.CliType,
                                    "").

       RUN pUpdateRequestActionRule("TARJ5",
                                    {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE},
                                    0,
                                    2,
                                    "CTServPac",
                                    "BB",
                                    "ReqCParam1",
                                    CliType.CliType,
                                    "").

       RUN pUpdateRequestActionRule("TARJ7",
                                    {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE},
                                    0,
                                    2,
                                    "CTServPac",
                                    "BB",
                                    "ReqCParam1",
                                    CliType.CliType,
                                    "").

       RUN pUpdateRequestActionRule("TARJ8",
                                    {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE},
                                    0,
                                    2,
                                    "CTServPac",
                                    "BB",
                                    "ReqCParam1",
                                    CliType.CliType,
                                    "").

       RUN pUpdateRequestActionRule("TARJ9",
                                    {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE},
                                    0,
                                    2,
                                    "CTServPac",
                                    "BB",
                                    "ReqCParam1",
                                    CliType.CliType,
                                    "").
       /* Activation */
       RUN pUpdateRequestActionRule("TARJ7",
                                    {&REQTYPE_CONTRACT_ACTIVATION},
                                    0,
                                    3,
                                    "CTServPac",
                                    "BB",
                                    "ReqCParam3",
                                    CliType.CliType,
                                    "").

       RUN pUpdateRequestActionRule("TARJ9",
                                    {&REQTYPE_CONTRACT_ACTIVATION},
                                    0,
                                    3,
                                    "CTServPac",
                                    "BB",
                                    "ReqCParam3",
                                    CliType.CliType,
                                    "").
       /* Deactication */
       RUN pUpdateRequestActionRule("*",
                                    {&REQTYPE_CONTRACT_TERMINATION},
                                    0,
                                    2,
                                    "Service",
                                    "VOIPVIDEO",
                                    "ReqCParam3",
                                    CliType.CliType,
                                    "").

       RUN pUpdateRequestActionRule("TARJ9",
                                    {&REQTYPE_CONTRACT_TERMINATION},
                                    0,
                                    2,
                                    "CTServPac",
                                    "BB",
                                    "ReqCParam3",
                                    CliType.CliType,
                                    "").

   END.

   RETURN "".

END PROCEDURE.

PROCEDURE pUpdateRequestActionRule:
    DEFINE INPUT PARAMETER icCliType        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iiReqType        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiPayType        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiAction         AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER icActionType     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icActionKey      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icParamField     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icParamValue     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icExclParamValue AS CHARACTER NO-UNDO.

    FIND FIRST RequestAction WHERE RequestAction.Brand      = Syst.Var:gcBrand      AND 
                                   RequestAction.CliType    = icCliType    AND 
                                   RequestAction.ReqType    = iiReqType    AND 
                                   RequestAction.PayType    = iiPayType    AND 
                                   RequestAction.Action     = iiAction     AND 
                                   RequestAction.ActionType = icActionType AND 
                                   RequestAction.ActionKey  = icActionKey  AND 
                                   RequestAction.ValidTo   >= TODAY        NO-LOCK NO-ERROR.
    IF AVAIL RequestAction THEN 
    DO:
        FIND FIRST RequestActionRule WHERE RequestActionRule.RequestActionID = RequestAction.RequestActionID AND RequestActionRule.ParamField = icParamField AND RequestActionRule.ToDate >= TODAY EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF AVAIL RequestActionRule THEN
        DO: 
            IF LOOKUP(icParamValue, RequestActionRule.ParamValue) = 0 THEN
                ASSIGN RequestActionRule.ParamValue = RequestActionRule.ParamValue + (IF RequestActionRule.ParamValue <> '' THEN "," ELSE "") + icParamValue.

            IF icExclParamValue > "" THEN 
            DO:
                IF LOOKUP(icExclParamValue, RequestActionRule.ExclParamValue) = 0 THEN
                    ASSIGN RequestActionRule.ExclParamValue = RequestActionRule.ExclParamValue + (IF RequestActionRule.ExclParamValue <> '' THEN "," ELSE "") + icExclParamValue.                
            END.      
        END.      
    END.

    RETURN "".
END PROCEDURE.


PROCEDURE pMatrix:
   DEFINE INPUT PARAMETER icCLIType          AS CHARACTER NO-UNDO.    
   DEFINE INPUT PARAMETER icAllowedBundles   AS CHARACTER NO-UNDO.

   DEFINE VARIABLE liCount AS INTEGER NO-UNDO.

   FIND FIRST Matrix WHERE Matrix.Brand = Syst.Var:gcBrand AND Matrix.MXKey = "PERCONTR" AND Matrix.MxName = icCLIType NO-LOCK NO-ERROR.
   IF NOT AVAIL Matrix THEN 
   DO:
       CREATE Matrix.
       ASSIGN
          Matrix.Brand  = Syst.Var:gcBrand
          Matrix.MXSeq  = fGetNextMXSeq()
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

FUNCTION fBDestAvailable RETURNS LOGICAL
   (icBDest AS CHARACTER):

   RETURN CAN-FIND(FIRST BDest NO-LOCK WHERE
                         BDest.Brand    = Syst.Var:gcBrand AND
                         BDest.BDest    = icBDest          AND
                         BDest.DestType = 0                AND
                         BDest.FromDate <= TODAY           AND
                         BDest.ToDate   >= TODAY).
     
END FUNCTION.

FUNCTION fCreateTMRItemValue RETURNS LOGICAL
   (iiTMRuleSeq AS INTEGER,
    icCounterItemValues AS CHARACTER):

   FIND FIRST TMRItemValue NO-LOCK WHERE
              TMRItemValue.TMRuleSeq         = iiTMRuleSeq         AND 
              TMRItemValue.CounterItemValues = icCounterItemValues AND
              TMRItemValue.ToDate           >= TODAY
   NO-ERROR.

   IF AVAILABLE TMRItemValue
   THEN UNDO, THROW NEW Progress.Lang.AppError
      (SUBSTITUTE("TMRItemValue where TMRuleSeq=&1 and CounterItemValues=&2" +
                  "is already defined and active",
                  iiTMRuleSeq, icCounterItemValues), 1). 

   CREATE TMRItemValue.
   ASSIGN 
      TMRItemValue.TMRuleSeq         = iiTMRuleSeq
      TMRItemValue.CounterItemValues = icCounterItemValues
      TMRItemValue.FromDate          = TODAY
      TMRItemValue.ToDate            = DATE(12,31,2049).        

   RETURN FALSE.

END FUNCTION.


PROCEDURE pTMRItemValue:

   DEFINE INPUT  PARAMETER icCLIType         AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER icMobileBundle    AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER icAllowedBundles  AS CHARACTER NO-UNDO.  
       
   DEFINE VARIABLE lcCodesToFind     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcBONOList        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llProcessBonoData AS LOGICAL   INITIAL FALSE NO-UNDO.
   DEFINE VARIABLE lii               AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcEntry           AS CHARACTER NO-UNDO.
     

   lcCodesToFind = SUBSTITUTE("&1_DATA_IN|GPRSDATA_&1",icMobileBundle).
   
   DO lii = 1 TO NUM-ENTRIES(lcCodesToFind,"|"):
      lcEntry = ENTRY(lii, lcCodesToFind, "|").
      IF fBDestAvailable(lcEntry)
      THEN DO:
         llProcessBonoData = TRUE.         
         fCreateTMRItemValue(14, lcEntry + "," + icCLIType).
      END.
   END.

   lcCodesToFind = SUBSTITUTE("&1_VOICE_IN",icMobileBundle).
   
   DO lii = 1 TO NUM-ENTRIES(lcCodesToFind,"|"):
      lcEntry = ENTRY(lii, lcCodesToFind, "|").
      IF fBDestAvailable(lcEntry)
      THEN DO:        
         fCreateTMRItemValue(34, lcEntry + "," + icCLIType).
         fCreateTMRItemValue(42, lcEntry + "," + icCLIType).
      END.
   END.
   
   IF llProcessBonoData
   THEN DO:
      lcBONOList = fCParamC("BONO_CONTRACTS").
      IF lcBONOList > "" THEN 
      DO lii = 1 TO NUM-ENTRIES(icAllowedBundles):     
         IF LOOKUP(ENTRY(lii,icAllowedBundles),lcBONOList) > 0
         THEN DO:
            fCreateTMRItemValue(33, "GPRSDATA_DATA*" + "," + icCLIType).
            LEAVE.
         END.
      END.
   END.


END PROCEDURE.



PROCEDURE pUpdateTMSParam:
   DEFINE INPUT PARAMETER icParamCode AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER icCLIType   AS CHARACTER NO-UNDO.   

   FIND FIRST TMSParam WHERE TMSParam.Brand = Syst.Var:gcBrand  AND TMSParam.ParamCode EQ icParamCode NO-LOCK NO-ERROR.
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
