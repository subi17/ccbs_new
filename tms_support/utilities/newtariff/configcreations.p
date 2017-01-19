/*------------------------------------------------------------------------
  MODULE .......: configcreations.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: Tue Feb 10 15:36:57 EET 2015
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{commpaa.i}
katun = "Cron".
gcBrand = "1".
{cparam2.i}
{eventlog.i}
{ftransdir.i}
{tariffconfig.i}
{tariffcons.i}

DEFINE BUFFER bCTServPac  FOR CTServPac.
DEFINE BUFFER bCTServEl   FOR CTServEl.
DEFINE BUFFER bCTServAttr FOR CTServAttr.
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

/* ***************************  Main Block  *************************** */
PROCEDURE pCreBDestination:
DEFINE INPUT PARAMETER icTariffCode AS CHARACTER NO-UNDO.    
DEFINE INPUT PARAMETER icDataLimit  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icVoiceLimit AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icBDestLimit AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcBDestination AS CHARACTER NO-UNDO.
DEFINE VARIABLE liBDCount      AS INTEGER   NO-UNDO.
DEFINE VARIABLE liBDLValue     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcBDestList    AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCCN          AS INTEGER   NO-UNDO.

IF icDataLimit NE "" THEN
   lcBDestList = "DATA_IN,DATA_OUT".

IF icVoiceLimit NE "" OR 
   icBDestLimit NE "" THEN 
   lcBDestList = IF lcBDestList NE "" THEN 
                    lcBDestList + "," + "VOICE_IN,VOICE_OUT"
                 ELSE "VOICE_IN,VOICE_OUT". 
                         
DO liBDCount = 1 TO NUM-ENTRIES(lcBDestList):
 
   ASSIGN lcBDestination = icTariffCode + "_" +
                           TRIM(ENTRY(liBDCount,lcBDestList,","))
          liCCN          = IF TRIM(ENTRY(liBDCount,lcBDestList,",")) BEGINS "DATA" 
                           THEN 93 ELSE 81.
    
   FIND FIRST BDest WHERE 
              BDest.Brand = gcBrand        AND 
              BDest.BDest = lcBDestination NO-LOCK NO-ERROR.
 
   IF NOT AVAILABLE BDest THEN DO:
    
      FIND LAST BDest USE-INDEX 
                BDestID NO-LOCK NO-ERROR.
    
      IF AVAILABLE BDest THEN 
         liBDLValue = BDest.BDestID + 1.
      ELSE liBDLValue = 1.
     
      CREATE BDest. 
      ASSIGN 
         BDest.Brand    = gcBrand    
         BDest.BDestID  = liBDLValue
         BDest.BDest    = lcBDestination
         BDest.BDName   = icTariffCode + " " + 
                          TRIM(ENTRY(liBDCount,lcBDestList,","))
         BDest.DestType = 0 
         BDest.CCN      = liCCN
         BDest.Class    = 1
         BDest.FromDate = TODAY 
         BDest.ToDate   = 12/31/49 NO-ERROR. 
       
      IF ERROR-STATUS:ERROR THEN 
         RETURN "Creating BDest".
               
   END.                 
END.
 
RETURN "OK".
   
END PROCEDURE.    

PROCEDURE pServLimitGroup:
DEFINE INPUT PARAMETER icTariffCode AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icTariffName AS CHARACTER NO-UNDO.

   FIND FIRST ServiceLimitGroup WHERE 
              ServiceLimitGroup.GroupCode = icTariffCode NO-LOCK NO-ERROR.
              
   IF AVAILABLE ServiceLimitGroup THEN
      RETURN "ServiceLimitGroup already exists".              

   CREATE ServiceLimitGroup.
   ASSIGN 
      ServiceLimitGroup.Brand     = gcBrand
      ServiceLimitGroup.GroupCode = icTariffCode    
      ServiceLimitGroup.GroupName = icTariffName    
      ServiceLimitGroup.ValidFrom = TODAY 
      ServiceLimitGroup.ValidTo   = 12/31/2049 NO-ERROR.    
   
   IF ERROR-STATUS:ERROR THEN 
      RETURN "Creating ServiceLimitGroup".

   RETURN "OK".
   
END PROCEDURE.

PROCEDURE pServiceLimit:
DEFINE INPUT  PARAMETER icTariffCode  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icTariff      AS CHARACTER NO-UNDO.  
DEFINE INPUT  PARAMETER icTariffName  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icTariffAmt   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icBDLimit     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icFMServLimit AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icLMServLimit AS CHARACTER NO-UNDO.  
DEFINE INPUT  PARAMETER icDialType    AS INTEGER   NO-UNDO.  
DEFINE OUTPUT PARAMETER liSLSeq       AS INTEGER   NO-UNDO.

DEFINE VARIABLE liInclUnit AS INTEGER NO-UNDO.

   IF ENTRY(2,icTariff,"_") EQ "DATA" THEN 
      liInclUnit = 4. 
   ELSE IF ENTRY(2,icTariff,"_") EQ "QTY" THEN
      liInclUnit = 7. 
   ELSE liInclUnit = 1.
   
   FIND LAST ServiceLimit NO-LOCK USE-INDEX SLSeq NO-ERROR.
               
   IF NOT AVAILABLE ServiceLimit 
      THEN liSLSeq = 1.
   ELSE liSLSeq = ServiceLimit.SLSeq + 1.
                  
   CREATE ServiceLimit.
   ASSIGN 
      ServiceLimit.GroupCode      = icTariffCode
      ServiceLimit.SLCode         = icTariff                                                    
      ServiceLimit.SLSeq          = liSLSeq 
      ServiceLimit.SLName         = icTariffName                             
      ServiceLimit.DialType       = icDialType
      ServiceLimit.InclAmt        = DECIMAL(icTariffAmt)                  
      ServiceLimit.InclUnit       = liInclUnit
      ServiceLimit.BDestLimit     = INTEGER(icBDLimit)
      ServiceLimit.ValidFrom      = TODAY 
      ServiceLimit.ValidTo        = 12/31/2049  
      ServiceLimit.FirstMonthCalc = IF icFMServLimit EQ "Full" THEN 0 ELSE 1
      ServiceLimit.LastMonthCalc  = IF icLMServLimit EQ "Full" THEN 0 ELSE 1
      Servicelimit.Web            = 1  NO-ERROR.                          
        
   IF ERROR-STATUS:ERROR THEN 
      RETURN "Creating ServiceLimit".
   ELSE            
      RETURN "OK".
      
END PROCEDURE.    

PROCEDURE pServiceTargets:
DEFINE INPUT PARAMETER iiSLSeq       AS INTEGER   NO-UNDO.    
DEFINE INPUT PARAMETER icTariffCode  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icTariffLimit AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcFinalLimitIN  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFinalLimitOUT AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBCList        AS CHARACTER NO-UNDO.
DEFINE VARIABLE liSTCount       AS INTEGER   NO-UNDO.

   IF icTariffLimit EQ "DATA" THEN 
      ASSIGN
         lcFinalLimitIN  = icTariffCode + "_DATA_IN"
         lcFinalLimitOUT = icTariffCode + "_DATA_OUT"
         lcBCList        = "14100001".
   
   IF icTariffLimit EQ "MIN" OR 
      icTariffLimit EQ "QTY" THEN 
      ASSIGN
         lcFinalLimitIN  = icTariffCode + "_VOICE_IN"
         lcFinalLimitOUT = icTariffCode + "_VOICE_OUT"
         lcBCList        = "10100001,10100003,10100005,CFOTHER,CFYOIGO".                   

   FIND FIRST BDest WHERE 
              BDest.Brand = gcBrand AND 
              BDest.BDest = lcFinalLimitIN NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE BDest THEN                 
      RETURN "ERROR: BDest-IN doesn't exists".             

   FIND FIRST BDest WHERE 
              BDest.Brand = gcBrand AND 
              BDest.BDest = lcFinalLimitOUT NO-LOCK NO-ERROR.

   IF NOT AVAILABLE BDest THEN 
      RETURN "ERROR: BDest-OUT doesn't exists".             

   DO liSTCount = 1 TO NUM-ENTRIES(lcBCList):  
      CREATE ServiceLimitTarget. 
      ASSIGN 
         ServiceLimitTarget.Slseq          = iiSLSeq
         ServiceLimitTarget.ServiceLMember = ENTRY(liSTCount,lcBCList,",")
         ServiceLimitTarget.InsideRate     = lcFinalLimitIN    /* Need to know */
         ServiceLimitTarget.outsideRate    = lcFinalLimitOUT NO-ERROR.
   
      IF ERROR-STATUS:ERROR THEN 
         RETURN "ERROR: Creating ServiceLimitTarget".
   END.
   
   RETURN "OK".
                
END PROCEDURE.    

PROCEDURE pCreateFeeModel:
DEFINE INPUT  PARAMETER icTariffCode AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icFMName     AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER lcFeeModel   AS CHARACTER NO-UNDO.

   IF icTariffCode EQ "" THEN
      RETURN "ERROR: Wrong FeeModel data available".
   ELSE
      lcFeeModel = icTariffCode + "MF".
    
   FIND FIRST FeeModel WHERE 
              FeeModel.FeeModel = lcFeeModel NO-LOCK NO-ERROR.
    
   IF AVAILABLE FeeModel THEN
      RETURN "ERROR: FeeModel already exists". 
                       
   CREATE FeeModel.
   ASSIGN 
      FeeModel.Brand    = gcBrand
      FeeModel.FeeModel = lcFeeModel
      FeeModel.FeeName  = icFMName               
      FeeModel.FMGroup  = 0 NO-ERROR.
  
   IF ERROR-STATUS:ERROR THEN 
      RETURN "ERROR: Creating Feemodel".
   ELSE 
      RETURN "OK".
                 
END PROCEDURE. 

PROCEDURE pCreateFMItem:
DEFINE INPUT PARAMETER icTariffCode    AS CHARACTER NO-UNDO.    
DEFINE INPUT PARAMETER icFeeModel      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icAmount        AS CHARACTER NO-UNDO.    
DEFINE INPUT PARAMETER icFMFeeCalc     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icLMFeeCalc     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icMFBC          AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lcMainPTariff   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lcTariffBundle  AS CHARACTER NO-UNDO.

DEFINE VARIABLE liFMFeeCalc AS INTEGER NO-UNDO.
DEFINE VARIABLE liLMFeeCalc AS INTEGER NO-UNDO.
DEFINE VARIABLE lcRatePlan AS CHARACTER NO-UNDO.

   IF icTariffCode EQ "" OR 
      icFeeModel   EQ "" THEN 
      RETURN "ERROR: Wrong FFItem data available".
      
   IF icFMFeeCalc BEGINS "F" THEN 
      liFMFeeCalc = 1.
   ELSE IF icFMFeeCalc BEGINS "U" THEN
      liFMFeeCalc = 2.
   ELSE liFMFeeCalc = 0.
         
   IF icLMFeeCalc BEGINS "F" THEN
      liLMFeeCalc = 1.
   ELSE IF icLMFeeCalc BEGINS "U" THEN
      liLMFeeCalc = 2.
   ELSE liLMFeeCalc = 0.
   
   IF lcTariffBundle NE "" THEN DO:
      FIND FIRST CLIType WHERE
                 CLIType.Brand   = gcBrand       AND
                 CLIType.CLIType = lcMainPTariff NO-LOCK NO-ERROR.

      IF AVAILABLE CLIType THEN
         ASSIGN lcRatePlan = CLIType.PricePlan.

   END.
   
   FIND FIRST RatePlan WHERE 
              RatePlan.Brand    = gcBrand AND 
              RatePlan.RatePlan = IF lcRatePlan NE "" THEN lcRatePlan 
                                  ELSE REPLACE(icTariffCode,"CONT","CONTRATO")
   NO-LOCK NO-ERROR.           
                
   IF NOT AVAILABLE RatePlan THEN 
      RETURN lcRateplan + " ERROR: RatePlan doesn't exists " + REPLACE(icTariffCode,"CONT","CONTRATO").
   
   FIND FIRST PListConf WHERE 
              PListConf.Brand    = gcBrand           AND 
              PListConf.RatePlan = RatePlan.RatePlan AND 
              PListConf.PriceList BEGINS "CONTRATO"  NO-LOCK NO-ERROR.   
   
   IF NOT AVAILABLE PListConf THEN 
      RETURN "ERROR: PListConf doesn't exists".
                             
   CREATE FMItem. 
   ASSIGN     
      FMItem.Brand             = gcBrand
      FMItem.PriceList         = PListConf.PriceList                                               
      FMItem.BillCode          = icMFBC            
      FMItem.FeeModel          = icFeeModel                  
      FMItem.FromDate          = TODAY       
      FMItem.ToDate            = 12/31/49 
      FMItem.BillType          = "MF"                  
      FMItem.Interval          = 1                   
      FMItem.BillCycle         = 2                   
      FMItem.FFItemQty         = 0 
      FMItem.FFEndDate         = ? 
      FMItem.Amount            = DECIMAL(icAmount)   
      FMItem.FirstMonthBR      = liFMFeeCalc
      FMItem.BrokenRental      = liLMFeeCalc
      FMItem.ServiceLimitGroup = ""  NO-ERROR.

   IF ERROR-STATUS:ERROR THEN 
      RETURN "ERROR: Creating FFItem".
   ELSE "OK".
   
END PROCEDURE.    

PROCEDURE pDayCampaign:
DEFINE INPUT PARAMETER icTariffCode  AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER icFeeModel    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icDCName      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icBBProfile   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icDSS2Comp    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icDSS2PL      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icNVComp      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icOVoip       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icTOC         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ilgSLCreated  AS LOGICAL   NO-UNDO.
DEFINE INPUT PARAMETER icMFBC        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icPaymentType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icBundleUpsell AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcFeeModel  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTOC       AS CHARACTER NO-UNDO.
DEFINE VARIABLE liPayType   AS INTEGER   NO-UNDO.  

   IF CAN-FIND(FIRST DayCampaign WHERE
                     DayCampaign.Brand   = gcBrand AND
                     DayCampaign.DCEvent = icTariffCode) THEN
      RETURN "ERROR: DayCampaign already exists".
 
   IF icPaymentType EQ "Postpaid" AND
      NOT CAN-FIND(FIRST FeeModel WHERE 
                         FeeModel.FeeModel = icFeeModel) THEN 
      RETURN "ERROR: FeeModel doesn't exists".                   
                            
   IF icTOC EQ "ServicePackage" THEN lcTOC = "1".
   ELSE IF icTOC EQ "PackageWithCounter" THEN lcTOC = "4".
  
   IF icPaymentType EQ "Postpaid" THEN 
      liPayType = 1.
   ELSE IF icPaymentType EQ "Prepaid" THEN
      liPayType = 2.
   ELSE liPayType = 0.   

   CREATE DayCampaign.
   ASSIGN 
      DayCampaign.Brand           = gcBrand
      DayCampaign.DCEvent         = icTariffCode 
      DayCampaign.DCName          = icDCName
      DayCampaign.PayType         = liPayType
      DayCampaign.ValidFrom       = TODAY 
      DayCampaign.ValidTo         = 12/31/49
      DayCampaign.StatusCode      = 1           /* Default value Active */
      DayCampaign.DCType          = lcTOC
      DayCampaign.InstanceLimit   = 1                            
      DayCampaign.BillCode        = icMFBC 
      DayCampaign.CCN             = IF icPaymentType EQ "Prepaid"
                                    THEN 93
                                    ELSE 0
      DayCampaign.InclUnit        = INTEGER(fTMSCodeValue("DayCampaign",
                                                          "InclUnit",
                                                          "Unit"))
      DayCampaign.InclStartCharge = YES                          
      DayCampaign.MaxChargeIncl   = 0                            
      DayCampaign.MaxChargeExcl   = 0                            
      DayCampaign.CalcMethod      = INTEGER(fTMSCodeValue("Daycampaign",
                                                          "CalcMethod",
                                                          "DCCounter"))
      DayCampaign.Effective       = INTEGER(fTMSCodeValue("Daycampaign",
                                                          "Effective",
                                                          "PerContr"))
      DayCampaign.DurType         = IF ilgSLCreated THEN 1 ELSE 4
      DayCampaign.DurMonth        = 0
      DayCampaign.DurUnit         = INTEGER(fTMSCodeValue("Daycampaign",
                                                          "DurUnit",
                                                          "PerContr"))
      DayCampaign.WeekDay         = ""
      DayCampaign.BundleUpsell    = icBundleUpsell
      DayCampaign.FeeModel        = icFeeModel
      DayCampaign.ModifyFeeModel  = ""                          
      DayCampaign.TermFeeModel    = ""                          
      DayCampaign.TermFeeCalc     = 0                  
/*      DayCampaign.BBProfile       = INTEGER(icBBProfile) 
      DayCampaign.DDS2Compatible  = LOGICAL(icDSS2Comp) 
      DayCampaign.DSS2PrimaryLine = LOGICAL(icDSS2PL) 
      DayCampaign.NativeVoipComp  = LOGICAL(icNVComp) 
      DayCampaign.OnlyVoice       = LOGICAL(icOVoip) */
      NO-ERROR.     
   
   IF ERROR-STATUS:ERROR THEN 
      RETURN "ERROR: Creating DayCampaign".
   ELSE 
      RETURN "OK".    
   
END PROCEDURE.   

PROCEDURE pDCServPackage:
DEFINE INPUT PARAMETER icTariffCode  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ilgDataLimit  AS LOGICAL   NO-UNDO.
DEFINE INPUT PARAMETER icBonoSupport AS CHARACTER NO-UNDO.

DEFINE VARIABLE liPackageID   AS INTEGER NO-UNDO.
DEFINE VARIABLE liComponentID AS INTEGER NO-UNDO.

    IF NOT CAN-FIND(FIRST DayCampaign WHERE 
                          DayCampaign.DCEvent = icTariffCode) THEN
       RETURN "ERROR: DayCampaign doesn't exists".
    
    FIND LAST DCServicePackage USE-INDEX DCServicePackageID 
              NO-LOCK NO-ERROR.
    
    IF AVAILABLE DCServicePackage THEN 
       liPackageID = DCServicePackage.DCServicePackageID + 1.
    ELSE liPackageID = 1.
           
    CREATE DCServicePackage.
    ASSIGN                            
       DCServicePackage.Brand               = gcBrand 
       DCServicePackage.DCEvent             = icTariffCode 
       DCServicePackage.DCServicePackageID  = liPackageID
       DCServicePackage.ServPac             = "SHAPER"                    
       DCServicePackage.FromDate            = TODAY 
       DCServicePackage.ToDate              = 12/31/49 NO-ERROR.
        
    IF ERROR-STATUS:ERROR THEN
       RETURN "ERROR: Creating DServicePackage".
    
    IF NOT CAN-FIND(FIRST DCServiceComponent WHERE
                          DCServiceComponent.DCServicePackageID = liPackageID AND
                          DCServiceComponent.DefParam  = icTariffCode AND  
                          DCServiceComponent.ServCom   = "SHAPER"     AND
                          DCServiceComponent.FromDate <= TODAY        AND
                          DCServiceComponent.ToDate   >= 12/31/49)    AND 
       ilgDataLimit THEN DO:
       
       FIND LAST DCServiceComponent USE-INDEX DCServiceComponentID 
              NO-LOCK NO-ERROR.
       IF AVAILABLE DCServiceComponent THEN 
          liComponentID = DCServiceComponent.DCServiceComponentID + 1.
       ELSE liComponentID = 1.    
       
       CREATE DCServiceComponent.
       ASSIGN 
          DCServiceComponent.DCServiceComponentID = liComponentID
          DCServiceComponent.DCServicePackageID   = liPackageID 
          DCServiceComponent.ServCom              = "SHAPER"
          DCServiceComponent.DefValue             = 1
          DCServiceComponent.DefParam             = IF LOGICAL(icBonoSupport) THEN 
                                                       icTariffCode + "#ADDBUNDLE"
                                                    ELSE icTariffCode   
          DCServiceComponent.FromDate             = TODAY 
          DCServiceComponent.ToDate               = 12/31/49 NO-ERROR.
       
       IF ERROR-STATUS:ERROR THEN  
          RETURN "ERROR: Creating DCServicePackage".
    END.
/*    ELSE 
       RETURN "DCServiceComponent already exists". */
       
    RETURN "OK".
       
END PROCEDURE.     

PROCEDURE pCreateCLIType:
DEFINE INPUT  PARAMETER icCLIType        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icCLIName        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icBaseBundle     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icLineType       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icFixLineType    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icCommFee        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icComparisonFee  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icServClass      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icWebStatCode    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icStatCode       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icPayType        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icUsageType      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icRateplan       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER llgCTServPac     AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER lcMainPTariff    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER lcTariffBundle   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocCLIType        AS CHARACTER NO-UNDO.

DEFINE VARIABLE liFinalBT  AS INTEGER   NO-UNDO.
DEFINE VARIABLE liFinalCR  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcRatePlan AS CHARACTER NO-UNDO.

   IF NOT CAN-FIND(FIRST CLIType WHERE 
                         CLIType.Brand   = gcBrand    AND 
                         CLIType.CLIType = icCLIType) THEN DO:
     
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
      IF lcTariffBundle NE "" THEN DO:   
         FIND FIRST CLIType WHERE 
                    CLIType.Brand   = gcBrand       AND 
                    CLIType.CLIType = lcMainPTariff NO-LOCK NO-ERROR.
                
         IF AVAILABLE CLIType THEN
            ASSIGN lcRatePlan = CLIType.PricePlan
                   liFinalBT  = CLIType.BillTarget.
               
      END.
       
      CREATE CLIType.
      ASSIGN 
         CLIType.Brand         = gcBrand
         CLIType.CLIType       = icCLIType
         CLIType.CLIName       = icCLIName                            
         CLIType.BaseBundle    = icBaseBundle
         CLIType.PricePlan     = IF lcRatePlan NE "" THEN lcRatePlan ELSE 
                                 REPLACE(icCLIType,"CONT","CONTRATO") 
         CLIType.ServicePack   = IF icPayType EQ "Postpaid" THEN "11" 
                                 ELSE "12"
         CLIType.LineType      = INTEGER(fTMSCValue("CLIType",
                                                    "LineType",
                                                    icLineType))
         CLIType.FixedLineType = INTEGER(fTMSCValue("CLIType",
                                                    "FixedLineType",
                                                    icFixLineType))
         CLIType.CommercialFee = DECIMAL(icCommFee)
         CLIType.CompareFee    = DECIMAL(icComparisonFee)
         ClIType.ServiceClass  = icServClass
         CLIType.WebStatusCode = INTEGER(fTMSCValue("CLIType",
                                                    "WebStatusCode",
                                                    icWebStatCode))
         CLIType.StatusCode    = INTEGER(fTMSCValue("CLIType",
                                                    "StatusCode",
                                                    icStatCode))
         CLIType.PayType       = INTEGER(fTMSCValue("CLIType",
                                                    "PayType",
                                                    icPayType))
         CLIType.UsageType     = INTEGER(fTMSCValue("CLIType",
                                                    "UsageType",
                                                    icUsageType)) 
         CLIType.ARAccNum      = IF CLIType.PayType = 1 THEN 43000000 
                                 ELSE IF CLIType.PayType = 2 THEN 43001000
                                 ELSE 0             
         CLIType.BillTarget    = liFinalBT + 1
         CLIType.ContrType     = IF icPayType EQ "Postpaid" THEN liFinalCR + 1 
                                 ELSE 1
         ocCLIType             = icCLIType           NO-ERROR.
                                            
      IF ERROR-STATUS:ERROR THEN 
         RETURN "ERROR: Creating CLIType".

      
      IF llgCTServPac THEN 
         RUN pCreServPack(INPUT ocCLIType,
                          INPUT icPayType) NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN 
         RETURN "ERROR: Creating CLIType Servpacks". 
   END.
   ELSE 
      RETURN "ERROR: CLIType already exists".
     
  RETURN "OK".                                                                                                                        
END PROCEDURE.    

PROCEDURE pCreServPack:
DEFINE INPUT PARAMETER icCLIType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icPayType AS CHARACTER NO-UNDO.

DEFINE VARIABLE liCTServEl AS INTEGER NO-UNDO.

   IF icPayType EQ "Postpaid" THEN  
      FIND FIRST CLIType WHERE 
                 CLIType.Brand   = gcBrand  AND 
                 CLIType.CLIType = "CONTS"  NO-LOCK NO-ERROR.
   ELSE IF icPayType EQ "Prepaid" THEN 
      FIND FIRST CLIType WHERE 
                 CLIType.Brand   = gcBrand  AND 
                 CLIType.CLIType = "TARJ7"  NO-LOCK NO-ERROR.       
   
   IF AVAILABLE CLIType THEN DO:              
      FOR EACH CTServPac WHERE 
               CTServPac.Brand   = gcBrand         AND 
               CTServPac.CLIType = CLIType.CLIType NO-LOCK:
         CREATE bCTServPac.
         BUFFER-COPY CTServPac EXCEPT CTServPac.CLIType 
                                      CTServPac.FromDate
                                   TO bCTServPac.
         ASSIGN bCTServPac.CLIType  = icCLIType
                bCTServPac.FromDate = TODAY NO-ERROR.
                
         IF ERROR-STATUS:ERROR THEN 
            RETURN "ERROR: Creating CTServPac".
            
         FOR EACH CTServEl WHERE 
                  CTServEl.Brand   = gcBrand           AND 
                  CTServEl.CLIType = CTServPac.CLIType AND 
                  CTServEl.ServPac = CTServPac.ServPac NO-LOCK:
            CREATE bCTServEl.
            BUFFER-COPY CTServEl EXCEPT CTServEl.CTServEl
                                        CTServEl.CLIType
                                        CTServEl.FromDate
                                     TO bCTServEl.
            ASSIGN bCTServEl.CTServEl = NEXT-VALUE(CTServEl)
                   bCTServEl.CLIType  = icCLIType 
                   bCTServEl.FromDate = TODAY 
                   liCTServEl         = bCTServEl.CTServEl NO-ERROR.                                        
         
            IF ERROR-STATUS:ERROR THEN 
               RETURN "ERROR: Creating CTServEl".
               
            FIND ServCom WHERE
                 ServCom.Brand   = gcBrand AND
                 ServCom.ServCom = CTServEl.ServCom NO-LOCK NO-ERROR.
            
            IF AVAILABLE ServCom AND ServCom.ServAttr = TRUE THEN    
               FOR EACH CTServAttr WHERE 
                        CTServAttr.CTServEl = CTServEl.CTServEl NO-LOCK:
                  CREATE bCTServAttr.
                  BUFFER-COPY CTServAttr EXCEPT CTServAttr.CTServEl
                                                CTServAttr.FromDate
                                             TO bCTServAttr.
                  ASSIGN bCTServAttr.CTServEl = liCTServEl
                         bCTServAttr.FromDate = TODAY NO-ERROR. 
                  
                  IF ERROR-STATUS:ERROR THEN 
                     RETURN "ERROR: Creating CTServAttr".                                        
               END.            
         END.                     
      END.
   END.
   ELSE 
      RETURN "ERROR: CLIType doesn't exists for creating ctservpac".
      
   RETURN "OK".
                             
END PROCEDURE.               
  
