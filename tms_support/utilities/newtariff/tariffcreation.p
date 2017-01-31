/*------------------------------------------------------------------------
  MODULE .......: tariffcreation.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: Wed Feb 04 11:12:54 EET 2015
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/  

/* ***************************  Definitions  ************************** */
ROUTINE-LEVEL ON ERROR UNDO, THROW.

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{cparam2.i}
{eventlog.i}
{ftransdir.i}
{tariffconfig.i}
{tariffcons.i}

DEFINE INPUT  PARAMETER icIncDir    AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icSpoolDir  AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcLogFile           AS CHARACTER NO-UNDO.
DEFINE VARIABLE h_config            AS HANDLE    NO-UNDO.

/* General Subscription Type Attributes */
DEFINE VARIABLE lcCliType                                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCliName                                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTariffBundle                           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRatePlanAction                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRatePlan                               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcReferenceRatePlan                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcWebStatus                              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSTCStatus                              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPaymentType                            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcUsageType                              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLineType                               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixLineType                            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLineDownload                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLineUpload                        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcServiceClass                           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCommFee                                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcComparisonFee                          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcAllowedBundles                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBundlesForActivateOnSTC                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcServicesForReCreateOnSTC               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCopyServicesFromCliType                AS CHARACTER NO-UNDO.
/* Mobile Base Bundle Attributes */
DEFINE VARIABLE lcMobile_BaseBundle                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_BaseBundleType                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_BaseBundleUpsell                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_BaseBundleBonoSupport           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_MonthlyFeeBillCode              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_CommercialFee                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_FirstMonthFeeCalc               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_LastMonthFeeCalc                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_DataLimit                       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_VoiceLimit                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_BDestLimit                      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_DataLimit_FirstMonthFeeCalc     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_DataLimit_LastMonthFeeCalc      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_VoiceLimit_FirstMonthFeeCalc    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_VoiceLimit_LastMonthFeeCalc     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_BDestLimit_FirstMonthFeeCalc    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_BDestLimit_LastMonthFeeCalc     AS CHARACTER NO-UNDO.
/* FixedLine Base Bundle Attributes */
DEFINE VARIABLE lcFixedLine_BaseBundle                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_BaseBundleType               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_BaseBundleUpsell             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_BaseBundleBonoSupport        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_MonthlyFeeBillCode           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_CommercialFee                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_FirstMonthFeeCalc            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_LastMonthFeeCalc             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_VoiceLimit                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_BDestLimit                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_VoiceLimit_FirstMonthFeeCalc AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_VoiceLimit_LastMonthFeeCalc  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_BDestLimit_FirstMonthFeeCalc AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLine_BDestLimit_LastMonthFeeCalc  AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttTariffCre NO-UNDO 
   FIELD FieldName  AS CHARACTER 
   FIELD FieldValue AS CHARACTER.
   
DEFINE STREAM TariffIn.
DEFINE STREAM TariffLog.
DEFINE STREAM TTransIn.
/* ********************  Functions  ******************** */
FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   icMessage = "ERROR:" + icMessage.
   
   PUT STREAM TariffLog UNFORMATTED
      TODAY                   " | " 
      STRING(TIME,"HH:MM:SS") " | "
      icMessage SKIP.
      
END FUNCTION.

/* ***************************  Main Block  *************************** */
DO ON ERROR UNDO, THROW:  

   ASSIGN lcLogFile = icSpoolDir + "tariffcreation.log".       

   RUN configcreations.p PERSISTENT SET h_config. 

   FUNCTION fTMSCValue RETURNS CHARACTER (iTableName AS CHAR, iFieldName AS CHAR,iCodeName AS CHAR) IN h_config.

   RUN pReadTariff. 

   RUN pReadCustomRatesForRateplan.  

   RUN pValidateData.

   RUN pProcessTT.

   RUN pSaveTariff.

   RUN pReadTranslation.

   RUN pSaveTranslation.

   RETURN "OK".

   CATCH e AS Progress.Lang.Error:
      OUTPUT STREAM TariffLog TO VALUE(lcLogFile) APPEND.
      fError(e:GetMessage(1)).
      OUTPUT STREAM TariffLog CLOSE.
      UNDO, THROW e.
   END CATCH.
   FINALLY:

   END FINALLY.
END.
/* ***************************  Main End  *************************** */ 
PROCEDURE pSaveTariff:
  DEFINE VARIABLE liSLSeq           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE llFeeModelCreated AS LOGICAL   NO-UNDO.

  DO ON ERROR UNDO, THROW:

      FOR EACH ttCliType
          ON ERROR UNDO, THROW:
         
         RUN pCliType IN h_config(BUFFER ttCliType).

         FOR EACH ttDayCampaign WHERE ttDayCampaign.CliType = ttCliType.CliType 
             ON ERROR UNDO, THROW:

              RUN pDayCampaign IN h_config(BUFFER ttDayCampaign).

              FOR EACH ttFMItem WHERE ttFMItem.FeeModel = ttDayCampaign.BillCode 
                  ON ERROR UNDO, THROW:

                  RUN pFeeModel IN h_config(ttFMItem.FeeModel, ttCliType.CliName, OUTPUT llFeeModelCreated).
                  
                  IF llFeeModelCreated THEN   
                      RUN pFMItem IN h_config(BUFFER ttFMItem, BUFFER ttCliType).

              END. 

              FOR EACH ttServiceLimitGroup WHERE ttServiceLimitGroup.GroupCode = ttDayCampaign.DCEvent
                  ON ERROR UNDO, THROW:
                   
                  RUN pServiceLimitGroup IN h_config(BUFFER ttServiceLimitGroup). 

                  FOR EACH ttServiceLimit WHERE ttServiceLimit.GroupCode = ttServiceLimitGroup.GroupCode
                      ON ERROR UNDO, THROW:

                      RUN pServiceLimit IN h_config(BUFFER ttServiceLimit, OUTPUT liSLSeq).

                      IF liSLSeq > 0 THEN 
                      DO:
                          /* ServiceLimitTarget */
                          FOR EACH ttServiceLimitTarget WHERE ttServiceLimitTarget.GroupCode = ttServiceLimit.GroupCode AND ttServiceLimitTarget.SLCode = ttServiceLimit.SLCode
                              ON ERROR UNDO, THROW:

                              FOR EACH ttBDest WHERE ttBDest.GroupCode = ttServiceLimitTarget.GroupCode AND ttBDest.SLCode = ttServiceLimitTarget.SLCode 
                                  ON ERROR UNDO, THROW:
                                  RUN pBDestination IN h_config(BUFFER ttBDest).
                              END.

                              RUN pServiceLimitTarget IN h_config(BUFFER ttServiceLimitTarget, liSLSeq).                                                  
                          END.

                          /* ProgLimit */  
                          FOR EACH ttProgLimit WHERE ttProgLimit.GroupCode = ttServiceLimit.GroupCode AND ttProgLimit.SLCode = ttServiceLimit.SLCode
                              ON ERROR UNDO, THROW:

                              FOR EACH ttBDest WHERE ttBDest.GroupCode = ttServiceLimit.GroupCode AND ttBDest.SLCode = ttServiceLimit.SLCode 
                                  ON ERROR UNDO, THROW:
                                  RUN pBDestination IN h_config(BUFFER ttBDest).
                              END.

                              RUN pProgLimit IN h_config(BUFFER ttProgLimit, liSLSeq).  
                          END.

                      END.  /* IF liSeq > 0 THEN */
                  END.  /* FOR EACH ttServiceLimit */
              END.  /* FOR EACH ttServiceLimitGroup */
         END. /* FOR EACH ttDayCampaign */

         FOR EACH ttTMRItemValue WHERE ttTMRItemValue.CliType = ttCliType.CliType
             ON ERROR UNDO, THROW:
             RUN pTMRItemValue IN h_config(BUFFER ttTMRItemValue).
         END.

      END. /* FOR EACH ttCliType */
      CATCH e AS Progress.Lang.Error:
          UNDO, THROW e.
      END CATCH.
  END.

  RETURN "".

END PROCEDURE.


PROCEDURE pReadTariff:   

   DEFINE VARIABLE lcLine      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO.

   DO ON ERROR UNDO, THROW:   
      
      ASSIGN lcInputFile = icIncDir + "tariffcreation.txt".

      INPUT STREAM TariffIn FROM VALUE(lcInputFile).

      REPEAT ON ERROR UNDO, THROW:
         IMPORT STREAM TariffIn UNFORMATTED lcLine.
    
         CREATE ttTariffCre.
         ASSIGN 
            ttTariffCre.FieldName  = TRIM(ENTRY(1,lcLine,";"))
            ttTariffCre.FieldValue = TRIM(ENTRY(2,lcLine,";")).

      END.      

      CATCH err AS Progress.Lang.Error:
         UNDO, THROW NEW Progress.Lang.AppError('Incorrect input file (tariffcreation.txt) data' + err:GetMessage(1), 1). 
      END CATCH.

      FINALLY:
         INPUT STREAM TariffIn CLOSE.
      END FINALLY.
   END.

   RETURN "".

END PROCEDURE.

PROCEDURE pReadCustomRatesForRateplan:
    DEFINE VARIABLE lcLine      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE liFirstLine AS INTEGER   NO-UNDO INITIAL 1.

    DO ON ERROR UNDO, THROW:
        ASSIGN lcInputFile = icIncDir + "rptariff_new.txt".
        
        FILE-INFO:FILE-NAME = lcInputFile.

        IF FILE-INFO:PATHNAME <> "" THEN   
        DO ON ERROR UNDO, THROW:

            INPUT STREAM TariffIn FROM VALUE(lcInputFile).
            
            REPEAT ON ERROR UNDO, THROW:
               IMPORT STREAM TariffIn UNFORMATTED lcLine.
               /* Ignore the first line - (Header) */
               IF liFirstLine = 1 THEN 
               DO:
                   liFirstLine = liFirstLine + 1.
                   NEXT.
               END.
                
               CREATE ttTariff.
               ASSIGN    
                  ttTariff.PriceList = TRIM(ENTRY(1,lcLine,";")) 
                  ttTariff.CCN       = TRIM(ENTRY(2,lcLine,";")) 
                  ttTariff.BDest     = TRIM(ENTRY(3,lcLine,";"))
                  ttTariff.BillItem  = TRIM(ENTRY(4,lcLine,";"))
                  ttTariff.PriceUnit = TRIM(ENTRY(5,lcLine,";"))
                  ttTariff.Price     = TRIM(ENTRY(6,lcLine,";"))
                  ttTariff.SetupFee  = TRIM(ENTRY(7,lcLine,";")).          
            END.

            CATCH err AS Progress.Lang.Error:
               UNDO, THROW NEW Progress.Lang.AppError('Incorrect input file (rptariff_new.txt) data' + err:GetMessage(1), 1). 
            END CATCH.

            FINALLY:
               INPUT STREAM TariffIn CLOSE.
               DELETE OBJECT FILE-INFO NO-ERROR.
            END FINALLY.       
        END.   
    END.

    RETURN "".

END PROCEDURE.

PROCEDURE pProcessTT:   
   DEFINE VARIABLE liFirstMonthBR    AS INTEGER NO-UNDO.
   DEFINE VARIABLE liLastMonthBR     AS INTEGER NO-UNDO.   

   DEFINE VARIABLE liDLFirstMonthBR  AS INTEGER NO-UNDO.
   DEFINE VARIABLE liDLLastMonthBR   AS INTEGER NO-UNDO.

   DEFINE VARIABLE liVLFirstMonthBR  AS INTEGER NO-UNDO.
   DEFINE VARIABLE liVLLastMonthBR   AS INTEGER NO-UNDO.

   DEFINE VARIABLE liBDLFirstMonthBR AS INTEGER NO-UNDO.
   DEFINE VARIABLE liBDLLastMonthBR  AS INTEGER NO-UNDO.  
   
   IF lcReferenceRatePlan > "" THEN    
       RUN pRatePlan IN h_config(lcRatePlan, lcCliName, lcReferenceRatePlan, lcRatePlanAction).

   IF CAN-FIND(FIRST ttTariff) THEN
   DO: 
       FOR EACH ttTariff 
            ON ERROR UNDO, THROW:
            RUN pCustomRates IN h_config(BUFFER ttTariff).            
       END.
   END.    

   IF lcTariffBundle > "" THEN
   DO:
       IF NOT CAN-FIND(FIRST CliType WHERE CliType.Brand = gcBrand AND CliType.CliType = lcCliType NO-LOCK) THEN 
       DO:
           /* Main Tariff */
           CREATE ttCliType.
           ASSIGN
              ttCliType.CliType                   = lcCliType
              ttCliType.CliName                   = lcCliName
              ttCliType.RatePlan                  = lcRatePlan              
              ttCliType.BaseBundle                = ""
              ttCliType.FixedLineBaseBundle       = ""
              ttCliType.WebStatusCode             = INTEGER(fTMSCValue("CLIType","WebStatusCode",lcWebStatus)) 
              ttCliType.StatusCode                = INTEGER(fTMSCValue("CLIType","StatusCode",lcSTCStatus)) 
              ttCliType.PayType                   = INTEGER(fTMSCValue("CLIType","PayType",lcPaymentType)) 
              ttCliType.UsageType                 = INTEGER(fTMSCValue("CLIType","UsageType",lcUsageType))  
              ttCliType.LineType                  = INTEGER(fTMSCValue("CLIType","LineType",lcLineType)) 
              ttCliType.FixedLineType             = INTEGER(fTMSCValue("CLIType","FixedLineType",lcFixLineType))                
              ttCliType.FixedLineDownload         = ""
              ttCliType.FixedLineUpload           = ""
              ttCliType.BundleType                = True
              ttCliType.Serviceclass              = lcServiceClass
              ttCliType.CommercialFee             = DECIMAL(lcCommFee)
              ttCliType.CompareFee                = DECIMAL(lcComparisonFee)              
              ttCliType.TariffBundle              = ""        
              ttCliType.ParentTariff              = ""      
              ttCliType.AllowedBundles            = lcAllowedBundles
              ttCliType.MobileBaseBundleDataLimit = 0
              ttCliType.BundlesForActivateOnSTC   = ""
              ttCliType.ServicesForReCreateOnSTC  = ""
              ttCliType.CopyServicesFromCliType   = lcCopyServicesFromCliType.
       END.
   END.

   /* Normal Tariff / Tariff Bundle */
   CREATE ttCliType.
   ASSIGN
      ttCliType.CliType                   = (IF lcTariffBundle > "" THEN lcTariffBundle ELSE lcCliType)
      ttCliType.CliName                   = lcCliName
      ttCliType.RatePlan                  = lcRatePlan
      ttCliType.BaseBundle                = lcMobile_BaseBundle 
      ttCliType.FixedLineBaseBundle       = lcFixedLine_BaseBundle
      ttCliType.WebStatusCode             = INTEGER(fTMSCValue("CLIType","WebStatusCode",lcWebStatus)) 
      ttCliType.StatusCode                = INTEGER(fTMSCValue("CLIType","StatusCode",lcSTCStatus)) 
      ttCliType.PayType                   = INTEGER(fTMSCValue("CLIType","PayType",lcPaymentType)) 
      ttCliType.UsageType                 = INTEGER(fTMSCValue("CLIType","UsageType",lcUsageType))  
      ttCliType.LineType                  = INTEGER(fTMSCValue("CLIType","LineType",(IF lcTariffBundle > "" THEN "Additional" ELSE lcLineType))) 
      ttCliType.FixedLineType             = INTEGER(fTMSCValue("CLIType","FixedLineType",lcFixLineType))  
      ttCliType.FixedLineDownload         = lcFixedLineDownload
      ttCliType.FixedLineUpload           = lcFixedLineUpload
      ttCliType.BundleType                = (IF lcTariffBundle > "" THEN True ELSE False)
      ttCliType.Serviceclass              = lcServiceClass
      ttCliType.CommercialFee             = DECIMAL(lcCommFee)
      ttCliType.CompareFee                = DECIMAL(lcComparisonFee)
      ttCliType.TariffBundle              = (IF lcTariffBundle > "" THEN lcTariffBundle ELSE "")  
      ttCliType.ParentTariff              = (IF lcTariffBundle > "" THEN lcCliType      ELSE "")  
      ttCliType.AllowedBundles            = (IF lcTariffBundle > "" THEN ""             ELSE lcAllowedBundles)
      ttCliType.MobileBaseBundleDataLimit = DECIMAL(lcMobile_DataLimit) 
      ttCliType.BundlesForActivateOnSTC   = lcBundlesForActivateOnSTC
      ttCliType.ServicesForReCreateOnSTC  = lcServicesForReCreateOnSTC
      ttCliType.CopyServicesFromCliType   = (IF lcTariffBundle > "" THEN "" ELSE lcCopyServicesFromCliType).

   IF lcPaymentType = "PostPaid" THEN 
   DO:
      IF lcMobile_BaseBundle > "" OR lcTariffBundle > "" THEN 
      DO:
         ASSIGN 
             liFirstMonthBR   = (IF lcMobile_FirstMonthFeeCalc BEGINS "Full" THEN 1 ELSE IF lcMobile_FirstMonthFeeCalc BEGINS "Usage" THEN 2 ELSE IF lcMobile_FirstMonthFeeCalc BEGINS "Relative" THEN 0 ELSE 0)
             liLastMonthBR    = (IF lcMobile_LastMonthFeeCalc  BEGINS "Full" THEN 1 ELSE IF lcMobile_LastMonthFeeCalc  BEGINS "Usage" THEN 2 ELSE IF lcMobile_LastMonthFeeCalc  BEGINS "Relative" THEN 0 ELSE 0)

             liDLFirstMonthBR = (IF lcMobile_DataLimit_FirstMonthFeeCalc BEGINS "Relative" THEN 1 ELSE IF lcMobile_DataLimit_FirstMonthFeeCalc BEGINS "Usage" THEN 2 ELSE IF lcMobile_DataLimit_FirstMonthFeeCalc BEGINS "Full" THEN 0 ELSE 0)
             liDLLastMonthBR  = (IF lcMobile_DataLimit_LastMonthFeeCalc  BEGINS "Relative" THEN 1 ELSE IF lcMobile_DataLimit_LastMonthFeeCalc  BEGINS "Usage" THEN 2 ELSE IF lcMobile_DataLimit_LastMonthFeeCalc  BEGINS "Full" THEN 0 ELSE 0) 

             liVLFirstMonthBR = (IF lcMobile_VoiceLimit_FirstMonthFeeCalc BEGINS "Relative" THEN 1 ELSE IF lcMobile_VoiceLimit_FirstMonthFeeCalc BEGINS "Usage" THEN 2 ELSE IF lcMobile_VoiceLimit_FirstMonthFeeCalc BEGINS "Full" THEN 0 ELSE 0)
             liVLLastMonthBR  = (IF lcMobile_VoiceLimit_LastMonthFeeCalc  BEGINS "Relative" THEN 1 ELSE IF lcMobile_VoiceLimit_LastMonthFeeCalc  BEGINS "Usage" THEN 2 ELSE IF lcMobile_VoiceLimit_LastMonthFeeCalc  BEGINS "Full" THEN 0 ELSE 0) 

             liBDLFirstMonthBR = (IF lcMobile_BDestLimit_FirstMonthFeeCalc BEGINS "Relative" THEN 1 ELSE IF lcMobile_BDestLimit_FirstMonthFeeCalc BEGINS "Usage" THEN 2 ELSE IF lcMobile_BDestLimit_FirstMonthFeeCalc BEGINS "Full" THEN 0 ELSE 0)
             liBDLLastMonthBR  = (IF lcMobile_BDestLimit_LastMonthFeeCalc  BEGINS "Relative" THEN 1 ELSE IF lcMobile_BDestLimit_LastMonthFeeCalc  BEGINS "Usage" THEN 2 ELSE IF lcMobile_BDestLimit_LastMonthFeeCalc  BEGINS "Full" THEN 0 ELSE 0).

         RUN pBundle("Mobile",
                     ttCliType.CliType,
                     (IF lcTariffBundle > "" THEN lcTariffBundle ELSE lcMobile_BaseBundle),
                     ttCliType.CliName,
                     lcMobile_BaseBundleType,
                     lcMobile_MonthlyFeeBillCode,
                     lcMobile_BaseBundleUpsell,
                     LOGICAL(lcMobile_BaseBundleBonoSupport),
                     DECIMAL(lcMobile_CommercialFee),
                     liFirstMonthBR,
                     liLastMonthBR,
                     DECIMAL(lcMobile_DataLimit),
                     liDLFirstMonthBR,
                     liDLLastMonthBR,
                     DECIMAL(lcMobile_VoiceLimit),
                     liVLFirstMonthBR,
                     liVLLastMonthBR,
                     DECIMAL(lcMobile_BDestLimit),
                     liBDLFirstMonthBR,
                     liBDLLastMonthBR).        
      END.

      IF lcFixedLine_BaseBundle > "" THEN 
      DO:
          ASSIGN 
             liFirstMonthBR   = (IF lcFixedLine_FirstMonthFeeCalc BEGINS "Full" THEN 1 ELSE IF lcFixedLine_FirstMonthFeeCalc BEGINS "Usage" THEN 2 ELSE IF lcFixedLine_FirstMonthFeeCalc BEGINS "Relative" THEN 0 ELSE 0)
             liLastMonthBR    = (IF lcFixedLine_LastMonthFeeCalc  BEGINS "Full" THEN 1 ELSE IF lcFixedLine_LastMonthFeeCalc  BEGINS "Usage" THEN 2 ELSE IF lcFixedLine_LastMonthFeeCalc  BEGINS "Relative" THEN 0 ELSE 0)

             liDLFirstMonthBR = 0
             liDLLastMonthBR  = 0

             liVLFirstMonthBR = (IF lcFixedLine_VoiceLimit_FirstMonthFeeCalc BEGINS "Relative" THEN 1 ELSE IF lcFixedLine_VoiceLimit_FirstMonthFeeCalc BEGINS "Usage" THEN 2 ELSE IF lcFixedLine_VoiceLimit_FirstMonthFeeCalc BEGINS "Full" THEN 0 ELSE 0)
             liVLLastMonthBR  = (IF lcFixedLine_VoiceLimit_LastMonthFeeCalc  BEGINS "Relative" THEN 1 ELSE IF lcFixedLine_VoiceLimit_LastMonthFeeCalc  BEGINS "Usage" THEN 2 ELSE IF lcFixedLine_VoiceLimit_LastMonthFeeCalc  BEGINS "Full" THEN 0 ELSE 0) 

             liBDLFirstMonthBR = (IF lcFixedLine_BDestLimit_FirstMonthFeeCalc BEGINS "Relative" THEN 1 ELSE IF lcFixedLine_BDestLimit_FirstMonthFeeCalc BEGINS "Usage" THEN 2 ELSE IF lcFixedLine_BDestLimit_FirstMonthFeeCalc BEGINS "Full" THEN 0 ELSE 0)
             liBDLLastMonthBR  = (IF lcFixedLine_BDestLimit_LastMonthFeeCalc  BEGINS "Relative" THEN 1 ELSE IF lcFixedLine_BDestLimit_LastMonthFeeCalc  BEGINS "Usage" THEN 2 ELSE IF lcFixedLine_BDestLimit_LastMonthFeeCalc  BEGINS "Full" THEN 0 ELSE 0).

          RUN pBundle("FixedLine",
                      ttCliType.CliType,
                      lcFixedLine_BaseBundle,
                      ttCliType.CliName,
                      lcFixedLine_BaseBundleType,
                      lcFixedLine_MonthlyFeeBillCode,
                      lcFixedLine_BaseBundleUpsell,
                      LOGICAL(lcFixedLine_BaseBundleBonoSupport),
                      DECIMAL(lcFixedLine_CommercialFee),
                      liFirstMonthBR,
                      liLastMonthBR,
                      0,
                      liDLFirstMonthBR,
                      liDLLastMonthBR,
                      DECIMAL(lcFixedLine_VoiceLimit),
                      liVLFirstMonthBR,
                      liVLLastMonthBR,
                      DECIMAL(lcFixedLine_BDestLimit),
                      liBDLFirstMonthBR,
                      liBDLLastMonthBR).        
      END.
   END.
   
END PROCEDURE.

PROCEDURE pCreateServiceLimit_Data:
    DEFINE INPUT PARAMETER icCliType          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icDCEvemt          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ideDataLimit       AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER iiDLFirstMonthCalc AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiDLLastMonthCalc  AS INTEGER   NO-UNDO.    
    DEFINE INPUT PARAMETER icMobileFixedLine  AS CHARACTER NO-UNDO.    

    DEFINE VARIABLE liCount    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcBONOList AS CHARACTER NO-UNDO.

    ASSIGN lcBONOList = fCParamC("BONO_CONTRACTS") .

    CREATE ttServiceLimit.      
    ASSIGN
       ttServiceLimit.GroupCode      = icDCEvemt
       ttServiceLimit.SLCode         = icDCEvemt + "_DATA"
       ttServiceLimit.SLName         = "Data" 
       ttServiceLimit.DialType       = 7
       ttServiceLimit.InclAmt        = ideDataLimit
       ttServiceLimit.FirstMonthCalc = iiDLFirstMonthCalc
       ttServiceLimit.LastMonthCalc  = iiDLLastMonthCalc.

    CREATE ttServiceLimitTarget.
    ASSIGN
       ttServiceLimitTarget.GroupCode      = ttServiceLimit.GroupCode
       ttServiceLimitTarget.SLCode         = ttServiceLimit.SLCode
       ttServiceLimitTarget.ServiceLMember = "14100001" 
       ttServiceLimitTarget.InSideRate     = icDCEvemt + "_DATA_IN" 
       ttServiceLimitTarget.OutSideRate    = icDCEvemt + "_DATA_OUT".

    CREATE ttBDest.
    ASSIGN
        ttBDest.GroupCode = ttServiceLimitTarget.GroupCode                 
        ttBDest.SLCode    = ttServiceLimitTarget.SLCode 
        ttBDest.BDest     = ttServiceLimitTarget.InSideRate
        ttBDest.BDName    = ttServiceLimitTarget.GroupCode + " " + "DATA IN"                 
        ttBDest.CCN       = 93.

    CREATE ttBDest.
    ASSIGN
        ttBDest.GroupCode = ttServiceLimitTarget.GroupCode    
        ttBDest.SLCode    = ttServiceLimitTarget.SLCode             
        ttBDest.BDest     = ttServiceLimitTarget.OutSideRate
        ttBDest.BDName    = ttServiceLimitTarget.GroupCode + " " + "DATA OUT"                 
        ttBDest.CCN       = 93.
    
    IF icMobileFixedLine = "Mobile" THEN 
    DO:      
        CREATE ttTMRItemValue.
        ASSIGN 
            ttTMRItemValue.TMRuleSeq = 14
            ttTMRItemValue.CliType   = icCliType    
            ttTMRItemValue.BDest     = icDCEvemt + "_DATA_IN".

        IF lcBONOList > "" THEN 
        DO liCount = 1 TO NUM-ENTRIES(lcAllowedBundles):     
            IF LOOKUP(ENTRY(liCount,lcAllowedBundles),lcBONOList) > 0 THEN 
            DO:
                CREATE ttTMRItemValue.
                ASSIGN 
                    ttTMRItemValue.TMRuleSeq = 33
                    ttTMRItemValue.CliType   = icCliType    
                    ttTMRItemValue.BDest     = "GPRSDATA_DATA*".
                LEAVE.    
            END.     
        END.
    END.

    RETURN "".

END PROCEDURE.

PROCEDURE pCreateServiceLimit_Voice:
    DEFINE INPUT PARAMETER icCliType          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icDCEvemt          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ideVoiceLimit      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER iiVLFirstMonthCalc AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiVLLastMonthCalc  AS INTEGER   NO-UNDO.    
    DEFINE INPUT PARAMETER icMobileFixedLine  AS CHARACTER NO-UNDO.    

    DEFINE VARIABLE liCount    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcBCList   AS CHARACTER NO-UNDO.

    ASSIGN lcBCList = "10100001,10100003,10100005,CFOTHER,CFYOIGO".

    CREATE ttServiceLimit.      
    ASSIGN
       ttServiceLimit.GroupCode      = icDCEvemt
       ttServiceLimit.SLCode         = icDCEvemt + "_MIN"
       ttServiceLimit.SLName         = "National calls" 
       ttServiceLimit.DialType       = (IF icMobileFixedLine = "Mobile" THEN 4 ELSE IF icMobileFixedLine = "FixedLine" THEN 1 ELSE 0)
       ttServiceLimit.InclAmt        = ideVoiceLimit
       ttServiceLimit.FirstMonthCalc = iiVLFirstMonthCalc
       ttServiceLimit.LastMonthCalc  = iiVLLastMonthCalc.        

     DO liCount = 1 TO NUM-ENTRIES(lcBCList):                

         CREATE ttServiceLimitTarget.
         ASSIGN
            ttServiceLimitTarget.GroupCode      = ttServiceLimit.GroupCode
            ttServiceLimitTarget.SLCode         = ttServiceLimit.SLCode
            ttServiceLimitTarget.ServiceLMember = ENTRY(liCount, lcBCList) 
            ttServiceLimitTarget.InSideRate     = icDCEvemt + "_VOICE_IN" 
            ttServiceLimitTarget.OutSideRate    = icDCEvemt + "_VOICE_OUT".
     END.

     CREATE ttBDest.
     ASSIGN
         ttBDest.GroupCode = ttServiceLimitTarget.GroupCode        
         ttBDest.SLCode    = ttServiceLimitTarget.SLCode          
         ttBDest.BDest     = ttServiceLimitTarget.InSideRate
         ttBDest.BDName    = ttServiceLimitTarget.GroupCode + " " + "VOICE IN"                 
         ttBDest.CCN       = 81.

     CREATE ttBDest.
     ASSIGN
         ttBDest.GroupCode = ttServiceLimitTarget.GroupCode                  
         ttBDest.SLCode    = ttServiceLimitTarget.SLCode 
         ttBDest.BDest     = ttServiceLimitTarget.OutSideRate
         ttBDest.BDName    = ttServiceLimitTarget.GroupCode + " " + "VOICE OUT"                 
         ttBDest.CCN       = 81.
     
     IF icMobileFixedLine = "Mobile" THEN 
     DO:
         CREATE ttTMRItemValue.
         ASSIGN 
             ttTMRItemValue.TMRuleSeq = 34
             ttTMRItemValue.CliType = icCliType    
             ttTMRItemValue.BDest   = icDCEvemt + "_VOICE_IN".

         CREATE ttTMRItemValue.
         ASSIGN 
             ttTMRItemValue.TMRuleSeq = 42
             ttTMRItemValue.CliType = icCliType    
             ttTMRItemValue.BDest   = icDCEvemt + "_VOICE_IN".      
     END.
         
     RETURN "".

END PROCEDURE.


PROCEDURE pCreateServiceLimit_BDest:
    DEFINE INPUT PARAMETER icCliType           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icDCEvemt           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ideBDestLimit       AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER iiBDLFirstMonthCalc AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiBDLLastMonthCalc  AS INTEGER   NO-UNDO.    
    DEFINE INPUT PARAMETER icMobileFixedLine   AS CHARACTER NO-UNDO.    

    DEFINE VARIABLE liCount    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcBCList   AS CHARACTER NO-UNDO.

    ASSIGN lcBCList = "10100001,10100003,10100005,CFOTHER,CFYOIGO".

    CREATE ttServiceLimit.      
    ASSIGN
       ttServiceLimit.GroupCode      = icDCEvemt
       ttServiceLimit.SLCode         = icDCEvemt + "_QTY"
       ttServiceLimit.SLName         = "BDest" 
       ttServiceLimit.DialType       = (IF icMobileFixedLine = "Mobile" THEN 0 ELSE IF icMobileFixedLine = "FixedLine" THEN 50 ELSE 0)
       ttServiceLimit.InclAmt        = ideBDestLimit
       ttServiceLimit.FirstMonthCalc = iiBDLFirstMonthCalc
       ttServiceLimit.LastMonthCalc  = iiBDLLastMonthCalc.

    DO liCount = 1 TO NUM-ENTRIES(lcBCList):                
         CREATE ttServiceLimitTarget.
         ASSIGN
            ttServiceLimitTarget.GroupCode      = ttServiceLimit.GroupCode
            ttServiceLimitTarget.SLCode         = ttServiceLimit.SLCode
            ttServiceLimitTarget.ServiceLMember = ENTRY(liCount, lcBCList) 
            ttServiceLimitTarget.InSideRate     = icDCEvemt + "_VOICE_IN" 
            ttServiceLimitTarget.OutSideRate    = icDCEvemt + "_VOICE_OUT".
     END.

     FIND FIRST ttBDest WHERE ttBDest.GroupCode = ttServiceLimitTarget.GroupCode  AND 
                              ttBDest.SLCode    = ttServiceLimitTarget.SLCode     AND 
                              ttBDest.BDest     = ttServiceLimitTarget.InSideRate NO-LOCK NO-ERROR.
     IF NOT AVAIL ttBDest THEN 
     DO:                         
         CREATE ttBDest.
         ASSIGN
             ttBDest.GroupCode = ttServiceLimitTarget.GroupCode       
             ttBDest.SLCode    = ttServiceLimitTarget.SLCode           
             ttBDest.BDest     = ttServiceLimitTarget.InSideRate
             ttBDest.BDName    = ttServiceLimitTarget.GroupCode + " " + "VOICE IN"                 
             ttBDest.CCN       = 81.
     END.    

     FIND FIRST ttBDest WHERE ttBDest.GroupCode = ttServiceLimitTarget.GroupCode   AND 
                              ttBDest.SLCode    = ttServiceLimitTarget.SLCode      AND 
                              ttBDest.BDest     = ttServiceLimitTarget.OutSideRate NO-LOCK NO-ERROR.
     IF NOT AVAIL ttBDest THEN 
     DO:
         CREATE ttBDest.
         ASSIGN
             ttBDest.GroupCode = ttServiceLimitTarget.GroupCode    
             ttBDest.SLCode    = ttServiceLimitTarget.SLCode              
             ttBDest.BDest     = ttServiceLimitTarget.OutSideRate
             ttBDest.BDName    = ttServiceLimitTarget.GroupCode + " " + "VOICE OUT"                 
             ttBDest.CCN       = 81.   
     END.    

     IF icMobileFixedLine = "Mobile" THEN 
     DO:
         FIND FIRST ttTMRItemValue WHERE ttTMRItemValue.TMRuleSeq = 34        AND 
                                         ttTMRItemValue.CliType   = icCliType AND 
                                         ttTMRItemValue.BDest     = icDCEvemt + "_VOICE_IN" NO-LOCK NO-ERROR.
         IF NOT AVAIL ttTMRItemValue THEN 
         DO:
             CREATE ttTMRItemValue.
             ASSIGN 
                 ttTMRItemValue.TMRuleSeq = 34
                 ttTMRItemValue.CliType = icCliType    
                 ttTMRItemValue.BDest   = icDCEvemt + "_VOICE_IN".
         END.
         
         FIND FIRST ttTMRItemValue WHERE ttTMRItemValue.TMRuleSeq = 42        AND 
                                         ttTMRItemValue.CliType   = lcCliType AND 
                                         ttTMRItemValue.BDest     = icDCEvemt + "_VOICE_IN" NO-LOCK NO-ERROR.
         IF NOT AVAIL ttTMRItemValue THEN 
         DO:    
             CREATE ttTMRItemValue.
             ASSIGN 
                 ttTMRItemValue.TMRuleSeq = 42
                 ttTMRItemValue.CliType   = icCliType    
                 ttTMRItemValue.BDest     = icDCEvemt + "_VOICE_IN".
         END.       
     END.

     RETURN "".

END PROCEDURE.

PROCEDURE pCreateProgressiveRatingLimit_Data:
    DEFINE INPUT PARAMETER icCliType          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icDCEvent          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ideDataLimit       AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER iiDLFirstMonthCalc AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiDLLastMonthCalc  AS INTEGER   NO-UNDO.    
    DEFINE INPUT PARAMETER icMobileFixedLine  AS CHARACTER NO-UNDO.    

    DEFINE VARIABLE liCount    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcBONOList AS CHARACTER NO-UNDO.

    ASSIGN lcBONOList = fCParamC("BONO_CONTRACTS") .

    CREATE ttServiceLimit.      
    ASSIGN
       ttServiceLimit.GroupCode      = icDCEvent
       ttServiceLimit.SLCode         = icDCEvent
       ttServiceLimit.SLName         = "Data" 
       ttServiceLimit.DialType       = 7
       ttServiceLimit.InclAmt        = ideDataLimit
       ttServiceLimit.FirstMonthCalc = iiDLFirstMonthCalc
       ttServiceLimit.LastMonthCalc  = iiDLLastMonthCalc.
    
    CREATE ttProgLimit.
    ASSIGN
       ttProgLimit.GroupCode = ttServiceLimit.GroupCode
       ttProgLimit.SLCode    = ttServiceLimit.SLCode 
       ttProgLimit.BDest     = "GPRSDATA_" + icDCEvent
       ttProgLimit.LimitFrom = 0
       ttProgLimit.LimitTo   = ideDataLimit.

    CREATE ttBDest.
    ASSIGN
        ttBDest.GroupCode = ttServiceLimit.GroupCode
        ttBDest.SLCode    = ttServiceLimit.SLCode
        ttBDest.BDest     = ttProgLimit.BDest
        ttBDest.BDName    = "GPRS DATA HIGH"                 
        ttBDest.CCN       = 93.

    IF icMobileFixedLine = "Mobile" THEN 
    DO:    
        CREATE ttTMRItemValue.
        ASSIGN 
            ttTMRItemValue.TMRuleSeq = 14    /* Base Contract Data */
            ttTMRItemValue.CliType   = icCliType    
            ttTMRItemValue.BDest     = ttBDest.BDest.
    END.
        
    CREATE ttProgLimit.
    ASSIGN
       ttProgLimit.GroupCode = ttServiceLimit.GroupCode
       ttProgLimit.SLCode    = ttServiceLimit.SLCode 
       ttProgLimit.BDest     = "GPRSDATA2_" + icDCEvent
       ttProgLimit.LimitFrom = ideDataLimit + 0.000001
       ttProgLimit.LimitTo   = 999999999.999999.

    CREATE ttBDest.
    ASSIGN
        ttBDest.GroupCode = ttServiceLimit.GroupCode
        ttBDest.SLCode    = ttServiceLimit.SLCode
        ttBDest.BDest     = ttProgLimit.BDest
        ttBDest.BDName    = "GPRS DATA SLOW"                 
        ttBDest.CCN       = 93.   

    IF lcBONOList > "" THEN 
    DO liCount = 1 TO NUM-ENTRIES(lcAllowedBundles):     
        IF LOOKUP(ENTRY(liCount,lcAllowedBundles),lcBONOList) > 0 THEN 
        DO:
            CREATE ttTMRItemValue.
            ASSIGN 
                ttTMRItemValue.TMRuleSeq = 33  /* Bono Data */
                ttTMRItemValue.CliType   = icCliType    
                ttTMRItemValue.BDest     = "GPRSDATA_DATA*".           
                    
            LEAVE.
        END.
    END.
    
    RETURN "".

END PROCEDURE.


PROCEDURE pBundle:
    DEFINE INPUT PARAMETER icMobileFixedLine   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icCliType           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icBundle            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icBundleName        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icBundleType        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icBillCode          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icUpSell            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ilBonoSupport       AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ideCommercialFee    AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER iiFirstMonthBR      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiLastMonthBR       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ideDataLimit        AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER iiDLFirstMonthCalc  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiDLLastMonthCalc   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ideVoiceLimit       AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER iiVLFirstMonthCalc  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiVLLastMonthCalc   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ideBDestLimit       AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER iiBDLFirstMonthCalc AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiBDLLastMonthCalc  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE liCount    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcBCList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcBONOList AS CHARACTER NO-UNDO.

    ASSIGN 
        lcBCList   = "10100001,10100003,10100005,CFOTHER,CFYOIGO"
        lcBONOList = fCParamC("BONO_CONTRACTS") .

    CREATE ttDayCampaign.
    ASSIGN
        ttDayCampaign.CliType     = icCliType
        ttDayCampaign.DCEvent     = icBundle
        ttDayCampaign.DCName      = icBundleName
        ttDayCampaign.DCType      = icBundleType
        ttDayCampaign.BillCode    = icBillCode
        ttDayCampaign.UpSell      = icUpSell
        ttDayCampaign.BonoSupport = ilBonoSupport
        ttDayCampaign.DataLimit   = ideDataLimit
        ttDayCampaign.SLCreated   = Yes WHEN (ideDataLimit > 0 OR ideVoiceLimit > 0 OR ideBDestLimit > 0).

    CREATE ttFMItem.
    ASSIGN 
        ttFMItem.FeeModel     = ttDayCampaign.BillCode
        ttFMItem.BillCode     = ttDayCampaign.BillCode
        ttFMItem.PriceList    = ""
        ttFMItem.Amount       = ideCommercialFee
        ttFMItem.FirstMonthBR = iiFirstMonthBR
        ttFMItem.BrokenRental = iiLastMonthBR.
     
    CREATE ttServiceLimitGroup.
    ASSIGN 
        ttServiceLimitGroup.GroupCode = ttDayCampaign.DCEvent
        ttServiceLimitGroup.GroupName = ttDayCampaign.DCName.
     
     IF icBundleType = "PackageWithCounter" THEN 
     DO:
         IF ideDataLimit > 0 THEN
         DO:
             RUN pCreateProgressiveRatingLimit_Data(lcCliType,
                                                    ttDayCampaign.DCEvent,
                                                    ideDataLimit,
                                                    iiDLFirstMonthCalc,
                                                    iiDLLastMonthCalc,
                                                    icMobileFixedLine).
         END. 
     END.
     ELSE IF icBundleType = "ServicePackage" THEN 
     DO:   
         IF ideDataLimit > 0 THEN 
         DO:         
             RUN pCreateServiceLimit_Data(lcCliType,
                                          ttDayCampaign.DCEvent,
                                          ideDataLimit,
                                          iiDLFirstMonthCalc,
                                          iiDLLastMonthCalc,
                                          icMobileFixedLine).             
         END.

         IF ideVoiceLimit > 0 THEN 
         DO:
            RUN pCreateServiceLimit_Voice(lcCliType,
                                          ttDayCampaign.DCEvent,
                                          ideVoiceLimit,
                                          iiVLFirstMonthCalc,
                                          iiVLLastMonthCalc,
                                          icMobileFixedLine).
         END.

         IF ideBDestLimit > 0 THEN 
         DO:
            RUN pCreateServiceLimit_BDest(lcCliType,
                                          ttDayCampaign.DCEvent,
                                          ideBDestLimit,
                                          iiBDLFirstMonthCalc,
                                          iiBDLLastMonthCalc, 
                                          icMobileFixedLine).
         END. 
    END.
    
END PROCEDURE.    


PROCEDURE pValidateData:

   DEFINE VARIABLE llgPostPaid         AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE llgTrafficBundle    AS LOGICAL   NO-UNDO.

   DO ON ERROR UNDO, THROW:

      FOR EACH ttTariffCre 
         ON ERROR UNDO, THROW:

         CASE ttTariffCre.FieldName:
            WHEN {&CT} THEN 
            DO:
               IF ttTariffCre.FieldValue EQ "" THEN 
                  UNDO, THROW NEW Progress.Lang.AppError("No CliType data available", 1).
               ELSE 
                  ASSIGN lcCliType = ttTariffCre.FieldValue.
            END.
            WHEN {&TN} THEN 
               ASSIGN lcCliName = ttTariffCre.FieldValue.
            WHEN {&TB} THEN 
            DO:
               IF ttTariffCre.FieldValue NE "" THEN 
                  ASSIGN 
                     llgTrafficBundle = YES
                     lcTariffBundle   = ttTariffCre.FieldValue.          
            END.            
            WHEN {&WS} THEN 
            DO:
               IF (ttTariffCre.FieldValue EQ "") OR LOOKUP(ttTariffCre.FieldValue,{&WEBSTATUS}) = 0 THEN
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong WebStatus data", 1).                  
               ELSE 
                  lcWebStatus = ttTariffCre.FieldValue.
            END.
            WHEN {&STCS} THEN 
            DO:
               IF (ttTariffCre.FieldValue EQ "") OR LOOKUP(ttTariffCre.FieldValue,{&STCSTATUS}) = 0 THEN
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong STCStatus data", 1).
               ELSE 
                  lcSTCStatus = ttTariffCre.FieldValue.
            END.
            WHEN {&PT} THEN 
            DO:
               IF (ttTariffCre.FieldValue EQ "") OR LOOKUP(ttTariffCre.FieldValue,{&PAYTYPE}) = 0 THEN   
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong Payment type data", 1).
               ELSE 
               DO:
                  lcPaymentType = ttTariffCre.FieldValue.            
                  IF ttTariffCre.FieldValue = "Postpaid" THEN 
                     llgPostPaid = YES. 
               END.
            END.
            WHEN {&UT} THEN 
            DO:
               IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&USAGETYPE}) EQ 0 THEN                
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong Usage type data", 1).                  
               ELSE 
                  lcUsageType = ttTariffCre.FieldValue. 
            END.            
            WHEN {&LT} THEN 
            DO:
               IF lcPaymentType EQ "Postpaid" THEN 
               DO:
                  IF (ttTariffCre.FieldValue EQ "") OR LOOKUP(ttTariffCre.FieldValue,{&LINETYPE}) = 0 THEN                   
                     UNDO, THROW NEW Progress.Lang.AppError("No LineType data available", 1).                     
                  ELSE 
                     ASSIGN lcLineType = ttTariffCre.FieldValue.
               END.
            END.
            WHEN {&FLT} THEN 
            DO:
               IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&FLINETYPE}) = 0 THEN
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong FixLineType data", 1).                        
               ELSE 
                  ASSIGN lcFixLineType = ttTariffCre.FieldValue.
            END.            
            WHEN {&FLD} THEN 
            DO:
               IF ttTariffCre.FieldValue NE "" THEN 
                  lcFixedLineDownload = ttTariffCre.FieldValue. 
            END.  
            WHEN {&FLU} THEN 
            DO:
               IF ttTariffCre.FieldValue NE "" THEN 
                  lcFixedLineUpload = ttTariffCre.FieldValue. 
            END.
            WHEN {&SC} THEN 
            DO:
               IF ttTariffCre.FieldValue NE "" THEN 
                  lcServiceClass = ttTariffCre.FieldValue.
            END.
            WHEN {&CF} THEN 
               lcCommFee = ttTariffCre.FieldValue. 
            WHEN {&CMF} THEN 
               lcComparisonFee = ttTariffCre.FieldValue.            
            WHEN {&AB} THEN             
               ASSIGN lcAllowedBundles = ttTariffCre.FieldValue.             
            WHEN {&STC_BT} THEN             
               ASSIGN lcBundlesForActivateOnSTC = ttTariffCre.FieldValue.             
            WHEN {&STC_SR} THEN             
               ASSIGN lcServicesForReCreateOnSTC = ttTariffCre.FieldValue.             
            WHEN {&CSF} THEN 
            DO:
               IF ttTariffCre.FieldValue = "" THEN
                  UNDO, THROW NEW Progress.Lang.AppError("Copy services from clitype is blank", 1).
               ELSE IF NOT CAN-FIND(FIRST CliType WHERE CliType.Brand = gcBrand AND CliType.CliType = ttTariffCre.FieldValue NO-LOCK) THEN 
                  UNDO, THROW NEW Progress.Lang.AppError("Invalid 'copy services from clitype'", 1).   
               ELSE 
                  lcCopyServicesFromCliType = ttTariffCre.FieldValue.
            END.                        
         END CASE.         

         IF llgPostPaid THEN 
         DO:
            CASE ttTariffCre.FieldName:
               WHEN {&RPA} THEN 
               DO:
                  IF ttTariffCre.FieldValue EQ "" AND LOOKUP(ttTariffCre.FieldValue,{&RP_ACTION}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Rateplan action is invalid", 1).
                  ELSE 
                     ASSIGN lcRatePlanAction = ttTariffCre.FieldValue.
               END.
               WHEN {&RP} THEN 
               DO:
                  IF ttTariffCre.FieldValue EQ "" THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Rateplan code is invalid", 1).
                  ELSE 
                     ASSIGN lcRatePlan = ttTariffCre.FieldValue.
               END.
               WHEN {&RRP} THEN 
               DO:
                  IF ttTariffCre.FieldValue EQ "" AND NOT CAN-FIND(FIRST RatePlan WHERE RatePlan.Brand = gcBrand AND RatePlan.RatePlan = ttTariffCre.FieldValue NO-LOCK) THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Reference Rateplan is invalid", 1).
                  ELSE 
                     ASSIGN lcReferenceRatePlan = ttTariffCre.FieldValue.
               END.
               /* Mobile */ 
               WHEN {&M_BB} THEN 
               DO:
                  IF ttTariffCre.FieldValue NE "" AND llgTrafficBundle THEN                
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong BaseBundle data available", 1).
                  ELSE 
                     ASSIGN lcMobile_BaseBundle = ttTariffCre.FieldValue.
               END.
               WHEN {&M_BBT} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&CONTRACT}) EQ 0 THEN
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Type of Contract data", 1).                  
                  ELSE IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&CONTRACT}) GT 0 THEN
                     lcMobile_BaseBundleType = ttTariffCre.FieldValue.
               END.
               WHEN {&M_UPSL} THEN 
               DO:
                  IF ttTariffCre.FieldValue NE "" THEN
                     lcMobile_BaseBundleUpsell  = ttTariffCre.FieldValue.
               END.
               WHEN {&M_BONO} THEN
               DO: 
                  IF ttTariffCre.FieldValue NE "" THEN 
                     lcMobile_BaseBundleBonoSupport = ttTariffCre.FieldValue.
               END.
               WHEN {&M_MFBC} THEN 
                  lcMobile_MonthlyFeeBillCode = ttTariffCre.FieldValue. 
               WHEN {&M_CF} THEN 
                  lcMobile_CommercialFee = ttTariffCre.FieldValue.                
               WHEN {&M_FMFC} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&FEECALC}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong First Month Fee calculation data", 1).
                 ELSE 
                    lcMobile_FirstMonthFeeCalc = ttTariffCre.FieldValue.
               END.
               WHEN {&M_LMFC} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&FEECALC}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Last Month Fee calculation data", 1).                  
                  ELSE 
                     lcMobile_LastMonthFeeCalc = ttTariffCre.FieldValue.
               END.    
               WHEN {&M_DL} THEN 
               DO:
                  IF ttTariffCre.FieldValue NE "" THEN
                     ASSIGN lcMobile_DataLimit  = ttTariffCre.FieldValue.
               END.
               WHEN {&M_VL} THEN 
               DO:
                  IF ttTariffCre.FieldValue NE "" THEN
                     ASSIGN lcMobile_VoiceLimit  = ttTariffCre.FieldValue.
               END.   
               WHEN {&M_BDL} THEN
               DO: 
                  IF ttTariffCre.FieldValue NE "" THEN 
                     ASSIGN lcMobile_BDestLimit  = ttTariffCre.FieldValue.
               END.
               WHEN {&M_FMDL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong First month data limit value", 1).                  
                  ELSE 
                     lcMobile_DataLimit_FirstMonthFeeCalc = ttTariffCre.FieldValue.
               END.
               WHEN {&M_LMDL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Last month data limit value", 1).
                  ELSE 
                     lcMobile_DataLimit_LastMonthFeeCalc = ttTariffCre.FieldValue.
               END.
               WHEN {&M_FMVL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong First month voice limit value", 1).                  
                  ELSE 
                     lcMobile_VoiceLimit_FirstMonthFeeCalc = ttTariffCre.FieldValue.
               END.
               WHEN {&M_LMVL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Last month voice limit value", 1).
                  ELSE 
                     lcMobile_VoiceLimit_LastMonthFeeCalc = ttTariffCre.FieldValue.
               END.
               WHEN {&M_FMBDL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong First month BDestination limit value", 1).                  
                  ELSE 
                     lcMobile_BDestLimit_FirstMonthFeeCalc = ttTariffCre.FieldValue.
               END.
               WHEN {&M_LMBDL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Last month BDestination limit value", 1).
                  ELSE 
                     lcMobile_BDestLimit_LastMonthFeeCalc = ttTariffCre.FieldValue.
               END.
               /* FixedLine */
               WHEN {&FL_BB} THEN 
               DO:
                  IF ttTariffCre.FieldValue NE "" AND llgTrafficBundle THEN                
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong BaseBundle data available", 1).
                  ELSE 
                     ASSIGN lcFixedLine_BaseBundle = ttTariffCre.FieldValue.
               END.
               WHEN {&FL_BBT} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&CONTRACT}) EQ 0 THEN
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Type of Contract data", 1).                  
                  ELSE IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&CONTRACT}) GT 0 THEN
                     lcFixedLine_BaseBundleType = ttTariffCre.FieldValue.
               END.
               WHEN {&FL_UPSL} THEN                
                  lcFixedLine_BaseBundleUpsell  = ttTariffCre.FieldValue.               
               WHEN {&FL_BONO} THEN               
                  lcFixedLine_BaseBundleBonoSupport = ttTariffCre.FieldValue.               
               WHEN {&FL_MFBC} THEN 
                  lcFixedLine_MonthlyFeeBillCode = ttTariffCre.FieldValue. 
               WHEN {&FL_CF} THEN 
                  lcFixedLine_CommercialFee = ttTariffCre.FieldValue.                
               WHEN {&FL_FMFC} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&FEECALC}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong First Month Fee calculation data", 1).
                 ELSE 
                    lcFixedLine_FirstMonthFeeCalc = ttTariffCre.FieldValue.
               END.
               WHEN {&FL_LMFC} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&FEECALC}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Last Month Fee calculation data", 1).                  
                  ELSE 
                     lcFixedLine_LastMonthFeeCalc = ttTariffCre.FieldValue.
               END.                   
               WHEN {&FL_VL} THEN 
               DO:
                  IF ttTariffCre.FieldValue NE "" THEN
                     ASSIGN lcFixedLine_VoiceLimit  = ttTariffCre.FieldValue.
               END.   
               WHEN {&FL_BDL} THEN
               DO: 
                  IF ttTariffCre.FieldValue NE "" THEN 
                     ASSIGN lcFixedLine_BDestLimit  = ttTariffCre.FieldValue.
               END.               
               WHEN {&FL_FMVL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong First month voice limit value", 1).                  
                  ELSE 
                     lcFixedLine_VoiceLimit_FirstMonthFeeCalc = ttTariffCre.FieldValue.
               END.
               WHEN {&FL_LMVL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Last month voice limit value", 1).
                  ELSE 
                     lcFixedLine_VoiceLimit_LastMonthFeeCalc = ttTariffCre.FieldValue.
               END.
               WHEN {&FL_FMBDL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong First month BDestination limit value", 1).                  
                  ELSE 
                     lcFixedLine_BDestLimit_FirstMonthFeeCalc = ttTariffCre.FieldValue.
               END.
               WHEN {&FL_LMBDL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Last month BDestination limit value", 1).
                  ELSE 
                     lcFixedLine_BDestLimit_LastMonthFeeCalc = ttTariffCre.FieldValue.
               END.              
            END CASE.             
         END. /* IF llgPostPaid THEN DO */      
      END. /* FOR EACH ttSubTypeCr */      

      /* Validations */      
      IF lcFixLineType <> "" AND lcFixLineType <> "None" AND (lcFixedLine_BaseBundle = "" OR lcFixedLineDownload = "" OR lcFixedLineUpload = "") THEN 
         UNDO, THROW NEW Progress.Lang.AppError("Fixed line base bundle or upload/download speed is invalid", 1).
      
      ELSE IF lcPaymentType = "PostPaid" AND lcServiceClass <> "" THEN  
         UNDO, THROW NEW Progress.Lang.AppError("Postpaid subscription contains Serviceclass data", 1).
      ELSE IF lcPaymentType = "PrePaid" AND lcServiceClass = "" THEN
         UNDO, THROW NEW Progress.Lang.AppError("Prepaid subscription doesn't contain any Serviceclass data", 1).      

      ELSE IF lcRatePlanAction = "New" AND lcRatePlan = lcReferenceRatePlan THEN 
         UNDO, THROW NEW Progress.Lang.AppError("Rateplan and Reference Rateplan are same, which is contradicting to Rateplan action", 1).         
      ELSE IF lcRatePlanAction = "New" AND CAN-FIND(FIRST RatePlan WHERE RatePlan.Brand = gcBrand AND RatePlan.RatePlan = lcRatePlan NO-LOCK) THEN 
         UNDO, THROW NEW Progress.Lang.AppError("Rateplan already exists, which is contradicting with Rateplan action", 1).         
      ELSE IF lcRatePlanAction = "UseExisting" AND NOT CAN-FIND(FIRST RatePlan WHERE RatePlan.Brand = gcBrand AND RatePlan.RatePlan = lcReferenceRatePlan NO-LOCK) THEN 
         UNDO, THROW NEW Progress.Lang.AppError("Reference Rateplan doesn't exists, which is contradicting with Rateplan action", 1).
      ELSE
      DO:
         CASE lcMobile_BaseBundleType:
            WHEN "ServicePackage" THEN 
            DO:      
               IF lcMobile_DataLimit = "" AND lcMobile_VoiceLimit = "" THEN 
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong ServicePackage-contract data with limits provided", 1).
            END.
            WHEN "PackageWithCounter" THEN 
            DO:
               IF lcMobile_DataLimit = "" AND lcMobile_VoiceLimit = "" THEN 
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong PackageWithCounter-contract data with limit provided", 1).
            END.
            WHEN "PackageWithoutCounter" THEN 
            DO:
               IF lcMobile_DataLimit <> "" OR lcMobile_VoiceLimit <> "" THEN 
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong PackageWithoutCounter-contract data with limit provided", 1).              
            END.
         END CASE.         
      END.   

      FOR EACH ttTariff ON ERROR UNDO, THROW:

          IF ttTariff.CCN EQ "" OR ttTariff.BillItem EQ "" THEN 
              UNDO, THROW NEW Progress.Lang.AppError("Custom rates for rateplan are missing with BillItem/CCN details", 1).         
      END.

      ASSIGN 
         llgTrafficBundle  = NO
         llgPostPaid       = NO.
   END.

   RETURN "".

END PROCEDURE.


PROCEDURE pReadTranslation:
   DEFINE VARIABLE lcLine      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO.   
   DEFINE VARIABLE liFirstLine AS INTEGER   NO-UNDO INITIAL 1.

   DO ON ERROR UNDO, THROW:
      
      ASSIGN lcInputFile = icIncDir + "tariff_trans.txt".      

      INPUT STREAM TTransIn FROM VALUE(lcInputFile).

      REPEAT ON ERROR UNDO, THROW:                               
        IMPORT STREAM TTransIn UNFORMATTED lcLine.                                
        
        IF liFirstLine = 1 THEN 
        DO:
           liFirstLine = liFirstLine + 1.
           NEXT.
        END.
                                                                          
        CREATE ttTrans.
        ASSIGN            
           ttTrans.tLangType  = TRIM(ENTRY(1,lcLine,";"))
           ttTrans.tTextType  = INT(TRIM(ENTRY(2,lcLine,";")))
           ttTrans.tLangint   = TRIM(ENTRY(3,lcLine,";"))
           ttTrans.tLangtext  = TRIM(ENTRY(4,lcLine,";"))
           ttTrans.tLangTrans = TRIM(ENTRY(5,lcLine,";")).
      END.

      CATCH err AS Progress.Lang.Error:
         UNDO, THROW NEW Progress.Lang.AppError('Incorrect input translation file (tariff_trans.txt) data' + err:GetMessage(1), 1). 
      END CATCH.

      FINALLY:
         INPUT STREAM TTransIn CLOSE.
      END FINALLY.

   END.

   RETURN "".

END PROCEDURE.


PROCEDURE pSaveTranslation:

   FOR EACH ttTrans NO-LOCK
       ON ERROR UNDO, THROW:
      
       FIND FIRST RepText WHERE RepText.Brand    = gcBrand                   AND 
                                RepText.TextType = ttTrans.tTextType         AND 
                                RepText.LinkCode = ttTrans.tLangType         AND 
                                RepText.Language = INTEGER(ttTrans.tLangint) AND 
                                RepText.ToDate   >= TODAY                    NO-LOCK NO-ERROR.
       IF AVAIL RepText THEN 
       DO:
           BUFFER RepText:FIND-CURRENT(EXCLUSIVE-LOCK,NO-WAIT).
           IF AVAIL RepText THEN 
               ASSIGN RepText.RepText = ttTrans.tLangTrans.
       END.
       ELSE 
       DO:                           
           CREATE RepText.
           ASSIGN 
              RepText.Brand    = gcBrand    
              RepText.TextType = ttTrans.tTextType                /* Default value */       
              RepText.LinkCode = ttTrans.tLangType        
              RepText.Language = INTEGER(ttTrans.tLangint)    
              RepText.FromDate = TODAY     
              RepText.ToDate   = DATE(12,31,2049)
              RepText.RepText  = ttTrans.tLangTrans.
       END.   
   END.
   
   RETURN "".

END PROCEDURE.
