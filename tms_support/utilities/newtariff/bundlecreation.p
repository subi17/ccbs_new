/*------------------------------------------------------------------------
  MODULE .......: bundlecreation.p
  TASK .........:
  APPLICATION ..: TMS
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/  

/* ***************************  Definitions  ************************** */
ROUTINE-LEVEL ON ERROR UNDO, THROW.

&GLOBAL-DEFINE BTYPE "BundleType"
&GLOBAL-DEFINE BBTYPE "BaseBundleType"
&GLOBAL-DEFINE DATALIMIT "DataLimit"
&GLOBAL-DEFINE VOICELIMIT "VoiceLimit"

DEFINE VARIABLE gcBundleType AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcBaseBundleType AS CHARACTER NO-UNDO.
DEFINE VARIABLE giDataLimit AS INTEGER NO-UNDO.
DEFINE VARIABLE giVoiceLimit AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttBundle NO-UNDO 
   FIELD FieldName  AS CHARACTER
   FIELD FieldValue AS CHARACTER
   FIELD TypeUse    AS CHARACTER
   FIELD Mandatory  AS LOGICAL
   FIELD ValueList  AS CHARACTER
   FIELD DataType   AS CHARACTER
   .
   
DEFINE TEMP-TABLE ttCharToIntMap NO-UNDO 
   FIELD charval    AS CHARACTER
   FIELD intval     AS CHARACTER
   FIELD FieldName  AS CHARACTER
   .

FUNCTION fCreatettCharToIntMap RETURNS LOGICAL
   ( icFieldName AS CHARACTER,
     icCharVal   AS CHARACTER,
     iiIntVal    AS INTEGER):
        
   CREATE ttCharToIntMap.
   ASSIGN
      ttCharToIntMap.FieldName = icFieldName
      ttCharToIntMap.charval   = icCharVal
      ttCharToIntMap.intval    = iiIntVal.
      
   RETURN FALSE.

END FUNCTION. 

FUNCTION fCreatettBundle RETURNS LOGICAL
   ( icFieldName AS CHARACTER,
     icDataType  AS CHARACTER,
     icTypeUse   AS CHARACTER,      
     icValueList AS CHARACTER,
     ilMandatory AS LOGICAL):

   CREATE ttBundle.
   ASSIGN 
      ttBundle.FieldName  = icFieldName
      ttBundle.TypeUse    = icTypeUse
      ttBundle.Mandatory  = ilMandatory
      ttBundle.ValueList  = icValueList
      ttBundle.DataType   = icDataType.
      
   RETURN FALSE.

END FUNCTION.

fCreatettBundle({&BTYPE}, "CHARACTER", "FixedLine,Mobile", "FixedLine,Mobile", YES).
fCreatettBundle("BaseBundle", "CHARACTER", "FixedLine,Mobile", "", YES).
fCreatettBundle("BaseBundleName", "CHARACTER", "FixedLine,Mobile", "", YES).
fCreatettBundle({&BBTYPE}, "CHARACTER", "FixedLine,Mobile", "ServicePackage,PackageWithCounter,PackagewithoutCounter,Upsell", YES).
fCreatettBundle("Upsell", "CHARACTER", "Mobile", "", NO).
fCreatettBundle("BonoSupport", "LOGICAL", "Mobile", "", YES).
fCreatettBundle("MonthlyFeeBillCode", "CHARACTER", "FixedLine,Mobile", "", YES).
fCreatettBundle("CommercialFee", "DECIMAL", "FixedLine,Mobile", "", YES).
fCreatettBundle("FirstMonthFeeCalc", "CHARACTER", "FixedLine,Mobile", "Full,Relative,UsageBased", YES).
fCreatettCharToIntMap("FirstMonthFeeCalc","Full",1).
fCreatettCharToIntMap("FirstMonthFeeCalc","Relative",0).
fCreatettCharToIntMap("FirstMonthFeeCalc","UsageBased",2).
fCreatettBundle("LastMonthFeeCalc", "CHARACTER", "FixedLine,Mobile", "Full,Relative,UsageBased", YES).
fCreatettCharToIntMap("LastMonthFeeCalc","Full",1).
fCreatettCharToIntMap("LastMonthFeeCalc","Relative",0).
fCreatettCharToIntMap("LastMonthFeeCalc","UsageBased",2).
fCreatettBundle({&DATALIMIT}, "INTEGER", "Mobile", "", NO).
fCreatettBundle({&VOICELIMIT}, "INTEGER", "FixedLine,Mobile", "", NO).
fCreatettBundle("BDestinationLimit", "INTEGER", "FixedLine,Mobile", "", YES).
fCreatettBundle("FirstMonthDataLimit", "CHARACTER", "Mobile", "Full,Relative", YES).
fCreatettCharToIntMap("FirstMonthDataLimit","Full",0).
fCreatettCharToIntMap("FirstMonthDataLimit","Relative",1).
fCreatettBundle("LastMonthDataLimit", "CHARACTER", "Mobile", "Full,Relative", YES).
fCreatettCharToIntMap("LastMonthDataLimit","Full",0).
fCreatettCharToIntMap("LastMonthDataLimit","Relative",1).
fCreatettBundle("FirstMonthVoiceLimit", "CHARACTER", "FixedLine,Mobile", "Full,Relative", YES). 
fCreatettCharToIntMap("FirstMonthVoiceLimit","Full",0).
fCreatettCharToIntMap("FirstMonthVoiceLimit","Relative",1).
fCreatettBundle("LastMonthVoiceLimit", "CHARACTER", "FixedLine,Mobile", "Full,Relative", YES).
fCreatettCharToIntMap("LastMonthVoiceLimit","Full",0).
fCreatettCharToIntMap("LastMonthVoiceLimit","Relative",1).
fCreatettBundle("FirstMonthBDestLimit", "CHARACTER", "FixedLine,Mobile", "Full,Relative", YES).
fCreatettCharToIntMap("FirstMonthBDestLimit","Full",0).
fCreatettCharToIntMap("FirstMonthBDestLimit","Relative",1).
fCreatettBundle("LastMonthBDestLimit", "CHARACTER", "FixedLine,Mobile", "Full,Relative", YES).
fCreatettCharToIntMap("LastMonthBDestLimit","Full",0).
fCreatettCharToIntMap("LastMonthBDestLimit","Relative",1).



FUNCTION fCheckStoreBundle RETURNS CHARACTER
   ( icFieldName  AS CHARACTER,
     icFieldValue AS CHARACTER):

   DEFINE VARIABLE liInteger  AS INTEGER NO-UNDO.
   DEFINE VARIABLE ldeDecimal AS INTEGER NO-UNDO.

   FIND ttBundle WHERE
      ttBundle.FieldName = icFieldName
   NO-ERROR.
   
   IF NOT AVAILABLE ttBundle
   THEN RETURN SUBSTITUTE("Unknown field name '&1'", icFieldName).
   
   IF ttBundle.FieldValue > ""
   THEN RETURN SUBSTITUTE("Tried to set value to field &1 multiple times", ttBundle.FieldName).
   
   CASE ttBundle.DataType:
      WHEN "INTEGER"
      THEN DO:
         liInteger = INTEGER(icFieldValue) NO-ERROR.
         IF ERROR-STATUS:ERROR
         THEN RETURN SUBSTITUTE("Cannot set value '&1' to integer field '&2'", icFieldValue, ttBundle.FieldName).
      END. 
      WHEN "DECIMAL"
      THEN DO:
         liInteger = DECIMAL(icFieldValue) NO-ERROR.
         IF ERROR-STATUS:ERROR
         THEN RETURN SUBSTITUTE("Cannot set value '&1' to decimal field '&2'", icFieldValue, ttBundle.FieldName).
      END.
   END.
   
   IF ttBundle.Mandatory AND ttBundle.ValueList > "" AND LOOKUP(icFieldValue, ttBundle.ValueList) = 0
   THEN RETURN SUBSTITUTE("Field '&1' needs one of following values '&2'. A value '&3' is invalid.", ttBundle.FieldName, ttBundle.ValueList, icFieldValue).
   
   ASSIGN 
      ttBundle.FieldName  = icFieldName
      ttBundle.FieldValue = icFieldValue
      .
   
   CASE ttBundle.FieldName:
      WHEN {&BTYPE}
      THEN gcBundleType = ttBundle.FieldValue.
      WHEN {&BBTYPE}
      THEN gcBaseBundleType = ttBundle.FieldValue.
      WHEN {&DATALIMIT}
      THEN giDataLimit = INTEGER(ttBundle.FieldValue).
      WHEN {&VOICELIMIT}
      THEN giVoiceLimit = INTEGER(ttBundle.FieldValue).
   END CASE.

   RETURN "".

END FUNCTION.

FUNCTION fValidateBundle RETURNS CHARACTER ():
   
   CASE gcBaseBundleType:
      WHEN "ServicePackage" OR WHEN "PackageWithCounter"
      THEN IF giDataLimit = 0 AND giVoiceLimit = 0
           THEN RETURN SUBSTITUTE("Wrong &1-contract data with limits provided", gcBaseBundleType).
      END.
      WHEN "PackageWithoutCounter"
      THEN IF giDataLimit > 0 OR giVoiceLimit > 0
      THEN RETURN SUBSTITUTE("Wrong &1-contract data with limits provided", gcBaseBundleType). 
   END CASE. 
   
   FOR EACH ttBundle:

      IF ttBundle.Mandatory AND ttBundle.FieldValue EQ ""
      THEN RETURN SUBSTITUTE("Field '&1' needs a value.", ttBundle.FieldName).

      IF ttBundle.FieldValue > "" AND
         LOOKUP(gcBundleType,ttBundle.TypeUse) EQ 0
      THEN RETURN SUBSTITUTE("Field '&1' cannot have a value when bundle type is &2", ttBundle.FieldName, gcBundleType).

   END.

   RETURN "".

END FUNCTION.
/*
{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/ftransdir.i}
{utilities/newtariff/tariffconfig.i}
{utilities/newtariff/tariffcons.i}
*/
DEFINE INPUT  PARAMETER icBaseFile AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icFile     AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icSpoolDir AS CHARACTER NO-UNDO.

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
DEFINE VARIABLE lcTariffType                             AS CHARACTER NO-UNDO.

DEFINE STREAM strin.
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

   lcLogFile   = icSpoolDir + icBaseFile + ".log".

   RUN utilities/newtariff/configcreations.p PERSISTENT SET h_config. 

   FUNCTION fTMSCValue RETURNS CHARACTER (iTableName AS CHAR, iFieldName AS CHAR,iCodeName AS CHAR) IN h_config.

   RUN pReadBundle. 

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
                              RUN pServiceLimitTarget IN h_config(BUFFER ttServiceLimitTarget, liSLSeq).                                                  
                          END.

                          /* ProgLimit */  
                          FOR EACH ttProgLimit WHERE ttProgLimit.GroupCode = ttServiceLimit.GroupCode AND ttProgLimit.SLCode = ttServiceLimit.SLCode
                              ON ERROR UNDO, THROW:
                              RUN pProgLimit IN h_config(BUFFER ttProgLimit, liSLSeq).  
                          END.

                          /* BDestination */  
                          FOR EACH ttBDest WHERE ttBDest.GroupCode = ttServiceLimit.GroupCode AND ttBDest.SLCode = ttServiceLimit.SLCode 
                              ON ERROR UNDO, THROW:
                              RUN pBDestination IN h_config(BUFFER ttBDest).
                          END.

                      END.  /* IF liSeq > 0 THEN */
                  END.  /* FOR EACH ttServiceLimit */
              END.  /* FOR EACH ttServiceLimitGroup */
         END. /* FOR EACH ttDayCampaign */

         FOR EACH ttTMRItemValue WHERE ttTMRItemValue.CliType = ttCliType.CliType
             ON ERROR UNDO, THROW:
             RUN pTMRItemValue IN h_config(BUFFER ttTMRItemValue).
         END.

         IF CAN-FIND(FIRST ttTariff) THEN
         DO: 
            FOR EACH ttTariff 
               ON ERROR UNDO, THROW:
               RUN pCustomRates IN h_config(BUFFER ttTariff).            
            END.
         END.

      END. /* FOR EACH ttCliType */
      CATCH e AS Progress.Lang.Error:
          UNDO, THROW e.
      END CATCH.
  END.

  RETURN "".

END PROCEDURE.


PROCEDURE pReadBundle:   

   DEFINE VARIABLE lcLine      AS CHARACTER NO-UNDO.

   INPUT STREAM strin FROM VALUE(icFile).

   REPEAT ON ERROR UNDO, THROW:
      
      IMPORT STREAM strin UNFORMATTED lcLine.
 
      lcError = fCheckStoreBundle(TRIM(ENTRY(1,lcLine,";")), TRIM(ENTRY(2,lcLine,";"))).
      
      IF lcError > ""
      THEN UNDO, THROW NEW Progress.Lang.AppError(lcError, 1). 
 
   END.

   lcError = fValidateBundle().   

   IF lcError > ""
   THEN UNDO, THROW NEW Progress.Lang.AppError(lcError, 1). 

   CATCH err AS Progress.Lang.Error:
      UNDO, THROW NEW Progress.Lang.AppError('Incorrect input file data' + err:GetMessage(1), 1). 
   END CATCH.

   FINALLY:
      INPUT STREAM strin CLOSE.
   END FINALLY.   


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

   IF lcTariffBundle > "" THEN
   DO:
       IF NOT CAN-FIND(FIRST CliType WHERE CliType.Brand = Syst.Var:gcBrand AND CliType.CliType = lcCliType NO-LOCK) THEN 
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
              ttCliType.CopyServicesFromCliType   = lcCopyServicesFromCliType
              ttClitype.TariffType                = INTEGER(fTMSCValue("CLIType","TariffType",lcTariffType)).
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
      ttCliType.CopyServicesFromCliType   = (IF lcTariffBundle > "" THEN "" ELSE lcCopyServicesFromCliType)
      ttClitype.TariffType                = INTEGER(fTMSCValue("CLIType","TariffType",lcTariffType)). 

   
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
                   ttCliType.PayType, 
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
                    ttCliType.PayType,
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
    
    RETURN "".
    
END PROCEDURE.

PROCEDURE pCreateServiceLimit_Data:
    DEFINE INPUT PARAMETER icCliType          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icDCEvemt          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ilPostpaid         AS LOGICAL   NO-UNDO.
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
       ttServiceLimit.InclUnit       = 4
       ttServiceLimit.InclAmt        = ideDataLimit
       ttServiceLimit.FirstMonthCalc = iiDLFirstMonthCalc
       ttServiceLimit.LastMonthCalc  = iiDLLastMonthCalc.

    IF ilPostpaid THEN 
    DO:   
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
    END.
    ELSE
    DO:
        CREATE ttBDest.
        ASSIGN
            ttBDest.GroupCode = ttServiceLimit.GroupCode
            ttBDest.SLCode    = ttServiceLimit.SLCode
            ttBDest.BDest     = "GPRSDATA_" + icDCEvemt
            ttBDest.BDName    = "GPRS Data " + icDCEvemt                 
            ttBDest.CCN       = 93.
    END.    
    RETURN "".

END PROCEDURE.

PROCEDURE pCreateServiceLimit_Voice:
    DEFINE INPUT PARAMETER icCliType          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icDCEvemt          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ilPostpaid         AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ideVoiceLimit      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER iiVLFirstMonthCalc AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiVLLastMonthCalc  AS INTEGER   NO-UNDO.    
    DEFINE INPUT PARAMETER icMobileFixedLine  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ideBDestLimit      AS DECIMAL   NO-UNDO.


    DEFINE VARIABLE liCount           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcBCList          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcFixedLineBCList AS CHARACTER NO-UNDO.

    ASSIGN 
        lcBCList          = "10100001,10100003,10100005,CFOTHER,CFYOIGO"
        lcFixedLineBCList = "F10100003,F10100005".

    CREATE ttServiceLimit.      
    ASSIGN
       ttServiceLimit.GroupCode      = icDCEvemt
       ttServiceLimit.SLCode         = icDCEvemt + "_MIN"
       ttServiceLimit.SLName         = "National calls" 
       ttServiceLimit.DialType       = (IF icMobileFixedLine = "Mobile" THEN 4 ELSE IF icMobileFixedLine = "FixedLine" THEN 1 ELSE 0)
       ttServiceLimit.InclUnit       = 1
       ttServiceLimit.InclAmt        = ideVoiceLimit
       ttServiceLimit.FirstMonthCalc = iiVLFirstMonthCalc
       ttServiceLimit.LastMonthCalc  = iiVLLastMonthCalc
       ttServiceLimit.BDestLimit     = INTEGER(ideBDestLimit).

     IF ilPostpaid THEN 
     DO: 
         IF icMobileFixedLine = "FixedLine" THEN 
         DO:    
            ASSIGN lcBCList = lcFixedLineBCList.

            CREATE ttServiceLimitTarget.
            ASSIGN
                ttServiceLimitTarget.GroupCode      = ttServiceLimit.GroupCode
                ttServiceLimitTarget.SLCode         = ttServiceLimit.SLCode
                ttServiceLimitTarget.ServiceLMember = "F10100003"
                ttServiceLimitTarget.InSideRate     = icDCEvemt + "_MIN_IN" 
                ttServiceLimitTarget.OutSideRate    = icDCEvemt + "_MIN_OUT".  

            CREATE ttBDest.
            ASSIGN
                 ttBDest.GroupCode = ttServiceLimitTarget.GroupCode        
                 ttBDest.SLCode    = ttServiceLimitTarget.SLCode          
                 ttBDest.BDest     = ttServiceLimitTarget.InSideRate
                 ttBDest.BDName    = ttServiceLimitTarget.GroupCode + " " + "MIN IN"                 
                 ttBDest.CCN       = 81.

            CREATE ttBDest.
            ASSIGN
                 ttBDest.GroupCode = ttServiceLimitTarget.GroupCode                  
                 ttBDest.SLCode    = ttServiceLimitTarget.SLCode 
                 ttBDest.BDest     = ttServiceLimitTarget.OutSideRate
                 ttBDest.BDName    = ttServiceLimitTarget.GroupCode + " " + "MIN OUT"                 
                 ttBDest.CCN       = 81.
         END.
         ELSE
         DO:
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

     END.

     RETURN "".

END PROCEDURE.


PROCEDURE pCreateServiceLimit_BDest:
    DEFINE INPUT PARAMETER icCliType           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icDCEvemt           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ilPostpaid          AS LOGICAL   NO-UNDO.
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
       ttServiceLimit.InclUnit       = 7
       ttServiceLimit.InclAmt        = ideBDestLimit
       ttServiceLimit.FirstMonthCalc = iiBDLFirstMonthCalc
       ttServiceLimit.LastMonthCalc  = iiBDLLastMonthCalc.

    IF ilPostpaid THEN 
    DO: 
        IF icMobileFixedLine = "FixedLine" THEN 
        DO:
            CREATE ttServiceLimitTarget.
            ASSIGN
                ttServiceLimitTarget.GroupCode      = ttServiceLimit.GroupCode
                ttServiceLimitTarget.SLCode         = ttServiceLimit.SLCode
                ttServiceLimitTarget.ServiceLMember = "F10100005"
                ttServiceLimitTarget.InSideRate     = icDCEvemt + "_QTY_IN" 
                ttServiceLimitTarget.OutSideRate    = icDCEvemt + "_QTY_OUT".

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
                    ttBDest.BDName    = ttServiceLimitTarget.GroupCode + " " + "QTY IN"                 
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
                    ttBDest.BDName    = ttServiceLimitTarget.GroupCode + " " + "QTY OUT"                 
                    ttBDest.CCN       = 81.   
            END.
        END.
        ELSE
        DO:
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
     END.
         
     RETURN "".

END PROCEDURE.

PROCEDURE pCreateProgressiveRatingLimit_Data:
    DEFINE INPUT PARAMETER icCliType          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icDCEvent          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ilPostpaid         AS LOGICAL   NO-UNDO.
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
       ttServiceLimit.InclUnit       = 4
       ttServiceLimit.InclAmt        = ideDataLimit
       ttServiceLimit.FirstMonthCalc = iiDLFirstMonthCalc
       ttServiceLimit.LastMonthCalc  = iiDLLastMonthCalc.
    
    IF ilPostpaid THEN 
    DO:
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
    END.

    RETURN "".

END PROCEDURE.


PROCEDURE pBundle:
    DEFINE INPUT PARAMETER icMobileFixedLine   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icCliType           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icBundle            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icBundleName        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iiPayType           AS INTEGER   NO-UNDO.
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

    DEFINE VARIABLE liCount           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lcBCList          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcFixedLineBCList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcBONOList        AS CHARACTER NO-UNDO.

    ASSIGN 
        lcBCList          = "10100001,10100003,10100005,CFOTHER,CFYOIGO"
        lcFixedLineBCList = "F10100003,F10100005" 
        lcBONOList        = fCParamC("BONO_CONTRACTS") .

    CREATE ttDayCampaign.
    ASSIGN
        ttDayCampaign.CliType     = icCliType
        ttDayCampaign.DCEvent     = icBundle
        ttDayCampaign.DCName      = icBundleName
        ttDayCampaign.DCType      = icBundleType
        ttDayCampaign.PayType     = iiPayType
        ttDayCampaign.BillCode    = icBillCode
        ttDayCampaign.UpSell      = icUpSell
        ttDayCampaign.BonoSupport = ilBonoSupport
        ttDayCampaign.DataLimit   = ideDataLimit
        ttDayCampaign.SLCreated   = Yes WHEN (ideDataLimit > 0 OR ideVoiceLimit > 0 OR ideBDestLimit > 0).

    IF icMobileFixedLine = "FixedLine" THEN 
    DO:
        IF INDEX(icBundleName, "Casa") = 0 THEN 
        DO:
            CREATE ttFMItem.
            ASSIGN 
                ttFMItem.FeeModel     = ttDayCampaign.BillCode
                ttFMItem.BillCode     = ttDayCampaign.BillCode
                ttFMItem.PriceList    = "CONTRATOFIXED"
                ttFMItem.Amount       = ideCommercialFee
                ttFMItem.FirstMonthBR = iiFirstMonthBR
                ttFMItem.BrokenRental = iiLastMonthBR.
        END.
        ELSE
        DO:      
            CREATE ttFMItem.
            ASSIGN 
                ttFMItem.FeeModel     = ttDayCampaign.BillCode
                ttFMItem.BillCode     = ttDayCampaign.BillCode
                ttFMItem.PriceList    = "CONTRATOFIXEDONLY"
                ttFMItem.Amount       = ideCommercialFee
                ttFMItem.FirstMonthBR = iiFirstMonthBR
                ttFMItem.BrokenRental = iiLastMonthBR.    
        END.    
    END.
    ELSE 
    DO:    
        CREATE ttFMItem.
        ASSIGN 
            ttFMItem.FeeModel     = ttDayCampaign.BillCode
            ttFMItem.BillCode     = ttDayCampaign.BillCode
            ttFMItem.PriceList    = ""
            ttFMItem.Amount       = ideCommercialFee
            ttFMItem.FirstMonthBR = iiFirstMonthBR
            ttFMItem.BrokenRental = iiLastMonthBR.
    END. 

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
                                                   (iiPayType = 1),
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
                                         (iiPayType = 1),
                                         ideDataLimit,
                                         iiDLFirstMonthCalc,
                                         iiDLLastMonthCalc,
                                         icMobileFixedLine).             
        END.

        IF ideVoiceLimit > 0 THEN 
        DO:
           RUN pCreateServiceLimit_Voice(lcCliType,
                                         ttDayCampaign.DCEvent,
                                         (iiPayType = 1),
                                         ideVoiceLimit,
                                         iiVLFirstMonthCalc,
                                         iiVLLastMonthCalc,
                                         icMobileFixedLine,
                                         ideBDestLimit).
        END.

        IF ideBDestLimit > 0 AND 
           ( /*icMobileFixedLine NE "mobile" OR */ ideVoiceLimit EQ 0) THEN
        DO:
           RUN pCreateServiceLimit_BDest(lcCliType,
                                         ttDayCampaign.DCEvent,
                                         (iiPayType = 1),
                                         ideBDestLimit,
                                         iiBDLFirstMonthCalc,
                                         iiBDLLastMonthCalc, 
                                         icMobileFixedLine).
        END. 
   END. 
    
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
      
       FIND FIRST RepText WHERE RepText.Brand    = Syst.Var:gcBrand                   AND 
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
              RepText.Brand    = Syst.Var:gcBrand    
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

