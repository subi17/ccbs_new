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
DEFINE VARIABLE lcMobile_BaseBundleName                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_BaseBundleType                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMobile_BaseBundlePayType               AS CHARACTER NO-UNDO.
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

DEFINE TEMP-TABLE ttBundleCre NO-UNDO 
   FIELD FieldName  AS CHARACTER 
   FIELD FieldValue AS CHARACTER.

DEFINE STREAM sInputFile.   
DEFINE STREAM BundleIn.
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

   ASSIGN lcLogFile = icSpoolDir + "bundlecreation.log".       

   RUN configcreations.p PERSISTENT SET h_config. 

   FUNCTION fTMSCValue RETURNS CHARACTER (iTableName AS CHAR, iFieldName AS CHAR,iCodeName AS CHAR) IN h_config.

   RUN pReadFileAndSaveBundle. 

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
PROCEDURE pSaveBundle:
  DEFINE VARIABLE liSLSeq           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE llFeeModelCreated AS LOGICAL   NO-UNDO.

  DO ON ERROR UNDO, THROW:

     FOR EACH ttDayCampaign 
         ON ERROR UNDO, THROW:

           RUN pDayCampaign IN h_config(BUFFER ttDayCampaign).

           FOR EACH ttFMItem WHERE ttFMItem.FeeModel = ttDayCampaign.BillCode 
               ON ERROR UNDO, THROW:

               RUN pFeeModel IN h_config(ttFMItem.FeeModel, ttDayCampaign.DCName, OUTPUT llFeeModelCreated).
              
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
      
      CATCH e AS Progress.Lang.Error:
          UNDO, THROW e.
      END CATCH.
  END.

  RETURN "".

END PROCEDURE.


PROCEDURE pReadFileAndSaveBundle:   

   DEFINE VARIABLE lcLine       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcInputFile  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcResultFile AS CHARACTER NO-UNDO.

   DO ON ERROR UNDO, THROW:   
      
      ASSIGN lcInputFile = icIncDir + "bundle*.txt".

      INPUT STREAM BundleIn FROM OS-DIR(icIncDir).

      REPEAT ON ERROR UNDO, THROW:

          IMPORT STREAM BundleIn UNFORMATTED lcResultFile.

          ASSIGN 
              lcResultFile = ENTRY(2, lcResultFile, " ")
              lcResultFile = TRIM(lcResultFile,'"').

          IF INDEX(lcResultFile, "bundle_") = 0 THEN
              NEXT.

          EMPTY TEMP-TABLE ttBundleCre.
          EMPTY TEMP-TABLE ttDayCampaign.
          EMPTY TEMP-TABLE ttFMItem.
          EMPTY TEMP-TABLE ttProgLimit.
          EMPTY TEMP-TABLE ttServiceLimit.
          EMPTY TEMP-TABLE ttServiceLimitGroup.
          EMPTY TEMP-TABLE ttServiceLimitTarget.
          EMPTY TEMP-TABLE ttBDest.

          ASSIGN
              lcMobile_BaseBundle                   = ""
              lcMobile_BaseBundleName               = ""
              lcMobile_BaseBundleType               = ""
              lcMobile_BaseBundlePayType            = ""
              lcMobile_BaseBundleUpsell             = ""
              lcMobile_BaseBundleBonoSupport        = ""
              lcMobile_MonthlyFeeBillCode           = ""
              lcMobile_CommercialFee                = ""
              lcMobile_FirstMonthFeeCalc            = ""
              lcMobile_LastMonthFeeCalc             = ""
              lcMobile_DataLimit                    = ""
              lcMobile_VoiceLimit                   = ""
              lcMobile_BDestLimit                   = ""
              lcMobile_DataLimit_FirstMonthFeeCalc  = ""
              lcMobile_DataLimit_LastMonthFeeCalc   = ""
              lcMobile_VoiceLimit_FirstMonthFeeCalc = ""
              lcMobile_VoiceLimit_LastMonthFeeCalc  = ""
              lcMobile_BDestLimit_FirstMonthFeeCalc = ""
              lcMobile_BDestLimit_LastMonthFeeCalc  = "".

          INPUT STREAM sInputFile FROM VALUE(lcResultFile).

          REPEAT ON ERROR UNDO, THROW:
             IMPORT STREAM sInputFile UNFORMATTED lcLine.
        
             CREATE ttBundleCre.
             ASSIGN 
                ttBundleCre.FieldName  = TRIM(ENTRY(1,lcLine,";"))
                ttBundleCre.FieldValue = TRIM(ENTRY(2,lcLine,";")).

          END.

          INPUT STREAM sInputFile CLOSE.

          RUN pValidateData.

          RUN pProcessTT.

          RUN pSaveBundle.
      END.

      CATCH err AS Progress.Lang.Error:
         UNDO, THROW NEW Progress.Lang.AppError('Incorrect input file (bundlecreation.txt) data' + err:GetMessage(1), 1). 
      END CATCH.

      FINALLY:
         INPUT STREAM BundleIn CLOSE.
      END FINALLY.
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
   
   ASSIGN 
       liFirstMonthBR   = (IF lcMobile_FirstMonthFeeCalc BEGINS "Full" THEN 1 ELSE IF lcMobile_FirstMonthFeeCalc BEGINS "Usage" THEN 2 ELSE IF lcMobile_FirstMonthFeeCalc BEGINS "Relative" THEN 0 ELSE 0)
       liLastMonthBR    = (IF lcMobile_LastMonthFeeCalc  BEGINS "Full" THEN 1 ELSE IF lcMobile_LastMonthFeeCalc  BEGINS "Usage" THEN 2 ELSE IF lcMobile_LastMonthFeeCalc  BEGINS "Relative" THEN 0 ELSE 0)

       liDLFirstMonthBR = (IF lcMobile_DataLimit_FirstMonthFeeCalc BEGINS "Relative" THEN 1 ELSE IF lcMobile_DataLimit_FirstMonthFeeCalc BEGINS "Usage" THEN 2 ELSE IF lcMobile_DataLimit_FirstMonthFeeCalc BEGINS "Full" THEN 0 ELSE 0)
       liDLLastMonthBR  = (IF lcMobile_DataLimit_LastMonthFeeCalc  BEGINS "Relative" THEN 1 ELSE IF lcMobile_DataLimit_LastMonthFeeCalc  BEGINS "Usage" THEN 2 ELSE IF lcMobile_DataLimit_LastMonthFeeCalc  BEGINS "Full" THEN 0 ELSE 0) 

       liVLFirstMonthBR = (IF lcMobile_VoiceLimit_FirstMonthFeeCalc BEGINS "Relative" THEN 1 ELSE IF lcMobile_VoiceLimit_FirstMonthFeeCalc BEGINS "Usage" THEN 2 ELSE IF lcMobile_VoiceLimit_FirstMonthFeeCalc BEGINS "Full" THEN 0 ELSE 0)
       liVLLastMonthBR  = (IF lcMobile_VoiceLimit_LastMonthFeeCalc  BEGINS "Relative" THEN 1 ELSE IF lcMobile_VoiceLimit_LastMonthFeeCalc  BEGINS "Usage" THEN 2 ELSE IF lcMobile_VoiceLimit_LastMonthFeeCalc  BEGINS "Full" THEN 0 ELSE 0) 

       liBDLFirstMonthBR = (IF lcMobile_BDestLimit_FirstMonthFeeCalc BEGINS "Relative" THEN 1 ELSE IF lcMobile_BDestLimit_FirstMonthFeeCalc BEGINS "Usage" THEN 2 ELSE IF lcMobile_BDestLimit_FirstMonthFeeCalc BEGINS "Full" THEN 0 ELSE 0)
       liBDLLastMonthBR  = (IF lcMobile_BDestLimit_LastMonthFeeCalc  BEGINS "Relative" THEN 1 ELSE IF lcMobile_BDestLimit_LastMonthFeeCalc  BEGINS "Usage" THEN 2 ELSE IF lcMobile_BDestLimit_LastMonthFeeCalc  BEGINS "Full" THEN 0 ELSE 0).

   RUN pBundle(lcMobile_BaseBundle,
               lcMobile_BaseBundleName,
               lcMobile_BaseBundleType,
               lcMobile_BaseBundlePayType,
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
   
END PROCEDURE.


PROCEDURE pBundle:
    DEFINE INPUT PARAMETER icBundle            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icBundleName        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icBundleType        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icBundlePayType     AS CHARACTER NO-UNDO.
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

    ASSIGN lcBCList   = "10100001,10100003,10100005,CFOTHER,CFYOIGO".

    CREATE ttDayCampaign.
    ASSIGN
        ttDayCampaign.DCEvent     = icBundle
        ttDayCampaign.DCName      = icBundleName
        ttDayCampaign.DCType      = icBundleType
        ttDayCampaign.PayType     = icBundlePayType
        ttDayCampaign.BillCode    = icBillCode
        ttDayCampaign.UpSell      = icUpSell
        ttDayCampaign.BonoSupport = ilBonoSupport
        ttDayCampaign.DataLimit   = ideDataLimit
        ttDayCampaign.SLCreated   = Yes WHEN (ideDataLimit > 0 OR ideVoiceLimit > 0 OR ideBDestLimit > 0).

    IF ideCommercialFee > 0 THEN
    DO:     
        CREATE ttFMItem.
        ASSIGN 
            ttFMItem.FeeModel     = ttDayCampaign.BillCode
            ttFMItem.BillCode     = ttDayCampaign.BillCode
            ttFMItem.PriceList    = "COMMON"
            ttFMItem.Amount       = ideCommercialFee
            ttFMItem.FirstMonthBR = iiFirstMonthBR
            ttFMItem.BrokenRental = iiLastMonthBR.
    END.

    CREATE ttServiceLimitGroup.
    ASSIGN 
        ttServiceLimitGroup.GroupCode = ttDayCampaign.DCEvent
        ttServiceLimitGroup.GroupName = ttDayCampaign.DCName.
     
     IF icBundleType = "Upsell" THEN
     DO:
         IF ideDataLimit > 0 THEN
         DO:
             RUN pCreateServiceLimit_Upsell(ttDayCampaign.DCEvent,
                                            ideDataLimit,
                                            iiDLFirstMonthCalc,
                                            iiDLLastMonthCalc,
                                            "Mobile").
         END.
     END.
     ELSE IF icBundleType = "PackageWithCounter" THEN 
     DO:
         IF ideDataLimit > 0 THEN
         DO:
             RUN pCreateProgressiveRatingLimit_Data(ttDayCampaign.DCEvent,
                                                    ideDataLimit,
                                                    iiDLFirstMonthCalc,
                                                    iiDLLastMonthCalc,
                                                    "Mobile").
         END. 
     END.
     ELSE IF icBundleType = "ServicePackage" THEN 
     DO:   
         IF ideDataLimit > 0 THEN 
         DO:         
             RUN pCreateServiceLimit_Data(ttDayCampaign.DCEvent,
                                          ideDataLimit,
                                          iiDLFirstMonthCalc,
                                          iiDLLastMonthCalc,
                                          "Mobile").             
         END.

         IF ideVoiceLimit > 0 THEN 
         DO:
            RUN pCreateServiceLimit_Voice(ttDayCampaign.DCEvent,
                                          ideVoiceLimit,
                                          iiVLFirstMonthCalc,
                                          iiVLLastMonthCalc,
                                          "Mobile").
         END.

         IF ideBDestLimit > 0 THEN 
         DO:
            RUN pCreateServiceLimit_BDest(ttDayCampaign.DCEvent,
                                          ideBDestLimit,
                                          iiBDLFirstMonthCalc,
                                          iiBDLLastMonthCalc, 
                                          "Mobile").
         END. 
    END.
    
END PROCEDURE.    


PROCEDURE pCreateServiceLimit_Data:
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
       ttServiceLimit.InclUnit       = 4
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

    RETURN "".

END PROCEDURE.

PROCEDURE pCreateServiceLimit_Voice:
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
       ttServiceLimit.InclUnit       = 1
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
         
     RETURN "".

END PROCEDURE.


PROCEDURE pCreateServiceLimit_BDest:
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
       ttServiceLimit.InclUnit       = 7
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

     RETURN "".

END PROCEDURE.

PROCEDURE pCreateProgressiveRatingLimit_Data:
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
       ttServiceLimit.InclUnit       = 4
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
    
    RETURN "".

END PROCEDURE.

PROCEDURE pCreateServiceLimit_Upsell:
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
       ttServiceLimit.SLCode         = icDCEvemt
       ttServiceLimit.SLName         = "Upsell" 
       ttServiceLimit.DialType       = 7
       ttServiceLimit.InclUnit       = 4
       ttServiceLimit.InclAmt        = ideDataLimit
       ttServiceLimit.FirstMonthCalc = iiDLFirstMonthCalc
       ttServiceLimit.LastMonthCalc  = iiDLLastMonthCalc.

    CREATE ttServiceLimitTarget.
    ASSIGN
       ttServiceLimitTarget.GroupCode      = ttServiceLimit.GroupCode
       ttServiceLimitTarget.SLCode         = ttServiceLimit.SLCode
       ttServiceLimitTarget.ServiceLMember = "14100001" 
       ttServiceLimitTarget.InSideRate     = "GPRS_" + icDCEvemt 
       ttServiceLimitTarget.OutSideRate    = "".

    CREATE ttBDest.
    ASSIGN
        ttBDest.GroupCode = ttServiceLimitTarget.GroupCode                 
        ttBDest.SLCode    = ttServiceLimitTarget.SLCode 
        ttBDest.BDest     = ttServiceLimitTarget.InSideRate
        ttBDest.BDName    = ttServiceLimitTarget.GroupCode + " " + "GPRS DATA"                 
        ttBDest.CCN       = 93.

    RETURN "".

END PROCEDURE.


PROCEDURE pValidateData:

   DO ON ERROR UNDO, THROW:

      FOR EACH ttBundleCre 
          ON ERROR UNDO, THROW:         
         
          CASE ttBundleCre.FieldName:
             /* Mobile */ 
             WHEN {&M_BB} THEN 
             DO:
                IF ttBundleCre.FieldValue = "" THEN                
                   UNDO, THROW NEW Progress.Lang.AppError("Wrong BaseBundle data available", 1).
                ELSE 
                   ASSIGN lcMobile_BaseBundle = ttBundleCre.FieldValue.
             END.
             WHEN {&M_BBN} THEN 
             DO:
                IF ttBundleCre.FieldValue = "" THEN                
                   UNDO, THROW NEW Progress.Lang.AppError("Wrong BaseBundle Name data available", 1).
                ELSE 
                   ASSIGN lcMobile_BaseBundleName = ttBundleCre.FieldValue.
             END.
             WHEN {&M_BBT} THEN 
             DO:
                IF ttBundleCre.FieldValue = "" OR (ttBundleCre.FieldValue NE "" AND LOOKUP(ttBundleCre.FieldValue,{&CONTRACT}) EQ 0) THEN
                   UNDO, THROW NEW Progress.Lang.AppError("Wrong Type of Contract data", 1).                  
                ELSE IF (ttBundleCre.FieldValue NE "") AND LOOKUP(ttBundleCre.FieldValue,{&CONTRACT}) GT 0 THEN
                   lcMobile_BaseBundleType = ttBundleCre.FieldValue.
             END.
             WHEN {&M_BBPT} THEN 
             DO:
                IF ttBundleCre.FieldValue NE "" THEN
                   lcMobile_BaseBundlePayType  = ttBundleCre.FieldValue.
             END.
             WHEN {&M_UPSL} THEN 
             DO:
                IF ttBundleCre.FieldValue NE "" THEN
                   lcMobile_BaseBundleUpsell  = ttBundleCre.FieldValue.
             END.
             WHEN {&M_BONO} THEN
             DO: 
                IF ttBundleCre.FieldValue NE "" THEN 
                   lcMobile_BaseBundleBonoSupport = ttBundleCre.FieldValue.
             END.
             WHEN {&M_MFBC} THEN 
                lcMobile_MonthlyFeeBillCode = ttBundleCre.FieldValue. 
             WHEN {&M_CF} THEN 
                lcMobile_CommercialFee = ttBundleCre.FieldValue.                
             WHEN {&M_FMFC} THEN 
             DO:
                IF (ttBundleCre.FieldValue NE "") AND LOOKUP(ttBundleCre.FieldValue,{&FEECALC}) EQ 0 THEN 
                   UNDO, THROW NEW Progress.Lang.AppError("Wrong First Month Fee calculation data", 1).
               ELSE 
                  lcMobile_FirstMonthFeeCalc = ttBundleCre.FieldValue.
             END.
             WHEN {&M_LMFC} THEN 
             DO:
                IF (ttBundleCre.FieldValue NE "") AND LOOKUP(ttBundleCre.FieldValue,{&FEECALC}) EQ 0 THEN 
                   UNDO, THROW NEW Progress.Lang.AppError("Wrong Last Month Fee calculation data", 1).                  
                ELSE 
                   lcMobile_LastMonthFeeCalc = ttBundleCre.FieldValue.
             END.    
             WHEN {&M_DL} THEN 
             DO:
                IF ttBundleCre.FieldValue NE "" THEN
                   ASSIGN lcMobile_DataLimit  = ttBundleCre.FieldValue.
             END.
             WHEN {&M_VL} THEN 
             DO:
                IF ttBundleCre.FieldValue NE "" THEN
                   ASSIGN lcMobile_VoiceLimit  = ttBundleCre.FieldValue.
             END.   
             WHEN {&M_BDL} THEN
             DO: 
                IF ttBundleCre.FieldValue NE "" THEN 
                   ASSIGN lcMobile_BDestLimit  = ttBundleCre.FieldValue.
             END.
             WHEN {&M_FMDL} THEN 
             DO:
                IF (ttBundleCre.FieldValue NE "") AND LOOKUP(ttBundleCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                   UNDO, THROW NEW Progress.Lang.AppError("Wrong First month data limit value", 1).                  
                ELSE 
                   lcMobile_DataLimit_FirstMonthFeeCalc = ttBundleCre.FieldValue.
             END.
             WHEN {&M_LMDL} THEN 
             DO:
                IF (ttBundleCre.FieldValue NE "") AND LOOKUP(ttBundleCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                   UNDO, THROW NEW Progress.Lang.AppError("Wrong Last month data limit value", 1).
                ELSE 
                   lcMobile_DataLimit_LastMonthFeeCalc = ttBundleCre.FieldValue.
             END.
             WHEN {&M_FMVL} THEN 
             DO:
                IF (ttBundleCre.FieldValue NE "") AND LOOKUP(ttBundleCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                   UNDO, THROW NEW Progress.Lang.AppError("Wrong First month voice limit value", 1).                  
                ELSE 
                   lcMobile_VoiceLimit_FirstMonthFeeCalc = ttBundleCre.FieldValue.
             END.
             WHEN {&M_LMVL} THEN 
             DO:
                IF (ttBundleCre.FieldValue NE "") AND LOOKUP(ttBundleCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                   UNDO, THROW NEW Progress.Lang.AppError("Wrong Last month voice limit value", 1).
                ELSE 
                   lcMobile_VoiceLimit_LastMonthFeeCalc = ttBundleCre.FieldValue.
             END.
             WHEN {&M_FMBDL} THEN 
             DO:
                IF (ttBundleCre.FieldValue NE "") AND LOOKUP(ttBundleCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                   UNDO, THROW NEW Progress.Lang.AppError("Wrong First month BDestination limit value", 1).                  
                ELSE 
                   lcMobile_BDestLimit_FirstMonthFeeCalc = ttBundleCre.FieldValue.
             END.
             WHEN {&M_LMBDL} THEN 
             DO:
                IF (ttBundleCre.FieldValue NE "") AND LOOKUP(ttBundleCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                   UNDO, THROW NEW Progress.Lang.AppError("Wrong Last month BDestination limit value", 1).
                ELSE 
                   lcMobile_BDestLimit_LastMonthFeeCalc = ttBundleCre.FieldValue.
             END.
                           
          END CASE.                  
      END. /* FOR EACH ttSubTypeCr */      

      /* Validations */      
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

   RETURN "".

END PROCEDURE.


PROCEDURE pReadTranslation:
   DEFINE VARIABLE lcLine      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO.   
   DEFINE VARIABLE liFirstLine AS INTEGER   NO-UNDO INITIAL 1.

   DO ON ERROR UNDO, THROW:
      
      ASSIGN lcInputFile = icIncDir + "translations.txt".      

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
