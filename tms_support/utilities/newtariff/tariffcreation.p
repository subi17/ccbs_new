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
DEFINE INPUT  PARAMETER iiPayType   AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER icRatePlan  AS CHARACTER NO-UNDO. /* Final CLIType list */

DEFINE VARIABLE lcLogFile           AS CHARACTER NO-UNDO.
DEFINE VARIABLE llgTrafficBundle    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llgPostPaid         AS LOGICAL   NO-UNDO.

DEFINE VARIABLE lcCliType           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTariffBundle      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBaseBundle        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLineType          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixLineType       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLineDownload AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLineUpload   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCommFee           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcComparisonFee     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcServiceClass      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcWebStatus         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSTCStatus         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPaymentType       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcUsageType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFMFeeCalc         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLMFeeCalc         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTOC               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDataLimit         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcVoiceLimit        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBDestLimit        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFMDataLimit       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLMDataLimit       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFMVoiceLimit      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLMVoiceLimit      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFMBDestLimit      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLMBDestLimit      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBBProfile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDSS2Comp          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDSS2PL            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcNVComp            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOnlyVoice         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFinalTariff       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFeeModel          AS CHARACTER NO-UNDO.
DEFINE VARIABLE liSLSeq             AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcSubCount          AS INTEGER   NO-UNDO.
DEFINE VARIABLE h_config            AS HANDLE    NO-UNDO.
DEFINE VARIABLE lcServList          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcServCount         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcFinalSLCode       AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE lcFinalSLName       AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE lcFinalLimit        AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE lcFinalBDLimit      AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE lcFinalFMLimit      AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE lcFinalLMLimit      AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE liFinalDType        AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE llgSLCreated        AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VARIABLE ocCLIType           AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE lcMFBC              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLIName           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSLGName           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFMName            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDCName            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTariffName        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBonoSupport       AS CHARACTER NO-UNDO.
/*convergence*/
DEFINE VARIABLE lcBundleUpsell             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBDestSLGTariff           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSLCode                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liKnt                      AS INTEGER   NO-UNDO.
DEFINE VARIABLE liBDestSLGKnt              AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcAllowedBundles           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixedLineBaseBundle      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCopyServicesFromCliType  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBundlesForTerminateOnSTC AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcServicesForReCreateOnSTC AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttTariffCre NO-UNDO 
   FIELD FieldName  AS CHARACTER 
   FIELD FieldValue AS CHARACTER.
   
DEFINE STREAM TariffIn.
DEFINE STREAM TariffLog.
DEFINE STREAM TTransIn.
DEFINE STREAM TTransLog.

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

   RUN pFill_TT_TariffCre.   
   
   RUN pFill_TT_Translation.

   RUN pValidateFileData.   

   RUN pDataCreValidation.     

   IF lcPaymentType EQ "Postpaid" THEN 
   DO: 
      IF lcMFBC NE "" THEN
      DO: 
         RUN pCreateFeeModel IN h_config(lcFinalTariff,
                                         lcFMName,    
                                         OUTPUT lcFeeModel).

         IF lcFeeModel > "" THEN
            RUN pCreateFMItem IN h_config(lcFinalTariff,
                                          lcFeeModel,
                                          lcCommFee,
                                          lcFMFeeCalc,
                                          lcLMFeeCalc,
                                          lcMFBC,
                                          lcCliType,       /* Main parent tariff */
                                          lcTariffBundle). /* Tariff Bundle      */
      END.

      IF lcDataLimit  NE "" OR lcVoiceLimit NE "" OR lcBDestLimit NE "" THEN 
      DO liKnt = 1 TO liBDestSLGKnt
         ON ERROR UNDO, THROW: 

         IF liKnt = 1 THEN 
            ASSIGN lcBDestSLGTariff = lcFinalTariff.
         ELSE 
            ASSIGN lcBDestSLGTariff = lcFixedLineBaseBundle.    

         RUN pCreBDestination IN h_config(lcBDestSLGTariff,
                                          lcDataLimit,
                                          lcVoiceLimit,
                                          lcBDestLimit).

         RUN pServLimitGroup IN h_config(lcBDestSLGTariff, lcSLGName).
         
         DO lcServCount = 1 TO NUM-ENTRIES(lcServList):

            ASSIGN 
               lcFinalSLCode = lcBDestSLGTariff + (IF ENTRY(lcServCount,lcServList) EQ {&DL} THEN 
                                                 "_DATA" 
                                              ELSE IF ENTRY(lcServCount,lcServList) EQ {&DL} THEN 
                                                 "_MIN" 
                                              ELSE IF ENTRY(lcServCount,lcServList) EQ {&DL} THEN 
                                                 "_QTY" 
                                              ELSE
                                                 "")
               lcFinalSLName = (IF INDEX(lcSLCode,"DATA") > 0 THEN 
                                   "Data" 
                                ELSE IF INDEX(lcSLCode,"MIN") > 0 THEN 
                                   "National Calls" 
                                ELSE IF INDEX(lcSLCode,"QTY") > 0 THEN
                                   "BDest" 
                                ELSE 
                                   "")
               liFinalDType  = (IF INDEX(lcSLCode,"DATA") > 0 THEN 
                                   7 
                                ELSE IF INDEX(lcSLCode,"MIN") > 0 THEN 
                                   4 
                                ELSE IF INDEX(lcSLCode,"QTY") > 0 THEN
                                   0 
                                ELSE 
                                   0)
               lcFinalLimit  = (IF INDEX(lcSLCode,"DATA") > 0 THEN 
                                   lcDataLimit 
                                ELSE IF INDEX(lcSLCode,"MIN") > 0 THEN 
                                   lcVoiceLimit 
                                ELSE IF INDEX(lcSLCode,"QTY") > 0 THEN
                                   lcBDestLimit 
                                ELSE 
                                   "")
               lcFinalBDLimit = "0"
               lcFinalFMLimit = (IF INDEX(lcSLCode,"DATA") > 0 THEN 
                                    lcFMDataLimit 
                                 ELSE IF INDEX(lcSLCode,"MIN") > 0 THEN 
                                    lcFMVoiceLimit 
                                 ELSE IF INDEX(lcSLCode,"QTY") > 0 THEN
                                    lcFMBDestLimit 
                                 ELSE 
                                    "")
               lcFinalLMLimit = (IF INDEX(lcSLCode,"DATA") > 0 THEN 
                                    lcLMDataLimit 
                                 ELSE IF INDEX(lcSLCode,"MIN") > 0 THEN 
                                    lcLMVoiceLimit 
                                 ELSE IF INDEX(lcSLCode,"QTY") > 0 THEN
                                    lcLMBDestLimit 
                                 ELSE 
                                    "").

            RUN pServiceLimit IN h_config(lcBDestSLGTariff,
                                          lcFinalSLCode,
                                          lcFinalSLName,
                                          lcFinalLimit,
                                          lcFinalBDLimit,
                                          lcFinalFMLimit,
                                          lcFinalLMLimit,
                                          liFinalDType,
                                          OUTPUT liSLSeq). 
            IF liSLSeq > 0 THEN
               RUN pServiceTargets IN h_config(liSLSeq, lcBDestSLGTariff, ENTRY(2,lcFinalSLCode,"_")).

            ASSIGN llgSLCreated = YES.
         END.
      END.
   END.      

   RUN pDayCampaign IN h_config(lcFinalTariff,
                                lcFeeModel,
                                lcDCName,                                
                                lcTOC,
                                llgSLCreated,
                                lcMFBC,
                                lcPaymentType,
                                lcBundleUpsell,                                
                                lcBonoSupport).

   RUN pCreateCLIType IN h_config(lcCliType,
                                  lcCLIName,        
                                  lcBaseBundle,
                                  lcFixedLineBaseBundle,
                                  lcLineType,
                                  lcFixLineType,
                                  lcFixedLineDownload,
                                  lcFixedLineUpload,
                                  lcCommFee,
                                  lcComparisonFee,
                                  lcServiceClass,
                                  lcWebStatus,
                                  lcSTCStatus,
                                  lcPaymentType,
                                  lcUsageType,
                                  icRatePlan,                                  
                                  lcCliType,        /* Main parent tariff */ 
                                  lcTariffBundle,   /* Tariff Bundle      */
                                  lcAllowedBundles,
                                  lcDataLimit,
                                  lcCopyServicesFromCliType,
                                  lcBundlesForTerminateOnSTC,
                                  lcServicesForReCreateOnSTC).  

   RUN pCreTranslations.  
      
   RETURN "".
   
   CATCH e AS Progress.Lang.Error:

      OUTPUT STREAM TariffLog TO VALUE(lcLogFile) APPEND.
      fError(RETURN-VALUE).
      OUTPUT STREAM TariffLog CLOSE.

      UNDO, THROW NEW Progress.Lang.AppError(RETURN-VALUE, 1).

   END CATCH.
   FINALLY:

   END FINALLY.

END.
/* ***************************  Main End  *************************** */ 
PROCEDURE pFill_TT_TariffCre:   

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
         UNDO, THROW NEW Progress.Lang.AppError('Incorrect input file data' + err:GetMessage(1), 1). 
      END CATCH.

      FINALLY:
         INPUT STREAM TariffIn CLOSE.
      END FINALLY.
   END.

   RETURN "".

END PROCEDURE.

PROCEDURE pFill_TT_Translation:
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
           ttTrans.tLangint   = TRIM(ENTRY(2,lcLine,";"))
           ttTrans.tLangtext  = TRIM(ENTRY(3,lcLine,";"))
           ttTrans.tLangTrans = TRIM(ENTRY(4,lcLine,";")).
      END.

      CATCH err AS Progress.Lang.Error:
         UNDO, THROW NEW Progress.Lang.AppError('Incorrect input translation file data' + err:GetMessage(1), 1). 
      END CATCH.

      FINALLY:
         INPUT STREAM TTransIn CLOSE.
      END FINALLY.

   END.

END PROCEDURE.

PROCEDURE pDataCreValidation:   
   
   IF lcPaymentType EQ "Postpaid" THEN 
      ASSIGN 
         lcFinalTariff = (IF lcTariffBundle NE "" THEN lcTariffBundle ELSE lcBaseBundle)
         lcCLIName = lcTariffName 
         lcFMName  = lcTariffName  + " "       + {&MOF}
         lcSLGName = lcTariffName
         lcDCName  = lcTariffName.
   ELSE 
      ASSIGN 
         lcFinalTariff = lcCliType
         lcCLIName     = lcTariffName
         lcSLGName     = lcTariffName
         lcDCName      = lcTariffName.          

   IF lcBaseBundle <> "" THEN 
       ASSIGN lcAllowedBundles = lcAllowedBundles + (IF lcAllowedBundles <> "" THEN "," ELSE "") + lcBaseBundle.

    IF lcFixedLineBaseBundle <> "" THEN 
       ASSIGN lcAllowedBundles = lcAllowedBundles + (IF lcAllowedBundles <> "" THEN "," ELSE "") + lcFixedLineBaseBundle.

    ASSIGN liBDestSLGKnt = (IF lcFixedLineBaseBundle > "" THEN 2 ELSE 1).   

   /* Listing ServiceLimits list */
   IF lcDataLimit NE "" THEN 
      lcServList = {&DL}.   
   
   IF lcVoiceLimit NE "" AND lcVoiceLimit NE "Unlimited" THEN 
      lcServList = lcServList + (IF lcServList NE "" THEN "," ELSE "") + {&VL}.
      
   IF lcBDestLimit NE "" THEN
      lcServList = lcServList + (IF lcServList NE "" THEN "," ELSE "") + {&BDL}.
   
END PROCEDURE.
    
PROCEDURE pValidateFileData:
   
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
               ASSIGN lcTariffName = ttTariffCre.FieldValue.
            WHEN {&TB} THEN 
            DO:
               IF ttTariffCre.FieldValue NE "" THEN 
                  ASSIGN 
                     llgTrafficBundle = YES
                     lcTariffBundle   = ttTariffCre.FieldValue.          
            END.
            WHEN {&BB} THEN 
            DO:
               IF ttTariffCre.FieldValue NE "" AND llgTrafficBundle THEN                
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong BaseBundle data available", 1).
               ELSE 
                  ASSIGN lcBaseBundle = ttTariffCre.FieldValue.
            END.
            WHEN {&AB} THEN             
               ASSIGN lcAllowedBundles = ttTariffCre.FieldValue.             
            WHEN {&STC_BT} THEN             
               ASSIGN lcBundlesForTerminateOnSTC = ttTariffCre.FieldValue.             
            WHEN {&STC_SR} THEN             
               ASSIGN lcServicesForReCreateOnSTC = ttTariffCre.FieldValue.             
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
            WHEN {&FLBB} THEN 
            DO:
               IF ttTariffCre.FieldValue NE "" THEN 
                  ASSIGN lcFixedLineBaseBundle = ttTariffCre.FieldValue.
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
            WHEN {&CF} THEN 
               lcCommFee = ttTariffCre.FieldValue. 
            WHEN {&CMF} THEN 
               lcComparisonFee = ttTariffCre.FieldValue.  
            WHEN {&SC} THEN 
            DO:
               IF ttTariffCre.FieldValue NE "" THEN 
                  lcServiceClass = ttTariffCre.FieldValue.
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
            WHEN {&CSF} THEN 
            DO:
               IF ttTariffCre.FieldValue = "" THEN
                  UNDO, THROW NEW Progress.Lang.AppError("Copy services from clitype is blank", 1).
               ELSE IF NOT CAN-FIND(FIRST CliType WHERE CliType.Brand = gcBrand AND CliType.CliType = ttTariffCre.FieldValue NO-LOCK) THEN 
                  UNDO, THROW NEW Progress.Lang.AppError("Invalid 'copy services from clitype'", 1).   
               ELSE 
                  lcCopyServicesFromCliType = ttTariffCre.FieldValue.
            END.
            WHEN {&TOC} THEN 
            DO:
               IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&CONTRACT}) EQ 0 THEN
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong Type of Contract data", 1).                  
               ELSE IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&CONTRACT}) GT 0 THEN
                  lcTOC = ttTariffCre.FieldValue.
            END.
            WHEN {&NVC} THEN 
            DO:
               IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LOGVALUE}) EQ 0 THEN
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong Native VOIP compatible data", 1).                  
               ELSE 
                   lcNVComp = ttTariffCre.FieldValue.
            END.
            WHEN {&DL} THEN 
            DO:
               IF ttTariffCre.FieldValue NE "" THEN
                  ASSIGN lcDataLimit  = ttTariffCre.FieldValue.
            END.
            WHEN {&VL} THEN 
            DO:
               IF ttTariffCre.FieldValue NE "" THEN
                  ASSIGN lcVoiceLimit  = ttTariffCre.FieldValue.
            END.
            /*convergence*/
            WHEN {&BUPS} THEN 
            DO:
               IF ttTariffCre.FieldValue NE "" THEN
                  lcBundleUpsell  = ttTariffCre.FieldValue.
            END.            
         END CASE. 
         

         IF llgPostPaid THEN 
         DO:
            CASE ttTariffCre.FieldName:
               WHEN {&MFBC} THEN 
                  lcMFBC = ttTariffCre.FieldValue.             
               WHEN {&FMFC} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&FEECALC}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong First Month Fee calculation data", 1).
                 ELSE 
                    lcFMFeeCalc = ttTariffCre.FieldValue.
               END.
               WHEN {&LMFC} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&FEECALC}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Last Month Fee calculation data", 1).                  
                  ELSE 
                     lcLMFeeCalc = ttTariffCre.FieldValue.
               END.    
               WHEN {&BDL} THEN 
                  IF ttTariffCre.FieldValue NE "" THEN 
                     lcBDestLimit  = ttTariffCre.FieldValue.
               WHEN {&FMDL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong First month data limit value", 1).                  
                  ELSE 
                     lcFMDataLimit = ttTariffCre.FieldValue.
               END.
               WHEN {&LMDL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Last month data limit value", 1).
                  ELSE 
                     lcLMDataLimit = ttTariffCre.FieldValue.
               END.
               WHEN {&FMVL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong First month voice limit value", 1).                  
                  ELSE 
                     lcFMVoiceLimit = ttTariffCre.FieldValue.
               END.
               WHEN {&LMVL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Last month voice limit value", 1).
                  ELSE 
                     lcLMVoiceLimit = ttTariffCre.FieldValue.
               END.
               WHEN {&FMBDL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong First month BDestination limit value", 1).                  
                  ELSE 
                     lcFMBDestLimit = ttTariffCre.FieldValue.
               END.
               WHEN {&LMBDL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Last month BDestination limit value", 1).
                  ELSE 
                     lcLMBDestLimit = ttTariffCre.FieldValue.
               END.
               WHEN {&BBP} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&BBPROFILE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong BBProfile data", 1).
                  ELSE 
                     lcBBProfile = ttTariffCre.FieldValue.
               END.
               WHEN {&DSS2C} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LOGVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong DSS2 Compatible data", 1).
                  ELSE 
                     lcDSS2Comp = ttTariffCre.FieldValue.
               END.
               WHEN {&DSS2PL} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LOGVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong DSS2 Primary line data", 1).                  
                  ELSE 
                     lcDSS2PL = ttTariffCre.FieldValue.
               END.
               WHEN {&OV} THEN 
               DO:
                  IF (ttTariffCre.FieldValue NE "") AND LOOKUP(ttTariffCre.FieldValue,{&LOGVALUE}) EQ 0 THEN 
                     UNDO, THROW NEW Progress.Lang.AppError("Wrong Only VOICE data", 1).                  
                  ELSE 
                     lcOnlyVoice = ttTariffCre.FieldValue.
               END.  
               WHEN {&BS} THEN 
                  IF ttTariffCre.FieldValue NE "" THEN 
                     lcBonoSupport = ttTariffCre.FieldValue.
            END CASE.             
         END. /* IF llgPostPaid THEN DO */      
      END. /* FOR EACH ttSubTypeCr */      

      /* Validations */
      IF (iiPayType = 2 AND lcPaymentType = "Postpaid") OR (iiPayType = 1 AND lcPaymentType = "Prepaid") THEN 
         UNDO, THROW NEW Progress.Lang.AppError("Rateplan and Tariff with different payment types", 1).
      ELSE IF lcFixLineType <> "" AND (lcFixedLineBaseBundle = "" OR lcFixedLineDownload = "" OR lcFixedLineUpload = "") THEN 
         UNDO, THROW NEW Progress.Lang.AppError("Fixed line base bundle or upload/download speed is invalid", 1).
      ELSE IF lcPaymentType = "PostPaid" AND lcServiceClass <> "" THEN  
         UNDO, THROW NEW Progress.Lang.AppError("Postpaid subscription contains Serviceclass data", 1).
      ELSE IF lcPaymentType = "PrePaid" AND lcServiceClass = "" THEN
         UNDO, THROW NEW Progress.Lang.AppError("Prepaid subscription doesn't contain any Serviceclass data", 1).      
      ELSE
      DO:
         CASE lcTOC:
            WHEN "ServicePackage" THEN 
            DO:      
               IF lcDataLimit = "" AND lcVoiceLimit = "" THEN 
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong ServicePackage-contract data with limits provided", 1).
            END.
            WHEN "PackageWithCounter" THEN 
            DO:
               IF lcDataLimit = "" AND lcVoiceLimit = "" THEN 
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong PackageWithCounter-contract data with limit provided", 1).
            END.
            WHEN "PackageWithoutCounter" THEN 
            DO:
               IF lcDataLimit <> "" OR lcVoiceLimit <> "" THEN 
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong PackageWithoutCounter-contract data with limit provided", 1).              
            END.
         END CASE.
      END.   

      ASSIGN 
         llgTrafficBundle  = NO
         llgPostPaid       = NO.         

      RETURN "".

   END.

END PROCEDURE.

PROCEDURE pCreTranslations:

   FOR EACH ttTrans NO-LOCK:
      IF CAN-FIND(FIRST CLIType WHERE 
                        CLIType.Brand   = gcBrand            AND 
                        CLIType.CLIType = ttTrans.tLangType) THEN DO:

         IF CAN-FIND(FIRST RepText WHERE
                           RepText.Brand    = gcBrand                    AND
                           RepText.LinkCode = ttTrans.tLangType          AND
                           RepText.Language = INTEGER(ttTrans.tLangint)) THEN 
            NEXT. 
                             
         CREATE RepText.
         ASSIGN 
            RepText.Brand    = gcBrand    
            RepText.TextType = 9               /* Default value */       
            RepText.LinkCode = ttTrans.tLangType        
            RepText.Language = INTEGER(ttTrans.tLangint)    
            RepText.FromDate = TODAY     
            RepText.ToDate   = 12/31/49       
            RepText.RepText  = ttTrans.tLangTrans NO-ERROR.
            
         IF ERROR-STATUS:ERROR THEN DO:
            fError("Creating translations for CLIType").
            RETURN "ERROR".
         END.
      END.  
      ELSE NEXT.
   END.
   
   RETURN "OK".

END.        

