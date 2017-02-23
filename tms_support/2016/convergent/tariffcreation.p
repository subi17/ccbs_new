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
{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/ftransdir.i}
{../tms_support/2016/convergent/tariffconfig.i}
{../tms_support/2016/convergent/tariffcons.i}

DEFINE INPUT  PARAMETER icIncDir    AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icSpoolDir  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iiPayType   AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER icRatePlan  AS CHARACTER NO-UNDO. /* Final CLIType list */

DEFINE VARIABLE lcInputFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLine            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE llgTrafficBundle  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llgSCPayTypeValue AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llgPostPaid       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llgServicePackage AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llgPackWCounter   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llgPackWOCounter  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llgDataLimit      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llgVoiceLimit     AS LOGICAL   NO-UNDO.

DEFINE VARIABLE lcCliType        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTariffBundle   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBaseBundle     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLineType       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFixLineType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCommFee        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcComparisonFee  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcServiceClass   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcWebStatus      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSTCStatus      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPaymentType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcUsageType      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFMFeeCalc      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLMFeeCalc      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTOC            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDataLimit      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcVoiceLimit     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBDestLimit     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFMDataLimit    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLMDataLimit    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFMVoiceLimit   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLMVoiceLimit   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFMBDestLimit   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLMBDestLimit   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBBProfile      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDSS2Comp       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDSS2PL         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcNVComp         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOnlyVoice      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFinalTariff    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFeeModel       AS CHARACTER NO-UNDO.
DEFINE VARIABLE icSLSeq          AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcSubList        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSubCount       AS INTEGER   NO-UNDO.
DEFINE VARIABLE h_config         AS HANDLE    NO-UNDO.
DEFINE VARIABLE lcServList       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcServCount      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcFinalSLCode    AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE lcFinalSLName    AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE lcFinalLimit     AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE lcFinalBDLimit   AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE lcFinalFMLimit   AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE lcFinalLMLimit   AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE liFinalDType     AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE llgSLCreated     AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VARIABLE ocCLIType        AS CHARACTER NO-UNDO INITIAL "".
DEFINE VARIABLE liFirstLine      AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE llgCTServPac     AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VARIABLE lcMFBC           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLIName        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSLGName        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFMName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDCName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTariffName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBonoSupport    AS CHARACTER NO-UNDO.
/*convergence*/
DEFINE VARIABLE lcBundleUpsell   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBundleList     AS CHARACTER NO-UNDO.


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

ASSIGN lcLogFile   = icSpoolDir + "tariffcreation.log" 
       lcInputFile = icIncDir /* + "tariffcreation.txt"*/.

INPUT STREAM TariffIn FROM VALUE(lcInputFile).
OUTPUT STREAM TariffLog TO VALUE(lcLogFile) APPEND.
    
REPEAT:

   IMPORT STREAM TariffIn UNFORMATTED lcLine.
    
   CREATE ttTariffCre.
   ASSIGN ttTariffCre.FieldName  = TRIM(ENTRY(1,lcLine,";"))
          ttTariffCre.FieldValue = TRIM(ENTRY(2,lcLine,";")) NO-ERROR.
   
   IF ERROR-STATUS:ERROR THEN DO:
      fError("Incorrect input data 1").
      RETURN "ERROR".
   END.
   
END.
               
RUN pValidateFileData.

IF RETURN-VALUE <> "OK" THEN DO:
   fError(RETURN-VALUE). 
   RETURN RETURN-VALUE.
END.
   
RUN tms_support/2016/convergent/configcreations.p PERSISTENT SET h_config. 

RUN pDataCreValidation.

IF RETURN-VALUE <> "OK" THEN DO:
   fError(RETURN-VALUE). 
   RETURN RETURN-VALUE.
END.

IF lcDataLimit  NE "" OR 
   lcVoiceLimit NE "" OR 
   lcBDestLimit NE "" THEN 
   RUN pCreBDestination IN h_config(lcFinalTariff,
                                    lcDataLimit,
                                    lcVoiceLimit,
                                    lcBDestLimit).

RUN pNamingConvention.

IF RETURN-VALUE <> "OK" THEN DO:
   fError(RETURN-VALUE). 
   RETURN RETURN-VALUE.
END.

RUN pServLimitGroup IN h_config(lcFinalTariff,
                                lcSLGName).   

IF RETURN-VALUE <> "OK" THEN DO:
   fError(RETURN-VALUE). 
   RETURN RETURN-VALUE.
END.

DO lcServCount = 1 TO NUM-ENTRIES(lcServList):
   IF ENTRY(lcServCount,lcServList,",") EQ {&DL} THEN 
      ASSIGN 
         lcFinalSLCode  = lcFinalTariff + "_DATA"
         lcFinalSLName  = "Data"
         liFinalDType   = 7
         lcFinalLimit   = lcDataLimit
         lcFinalBDLimit = "0"
         lcFinalFMLimit = lcFMDataLimit
         lcFinalLMLimit = lcLMDataLimit
         llgDataLimit   = YES.         
   ELSE IF ENTRY(lcServCount,lcServList,",") EQ {&VL} THEN
      ASSIGN 
         lcFinalSLCode  = lcFinalTariff + "_MIN"
         lcFinalSLName  = "National Calls"
         liFinalDType   = 4
         lcFinalLimit   = lcVoiceLimit
         lcFinalBDLimit = "0"
         lcFinalFMLimit = lcFMVoiceLimit
         lcFinalLMLimit = lcFMVoiceLimit.
   ELSE IF ENTRY(lcServCount,lcServList,",") EQ {&BDL} THEN
      ASSIGN 
         lcFinalSLCode  = lcFinalTariff + "_QTY"
         lcFinalSLName  = "Bdest"
         liFinalDType   = 0
         lcFinalLimit   = lcBDestLimit
         lcFinalBDLimit = "0"
         lcFinalFMLimit = lcFMBDestLimit
         lcFinalLMLimit = lcLMBDestLimit.
   ELSE IF ENTRY(lcServCount,lcServList,",") EQ {&VBDES} THEN 
      ASSIGN 
         lcFinalSLCode  = lcFinalTariff + "_MIN"
         lcFinalSLName  = "National Calls"
         liFinalDType   = 4
         lcFinalLimit   = lcVoiceLimit
         lcFinalBDLimit = lcBDestLimit
         lcFinalFMLimit = lcFMVoiceLimit
         lcFinalLMLimit = lcFMVoiceLimit.
   
   IF lcFinalSLCode NE "" THEN DO:       
      RUN pServiceLimit IN h_config(lcFinalTariff,
                                    lcFinalSLCode,
                                    lcFinalSLName,
                                    lcFinalLimit,
                                    lcFinalBDLimit,
                                    lcFinalFMLimit,
                                    lcFinalLMLimit,
                                    liFinalDType,
                                    OUTPUT icSLSeq).
      IF RETURN-VALUE EQ "OK" THEN DO:
         IF lcPaymentType EQ "Postpaid" AND
            icSLSeq > 0          AND 
            lcMFBC NE ""         THEN DO: 
            RUN pServiceTargets IN h_config(icSLSeq,
                                            lcFinalTariff,
                                            ENTRY(2,lcFinalSLCode,"_")).
            IF RETURN-VALUE <> "OK" THEN DO:
               fError(RETURN-VALUE). 
               RETURN RETURN-VALUE.
            END.
            ELSE llgSLCreated = YES.
         END.
      END.
      ELSE DO:
         fError(RETURN-VALUE). 
         RETURN RETURN-VALUE.
      END.                              
   END.
   
   ASSIGN     
      lcFinalSLCode  = ""
      lcFinalSLName  = ""
      liFinalDType   = 0
      lcFinalLimit   = ""
      lcFinalBDLimit = ""
      lcFinalFMLimit = ""
      lcFinalLMLimit = ""
      icSLSeq        = 0.                                                                
END.

IF lcPaymentType EQ "Postpaid" AND
   lcMFBC NE "" THEN DO: 
   RUN pCreateFeeModel IN h_config(lcFinalTariff,
                                   lcFMName,    
                                   OUTPUT lcFeeModel).
    
   IF RETURN-VALUE EQ "OK" THEN DO:
      RUN pCreateFMItem IN h_config(lcFinalTariff,
                                    lcFeeModel,
                                    lcCommFee,
                                    lcFMFeeCalc,
                                    lcLMFeeCalc,
                                    lcMFBC,
                                    lcCliType,      /* Main parent tariff */
                                    lcTariffBundle  /* Tariff Bundle      */).
   
      IF RETURN-VALUE <> "OK" THEN DO:
         fError(RETURN-VALUE).
         RETURN RETURN-VALUE.
      END.    
   END.
   ELSE DO:
      fError(RETURN-VALUE).
      RETURN RETURN-VALUE.
   END.
END.

RUN pDayCampaign IN h_config(lcFinalTariff,
                             lcFeeModel,
                             lcDCName,
                             lcBBProfile,
                             lcDSS2Comp,
                             lcDSS2PL,
                             lcNVComp,
                             lcOnlyVoice,
                             lcTOC,
                             llgSLCreated,
                             lcMFBC,
                             lcPaymentType,
                             lcBundleUpsell).

IF RETURN-VALUE <> "OK" THEN DO:
   fError(RETURN-VALUE).
   RETURN RETURN-VALUE.
END.
ELSE DO:         
   RUN pDCServPackage IN h_config(lcFinalTariff,
                                  llgDataLimit,
                                  lcBonoSupport).

   IF RETURN-VALUE <> "OK" THEN DO:
      fError(RETURN-VALUE).
      RETURN RETURN-VALUE.
   END.
END.
   
ASSIGN
   lcInputFile = /*icIncDir +*/ "/store/riftp/tariff/incoming/tariff_trans.txt"
   lcLogFile   = icSpoolDir + "tariff_trans.log".
                     
INPUT STREAM TTransIn FROM VALUE(lcInputFile).
OUTPUT STREAM TTransLog TO VALUE(lcLogFile) APPEND.

REPEAT:
                             
   IMPORT STREAM TTransIn UNFORMATTED lcLine.
                                   
   /* Ignore the first line - (Header) */
   IF liFirstLine = 1 THEN DO:
      liFirstLine = liFirstLine + 1.
      NEXT.
   END.
                                                                        
   CREATE ttTrans.
   ASSIGN 
      ttTrans.tLangType  = TRIM(ENTRY(1,lcLine,";"))
      ttTrans.tLangint   = TRIM(ENTRY(2,lcLine,";"))
      ttTrans.tLangtext  = TRIM(ENTRY(3,lcLine,";"))
      ttTrans.tLangTrans = TRIM(ENTRY(4,lcLine,";")) NO-ERROR.
/*
   IF ERROR-STATUS:ERROR THEN DO:
      fError("Incorrect input data").
      RETURN "ERROR".
   END.
*/
END.

DO lcSubCount = 1 TO NUM-ENTRIES(lcSubList):    
   
   IF lcCliType    EQ TRIM(ENTRY(lcSubCount,lcSubList,",")) OR 
      lcBaseBundle EQ TRIM(ENTRY(lcSubCount,lcSubList,",")) THEN 
      llgCTServPac = YES.
   ELSE 
      llgCTServPac = NO.

   RUN pCreateCLIType IN h_config(ENTRY(lcSubCount,lcSubList,","),
                                  lcCLIName,        
                                  lcBaseBundle,
                                  lcLineType,
                                  lcFixLineType,
                                  lcCommFee,
                                  lcComparisonFee,
                                  lcServiceClass,
                                  lcWebStatus,
                                  lcSTCStatus,
                                  lcPaymentType,
                                  lcUsageType,
                                  icRatePlan,
                                  llgCTServPac,
                                  lcCliType,       /* Main parent tariff */ 
                                  lcTariffBundle,  /* Tariff Bundle      */
                                  OUTPUT ocCLIType).
    
    IF RETURN-VALUE <> "OK" THEN DO:
       fError(RETURN-VALUE).
       RETURN RETURN-VALUE. 
    END.
                                  
END.

RUN pCreTranslations.

IF RETURN-VALUE <> "OK" THEN DO:
   fError(RETURN-VALUE).
   RETURN RETURN-VALUE.
END.

OUTPUT STREAM TariffLog CLOSE.
OUTPUT STREAM TTransLog CLOSE.
INPUT  STREAM TariffIn  CLOSE.
INPUT  STREAM TTransIn  CLOSE.
       
RETURN "OK".

/* ***************************  Main End  *************************** */ 

PROCEDURE pDataCreValidation:

   ASSIGN 
      lcSubList  = ""
      lcServList = "".
   
   /* Validating final subscription type */
   IF lcPaymentType EQ "Postpaid" THEN 
      lcFinalTariff = IF lcTariffBundle NE "" THEN lcTariffBundle 
                      ELSE lcBaseBundle.
   ELSE lcFinalTariff = lcCliType.    
   
   /* Listing Subscription creation list */
   FIND FIRST CLIType WHERE 
              CLIType.CLIType = lcCliType NO-LOCK NO-ERROR. 
   
   IF NOT AVAILABLE CLIType THEN DO:
      IF lcTariffBundle NE ""         AND 
         lcTariffBundle NE lcCliType  THEN 
         lcSubList = lcCliType /* + "," + lcTariffBundle*/.
      
      IF lcBaseBundle NE ""        AND 
         lcBaseBundle NE lcCliType THEN 
         lcSubList = lcCliType /* + "," + lcBaseBundle*/.

      IF lcBaseBundle NE ""        AND
         lcBaseBundle EQ lcCliType THEN
         lcSubList = lcBaseBundle.
   END.  
   ELSE DO:
      IF lcTariffBundle NE ""         AND 
         lcTariffBundle NE lcCliType  THEN 
         lcSubList = lcTariffBundle.
      
      IF lcBaseBundle NE ""        AND 
         lcBaseBundle NE lcCliType THEN 
         lcSubList = lcBaseBundle.
   END.    
   
   IF lcSubList = "" THEN lcSubList = lcCliType.

   /* Listing ServiceLimits list */
   IF lcDataLimit NE "" THEN 
      lcServList = {&DL}. 
  
   IF lcVoiceLimit NE ""          AND
      lcVoiceLimit NE "Unlimited" AND
      lcBDestLimit NE ""          THEN
      lcServList = lcServList + "," + {&VBDES}.
   
   IF INDEX(lcServList,{&VBDES}) EQ 0 THEN DO:
      IF lcVoiceLimit NE ""          AND 
         lcVoiceLimit NE "Unlimited" THEN 
         lcServList = IF lcServList NE "" THEN 
                         lcServList + "," + {&VL}
                      ELSE {&VL}. 
      
      IF lcBDestLimit NE "" THEN 
         lcServList = IF lcServList NE "" THEN 
                         lcServList + "," + {&BDL}
                      ELSE {&BDL}.        
   END. 
      
END PROCEDURE.
    
PROCEDURE pValidateFileData:
    
FOR EACH ttTariffCre NO-LOCK:

   CASE ttTariffCre.FieldName:
      WHEN {&CT} THEN DO:
         IF ttTariffCre.FieldValue EQ "" THEN DO: 
            fError("No CliType data available").
            RETURN "ERROR".       
         END. 
         ELSE lcCliType = ttTariffCre.FieldValue.
      END.
      WHEN {&TB} THEN DO:
         IF ttTariffCre.FieldValue NE "" THEN 
            ASSIGN llgTrafficBundle = YES
                   lcTariffBundle   = ttTariffCre.FieldValue.          
      END.   
      WHEN {&BB} THEN DO:
         IF ttTariffCre.FieldValue NE "" AND 
            llgTrafficBundle THEN DO: 
            fError("Wrong BaseBundle data available").
            RETURN "ERROR".
         END.   
         ELSE lcBaseBundle = ttTariffCre.FieldValue.
      END.    
      WHEN {&TN} THEN 
         lcTariffName = ttTariffCre.FieldValue.
      WHEN {&LT} THEN DO:
         IF lcPaymentType EQ "Postpaid" THEN DO:
            IF (ttTariffCre.FieldValue EQ "") OR 
               LOOKUP(ttTariffCre.FieldValue,{&LINETYPE}) = 0 THEN DO:
               fError("No LineType data available").
               RETURN "ERROR".
            END.
            ELSE lcLineType = ttTariffCre.FieldValue.
         END.
      END.
      WHEN {&FLT} THEN DO:
         IF (ttTariffCre.FieldValue NE "") AND 
            LOOKUP(ttTariffCre.FieldValue,{&FLINETYPE}) = 0 THEN DO:
            fError("Wrong FixLineType data").
            RETURN "ERROR".    
         END.     
         ELSE lcFixLineType = ttTariffCre.FieldValue.
      END.     
      WHEN {&CF} THEN 
         lcCommFee = ttTariffCre.FieldValue. 
      WHEN {&CMF} THEN 
         lcComparisonFee = ttTariffCre.FieldValue.  
      WHEN {&SC} THEN DO:
         IF ttTariffCre.FieldValue NE "" THEN 
            ASSIGN llgSCPayTypeValue = YES
                   lcServiceClass    = ttTariffCre.FieldValue.
      END.  
      WHEN {&WS} THEN DO:
         IF (ttTariffCre.FieldValue EQ "") OR 
            LOOKUP(ttTariffCre.FieldValue,{&WEBSTATUS}) = 0 THEN DO:   
            fError("Wrong WebStatus data").
            RETURN "ERROR". 
         END.    
         ELSE lcWebStatus = ttTariffCre.FieldValue.
      END.
      WHEN {&STCS} THEN DO:
         IF (ttTariffCre.FieldValue EQ "") OR 
            LOOKUP(ttTariffCre.FieldValue,{&STCSTATUS}) = 0 THEN DO:   
            fError("Wrong STCStatus data").
            RETURN "ERROR". 
         END.    
         ELSE lcSTCStatus = ttTariffCre.FieldValue.
      END.
      WHEN {&PT} THEN DO:
         IF (ttTariffCre.FieldValue EQ "") OR 
            LOOKUP(ttTariffCre.FieldValue,{&PAYTYPE}) = 0 THEN DO:   
            fError("Wrong Payment type data").
            RETURN "ERROR". 
         END.
         ELSE DO:
            IF llgSCPayTypeValue AND 
               ttTariffCre.FieldValue = "Postpaid" THEN DO:
               fError("Postpaid subscription contains Serviceclass data").
               RETURN "ERROR".
            END.
            ELSE IF NOT llgSCPayTypeValue AND
                        ttTariffCre.FieldValue = "Prepaid" THEN DO:         
               fError("Prepaid subscription doesn't contain any Serviceclass data").
               RETURN "ERROR".
            END.             
            
            lcPaymentType = ttTariffCre.FieldValue.
            
            IF ttTariffCre.FieldValue = "Postpaid" THEN 
                llgPostPaid = YES. 
         END.
      END.
      WHEN {&UT} THEN DO:
         IF (ttTariffCre.FieldValue NE "") AND 
            LOOKUP(ttTariffCre.FieldValue,{&USAGETYPE}) EQ 0 THEN DO:
            fError("Wrong Usage type data").
            RETURN "ERROR".
         END.
         ELSE lcUsageType = ttTariffCre.FieldValue. 
      END.
      WHEN {&TOC} THEN DO:
         IF (ttTariffCre.FieldValue NE "") AND
            LOOKUP(ttTariffCre.FieldValue,{&CONTRACT}) GT 0 THEN DO:
            IF ttTariffCre.FieldValue EQ "ServicePackage" THEN
               llgServicePackage = YES.
            ELSE IF ttTariffCre.FieldValue EQ "PackageWithCounter" THEN
               llgPackWCounter = YES.
            ELSE IF ttTariffCre.FieldValue EQ "PackageWithOutCounter" THEN
               llgPackWOCounter = YES.

            lcTOC = ttTariffCre.FieldValue.
         END.
         ELSE IF (ttTariffCre.FieldValue NE "") AND
                 LOOKUP(ttTariffCre.FieldValue,{&CONTRACT}) EQ 0 THEN DO:
            fError("Wrong Type of Contract data").
            RETURN "ERROR".
         END.
      END.
      WHEN {&NVC} THEN DO:
         IF (ttTariffCre.FieldValue NE "") AND
            LOOKUP(ttTariffCre.FieldValue,{&LOGVALUE}) EQ 0 THEN DO:
            fError("Wrong Native VOIP compatible data").
            RETURN "ERROR".
         END.
         ELSE lcNVComp = ttTariffCre.FieldValue.
      END.
      WHEN {&DL} THEN DO:
          IF ttTariffCre.FieldValue NE "" THEN
             ASSIGN llgDataLimit = YES
                    lcDataLimit  = ttTariffCre.FieldValue.
      END.
      WHEN {&VL} THEN DO:
          IF ttTariffCre.FieldValue NE "" THEN
             ASSIGN llgVoiceLimit = YES
                    lcVoiceLimit  = ttTariffCre.FieldValue.
      END.
      /*convergence*/
      WHEN {&BUPS} THEN DO:
          IF ttTariffCre.FieldValue NE "" THEN
             lcBundleUpsell  = ttTariffCre.FieldValue.
      END.
   END CASE. 
        
   IF llgPostPaid THEN DO:
       
       CASE ttTariffCre.FieldName:
          WHEN {&MFBC} THEN 
             lcMFBC = ttTariffCre.FieldValue.             
          WHEN {&FMFC} THEN DO:
             IF (ttTariffCre.FieldValue NE "") AND 
                LOOKUP(ttTariffCre.FieldValue,{&FEECALC}) EQ 0 THEN DO:
                fError("Wrong First Month Fee calculation data").
                RETURN "ERROR". 
             END. 
             ELSE lcFMFeeCalc = ttTariffCre.FieldValue.
          END.
          WHEN {&LMFC} THEN DO:
             IF (ttTariffCre.FieldValue NE "") AND 
                LOOKUP(ttTariffCre.FieldValue,{&FEECALC}) EQ 0 THEN DO:
                fError("Wrong First Month Fee calculation data").
                RETURN "ERROR". 
             END.
             ELSE lcLMFeeCalc = ttTariffCre.FieldValue.
          END.    
          WHEN {&BDL} THEN 
              IF ttTariffCre.FieldValue NE "" THEN 
                 lcBDestLimit  = ttTariffCre.FieldValue.
          WHEN {&FMDL} THEN DO:
             IF (ttTariffCre.FieldValue NE "") AND 
                LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN DO:
                fError("Wrong First month data limit value").
                RETURN "ERROR".    
             END.  
             ELSE lcFMDataLimit = ttTariffCre.FieldValue.
          END.
          WHEN {&LMDL} THEN DO:
             IF (ttTariffCre.FieldValue NE "") AND 
                LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN DO:
                fError("Wrong last month data limit value").
                RETURN "ERROR".    
             END. 
             ELSE lcLMDataLimit = ttTariffCre.FieldValue.
          END.
          WHEN {&FMVL} THEN DO:
             IF (ttTariffCre.FieldValue NE "") AND 
                LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN DO:
                fError("Wrong First month voice limit value").
                RETURN "ERROR".    
             END.    
             ELSE lcFMVoiceLimit = ttTariffCre.FieldValue.
          END.
          WHEN {&LMVL} THEN DO:
             IF (ttTariffCre.FieldValue NE "") AND 
                LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN DO:
                fError("Wrong Last month voice limit value").
                RETURN "ERROR".    
             END.             
             ELSE lcLMVoiceLimit = ttTariffCre.FieldValue.
          END.
          WHEN {&FMBDL} THEN DO:
             IF (ttTariffCre.FieldValue NE "") AND 
                LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN DO:
                fError("Wrong First month BDestination limit value").
                RETURN "ERROR".    
             END.
             ELSE lcFMBDestLimit = ttTariffCre.FieldValue.
          END.
          WHEN {&LMBDL} THEN DO:
             IF (ttTariffCre.FieldValue NE "") AND 
                LOOKUP(ttTariffCre.FieldValue,{&LIMITVALUE}) EQ 0 THEN DO:
                fError("Wrong Last month BDestination limit value").
                RETURN "ERROR".    
             END.
             ELSE lcLMBDestLimit = ttTariffCre.FieldValue.
          END.
          WHEN {&BBP} THEN DO:
             IF (ttTariffCre.FieldValue NE "") AND 
                LOOKUP(ttTariffCre.FieldValue,{&BBPROFILE}) EQ 0 THEN DO:
                fError("Wrong BBProfile data").
                RETURN "ERROR".    
             END. 
             ELSE lcBBProfile = ttTariffCre.FieldValue.
          END.
          WHEN {&DSS2C} THEN DO:
             IF (ttTariffCre.FieldValue NE "") AND 
                LOOKUP(ttTariffCre.FieldValue,{&LOGVALUE}) EQ 0 THEN DO:
                fError("Wrong DSS2 Compatible data").
                RETURN "ERROR".    
             END.
             ELSE lcDSS2Comp = ttTariffCre.FieldValue.
          END.
          WHEN {&DSS2PL} THEN DO:
             IF (ttTariffCre.FieldValue NE "") AND 
                LOOKUP(ttTariffCre.FieldValue,{&LOGVALUE}) EQ 0 THEN DO:
                fError("Wrong DSS2 Primary line data").
                RETURN "ERROR".    
             END.
             ELSE lcDSS2PL = ttTariffCre.FieldValue.
          END.
          WHEN {&OV} THEN DO:
             IF (ttTariffCre.FieldValue NE "") AND 
                LOOKUP(ttTariffCre.FieldValue,{&LOGVALUE}) EQ 0 THEN DO:
                fError("Wrong Only VOICE data").
                RETURN "ERROR".    
             END.   
             ELSE lcOnlyVoice = ttTariffCre.FieldValue.
          END.  
          WHEN {&BS} THEN 
             IF ttTariffCre.FieldValue NE "" THEN 
                 lcBonoSupport = ttTariffCre.FieldValue.
       END CASE.  
       
   END. /* IF llgPostPaid THEN DO */      

END. /* FOR EACH ttSubTypeCr */      

IF llgServicePackage THEN DO:
   IF NOT llgDataLimit AND /*OR*/
      NOT llgVoiceLimit THEN DO:
      fError("Wrong ServicePackage-contract data with limits provided").
      RETURN "ERROR".
   END.
END.
ELSE IF llgPackWCounter THEN DO:
   IF NOT llgDataLimit AND
      NOT llgVoiceLimit THEN DO:
      fError("Wrong PackageWithCounter-contract data with limit provided").
      RETURN "ERROR".
   END.
END.
ELSE IF llgPackWOCounter THEN DO:
   IF llgDataLimit OR
      llgVoiceLimit THEN DO:
      fError("Wrong PackageWithoutCounter-contract data with limit provided").
      RETURN "ERROR".
   END.
END.

IF (iiPayType = 2 AND lcPaymentType = "Postpaid") OR
   (iiPayType = 1 AND lcPaymentType = "Prepaid") THEN DO:
   fError("Rateplan and Tariff with different payment types").
   RETURN "ERROR".
END.

ASSIGN llgTrafficBundle  = NO
       llgSCPayTypeValue = NO
       llgPostPaid       = NO
       llgServicePackage = NO
       llgPackWCounter   = NO
       llgPackWOCounter  = NO
       llgDataLimit      = NO
       llgVoiceLimit     = NO.

RETURN "OK".

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

PROCEDURE pNamingConvention:
DEFINE VARIABLE lcPrice   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPostFix AS CHARACTER NO-UNDO.

   ASSIGN 
      lcPostFix = IF lcCliType EQ "CONTS" THEN "S"
                  ELSE IF lcCliType EQ "CONTF" THEN "F"
                  ELSE IF lcCliType EQ "CONTSF" THEN "SF"
                  ELSE ""  
      lcPrice   = STRING(TRUNCATE(DECIMAL(lcComparisonFee),0)).   
   
   IF lcPaymentType EQ "Postpaid" THEN DO:
      ASSIGN 
         lcCLIName = lcTariffName 
         lcFMName  = lcTariffName  + " "       + {&MOF}
         lcSLGName = lcTariffName
         lcDCName  = lcTariffName.                    
   END.
   ELSE IF lcPaymentType EQ "Prepaid" THEN
      ASSIGN 
         lcCLIName = lcTariffName
         lcSLGName = lcTariffName
         lcDCName  = lcTariffName.

END PROCEDURE.    
