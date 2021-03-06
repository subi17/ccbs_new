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

{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/ftransdir.i}
{utilities/newtariff/tariffcons.i}
{utilities/newtariff/tariffconfig.i}

DEFINE INPUT  PARAMETER icBaseFile AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icFile     AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icSpoolDir AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcLogFile           AS CHARACTER NO-UNDO.
DEFINE VARIABLE h_config            AS HANDLE    NO-UNDO.

/* General Subscription Type Attributes */
DEFINE VARIABLE lcCliType                                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCliName                                AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE lcPROFee                                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcMainLineTariffs                        AS CHARACTER NO-UNDO.

/* Mobile Base Bundle Attributes */
DEFINE VARIABLE lcMobile_BaseBundle                      AS CHARACTER NO-UNDO.

/* FixedLine Base Bundle Attributes */
DEFINE VARIABLE lcFixedLine_BaseBundle                   AS CHARACTER NO-UNDO.

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

   ASSIGN lcLogFile   = icSpoolDir + icBaseFile + ".log".

   RUN utilities/newtariff/configcreations.p PERSISTENT SET h_config. 

   FUNCTION fTMSCValue RETURNS CHARACTER (iTableName AS CHAR, iFieldName AS CHAR,iCodeName AS CHAR) IN h_config.

   RUN pReadTariff.  

   RUN pValidateData.

   RUN pProcessTT.

   RUN pSaveTariff.

   RETURN "OK".

   CATCH e AS Progress.Lang.Error:
      OUTPUT STREAM TariffLog TO VALUE(lcLogFile) APPEND.
      fError(e:GetMessage(1)).
      OUTPUT STREAM TariffLog CLOSE.
      UNDO, THROW e.
   END CATCH.
   FINALLY:
      IF VALID-HANDLE(h_config)
      THEN DELETE OBJECT h_config.
   END FINALLY.
END.
/* ***************************  Main End  *************************** */ 
PROCEDURE pSaveTariff:
  DEFINE VARIABLE ldePROFee AS DECIMAL NO-UNDO. 

  DO ON ERROR UNDO, THROW:

      FOR EACH ttCliType
          ON ERROR UNDO, THROW:
         
         RUN pCliType IN h_config(BUFFER ttCliType).

         IF lcPROFee <> "" AND
            lcFixedLine_BaseBundle <> "" 
         THEN DO:
            ldePROFee = DECIMAL(lcProFee).

            RUN pPriceList_PRO IN h_config("PRO_" + ttClitype.CliType).
            
            FIND DayCampaign NO-LOCK WHERE
               DayCampaign.Brand   = Syst.Var:gcBrand AND
               DayCampaign.DCEvent = lcFixedLine_BaseBundle
            NO-ERROR.
            
            IF NOT AVAILABLE DayCampaign
            THEN UNDO, THROW NEW Progress.Lang.AppError
                  (SUBSTITUTE("Cannot find DayCampaign for a fixed line " +
                              "base bundle '&1'", lcFixedLine_BaseBundle), 1).
            
            RUN pFMItem_PRO IN h_config(ttClitype.CliType + "PRO",  /* BillItem  */
                                        DayCampaign.FeeModel,       /* FeeModel  */ 
                                        "PRO_" + ttClitype.CliType, /* PriceList */
                                        ldePROFee                   /* Amount    */ 
                                       ).
         END.

         IF lcPaymentType = "Postpaid" AND lcMobile_BaseBundle > ""
         THEN RUN pTMRItemValue IN h_config(lcCliType, lcMobile_BaseBundle, lcAllowedBundles).   
      
         /* Creating Matrix and MXitem for Extra Line */ 
         IF lcMainLineTariffs <> "" AND lcMobile_BaseBundle > "" 
         THEN RUN pMatrix IN h_config(ttClitype.CliType, lcMainLineTariffs, "EXTRALINE").

      END. /* FOR EACH ttCliType */
      CATCH e AS Progress.Lang.Error:
          UNDO, THROW e.
      END CATCH.
  END.

  RETURN "".

END PROCEDURE.


PROCEDURE pReadTariff:   

   DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.

   DO ON ERROR UNDO, THROW:   

      INPUT STREAM TariffIn FROM VALUE(icFile).

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


PROCEDURE pProcessTT:   
   
   IF lcReferenceRatePlan > "" THEN    
       RUN pRatePlan IN h_config(lcRatePlan, lcCliName, lcReferenceRatePlan, lcRatePlanAction).   

   CREATE ttCliType.
   ASSIGN
      ttCliType.CliType                   = lcCliType
      ttCliType.CliName                   = lcCliName
      ttCliType.RatePlan                  = lcRatePlan
      ttCliType.BaseBundle                = lcMobile_BaseBundle 
      ttCliType.FixedLineBaseBundle       = lcFixedLine_BaseBundle
      ttCliType.WebStatusCode             = INTEGER(fTMSCValue("CLIType","WebStatusCode",lcWebStatus)) 
      ttCliType.StatusCode                = INTEGER(fTMSCValue("CLIType","StatusCode",lcSTCStatus)) 
      ttCliType.PayType                   = INTEGER(fTMSCValue("CLIType","PayType",lcPaymentType)) 
      ttCliType.UsageType                 = INTEGER(fTMSCValue("CLIType","UsageType",lcUsageType))  
      ttCliType.LineType                  = INTEGER(fTMSCValue("CLIType","LineType",lcLineType)) 
      ttCliType.FixedLineType             = INTEGER(fTMSCValue("CLIType","FixedLineType",lcFixLineType))  
      ttCliType.FixedLineDownload         = lcFixedLineDownload
      ttCliType.FixedLineUpload           = lcFixedLineUpload
      ttCliType.Serviceclass              = lcServiceClass
      ttCliType.CommercialFee             = DECIMAL(lcCommFee)
      ttCliType.CompareFee                = DECIMAL(lcComparisonFee)
      ttCliType.AllowedBundles            = lcAllowedBundles
      ttCliType.BundlesForActivateOnSTC   = lcBundlesForActivateOnSTC
      ttCliType.ServicesForReCreateOnSTC  = lcServicesForReCreateOnSTC
      ttCliType.CopyServicesFromCliType   = lcCopyServicesFromCliType
      ttClitype.TariffType                = INTEGER(fTMSCValue("CLIType","TariffType",lcTariffType)). 
    
    RETURN "".
    
END PROCEDURE.

PROCEDURE pValidateData:

   DEFINE VARIABLE llgPostPaid         AS LOGICAL   NO-UNDO.

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
            WHEN {&TT} THEN 
            DO:
               IF (ttTariffCre.FieldValue EQ "") OR LOOKUP(ttTariffCre.FieldValue,{&TARIFFTYPE}) EQ 0 THEN
                  UNDO, THROW NEW Progress.Lang.AppError("Wrong TariffType data", 1).                  
               ELSE 
                  ASSIGN lcTariffType = ttTariffCre.FieldValue.                   
            END.  
            WHEN {&PF} THEN DO:
              ASSIGN lcProFee = ttTariffCre.FieldValue.
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
               ELSE IF NOT CAN-FIND(FIRST CliType WHERE CliType.Brand = Syst.Var:gcBrand AND CliType.CliType = ttTariffCre.FieldValue NO-LOCK) THEN 
                  UNDO, THROW NEW Progress.Lang.AppError("Invalid 'copy services from clitype'", 1).   
               ELSE 
                  lcCopyServicesFromCliType = ttTariffCre.FieldValue.
            END.                       
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
               IF ttTariffCre.FieldValue EQ "" AND NOT CAN-FIND(FIRST RatePlan WHERE RatePlan.Brand = Syst.Var:gcBrand AND RatePlan.RatePlan = ttTariffCre.FieldValue NO-LOCK) THEN 
                  UNDO, THROW NEW Progress.Lang.AppError("Reference Rateplan is invalid", 1).
               ELSE 
                  ASSIGN lcReferenceRatePlan = ttTariffCre.FieldValue.
            END.

            /* Mobile */ 
            WHEN {&M_BB}
            THEN ASSIGN lcMobile_BaseBundle = ttTariffCre.FieldValue.

            /* FixedLine */
            WHEN {&FL_BB}
            THEN  ASSIGN lcFixedLine_BaseBundle = ttTariffCre.FieldValue.

            WHEN {&MLT} THEN 
            DO:
              ASSIGN lcMainLineTariffs = ttTariffCre.FieldValue.
            END. 
             
         END CASE.             
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
      ELSE IF lcRatePlanAction = "New" AND CAN-FIND(FIRST RatePlan WHERE RatePlan.Brand = Syst.Var:gcBrand AND RatePlan.RatePlan = lcRatePlan NO-LOCK) THEN 
         UNDO, THROW NEW Progress.Lang.AppError("Rateplan already exists, which is contradicting with Rateplan action", 1).         
      ELSE IF lcRatePlanAction = "UseExisting" AND NOT CAN-FIND(FIRST RatePlan WHERE RatePlan.Brand = Syst.Var:gcBrand AND RatePlan.RatePlan = lcReferenceRatePlan NO-LOCK) THEN 
         UNDO, THROW NEW Progress.Lang.AppError("Reference Rateplan doesn't exists, which is contradicting with Rateplan action", 1).

      ASSIGN 
         llgPostPaid       = NO.
   END.

   RETURN "".

END PROCEDURE.

