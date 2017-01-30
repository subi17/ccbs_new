/*------------------------------------------------------------------------
  MODULE .......: tariffconfig.i
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: Fri Feb 06 13:24:09 EET 2015
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/ 
/* ***************************  Definitions  ************************** */
 
DEFINE TEMP-TABLE ttTrans NO-UNDO 
    FIELD tTextType  AS INTEGER
    FIELD tLangType  AS CHARACTER 
    FIELD tLangint   AS CHARACTER 
    FIELD tLangtext  AS CHARACTER
    FIELD tLangTrans AS CHARACTER.

DEFINE TEMP-TABLE ttTariff NO-UNDO 
   FIELD PriceList AS CHARACTER 
   FIELD CCN       AS CHARACTER 
   FIELD BDest     AS CHARACTER 
   FIELD BillItem  AS CHARACTER 
   FIELD PriceUnit AS CHARACTER
   FIELD Price     AS CHARACTER 
   FIELD SetupFee  AS CHARACTER.

DEFINE TEMP-TABLE ttTMRItemValue NO-UNDO
   FIELD TMRuleSeq AS INTEGER
   FIELD CliType   AS CHARACTER
   FIELD BDest     AS CHARACTER
   INDEX IdxTMRuleSeq IS UNIQUE PRIMARY TMRuleSeq CliType BDest.
   
DEFINE TEMP-TABLE ttCliType NO-UNDO
    FIELD CliType                   AS CHARACTER
    FIELD CliName                   AS CHARACTER
    FIELD RatePlan                  As CHARACTER
    FIELD BaseBundle                AS CHARACTER
    FIELD FixedLineBaseBundle       AS CHARACTER
    FIELD WebStatusCode             AS INTEGER
    FIELD StatusCode                AS INTEGER
    FIELD PayType                   AS INTEGER 
    FIELD UsageType                 AS INTEGER
    FIELD LineType                  AS INTEGER
    FIELD FixedLineType             AS INTEGER
    FIELD FixedLineDownload         AS CHARACTER
    FIELD FixedLineUpload           AS CHARACTER
    FIELD Serviceclass              AS CHARACTER
    FIELD CommercialFee             AS DECIMAL
    FIELD CompareFee                AS DECIMAL
    FIELD BundleType                AS LOGICAL
    FIELD ParentTariff              AS CHARACTER
    FIELD TariffBundle              AS CHARACTER    
    FIELD AllowedBundles            AS CHARACTER
    FIELD MobileBaseBundleDataLimit AS DECIMAL
    FIELD BundlesForActivateOnSTC   AS CHARACTER
    FIELD ServicesForReCreateOnSTC  AS CHARACTER
    FIELD CopyServicesFromCliType   AS CHARACTER 
    INDEX IdxCliType IS UNIQUE PRIMARY CliType.  

DEFINE TEMP-TABLE ttDayCampaign NO-UNDO
    FIELD CliType        AS CHARACTER
    FIELD DCEvent        AS CHARACTER
    FIELD DCName         AS CHARACTER
    FIELD DCType         AS CHARACTER
    FIELD PayType        AS INTEGER
    FIELD BillCode       AS CHARACTER
    FIELD UpSell         AS CHARACTER
    FIELD BonoSupport    AS LOGICAL
    FIELD SLCreated      AS LOGICAL
    FIELD DataLimit      AS DECIMAL
    INDEX IdxDCEvent IS UNIQUE PRIMARY DCEvent. 

DEFINE TEMP-TABLE ttFMItem NO-UNDO
    FIELD FeeModel     AS CHARACTER
    FIELD BillCode     AS CHARACTER        
    FIELD PriceList    AS CHARACTER    
    FIELD Amount       AS DECIMAL    
    FIELD FirstMonthBR AS INTEGER
    FIELD BrokenRental AS INTEGER
    INDEX IdxFMItem IS UNIQUE PRIMARY FeeModel.

DEFINE TEMP-TABLE ttServiceLimitGroup NO-UNDO
    FIELD GroupCode AS CHARACTER
    FIELD GroupName AS CHARACTER
    INDEX IdxGroupCode IS UNIQUE PRIMARY GroupCode.

DEFINE TEMP-TABLE ttServiceLimit NO-UNDO
    FIELD GroupCode      AS CHARACTER
    FIELD SLCode         AS CHARACTER
    FIELD SLName         AS CHARACTER
    FIELD DialType       AS INTEGER
    FIELD InclAmt        AS DECIMAL
    FIELD FirstMonthCalc AS INTEGER
    FIELD LastMonthCalc  AS INTEGER
    INDEX IdxGroupCode IS UNIQUE PRIMARY GroupCode SLCode.

DEFINE TEMP-TABLE ttServiceLimitTarget NO-UNDO
    FIELD GroupCode      AS CHARACTER
    FIELD SLCode         AS CHARACTER
    FIELD ServiceLMember AS CHARACTER
    FIELD InsideRate     AS CHARACTER
    FIELD OutSideRate    AS CHARACTER
    INDEX IdxGroupCodeMember IS UNIQUE PRIMARY GroupCode SLCode ServiceLMember.

DEFINE TEMP-TABLE ttProgLimit NO-UNDO
    FIELD GroupCode AS CHARACTER
    FIELD SLCode    AS CHARACTER
    FIELD BDest     AS CHARACTER
    FIELD LimitFrom AS DECIMAL
    FIELD LimitTo   AS DECIMAL
    INDEX IdxGroupCodeBDest IS UNIQUE PRIMARY GroupCode SLCode BDest.

DEFINE TEMP-TABLE ttBDest NO-UNDO
    FIELD GroupCode AS CHARACTER
    FIELD SLCode    AS CHARACTER
    FIELD BDest     AS CHARACTER
    FIELD BDName    AS CHARACTER
    FIELD CCN       AS INTEGER
    INDEX IdxGroupCodeBDest IS UNIQUE PRIMARY GroupCode SLCode BDest.

            



  