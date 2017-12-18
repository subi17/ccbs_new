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
    FIELD AllowedBundles            AS CHARACTER
    FIELD MobileBaseBundleDataLimit AS DECIMAL
    FIELD BundlesForActivateOnSTC   AS CHARACTER
    FIELD ServicesForReCreateOnSTC  AS CHARACTER
    FIELD CopyServicesFromCliType   AS CHARACTER
    FIELD TariffType                AS INTEGER
    INDEX IdxCliType IS UNIQUE PRIMARY CliType. 
