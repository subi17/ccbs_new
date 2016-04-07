/* ----------------------------------------------------------------------
  MODULE .......: fexternalapi.i
  TASK .. ......: External API functions
  APPLICATION ..: TMS
  AUTHOR .......: Janne Tourunen
  CREATED ......: 06.06.2012
  VERSION ......:
  CONTENTS .....: TMSCode function for EXT API
                   - fchkTMSCodeValues
-----------------------------------------------------------------------*/

DEF VAR lcYoigoAppId AS CHAR NO-UNDO INITIAL "501,502".
DEF VAR lcIvrAppId   AS CHAR NO-UNDO INITIAL "601,602".
DEF VAR lcUserId     AS CHAR NO-UNDO. 
/*For detailed viewing*/
DEF VAR lcMiYoigoWebId AS CHAR NO-UNDO INITIAL "501".
DEF VAR lcMiYoigoAppId AS CHAR NO-UNDO INITIAL "502".
DEF VAR lcSpotifyId AS CHAR NO-UNDO INITIAL "503".
DEF VAR lcRoamingTUSId AS CHAR NO-UNDO INITIAL "504".
DEF VAR lcBonoIPLUpgId AS CHAR NO-UNDO INITIAL "505".
DEF VAR lcEBMId AS CHAR NO-UNDO INITIAL "506".
DEF VAR lcCollectionId AS CHAR NO-UNDO INITIAL "507".
DEF VAR lcHRLPId AS CHAR NO-UNDO INITIAL "509".
DEF VAR lcCCGWId AS CHAR NO-UNDO INITIAL "650".
DEF VAR lcCTCId AS CHAR NO-UNDO INITIAL "680".
DEF VAR lcIFSId AS CHAR NO-UNDO INITIAL "701".


/* Check certain TMSCode existence */
FUNCTION fchkTMSCodeValues RETURNS LOGICAL
   (iCodeValue AS CHAR,
    iConfValue AS CHAR).

   FIND FIRST TMSCodes NO-LOCK WHERE
              TMSCodes.TableName = "AppIdPrefix" AND
              TMSCodes.FieldName = "ApplicationId" AND
              TMSCodes.CodeValue = iCodeValue NO-ERROR.
   
   
   IF AVAILABLE TMSCodes AND 
   LOOKUP(iConfValue,TMSCodes.ConfigValue) > 0 THEN RETURN TRUE.
   RETURN FALSE. 

END FUNCTION.

/* Check certain TMSCode existence */
FUNCTION fchkOrderCancelRule RETURNS LOGICAL
   (iCodeValue AS CHAR,
    iConfValue AS CHAR).

   FIND FIRST TMSCodes NO-LOCK WHERE
              TMSCodes.TableName = "OrderCancel" AND
              TMSCodes.FieldName = "Channel" AND
              TMSCodes.CodeValue = iCodeValue NO-ERROR.
   IF AVAILABLE TMSCodes AND
   LOOKUP(iConfValue,TMSCodes.ConfigValue) > 0 THEN RETURN TRUE.
   RETURN FALSE.

END FUNCTION.

FUNCTION fgetAppUserId RETURNS CHARACTER(INPUT icAppId     AS CHARACTER,
                                         INPUT icAppUserId AS CHARACTER):

    IF LOOKUP(icAppId,lcYoigoAppId) > 0 THEN 
        lcUserId = "Yoigo" + "_" + icAppUserId.
    ELSE IF LOOKUP(icAppId,lcIvrAppId) > 0 THEN
        lcUserId = "IVR" + "_" + icAppUserId.
    ELSE lcUserId = "SelfService" + "_" + icAppUserId.            

    RETURN lcUserId.

END FUNCTION.

/* YTS-6900 From Elena: 505 should be 'Landing Page_MSISDN' 507 should be 'Collection Landing Page' */

FUNCTION fgetAppDetailedUserId RETURNS CHARACTER(INPUT icAppId     AS CHARACTER,
                                         INPUT icAppUserId AS CHARACTER):

    IF LOOKUP(icAppId,lcMiYoigoAppId) > 0 THEN
        lcUserId = "Mi Yoigo APP" + "_" + icAppUserId.
    ELSE IF LOOKUP(icAppId,lcMiYoigoWebId) > 0 THEN
        lcUserId = "Mi Yoigo WEB" + "_" + icAppUserId.
    ELSE IF LOOKUP(icAppId,lcCollectionId) > 0 THEN
        lcUserId = "Collection Landing Page" + "_" + icAppUserId.
    ELSE IF LOOKUP(icAppId,lcIvrAppId) > 0 THEN
        lcUserId = "IVR" + "_" + icAppUserId.
    ELSE IF LOOKUP(icAppId,lcCCGWId) > 0 THEN
        lcUserId = "CCGW" + "_" + icAppUserId.
    ELSE IF LOOKUP(icAppId,lcSpotifyId) > 0 THEN
        lcUserId = "Spotify registration" + "_" + icAppUserId.
    ELSE IF LOOKUP(icAppId,lcRoamingTUSId) > 0 THEN
        lcUserId = "Roaming tariff upsell" + "_" + icAppUserId.
    ELSE IF LOOKUP(icAppId,lcEBMId) > 0 THEN
        lcUserId = "EBM" + "_" + icAppUserId.
    ELSE IF LOOKUP(icAppId,lcCTCId) > 0 THEN
        lcUserId = "CTC" + "_" + icAppUserId.
    ELSE IF LOOKUP(icAppId,lcBonoIPLUpgId) > 0 THEN
        lcUserId = "Landing Page" + "_" + icAppUserId.
    ELSE IF LOOKUP(icAppId,lcIFSId) > 0 THEN
        lcUserId = "IFS" + "_" + icAppUserId.
    ELSE IF LOOKUP(icAppId,lcHRLPId) > 0 THEN
        lcUserId = icAppId + "_" + icAppUserId.
    ELSE lcUserId = "SelfService" + "_" + icAppUserId.

    RETURN lcUserId.

END FUNCTION.




