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
