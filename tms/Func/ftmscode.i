/* ftmscode.i      28.10.02/aam
*/


/* get the description for status codes etc. */
FUNCTION fTMSCodeName RETURNS CHARACTER
   (iTableName AS CHAR,
    iFieldName AS CHAR,
    iCodeValue AS CHAR).

    FIND FIRST TMSCodes NO-LOCK WHERE
               TMSCodes.TableName = iTableName AND
               TMSCodes.FieldName = iFieldName AND
               TMSCodes.CodeValue = iCodeValue NO-ERROR.

    IF AVAILABLE TMSCodes THEN RETURN TMSCodes.CodeName.
    ELSE RETURN "".

END FUNCTION.

/* get the configuration value for status codes etc. */
FUNCTION fTMSCodeConfigValue RETURNS CHARACTER
   (iTableName AS CHAR,
    iFieldName AS CHAR,
    iCodeValue AS CHAR).

    FIND FIRST TMSCodes NO-LOCK WHERE
               TMSCodes.TableName = iTableName AND
               TMSCodes.FieldName = iFieldName AND
               TMSCodes.CodeValue = iCodeValue NO-ERROR.

    IF AVAILABLE TMSCodes THEN RETURN TMSCodes.ConfigValue.
    ELSE RETURN "".

END FUNCTION.

