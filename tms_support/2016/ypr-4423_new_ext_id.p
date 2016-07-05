FIND FIRST TmsCodes WHERE
           TMSCodes.TableName = "AppIdPrefix" AND
           TMSCodes.FieldName = "ApplicationId" NO-ERROR.
IF AVAIL TmsCodes THEN DO:
    DISP tmsCodes.configvalue FORMAT "x(75)".
    IF LOOKUP("901",tmsCodes.configvalue) EQ 0 THEN
       tmsCodes.configvalue = tmsCodes.configvalue + ",901".
END.

