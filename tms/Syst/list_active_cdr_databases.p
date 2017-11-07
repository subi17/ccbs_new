DEFINE VARIABLE lcTableNames     AS CHARACTER
     INITIAL "MobCDR,McdrDtl2,PrepCDR,RoamCDR" NO-UNDO.
DEFINE VARIABLE lii              AS INTEGER    NO-UNDO.
DEFINE VARIABLE lcPFText         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lcConnectionType AS CHARACTER  NO-UNDO.
DEFINE VARIABLE liDBState        AS INTEGER    INITIAL 0 NO-UNDO.

lcConnectionType = SESSION:PARAMETER.

IF NUM-ENTRIES(lcConnectionType) > 1
THEN DO:
   liDBState = INTEGER(ENTRY(2,lcConnectionType)) NO-ERROR.
   IF ERROR-STATUS:ERROR
   THEN liDBState = 0.
   lcConnectionType = ENTRY(1,lcConnectionType).
END.

DO lii = 1 TO NUM-ENTRIES(lcTableNames):
   FOR FIRST DBConfig NO-LOCK WHERE
      DBConfig.Brand     = "1"                     AND
      DBConfig.TableName = ENTRY(lii,lcTableNames) AND
      DBConfig.DBState   = liDBState               AND
      DBConfig.ToDate   >= DBConfig.FromDate:
      
      CASE lcConnectionType:
         WHEN "local"
         THEN lcPFText = lcPFText + "'" + DBConfig.LogicalName + "' : [" + 
                         "'-db', '" + DBConfig.DirectConnect + "/" +
                         DBConfig.DBConnName + "', '-ld', '" +
                         DBConfig.LogicalName + "'], ".
         WHEN "tcp"
         THEN lcPFText = lcPFText + "'" + DBConfig.LogicalName + "' : [" + 
                         "'-db', '" + DBConfig.DBConnName + "', '-H', '" +
                         DBConfig.Host + "', '-S', '" + DBConfig.Service +
                         "', '-ld', '" + DBConfig.LogicalName + "'], ".
      END CASE.
   END.
END.

PUT UNFORMATTED "~{" + SUBSTRING(lcPFText,1,LENGTH(lcPFText) - 2) + "~}".
