FIND TableToken NO-LOCK WHERE
   TableToken.TableName = "DumpHPD" AND
   TableToken.TokenCode = "VENDOR"
NO-ERROR.

IF NOT AVAILABLE TableToken
THEN DO:
   CREATE TableToken.
   ASSIGN
      TableToken.TableName = "DumpHPD"
      TableToken.TokenCode = "VENDOR".
END.

FIND MenuText EXCLUSIVE-LOCK WHERE
   MenuText.MenuNum  = 9847
NO-ERROR.

IF NOT AVAILABLE MenuText
THEN DO:
   CREATE MenuText.
END.

ASSIGN
   MenuText.MenuNum = 9847
   MenuText.MenuText = "HPD     CONFIG  ".

FUNCTION fCreateTMSCodes RETURNS LOGICAL
   ( icCodeValue AS CHARACTER ):

   FIND FIRST TMSCodes NO-LOCK WHERE
     TMSCodes.TableName = "DumpHPD" AND
     TMSCodes.FieldName = "UnitType" AND
     TMSCodes.CodeGroup = "DumpHPD" AND
     TMSCodes.CodeName = icCodeValue AND
     TMSCodes.InUse =  1
   NO-ERROR.

   IF NOT AVAILABLE TMSCodes
   THEN CREATE TMSCodes.

   ASSIGN
     TMSCodes.TableName = "DumpHPD"
     TMSCodes.FieldName = "UnitType"
     TMSCodes.CodeGroup = "DumpHPD"
     TMSCodes.CodeName = icCodeValue
     TMSCodes.InUse =  1
     TMSCodes.CodeValue = icCodeValue.

END FUNCTION.

fCreateTMSCodes("hours").
fCreateTMSCodes("days").
fCreateTMSCodes("weeks").
fCreateTMSCodes("months").
fCreateTMSCodes("years").
