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
