TRIGGER PROCEDURE FOR REPLICATION-WRITE OF ServPac OLD BUFFER oldServPac.

{HPD/HPDConst.i}

&IF {&SERVPAC_WRITE_TRIGGER_ACTIVE} &THEN

IF NEW(ServPac) AND ServPac.Brand NE "1"
THEN RETURN.

IF NOT NEW(ServPac) AND oldServPac.Brand NE "1" AND ServPac.Brand NE "1"
THEN RETURN.

/* If this is an old cdr which is changed to error status
   will will send it as delete type */
CREATE mobile.RepLog.
ASSIGN
   mobile.RepLog.TableName = "ServPac"
   mobile.RepLog.EventType = (IF NEW(ServPac)
                            THEN "CREATE"
                            ELSE IF ServPac.Brand NE "1" AND oldServPac.Brand = "1"
                            THEN "DELETE"
                            ELSE "MODIFY") 
   mobile.RepLog.EventTime = NOW
   .

IF mobile.RepLog.EventType = "DELETE"
THEN mobile.RepLog.KeyValue = {HPD/keyvalue.i ServPac . {&HPDKeyDelimiter} ServPac}.
ELSE mobile.RepLog.RowID    = STRING(ROWID(ServPac)).
   
IF NOT NEW(ServPac)
THEN DO:   
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE ServPac USING
      ServPac
   TO oldServPac SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
      CREATE mobile.RepLog.
      ASSIGN
         mobile.RepLog.TableName = "ServPac"
         mobile.RepLog.EventType = "DELETE"
         mobile.RepLog.EventTime = NOW
         mobile.RepLog.KeyValue  = {HPD/keyvalue.i oldServPac . {&HPDKeyDelimiter} ServPac}
         .
   END.
END.

&ENDIF