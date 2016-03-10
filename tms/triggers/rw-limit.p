TRIGGER PROCEDURE FOR REPLICATION-WRITE OF Limit OLD BUFFER oldLimit.

{HPD/HPDConst.i}
{Syst/tmsconst.i}

&IF {&LIMIT_WRITE_TRIGGER_ACTIVE} &THEN

IF NEW(Limit) AND Limit.LimitType NE {&LIMIT_TYPE_Q25_DISCOUNT}
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "Limit"
   Common.RepLog.EventType = (IF NEW(Limit)
                               THEN "CREATE"
                               ELSE IF Limit.LimitType NE {&LIMIT_TYPE_Q25_DISCOUNT}
                               THEN "DELETE"
                               ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF Common.RepLog.EventType = "DELETE" 
THEN Common.RepLog.KeyValue = {HPD/keyvalue.i Limit . {&HPDKeyDelimiter} CustNum MsSeq LimitType ValidTo}.
ELSE Common.RepLog.RowID    = STRING(ROWID(Limit)).

IF NOT NEW(Limit)
THEN DO:   
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.
      
   BUFFER-COMPARE Limit USING
      CustNum MsSeq LimitType ValidTo
   TO oldLimit SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName = "Limit"
         Common.RepLog.EventType = "DELETE"
         Common.RepLog.EventTime = NOW
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldLimit . {&HPDKeyDelimiter} CustNum MsSeq LimitType ValidTo}
         .
   END.
END.

&ENDIF