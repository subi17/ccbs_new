TRIGGER PROCEDURE FOR REPLICATION-WRITE OF Limit OLD BUFFER oldLimit.

{HPD/HPDConst.i}
{HPD/HPDTrigger.i}
{Syst/tmsconst.i}

&IF {&LIMIT_WRITE_TRIGGER_ACTIVE} &THEN

IF NEW(Limit) AND Limit.LimitType NE {&LIMIT_TYPE_Q25_DISCOUNT}
THEN RETURN.

IF NOT NEW(Limit) AND Limit.LimitType NE {&LIMIT_TYPE_Q25_DISCOUNT} AND oldLimit.LimitType NE {&LIMIT_TYPE_Q25_DISCOUNT}
THEN RETURN.

{triggers/replog_tenantname.i}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "Limit"
   Common.RepLog.EventType = (IF NEW(Limit)
                               THEN "CREATE"
                               ELSE IF Limit.LimitType NE {&LIMIT_TYPE_Q25_DISCOUNT} AND oldLimit.LimitType = {&LIMIT_TYPE_Q25_DISCOUNT}
                               THEN "DELETE"
                               ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   Common.RepLog.TenantName = fRepLogTenantName(BUFFER Limit:HANDLE)
   .

IF Common.RepLog.EventType = "DELETE" 
THEN Common.RepLog.KeyValue = {HPD/keyvalue.i Limit . {&HPDKeyDelimiter} CustNum MsSeq LimitType FromDate}.
ELSE Common.RepLog.RowID    = STRING(ROWID(Limit)).

IF NOT NEW(Limit)
THEN DO:   
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.
      
   BUFFER-COMPARE Limit USING
      CustNum MsSeq LimitType ToDate
   TO oldLimit SAVE RESULT IN llSameValues.
   
   IF NOT llSameValues
   THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName  = "Limit"
         Common.RepLog.EventType  = "DELETE"
         Common.RepLog.EventTime  = NOW
         Common.RepLog.TenantName = fRepLogTenantName(BUFFER oldLimit:HANDLE)
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldLimit . {&HPDKeyDelimiter} CustNum MsSeq LimitType FromDate}
         .
   END.
END.

&ENDIF