TRIGGER PROCEDURE FOR REPLICATION-DELETE OF Limit.

{HPD/HPDConst.i}
{Syst/tmsconst.i}

&IF {&LIMIT_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW Limit OR Limit.LimitType NE {&LIMIT_TYPE_Q25_DISCOUNT}
THEN RETURN.

{triggers/replog_tenantname.i}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName  = "Limit"
   Common.RepLog.EventType  = "DELETE"
   Common.RepLog.EventTime  = NOW
   Common.RepLog.TenantName = fRepLogTenantName(BUFFER Limit:HANDLE)
   Common.RepLog.KeyValue  = {HPD/keyvalue.i Limit . {&HPDKeyDelimiter} CustNum MsSeq LimitType FromDate}
   .

&ENDIF