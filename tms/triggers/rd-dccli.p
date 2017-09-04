TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DCCLI.

{HPD/HPDConst.i}

&IF {&DCCLI_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW DCCLI
THEN RETURN.

{triggers/replog_tenantname.i}

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.TableName  = "DCCLI"
   Mobile.RepLog.EventType  = "DELETE"
   Mobile.RepLog.EventTime  = NOW
   Mobile.RepLog.TenantName = fRepLogTenantName(BUFFER DCCLI:HANDLE)
   Mobile.RepLog.KeyValue  = {HPD/keyvalue.i DCCLI . {&HPDKeyDelimiter} PerContractID} 
   .

&ENDIF
