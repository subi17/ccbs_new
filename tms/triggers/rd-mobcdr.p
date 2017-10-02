TRIGGER PROCEDURE FOR REPLICATION-DELETE OF MobCDR.

{HPD/HPDConst.i}
{HPD/HPDTrigger.i}

&IF {&MOBCDR_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW MobCDR
THEN RETURN.

/* If this is an error cdr we won't send the information */
IF MobCDR.ErrorCode > 0
THEN RETURN.

{triggers/replog_tenantname.i}

CREATE mcdr.RepLog.
ASSIGN
   mcdr.RepLog.TableName  = "MobCDR"
   mcdr.RepLog.EventType  = "DELETE"
   mcdr.RepLog.EventTime  = NOW
   mcdr.RepLog.TenantName = fRepLogTenantName(BUFFER MobCDR:HANDLE)
   mcdr.RepLog.KeyValue  = {HPD/keyvalue.i MobCDR . {&HPDKeyDelimiter} MsSeq DtlSeq DateSt} 
   .

&ENDIF