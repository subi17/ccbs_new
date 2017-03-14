TRIGGER PROCEDURE FOR REPLICATION-DELETE OF PrepCDR.

{HPD/HPDConst.i}

&IF {&PREPCDR_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW PrepCDR
THEN RETURN.

/* If this is an error cdr we won't send the information */
IF PrepCDR.ErrorCode > 0
THEN RETURN.

{triggers/replog_tenantname.i}

CREATE mcdr.RepLog.
ASSIGN
   mcdr.RepLog.TableName = "PrepCDR"
   mcdr.RepLog.EventType = "DELETE"
   mcdr.RepLog.EventTime = NOW
   mcdr.RepLog.TenantName = fRepLogTenantName(BUFFER PrepCDR:HANDLE)
   mcdr.RepLog.KeyValue  = {HPD/keyvalue.i PrepCDR . {&HPDKeyDelimiter} MsSeq DtlSeq DateSt}
   .

&ENDIF