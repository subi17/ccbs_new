TRIGGER PROCEDURE FOR REPLICATION-DELETE OF SingleFee.

{HPD/HPDConst.i}

&IF {&SINGLEFEE_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW SingleFee
THEN RETURN.

IF SingleFee.HostTable NE "MobSub" OR SingleFee.Concerns[1] = 0 OR SingleFee.Concerns[1] = ?
THEN RETURN.

IF NOT CAN-FIND(FIRST MobSub NO-LOCK WHERE MobSub.MsSeq = INTEGER(SingleFee.KeyValue))
THEN RETURN.

{triggers/replog_tenantname.i}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "SingleFee"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.EventTime = NOW
   Common.RepLog.TenantName = fRepLogTenantName(BUFFER SingleFee:HANDLE)
   Common.RepLog.KeyValue  = {HPD/keyvalue.i SingleFee . {&HPDKeyDelimiter} FMItemId}
   .

&ENDIF