TRIGGER PROCEDURE FOR REPLICATION-DELETE OF MServiceLimit.

{HPD/HPDConst.i}

&IF {&MSERVICELIMIT_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW MServiceLimit
THEN RETURN.

IF MServiceLimit.MSID = 0 OR MServiceLimit.MSID = ?
THEN RETURN.

{triggers/mservicelimit.i}

IF NOT fCheckHPDStatus(MServiceLimit.MsSeq,
                       MServiceLimit.CustNum,
                       MServiceLimit.EndTS)
THEN RETURN.

{triggers/replog_tenantname.i}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName  = "MServiceLimit"
   Common.RepLog.EventType  = "DELETE"
   Common.RepLog.EventTime  = NOW
   Common.RepLog.TenantName = fRepLogTenantName(BUFFER MServiceLimit:HANDLE)
   Common.RepLog.KeyValue  = {HPD/keyvalue.i MServiceLimit . {&HPDKeyDelimiter} MSID}
   .

&ENDIF
