TRIGGER PROCEDURE FOR REPLICATION-DELETE OF MServiceLimit.

{HPD/HPDConst.i}

&IF {&MSERVICELIMIT_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW MServiceLimit
THEN RETURN.

{triggers/mservicelimit.i}

IF NOT fCheckHPDStatus(MServiceLimit.MsSeq,
                       MServiceLimit.CustNum,
                       MServiceLimit.EndTS)
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "MServiceLimit"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.EventTime = NOW
   Common.RepLog.KeyValue  = {HPD/keyvalue.i MServiceLimit . {&HPDKeyDelimiter} MSID}
   .

&ENDIF