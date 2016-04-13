TRIGGER PROCEDURE FOR REPLICATION-DELETE OF FixedFee.

{HPD/HPDConst.i}

&IF {&FIXEDFEE_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW FixedFee
THEN RETURN.

IF FixedFee.HostTable NE "MobSub"
THEN RETURN.

IF NOT CAN-FIND(FIRST MobSub NO-LOCK WHERE MobSub.MsSeq = INTEGER(FixedFee.KeyValue))
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "FixedFee"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.EventTime = NOW
   Common.RepLog.KeyValue  = {HPD/keyvalue.i FixedFee . {&HPDKeyDelimiter} FFNum}
   .

&ENDIF