TRIGGER PROCEDURE FOR REPLICATION-DELETE OF SingleFee.

{HPD/HPDConst.i}

&IF {&SINGLEFEE_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW SingleFee
THEN RETURN.

IF SingleFee.HostTable NE "MobSub"
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "SingleFee"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.EventTime = NOW
   Common.RepLog.KeyValue  = {HPD/keyvalue.i SingleFee . {&HPDKeyDelimiter} FMItemId}
   .

&ENDIF