TRIGGER PROCEDURE FOR REPLICATION-DELETE OF MServiceLPool.

{HPD/HPDConst.i}

&IF {&MSERVICELPOOL_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW MServiceLPool
THEN RETURN.

{triggers/mservicelpool.i}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "MServiceLPool"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.EventTime = NOW
   Common.RepLog.KeyValue  = {HPD/keyvalue.i MServiceLPool . {&HPDKeyDelimiter} CustNum MsSeq SLSeq EndTS}
   .

&ENDIF