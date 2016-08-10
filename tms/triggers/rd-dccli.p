TRIGGER PROCEDURE FOR REPLICATION-DELETE OF DCCLI.

{HPD/HPDConst.i}

&IF {&DCCLI_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW DCCLI
THEN RETURN.

CREATE Mobile.RepLog.
ASSIGN
   Mobile.RepLog.TableName = "DCCLI"
   Mobile.RepLog.EventType = "DELETE"
   Mobile.RepLog.EventTime = NOW
   Mobile.RepLog.KeyValue  = {HPD/keyvalue.i DCCLI . {&HPDKeyDelimiter} PerContractID} 
   .

&ENDIF
