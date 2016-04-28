TRIGGER PROCEDURE FOR REPLICATION-DELETE OF ServPac.

{HPD/HPDConst.i}

&IF {&SERVPAC_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW ServPac
THEN RETURN.

IF ServPac.Brand NE "1"
THEN RETURN.

CREATE mobile.RepLog.
ASSIGN
   mobile.RepLog.TableName = "ServPac"
   mobile.RepLog.EventType = "DELETE"
   mobile.RepLog.EventTime = NOW
   mobile.RepLog.KeyValue  = {HPD/keyvalue.i ServPac . {&HPDKeyDelimiter} ServPac} 
   .

&ENDIF