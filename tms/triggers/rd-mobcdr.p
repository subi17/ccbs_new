TRIGGER PROCEDURE FOR REPLICATION-DELETE OF MobCDR.

{HPD/HPDConst.i}

&IF {&MOBCDR_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW MobCDR
THEN RETURN.

/* If this is an error cdr we won't send the information */
IF MobCDR.ErrorCode > 0
THEN RETURN.

CREATE mcdr.RepLog.
ASSIGN
   mcdr.RepLog.TableName = "MobCDR"
   mcdr.RepLog.EventType = "DELETE"
   mcdr.RepLog.EventTime = NOW
   mcdr.RepLog.KeyValue  = {HPD/keyvalue.i MobCDR . {&HPDKeyDelimiter} MSCID DtlSeq DateSt} 
   .

&ENDIF