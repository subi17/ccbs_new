TRIGGER PROCEDURE FOR REPLICATION-DELETE OF PrepEDR.

{HPD/HPDConst.i}

&IF {&PREPEDR_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW PrepEDR
THEN RETURN.

/* If this is an error cdr we won't send the information */
IF PrepEDR.ErrorCode > 0
THEN RETURN.

CREATE mcdr.RepLog.
ASSIGN
   mcdr.RepLog.TableName = "PrepEDR"
   mcdr.RepLog.EventType = "DELETE"
   mcdr.RepLog.EventTime = NOW
   mcdr.RepLog.KeyValue  = {HPD/keyvalue.i PrepEDR . {&HPDKeyDelimiter} MsSeq DtlSeq DateSt}
   .

&ENDIF