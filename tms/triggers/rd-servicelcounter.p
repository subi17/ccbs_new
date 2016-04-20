TRIGGER PROCEDURE FOR REPLICATION-DELETE OF ServiceLCounter.

{HPD/HPDConst.i}

&IF {&ServiceLCounter_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW ServiceLCounter
THEN RETURN.

LOOKUP(STRING(ServiceLCounter.SlSeq),"{&DSS_SLSEQS}") = 0
THEN RETURN.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "ServiceLCounter"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.EventTime = NOW
   Common.RepLog.KeyValue  = IF ServiceLCounter.MsSeq = 0
                             THEN {HPD/keyvalue.i ServiceLCounter . {&HPDKeyDelimiter} CustNum SlSeq Period}
                             ELSE {HPD/keyvalue.i ServiceLCounter . {&HPDKeyDelimiter} MsSeq SlSeq Period}
   .

&ENDIF