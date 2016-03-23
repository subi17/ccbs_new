{HPD/HPDConst.i}

&IF {&MSERVICELPOOL_DELETE_TRIGGER_ACTIVE} &THEN

IF NEW MServiceLPool
THEN RETURN.

DEFINE BUFFER lbMServiceLPool FOR MServiceLPool.

/* We will send delete only the newest one */
FOR
   FIRST lbMServiceLPool FIELDS (MsSeq SLSeq EndTS) NO-LOCK USE-INDEX MsSeq WHERE
      lbMServiceLPool.MsSeq = MServiceLPool.MsSeq  AND
      lbMServiceLPool.SlSeq = MServiceLPool.SlSeq:

   IF lbMServiceLPool.EndTS > MServiceLPool.EndTS
   THEN RETURN.
END.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "MServiceLPool"
   Common.RepLog.EventType = "DELETE"
   Common.RepLog.EventTime = NOW
   Common.RepLog.KeyValue  = {HPD/keyvalue.i MServiceLPool . {&HPDKeyDelimiter} CustNum MsSeq SLSeq EndTS}
   .

&ENDIF