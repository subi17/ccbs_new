TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MServiceLPool OLD BUFFER oldMServiceLPool.

{HPD/HPDConst.i}

&IF {&MSERVICELPOOL_WRITE_TRIGGER_ACTIVE} &THEN

DEFINE BUFFER lbMServiceLPool FOR MServiceLPool.

/* We will send only the newest one */
FOR
   FIRST lbMServiceLPool FIELDS (MsSeq SLSeq EndTS) NO-LOCK USE-INDEX MsSeq WHERE
      lbMServiceLPool.MsSeq = MServiceLPool.MsSeq  AND
      lbMServiceLPool.SlSeq = MServiceLPool.SlSeq:

   IF lbMServiceLPool.EndTS > MServiceLPool.EndTS
   THEN RETURN.
END.

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.RowID     = STRING(ROWID(MServiceLPool))
   Common.RepLog.TableName = "MServiceLPool"
   Common.RepLog.EventType = (IF NEW(MServiceLPool)
                               THEN "CREATE"
                               ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF NOT NEW(MServiceLPool)
THEN DO:
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE MServiceLPool USING
      CustNum MsSeq SLSeq
   TO oldMServiceLPool SAVE RESULT IN llSameValues.

   IF NOT llSameValues
   THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName = "MServiceLPool"
         Common.RepLog.EventType = "DELETE"
         Common.RepLog.EventTime = NOW
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldMServiceLPool . {&HPDKeyDelimiter} CustNum MsSeq SLSeq EndTS}
         .
   END.
END.

&ENDIF