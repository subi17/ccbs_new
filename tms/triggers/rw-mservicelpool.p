TRIGGER PROCEDURE FOR REPLICATION-WRITE OF MServiceLPool OLD BUFFER oldMServiceLPool.

{HPD/HPDConst.i}

&IF {&MSERVICELPOOL_WRITE_TRIGGER_ACTIVE} &THEN

{triggers/check_mobsub.i MServiceLPool MsSeq}

{triggers/mservicelpool.i}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "MServiceLPool"
   Common.RepLog.EventType = (IF NEW(MServiceLPool)
                              THEN "CREATE"
                              ELSE IF llMobSubWasAvailable AND llMobSubIsAvailable = FALSE
                              THEN "DELETE"
                              ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF Common.RepLog.EventType = "DELETE" 
THEN Common.RepLog.KeyValue = {HPD/keyvalue.i MServiceLPool . {&HPDKeyDelimiter} CustNum MsSeq SLSeq EndTS}.
ELSE Common.RepLog.RowID    = STRING(ROWID(MServiceLPool)).

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