TRIGGER PROCEDURE FOR REPLICATION-WRITE OF SingleFee OLD BUFFER oldSingleFee.

{HPD/HPDConst.i}

&IF {&SINGLEFEE_WRITE_TRIGGER_ACTIVE} &THEN

/* If hosttable is not mobsub, we won't send the information */ 
IF SingleFee.HostTable NE "MobSub"
THEN RETURN.

{triggers/check_mobsub.i SingleFee KeyValue}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "SingleFee"
   Common.RepLog.EventType = (IF NEW(SingleFee)
                              THEN "CREATE"
                              ELSE IF llMobSubWasAvailable AND llMobSubIsAvailable = FALSE
                              THEN "DELETE"
                              ELSE "MODIFY")
   Common.RepLog.EventTime = NOW
   .

IF Common.RepLog.EventType = "DELETE" 
THEN Common.RepLog.KeyValue = {HPD/keyvalue.i SingleFee . {&HPDKeyDelimiter} FMItemId}.
ELSE Common.RepLog.RowID    = STRING(ROWID(SingleFee)).

IF NOT NEW(SingleFee)
THEN DO:
   DEFINE VARIABLE llSameValues AS LOGICAL NO-UNDO.

   BUFFER-COMPARE SingleFee USING
      FMItemId
   TO oldSingleFee SAVE RESULT IN llSameValues.

   IF NOT llSameValues
   THEN DO:
      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.TableName = "SingleFee"
         Common.RepLog.EventType = "DELETE"
         Common.RepLog.EventTime = NOW
         Common.RepLog.KeyValue  = {HPD/keyvalue.i oldSingleFee . {&HPDKeyDelimiter} FMItemId}
         .
   END.
END.

&ENDIF