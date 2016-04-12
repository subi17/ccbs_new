TRIGGER PROCEDURE FOR REPLICATION-WRITE OF SingleFee OLD BUFFER oldSingleFee.

{HPD/HPDConst.i}

&IF {&SINGLEFEE_WRITE_TRIGGER_ACTIVE} &THEN

/* If hosttable is not mobsub, we won't send the information */ 
IF NEW(SingleFee) AND (SingleFee.Concerns[1] = 0 OR SingleFee.Concerns[1] = ? OR SingleFee.HostTable NE "MobSub") 
THEN RETURN.

IF NOT NEW(SingleFee) AND
   ( ( SingleFee.HostTable NE "MobSub" AND oldSingleFee.HostTable NE "MobSub" ) OR
     NOT ( SingleFee.Concerns[1] > 0 AND oldSingleFee.Concerns[1] > 0 ) )
THEN RETURN.

{triggers/check_mobsub.i SingleFee KeyValue}

CREATE Common.RepLog.
ASSIGN
   Common.RepLog.TableName = "SingleFee"
   Common.RepLog.EventType = (IF NEW(SingleFee)
                              THEN "CREATE"
                              ELSE IF (llMobSubWasAvailable AND llMobSubIsAvailable = FALSE) OR 
                                      ( (SingleFee.Concerns[1] = 0 OR SingleFee.Concerns[1] = ?) AND
                                         oldSingleFee.Concerns[1] > 0 ) OR
                                      (SingleFee.HostTable NE "MobSub" AND oldSingleFee.HostTable = "MobSub")
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